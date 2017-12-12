//! Phases of our custom derive compiler, and passes that perform phase changes.

use ast::{CollectIdents, StateMachine};
use darling;
use heck::SnakeCase;
use petgraph;
use petgraph::algo::has_path_connecting;
use quote;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;
use syn;

// Create a dummy `FromMetaItem` implementation for the given type. This is only
// used because the way that `darling` emits bounds on generic items forces all
// our extra state to implement a bunch of things that it won't ever use.
macro_rules! dummy_from_meta_item {
    ( $t:ty ) => {
        impl darling::FromMetaItem for $t {}
    }
}

// Same as above.
macro_rules! dummy_default {
    ( $t:ty ) => {
        impl Default for $t {
            fn default() -> $t {
                panic!("dummy implementation")
            }
        }
    }
}

/// A phase represents a state in the pipeline, and the extra data we've
/// accumulated up to this point.
pub trait Phase: fmt::Debug + darling::FromMetaItem {
    /// Extra data accumulated on the `StateMachine` at this phase.
    type StateMachineExtra: Default + fmt::Debug + darling::FromMetaItem;

    /// Extra data accumulated on each `State` in the `StateMachine` at this
    /// phase.
    type StateExtra: Default + fmt::Debug + darling::FromMetaItem;
}

/// A special phase representing the lack of a phase.
///
/// This phase is used when we have split the state machine from its accumulated
/// phase data and are operating on them independently.
///
/// See `State::split` and `StateMachine::split` for usage.
#[derive(FromMetaItem, Debug)]
pub struct NoPhase;

impl Phase for NoPhase {
    type StateMachineExtra = ();
    type StateExtra = ();
}

/// A pass is a single phase-to-phase translation step in our pipeline.
///
/// When implementing the `From -> To` pass, this trait is implemented for `To`
/// and this trait's associated type would be `From`.
pub trait Pass: Phase {
    /// The current phase we are creating the next phase from.
    type FromPhase: Phase;

    /// The function to translate between these phases.
    fn pass(StateMachine<Self::FromPhase>) -> StateMachine<Self>;
}

/// The state machine AST has been parsed from the custom derive input.
#[derive(FromMetaItem, Debug)]
pub struct Parsed;

impl Phase for Parsed {
    type StateMachineExtra = ();
    type StateExtra = ();
}

/// We've found the indices into `states` for the unique start, ready, and error
/// states.
#[derive(FromMetaItem, Debug, Default)]
pub struct StartReadyError {
    pub start: usize,
    pub ready: usize,
    pub error: usize,
}

impl Phase for StartReadyError {
    type StateMachineExtra = Self;
    type StateExtra = ();
}

impl Pass for StartReadyError {
    type FromPhase = Parsed;

    fn pass(machine: StateMachine<Parsed>) -> StateMachine<StartReadyError> {
        machine.and_then(|machine, (), states| {
            let mut start = None;
            let mut ready = None;
            let mut error = None;

            let states = states
                .into_iter()
                .enumerate()
                .map(|(idx, state)| {
                    if state.start {
                        assert!(start.is_none(), "There must only be a single `start` state");
                        start = Some(idx);
                    }

                    if state.ready {
                        assert!(ready.is_none(), "There must only be a single `ready` state");
                        assert!(
                            state.data.style.is_tuple(),
                            "The `ready` state must be a tuple variant, for example: `Ready(Item)`"
                        );
                        assert_eq!(
                            state.data.fields.len(),
                            1,
                            "The `ready` state must only have one field, for example: `Ready(Item)`"
                        );
                        assert!(
                            state.transitions.is_empty(),
                            "The `ready` state must not transition to any other states"
                        );
                        ready = Some(idx);
                    }

                    if state.error {
                        assert!(error.is_none(), "There must only be a single `error` state");
                        assert!(
                            state.data.style.is_tuple(),
                            "The `error` state must be a tuple variant, for example: `Error(Item)`"
                        );
                        assert_eq!(
                            state.data.fields.len(),
                            1,
                            "The `error` state must only have one field, for example: `Error(Item)`"
                        );
                        assert!(
                            state.transitions.is_empty(),
                            "The `error` state must not transition to any other states"
                        );
                        error = Some(idx);
                    }

                    if !state.ready && !state.error {
                        assert!(
                            !state.transitions.is_empty(),
                            "Non-{ready,error} states must have transitions"
                        );
                    }

                    state.and_then(|state, ()| state.join(()))
                })
                .collect();

            let start = start.expect("Must specify one `start` state");
            let ready = ready.expect("Must specify one `ready` state");
            let error = error.expect("Must specify one `error` state");

            machine.join(
                StartReadyError {
                    start,
                    ready,
                    error,
                },
                states,
            )
        })
    }
}

/// A phase after which we know that all transitions are to valid states. That
/// is, we will never get any "cannot find type `UnknownState` in this scope"
/// compilation errors from any code we emit.
#[derive(FromMetaItem, Debug)]
pub struct ValidTransitionEdges;

impl Phase for ValidTransitionEdges {
    type StateMachineExtra = <StartReadyError as Phase>::StateMachineExtra;
    type StateExtra = ();
}

impl Pass for ValidTransitionEdges {
    type FromPhase = StartReadyError;

    fn pass(machine: StateMachine<StartReadyError>) -> StateMachine<ValidTransitionEdges> {
        machine.and_then(|machine, extra, states| {
            let state_idents: HashSet<syn::Ident> =
                HashSet::from_iter(states.iter().map(|s| s.ident.clone()));

            let states = states
                .into_iter()
                .map(|state| {
                    state.and_then(|s, ()| {
                        s.transitions.iter().for_each(|t| {
                            assert!(
                                state_idents.contains(t),
                                "Transition to unknown state `{}` from state `{}`",
                                t.to_string(),
                                s.ident.to_string()
                            );
                        });
                        s.join(())
                    })
                })
                .collect();

            machine.join(extra, states)
        })
    }
}

/// All paths through the state machine's states lead to the ready or error
/// state, and no intermediate state is unreachable.
#[derive(FromMetaItem, Debug)]
pub struct ValidPaths;

impl Phase for ValidPaths {
    type StateMachineExtra = <ValidTransitionEdges as Phase>::StateMachineExtra;
    type StateExtra = ();
}

impl Pass for ValidPaths {
    type FromPhase = ValidTransitionEdges;

    fn pass(machine: StateMachine<ValidTransitionEdges>) -> StateMachine<ValidPaths> {
        machine.and_then(|machine, extra, states| {
            let mut nodes: HashMap<String, petgraph::graph::NodeIndex<_>> = HashMap::new();
            let mut graph: petgraph::Graph<String, ()> = petgraph::Graph::new();

            // First, create a node for each state and insert it into `nodes`.
            states.iter().for_each(|s| {
                let s = s.ident.to_string();
                nodes.insert(s.clone(), graph.add_node(s));
            });

            // Second, construct the edges between states.
            graph.extend_with_edges(states.iter().flat_map(|s| {
                let s_name = s.ident.to_string();
                s.transitions
                    .iter()
                    .map(|t| {
                        let t = t.to_string();
                        let from = nodes[&s_name];
                        let to = nodes[&t];
                        (from, to)
                    })
                    .collect::<Vec<_>>()
            }));

            let start_name = states[extra.start].ident.to_string();
            let ready_name = states[extra.ready].ident.to_string();
            let error_name = states[extra.error].ident.to_string();
            let start = nodes[&start_name];
            let ready = nodes[&ready_name];
            let error = nodes[&error_name];

            // Check that every non-final state is
            //
            // 1. Reachable from the start state, or is the start state, and
            // 2. Has a path leading to a final state (ready or error).
            //
            // TODO: This would be a lot more efficient if we didn't throw away
            // the incremental results from each of these queries... But that
            // means not using `has_path_connecting` and rolling our own thing,
            // which is more work than I want to do this moment.
            let mut dfs_space = petgraph::algo::DfsSpace::new(&graph);
            states
                .iter()
                .filter(|s| !s.ready && !s.error)
                .for_each(|s| {
                    let s_name = s.ident.to_string();
                    let s_node = nodes[&s_name];
                    assert!(
                        has_path_connecting(&graph, s_node, ready, Some(&mut dfs_space))
                            || has_path_connecting(&graph, s_node, error, Some(&mut dfs_space)),
                        "The `{}` state must have a transition path to either the ready \
                         state (`{}`) or the error state (`{}`) but it does not",
                        s_name,
                        ready_name,
                        error_name
                    );

                    assert!(
                        s.start || has_path_connecting(&graph, start, s_node, Some(&mut dfs_space)),
                        "The `{}` state must be reachable from the start state (`{}`) but \
                         it is not",
                        s_name,
                        start_name
                    );
                });

            let states = states
                .into_iter()
                .map(|state| state.and_then(|s, ()| s.join(())))
                .collect();

            machine.join(extra, states)
        })
    }
}

/// Builds the generics for all states, based on the generics of the state machine.
#[derive(FromMetaItem, Debug)]
pub struct StateGenerics;

dummy_default!(StateGenerics);

#[derive(Debug)]
pub struct StateGenericsExtra {
    pub generics: Rc<syn::Generics>,
}

dummy_default!(StateGenericsExtra);
dummy_from_meta_item!(StateGenericsExtra);

impl Phase for StateGenerics {
    type StateMachineExtra = <ValidPaths as Phase>::StateMachineExtra;
    type StateExtra = StateGenericsExtra;
}

impl Pass for StateGenerics {
    type FromPhase = ValidPaths;

    fn pass(machine: StateMachine<ValidPaths>) -> StateMachine<StateGenerics> {
        // For each parameter(type parameters, lifetimes or where predicates of the generic
        // arguments), collect the set of bounds it participates in and the set of all idents used
        // by those bounds. E.g. `MyClass<T: Clone>` for `T` generates (`T: Clone`, `T`, `Clone`).
        // The `get_bound` function takes a generic parameter and returns the applicable bounds as
        // a `R`.
        fn prepare_generic_params<'a, T, F, R>(
            params: &'a [T],
            get_bound: F,
        ) -> Vec<(&'a T, R, HashSet<syn::Ident>)>
        where
            F: Fn(&'a T) -> R,
            T: CollectIdents,
        {
            params
                .iter()
                .map(|p| {
                    let mut idents = HashSet::new();
                    p.collect_idents(&mut idents);

                    (p, get_bound(p), idents)
                })
                .collect::<Vec<_>>()
        }

        // Checks if a generic parameter is part of the `state_idents` (the idents used by the
        // state) by calling the `contains_bound` function. If `contains_bound` returns true, all
        // idents that were collected for this generic parameter are added to the `state_idents`.
        // For example `<T: Clone>`, adds ident 'Clone' to `state_idents`. Or, `<C, T = C>` for `T`,
        // adds ident 'C' to `state_idents`.
        fn extend_state_idents<R, S, F>(
            state_idents: &mut HashSet<syn::Ident>,
            params: &[(R, S, HashSet<syn::Ident>)],
            contains_bound: F,
        ) where
            F: Fn(&HashSet<syn::Ident>, &S) -> bool,
        {
            let mut iter = params.iter();
            while let Some(&(_, ref bound, ref idents)) = iter.next() {
                if contains_bound(state_idents, bound) {
                    let old_len = state_idents.len();
                    state_idents.extend(idents.iter().cloned());

                    // When new elements are added, start from the beginning.
                    // The current generic_param could have added an ident that belongs to a
                    // generic_param before this generic_param. So, by starting at the beginning,
                    // we ensure that all generic_params are added.
                    assert!(old_len <= state_idents.len());
                    if old_len != state_idents.len() {
                        iter = params.iter();
                        continue;
                    }
                }
            }
        }

        machine.and_then(|machine, extra, states| {
            let states = {
                let mgenerics = &machine.generics;

                // We begin with preparing all params of the machine generics
                let ty_params = prepare_generic_params(&mgenerics.ty_params, |t| &t.ident);
                let lifetimes = prepare_generic_params(&mgenerics.lifetimes, |l| &l.lifetime.ident);
                let where_preds = prepare_generic_params(&mgenerics.where_clause.predicates, |w| {
                    let mut bound_idents = HashSet::new();
                    match w {
                        &syn::WherePredicate::BoundPredicate(ref bound) => {
                            bound.bounded_ty.collect_idents(&mut bound_idents)
                        }
                        &syn::WherePredicate::EqPredicate(ref eq) => {
                            eq.lhs_ty.collect_idents(&mut bound_idents)
                        }
                        &syn::WherePredicate::RegionPredicate(ref region) => {
                            bound_idents.insert(region.lifetime.ident.clone());
                        }
                    };
                    bound_idents
                });

                states
                    .into_iter()
                    .map(|state| {
                        state.and_then(|state, ()| {
                            // Collect all the idents of the state
                            let mut state_idents = HashSet::new();
                            state
                                .data
                                .fields
                                .iter()
                                .for_each(|f| f.ty.collect_idents(&mut state_idents));

                            // Begin with the where predicates
                            extend_state_idents(
                                &mut state_idents,
                                where_preds.as_slice(),
                                |state_idents, bounds| {
                                    bounds.iter().any(|b| state_idents.contains(b))
                                },
                            );
                            // Then the ty_params
                            extend_state_idents(
                                &mut state_idents,
                                ty_params.as_slice(),
                                |state_idents, bound| state_idents.contains(bound),
                            );
                            // And the lifetimes lastly
                            extend_state_idents(
                                &mut state_idents,
                                lifetimes.as_slice(),
                                |state_idents, bound| state_idents.contains(bound),
                            );

                            // After we have ALL important idents, we filter out all not necessary
                            // params
                            let where_preds = where_preds
                                .iter()
                                .filter(|&&(_, ref bounds, _)| {
                                    bounds.iter().any(|b| state_idents.contains(b))
                                })
                                .map(|v| v.0)
                                .cloned()
                                .collect();

                            let ty_params = ty_params
                                .iter()
                                .filter(|&&(_, ref bound, _)| state_idents.contains(bound))
                                .map(|v| v.0)
                                .cloned()
                                .collect();

                            let lifetimes = lifetimes
                                .iter()
                                .filter(|&&(_, ref bound, _)| state_idents.contains(bound))
                                .map(|v| v.0)
                                .cloned()
                                .collect();

                            let generics = Rc::new(syn::Generics {
                                lifetimes,
                                ty_params,
                                where_clause: syn::WhereClause {
                                    predicates: where_preds,
                                },
                            });

                            state.join(StateGenericsExtra { generics })
                        })
                    })
                    .collect()
            };

            machine.join(extra, states)
        })
    }
}

/// This state builds the generic parameters for the after state enums.
#[derive(FromMetaItem, Debug)]
pub struct AfterStateGenerics;

dummy_default!(AfterStateGenerics);

#[derive(Debug)]
pub struct AfterStateGenericsExtra {
    /// The generics for the state.
    pub generics: Rc<syn::Generics>,
    /// The generics for the after state enum.
    pub after_state_generics: Rc<syn::Generics>,
    /// The generics of the transition states.
    pub transition_state_generics: HashMap<syn::Ident, Rc<syn::Generics>>,
}

dummy_default!(AfterStateGenericsExtra);
dummy_from_meta_item!(AfterStateGenericsExtra);

impl Phase for AfterStateGenerics {
    type StateMachineExtra = <StateGenerics as Phase>::StateMachineExtra;
    type StateExtra = AfterStateGenericsExtra;
}

impl Pass for AfterStateGenerics {
    type FromPhase = StateGenerics;

    fn pass(machine: StateMachine<StateGenerics>) -> StateMachine<AfterStateGenerics> {
        machine.and_then(|machine, extra, states| {
            let states = {
                let mgenerics = &machine.generics;

                // Build a HashMap that maps from ident to generics
                let ident_to_generics = states
                    .iter()
                    .map(|s| (s.ident.clone(), s.extra.generics.clone()))
                    .collect::<HashMap<_, _>>();

                states
                    .into_iter()
                    .map(|state| {
                        state.and_then(|state, extra| {
                            // Filter all generic_params in the order they appear in the machine
                            // generics.
                            let lifetimes = mgenerics
                                .lifetimes
                                .iter()
                                .filter(|l| {
                                    state.transitions.iter().any(|ident| {
                                        ident_to_generics
                                            .get(ident)
                                            .map(|v| v.lifetimes.contains(l))
                                            .unwrap_or(false)
                                    })
                                })
                                .cloned()
                                .collect::<Vec<_>>();

                            let ty_params = mgenerics
                                .ty_params
                                .iter()
                                .filter(|t| {
                                    state.transitions.iter().any(|ident| {
                                        ident_to_generics
                                            .get(ident)
                                            .map(|v| v.ty_params.contains(t))
                                            .unwrap_or(false)
                                    })
                                })
                                .cloned()
                                .collect::<Vec<_>>();

                            let where_preds = mgenerics
                                .where_clause
                                .predicates
                                .iter()
                                .filter(|p| {
                                    state.transitions.iter().any(|ident| {
                                        ident_to_generics
                                            .get(ident)
                                            .map(|v| v.where_clause.predicates.contains(p))
                                            .unwrap_or(false)
                                    })
                                })
                                .cloned()
                                .collect::<Vec<_>>();

                            let after_state_generics = Rc::new(syn::Generics {
                                lifetimes,
                                ty_params,
                                where_clause: syn::WhereClause {
                                    predicates: where_preds,
                                },
                            });

                            let transition_state_generics = ident_to_generics
                                .iter()
                                .filter(|&(ident, _)| state.transitions.contains(ident))
                                .map(|(ref ident, ref generics)| {
                                    ((*ident).clone(), (*generics).clone())
                                })
                                .collect::<HashMap<_, _>>();

                            state.join(AfterStateGenericsExtra {
                                generics: extra.generics,
                                after_state_generics,
                                transition_state_generics,
                            })
                        })
                    })
                    .collect()
            };

            machine.join(extra, states)
        })
    }
}

/// The final state, where we have computed everything required for codegen.
#[derive(Debug)]
pub struct ReadyForCodegen {
    pub start: usize,
    pub ready: usize,
    pub error: usize,
    pub states_enum: Rc<quote::Ident>,
    pub poll_trait: Rc<quote::Ident>,
    pub futures_crate: Rc<quote::Ident>,
    pub smf_crate: Rc<quote::Ident>,
}

dummy_default!(ReadyForCodegen);
dummy_from_meta_item!(ReadyForCodegen);

#[derive(Debug)]
pub struct CodegenStateExtra {
    pub vis: Rc<syn::Visibility>,
    pub description_ident: Rc<syn::Ident>,
    pub states_enum: Rc<quote::Ident>,
    pub error_type: Rc<syn::Ty>,
    pub error_ident: Rc<syn::Ident>,
    pub after: quote::Ident,
    pub derive: Rc<darling::util::IdentList>,
    pub poll_trait: Rc<quote::Ident>,
    pub poll_method: quote::Ident,
    pub futures_crate: Rc<quote::Ident>,
    pub smf_crate: Rc<quote::Ident>,
    pub generics: Rc<syn::Generics>,
    pub after_state_generics: Rc<syn::Generics>,
    pub transition_state_generics: HashMap<syn::Ident, Rc<syn::Generics>>,
}

dummy_from_meta_item!(CodegenStateExtra);
dummy_default!(CodegenStateExtra);

impl Phase for ReadyForCodegen {
    type StateMachineExtra = Self;
    type StateExtra = CodegenStateExtra;
}

impl Pass for ReadyForCodegen {
    type FromPhase = AfterStateGenerics;

    fn pass(machine: StateMachine<AfterStateGenerics>) -> StateMachine<ReadyForCodegen> {
        machine.and_then(|machine, extra, states| {
            let StartReadyError {
                start,
                ready,
                error,
            } = extra;

            let vis = Rc::new(machine.vis.clone());

            let description_ident = Rc::new(machine.ident.clone());

            let error_ident = states[error].ident.clone();
            let error_ident = Rc::new(error_ident);

            let error_type = states[error].data.fields[0].ty.clone();
            let error_type = Rc::new(error_type);

            let derive = Rc::new(machine.derive.clone());

            let machine_name = machine.ident.to_string();

            let mut states_enum = machine_name.clone();
            states_enum += "States";
            let states_enum = Rc::new(quote::Ident::new(states_enum));

            let mut poll_trait = String::from("Poll");
            poll_trait += &machine_name;
            let poll_trait = Rc::new(quote::Ident::new(poll_trait));

            let mut futures_crate = String::from("__smf_");
            futures_crate += machine_name.clone().to_snake_case().as_str();
            futures_crate += "_futures";
            let futures_crate = Rc::new(quote::Ident::new(futures_crate));

            let mut smf_crate = String::from("__smf_");
            smf_crate += machine_name.clone().to_snake_case().as_str();
            smf_crate += "_state_machine_future";
            let smf_crate = Rc::new(quote::Ident::new(smf_crate));

            let states = states
                .into_iter()
                .map(|state| {
                    state.and_then(|state, extra| {
                        let vis = vis.clone();
                        let description_ident = description_ident.clone();
                        let error_ident = error_ident.clone();
                        let error_type = error_type.clone();
                        let generics = extra.generics.clone();
                        let after_state_generics = extra.after_state_generics.clone();
                        let transition_state_generics = extra.transition_state_generics.clone();
                        let derive = derive.clone();
                        let states_enum = states_enum.clone();
                        let poll_trait = poll_trait.clone();
                        let futures_crate = futures_crate.clone();
                        let smf_crate = smf_crate.clone();

                        let ident_name = state.ident.to_string();

                        let mut after = String::from("After");
                        after.push_str(&ident_name);
                        let after = quote::Ident::new(after);

                        let mut poll_method = String::from("poll_");
                        poll_method.push_str(&ident_name.to_snake_case());
                        let poll_method = quote::Ident::new(poll_method);

                        state.join(CodegenStateExtra {
                            vis,
                            description_ident,
                            states_enum,
                            error_ident,
                            error_type,
                            after,
                            derive,
                            poll_trait,
                            poll_method,
                            futures_crate,
                            smf_crate,
                            generics,
                            after_state_generics,
                            transition_state_generics,
                        })
                    })
                })
                .collect();

            machine.join(
                ReadyForCodegen {
                    start,
                    ready,
                    error,
                    states_enum,
                    poll_trait,
                    futures_crate,
                    smf_crate,
                },
                states,
            )
        })
    }
}
