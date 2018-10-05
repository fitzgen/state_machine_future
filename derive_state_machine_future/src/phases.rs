//! Phases of our custom derive compiler, and passes that perform phase changes.

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{BuildHasher, Hash};
use std::iter::FromIterator;
use std::rc::Rc;

use darling;
use darling::usage::{
    CollectLifetimes, CollectTypeParams,
    GenericsExt, IdentRefSet, LifetimeRefSet, Purpose, UsesTypeParams,
};
use heck::SnakeCase;
use petgraph;
use petgraph::algo::has_path_connecting;
use proc_macro2::{Ident, Span};
use syn;

use ast::StateMachine;

// Create a dummy `FromMeta` implementation for the given type. This is only
// used because the way that `darling` emits bounds on generic items forces all
// our extra state to implement a bunch of things that it won't ever use.
macro_rules! dummy_from_meta {
    ( $t:ty ) => {
        impl darling::FromMeta for $t {}
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
pub trait Phase: fmt::Debug + darling::FromMeta {
    /// Extra data accumulated on the `StateMachine` at this phase.
    type StateMachineExtra: Default + fmt::Debug + darling::FromMeta;

    /// Extra data accumulated on each `State` in the `StateMachine` at this
    /// phase.
    type StateExtra: Default + fmt::Debug + darling::FromMeta;
}

/// A special phase representing the lack of a phase.
///
/// This phase is used when we have split the state machine from its accumulated
/// phase data and are operating on them independently.
///
/// See `State::split` and `StateMachine::split` for usage.
#[derive(FromMeta, Debug)]
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
#[derive(FromMeta, Debug)]
pub struct Parsed;

impl Phase for Parsed {
    type StateMachineExtra = ();
    type StateExtra = ();
}

/// We've found the indices into `states` for the unique start, ready, and error
/// states.
#[derive(FromMeta, Debug, Default)]
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
                            state.fields.style.is_tuple(),
                            "The `ready` state must be a tuple variant, for example: `Ready(Item)`"
                        );
                        assert_eq!(
                            state.fields.fields.len(),
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
                            state.fields.style.is_tuple(),
                            "The `error` state must be a tuple variant, for example: `Error(Item)`"
                        );
                        assert_eq!(
                            state.fields.fields.len(),
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
#[derive(FromMeta, Debug)]
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
#[derive(FromMeta, Debug)]
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
#[derive(FromMeta, Debug)]
pub struct StateGenerics;

dummy_default!(StateGenerics);

#[derive(Debug)]
pub struct StateGenericsExtra {
    pub generics: Rc<syn::Generics>,
}

dummy_default!(StateGenericsExtra);
dummy_from_meta!(StateGenericsExtra);

impl Phase for StateGenerics {
    type StateMachineExtra = <ValidPaths as Phase>::StateMachineExtra;
    type StateExtra = StateGenericsExtra;
}

impl Pass for StateGenerics {
    type FromPhase = ValidPaths;

    fn pass(machine: StateMachine<ValidPaths>) -> StateMachine<StateGenerics> {
        machine.and_then(|machine, extra, states| {
            let states = {
                let mgenerics = &machine.generics;

                // Return a `HashMap` of keys mapped to `Vec`s of values. Keys and values
                // are taken from `(Key, Value)` tuple pairs yielded by the input iterator.
                fn into_group_map<I, K, V, S>(iter: I) -> HashMap<K, Vec<V>, S>
                where
                    I: Iterator<Item = (K, V)>,
                    K: Hash + Eq,
                    S: BuildHasher + Default,
                {
                    let mut lookup = HashMap::default();

                    for (key, val) in iter {
                        lookup.entry(key).or_insert_with(Vec::new).push(val);
                    }

                    lookup
                }

                // Return a `HashMap` of keys mapped to `Vec`s of values. Keys and values
                // are taken from `(IntoIterator<Item = Key>, Value)` tuple pairs yielded by the
                // input iterator.
                fn into_group_map_multikey<I, J, K, V, S>(iter: I) -> HashMap<K, Vec<V>, S>
                where
                    I: Iterator<Item = (J, V)>,
                    J: IntoIterator<Item = K>,
                    K: Hash + Eq,
                    V: Clone,
                    S: BuildHasher + Default,
                {
                    let mut lookup = HashMap::default();

                    for (keys, val) in iter {
                        for key in keys {
                            lookup.entry(key).or_insert_with(Vec::new).push(val.clone());
                        }
                    }

                    lookup
                }

                let options = Purpose::Declare.into();
                let declared_lifetimes = mgenerics.declared_lifetimes();
                let declared_type_params = mgenerics.declared_type_params();

                // Collect declared lifetimes with their definitions.
                //
                // E.g. for `'a: 'b + 'c` the hash map wil have:
                // * `'a` as key;
                // * `'a: 'b + 'c` as value.
                let lifetime_defs = mgenerics
                    .lifetimes()
                    .map(|ld| (&ld.lifetime, ld))
                    .collect::<HashMap<_, _>>();

                // Collect declared type parameter identifiers with their definitions.
                //
                // E.g. for `T: 'r + fmt::Debug = String` the hash map will have:
                // * `T` as key;
                // * `T: 'r + fmt::Debug = String` as value.
                let type_param_defs = mgenerics
                    .type_params()
                    .map(|tp| (&tp.ident, tp))
                    .collect::<HashMap<_, _>>();

                // Collect lifetimes in where clauses with respective lifetime predicates.
                //
                // E.g. for `'a: 'b + 'c` the hash map wil have:
                // * `'a` as key;
                // * `'a: 'b + 'c` as value.
                let where_clause_lifetimes = mgenerics.where_clause.as_ref().map(|where_clause| {
                    into_group_map(where_clause.predicates.iter().filter_map(|predicate| {
                        match *predicate {
                            syn::WherePredicate::Type(_) => None,
                            syn::WherePredicate::Lifetime(ref lifetime_def) => {
                                Some((&lifetime_def.lifetime, predicate))
                            },
                            syn::WherePredicate::Eq(_) => None,
                        }
                    }))
                }).unwrap_or_else(HashMap::new);

                // Collect type parameter identifiers in where clauses with respective type and
                // equality predicates.
                //
                // E.g. for `for<'c> Foo<'c, P>: Trait<'c, P>` the hash map will have:
                // * `{Foo, P}` as key;
                // * `for<'c> Foo<'c, P>: Trait<'c, P>` as value.
                //
                // Or e.g. for `X = Bar` the hash map will have:
                // * `X` as key;
                // * `X = Bar` as value.
                let where_clause_types = mgenerics.where_clause.as_ref().map(|where_clause| {
                    let declared_type_params_ref = &declared_type_params;
                    into_group_map_multikey(where_clause.predicates.iter().filter_map(|predicate| {
                        match *predicate {
                            syn::WherePredicate::Type(ref type_param) => {
                                let idents = type_param.bounded_ty
                                    .uses_type_params(&options, declared_type_params_ref);
                                Some((idents, predicate))
                            },
                            syn::WherePredicate::Lifetime(_) => None,
                            syn::WherePredicate::Eq(ref eq) => {
                                let idents = eq.lhs_ty
                                    .uses_type_params(&options, declared_type_params_ref);
                                Some((idents, predicate))
                            },
                        }
                    }))
                }).unwrap_or_else(HashMap::new);

                states
                    .into_iter()
                    .map(|state| {
                        // Perform a breadth-first search across the graph of dependencies between
                        // lifetimes and type parameters, then collect all reachable nodes as
                        // generic parameters and where predicates.
                        state.and_then(|state, ()| {
                            // Lifetimes and type parameter identifiers for processing in any
                            // current iteration.
                            let mut current_lifetimes = state.fields.fields
                                .collect_lifetimes(&options, &declared_lifetimes);
                            let mut current_type_params = state.fields.fields
                                .collect_type_params(&options, &declared_type_params);

                            // Lifetimes and type parameter identifiers we already processed.
                            let mut state_lifetimes = LifetimeRefSet::default();
                            let mut state_type_params = IdentRefSet::default();

                            // While we have new lifetimes and type parameter identifiers to
                            // process...
                            while !current_lifetimes.is_empty() || !current_type_params.is_empty() {
                                // Collect elements for the next iteration from definitions and
                                // where predicates.
                                //
                                // @hcpl: made as a macro because I have no idea how to generalize
                                // this as a function in a readable way.
                                macro_rules! new_elems_from_elem {
                                    (
                                        $elem:ident,
                                        $elem_defs:ident,
                                        $where_clause_elems:ident,
                                        $collect_elems:ident,
                                        $declared_elems:ident,
                                    ) => {{
                                        let from_def_bounds = $elem_defs
                                            .get($elem)
                                            .unwrap()
                                            .bounds
                                            .$collect_elems(&options, &$declared_elems)
                                            .into_iter();
                                        let from_where_clause_bounds = $where_clause_elems
                                            .get($elem)
                                            .map(|predicates| predicates.iter().map(|p| *p))
                                            .into_iter()
                                            .flat_map(|ps| ps)
                                            .$collect_elems(&options, &$declared_elems)
                                            .into_iter();

                                        from_def_bounds.chain(from_where_clause_bounds)
                                    }};
                                }

                                // Collect lifetimes for the next_iteration.
                                let new_lifetimes = current_lifetimes.iter().flat_map(|lifetime| {
                                    new_elems_from_elem!(
                                        lifetime, lifetime_defs, where_clause_lifetimes,
                                        collect_lifetimes, declared_lifetimes,
                                    )
                                }).chain(current_type_params.iter().flat_map(|ident| {
                                    new_elems_from_elem!(
                                        ident, type_param_defs, where_clause_types,
                                        collect_lifetimes, declared_lifetimes,
                                    )
                                })).filter(|lifetime| {
                                    !state_lifetimes.contains(lifetime)
                                        && !current_lifetimes.contains(lifetime)
                                }).collect();

                                // Collect type parameter identifiers for the next iteration.
                                let new_type_params = current_type_params.iter().flat_map(|ident| {
                                    new_elems_from_elem!(
                                        ident, type_param_defs, where_clause_types,
                                        collect_type_params, declared_type_params,
                                    )
                                }).filter(|ident| {
                                    !state_type_params.contains(ident)
                                        && !current_type_params.contains(ident)
                                }).collect();

                                // Prepare the loop state for the next iteration.
                                state_lifetimes.extend(current_lifetimes);
                                state_type_params.extend(current_type_params);

                                current_lifetimes = new_lifetimes;
                                current_type_params = new_type_params;
                            }

                            // Collect generic parameters for this state.
                            // The result will preserve the order they were declared for the state
                            // machine enum.
                            let params = {
                                let param_lifetime_defs = mgenerics
                                    .lifetimes()
                                    .map(|lifetime_def| &lifetime_def.lifetime)
                                    .filter(|lifetime| state_lifetimes.contains(lifetime))
                                    .map(|lifetime| {
                                        let lifetime_def = *lifetime_defs.get(lifetime).unwrap();
                                        syn::GenericParam::Lifetime(lifetime_def.clone())
                                    });

                                let param_type_param_defs = mgenerics
                                    .type_params()
                                    .map(|type_param| &type_param.ident)
                                    .filter(|ident| state_type_params.contains(ident))
                                    .map(|ident| {
                                        let type_param = *type_param_defs.get(ident).unwrap();
                                        syn::GenericParam::Type(type_param.clone())
                                    });

                                param_lifetime_defs.chain(param_type_param_defs).collect()
                            };

                            // Collect where predicates for this state.
                            // The result will preserve the order they were declared for the state
                            // machine enum.
                            let where_preds = {
                                let where_preds_lifetimes = state_lifetimes
                                    .iter()
                                    .filter_map(|lifetime| {
                                        where_clause_lifetimes.get(lifetime).map(|predicates| {
                                            predicates.iter().map(|&p| p.clone())
                                        })
                                    })
                                    .flat_map(|ps| ps);

                                let where_preds_types = state_type_params
                                    .iter()
                                    .filter_map(|ident| {
                                        where_clause_types.get(ident).map(|predicates| {
                                            predicates.iter().map(|&p| p.clone())
                                        })
                                    })
                                    .flat_map(|ps| ps);

                                where_preds_lifetimes.chain(where_preds_types).collect()
                            };

                            let generics = Rc::new(syn::Generics {
                                lt_token: Some(<Token![<]>::default()),
                                params,
                                gt_token: Some(<Token![>]>::default()),
                                where_clause: Some(syn::WhereClause {
                                    where_token: <Token![where]>::default(),
                                    predicates: where_preds,
                                }),
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
#[derive(FromMeta, Debug)]
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
dummy_from_meta!(AfterStateGenericsExtra);

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
                            let params = {
                                // Filter all generic_params in the order they appear in the machine
                                // generics.
                                let lifetimes = mgenerics
                                    .lifetimes()
                                    .filter(|l| {
                                        state.transitions.iter().any(|ident| {
                                            ident_to_generics
                                                .get(ident)
                                                .map(|v| v.lifetimes().any(|l_| l_ == *l))
                                                .unwrap_or(false)
                                        })
                                    })
                                    .cloned();

                                let type_params = mgenerics
                                    .type_params()
                                    .filter(|t| {
                                        state.transitions.iter().any(|ident| {
                                            ident_to_generics
                                                .get(ident)
                                                .map(|v| v.type_params().any(|t_| t_ == *t))
                                                .unwrap_or(false)
                                        })
                                    })
                                    .cloned();

                                type_params.map(syn::GenericParam::Type)
                                    .chain(lifetimes.map(syn::GenericParam::Lifetime))
                                    .collect()
                            };

                            let where_preds = mgenerics
                                .where_clause
                                .as_ref()
                                .map(|clause| {
                                    clause
                                        .predicates
                                        .iter()
                                        .filter(|p| {
                                            state.transitions.iter().any(|ident| {
                                                ident_to_generics
                                                    .get(ident)
                                                    .map(|v| {
                                                        v.where_clause
                                                            .as_ref()
                                                            .map(|clause| {
                                                                clause
                                                                    .predicates
                                                                    .iter()
                                                                    .any(|p_| p_ == *p)
                                                            })
                                                            .unwrap_or(false)
                                                    })
                                                    .unwrap_or(false)
                                            })
                                        })
                                        .cloned()
                                        .collect()
                                })
                                .unwrap_or_default();

                            let after_state_generics = Rc::new(syn::Generics {
                                lt_token: Some(<Token![<]>::default()),
                                params,
                                gt_token: Some(<Token![>]>::default()),
                                where_clause: Some(syn::WhereClause {
                                    where_token: <Token![where]>::default(),
                                    predicates: where_preds,
                                }),
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
    pub states_enum: Rc<Ident>,
    pub poll_trait: Rc<Ident>,
    pub futures_crate: Rc<Ident>,
    pub smf_crate: Rc<Ident>,
}

dummy_default!(ReadyForCodegen);
dummy_from_meta!(ReadyForCodegen);

#[derive(Debug)]
pub struct CodegenStateExtra {
    pub vis: Rc<syn::Visibility>,
    pub description_ident: Rc<syn::Ident>,
    pub states_enum: Rc<Ident>,
    pub error_type: Rc<syn::Type>,
    pub error_ident: Rc<syn::Ident>,
    pub after: Ident,
    pub derive: Rc<darling::util::IdentList>,
    pub poll_trait: Rc<Ident>,
    pub poll_method: Ident,
    pub futures_crate: Rc<Ident>,
    pub smf_crate: Rc<Ident>,
    pub generics: Rc<syn::Generics>,
    pub after_state_generics: Rc<syn::Generics>,
    pub transition_state_generics: HashMap<syn::Ident, Rc<syn::Generics>>,
}

dummy_from_meta!(CodegenStateExtra);
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

            let error_type = states[error].fields.fields[0].ty.clone();
            let error_type = Rc::new(error_type);

            let derive = Rc::new(machine.derive.clone());

            let machine_name = machine.ident.to_string();

            let mut states_enum = machine_name.clone();
            states_enum += "States";
            let states_enum = Rc::new(Ident::new(&states_enum, Span::call_site()));

            let mut poll_trait = String::from("Poll");
            poll_trait += &machine_name;
            let poll_trait = Rc::new(Ident::new(&poll_trait, Span::call_site()));

            let mut futures_crate = String::from("__smf_");
            futures_crate += machine_name.clone().to_snake_case().as_str();
            futures_crate += "_futures";
            let futures_crate = Rc::new(Ident::new(&futures_crate, Span::call_site()));

            let mut smf_crate = String::from("__smf_");
            smf_crate += machine_name.clone().to_snake_case().as_str();
            smf_crate += "_state_machine_future";
            let smf_crate = Rc::new(Ident::new(&smf_crate, Span::call_site()));

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
                        let after = Ident::new(&after, Span::call_site());

                        let mut poll_method = String::from("poll_");
                        poll_method.push_str(&ident_name.to_snake_case());
                        let poll_method = Ident::new(&poll_method, Span::call_site());

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
