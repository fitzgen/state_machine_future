//! AST types for state machines and their states.

use darling;
use phases;
use syn;

use std::collections::HashSet;

/// A description of a state machine: its various states, which is the start
/// state, ready state, and error state.
#[derive(Debug, FromDeriveInput)]
#[darling(attributes(state_machine_future), supports(enum_any), forward_attrs(allow, cfg))]
pub struct StateMachine<P: phases::Phase> {
    pub ident: syn::Ident,
    pub vis: syn::Visibility,
    pub generics: syn::Generics,
    pub body: darling::ast::Body<State<P>, ()>,
    pub attrs: Vec<syn::Attribute>,

    /// I guess we can't get other derives into `attrs` so we have to create our
    /// own derive list.
    #[darling(default)]
    pub derive: darling::util::IdentList,

    /// Extra per-phase data.
    #[darling(default)]
    pub extra: P::StateMachineExtra,
}

/// In individual state in a state machine.
#[derive(Debug, FromVariant)]
#[darling(attributes(state_machine_future, transitions, start, ready, error),
          forward_attrs(allow, doc, cfg))]
pub struct State<P: phases::Phase> {
    pub ident: syn::Ident,
    pub attrs: Vec<syn::Attribute>,
    pub data: darling::ast::VariantData<syn::Field>,

    /// Whether this is the start state.
    #[darling(default)]
    pub start: bool,

    /// Whether this is the ready state.
    #[darling(default)]
    pub ready: bool,

    /// Whether this is the error state.
    #[darling(default)]
    pub error: bool,

    /// The set of other states that this one can transition to.
    #[darling(default)]
    pub transitions: darling::util::IdentList,

    /// Any extra per-phase data.
    #[darling(default)]
    pub extra: P::StateExtra,
}

impl<P> StateMachine<P>
where
    P: phases::Phase,
{
    /// Split this state machine into its parts, and then create a state machine
    /// in another phase.
    pub fn and_then<F, Q>(self, mut f: F) -> StateMachine<Q>
    where
        Q: phases::Phase,
        F: FnMut(StateMachine<phases::NoPhase>, P::StateMachineExtra, Vec<State<P>>)
            -> StateMachine<Q>,
    {
        let (state_machine, extra, states) = self.split();
        f(state_machine, extra, states)
    }

    /// Split this state machine into its parts, separating per-phase data from
    /// the state machine.
    pub fn split(
        self,
    ) -> (
        StateMachine<phases::NoPhase>,
        P::StateMachineExtra,
        Vec<State<P>>,
    ) {
        let states = self.body.take_enum().unwrap();
        let extra = self.extra;
        let machine = StateMachine {
            ident: self.ident,
            vis: self.vis,
            generics: self.generics,
            body: darling::ast::Body::Enum(vec![]),
            attrs: self.attrs,
            derive: self.derive,
            extra: (),
        };
        (machine, extra, states)
    }

    /// Get this state machine's states.
    pub fn states(&self) -> &[State<P>] {
        match self.body {
            darling::ast::Body::Enum(ref states) => states,
            darling::ast::Body::Struct(_) => unreachable!(),
        }
    }
}

impl StateMachine<phases::NoPhase> {
    /// Join the state machine with the new phase's extra data, creating a state
    /// machine in the new phase.
    pub fn join<P>(self, extra: P::StateMachineExtra, states: Vec<State<P>>) -> StateMachine<P>
    where
        P: phases::Phase,
    {
        StateMachine {
            ident: self.ident,
            vis: self.vis,
            generics: self.generics,
            body: darling::ast::Body::Enum(states),
            attrs: self.attrs,
            derive: self.derive,
            extra,
        }
    }
}

impl<P> State<P>
where
    P: phases::Phase,
{
    /// Split this state into its parts, and then construct a state in some new
    /// phase.
    pub fn and_then<F, Q>(self, mut f: F) -> State<Q>
    where
        F: FnMut(State<phases::NoPhase>, P::StateExtra) -> State<Q>,
        Q: phases::Phase,
    {
        let (state, extra) = self.split();
        f(state, extra)
    }

    /// Split this state into its parts, separating its per-phase data out.
    pub fn split(self) -> (State<phases::NoPhase>, P::StateExtra) {
        let extra = self.extra;
        let state = State {
            ident: self.ident,
            attrs: self.attrs,
            data: self.data,
            start: self.start,
            ready: self.ready,
            error: self.error,
            transitions: self.transitions,
            extra: (),
        };
        (state, extra)
    }
}

pub trait CollectIdents {
    /// Collects idents that could be a type/lifetime parameter
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>);
}

impl CollectIdents for syn::Ty {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::Ty::Path(ref qself, ref p) => {
                if let &Some(ref qself) = qself {
                    qself.ty.collect_idents(idents);
                }

                p.collect_idents(idents);
            }
            syn::Ty::Slice(ref ty) | syn::Ty::Paren(ref ty) => ty.collect_idents(idents),
            syn::Ty::Ptr(ref ty) => ty.ty.collect_idents(idents),
            syn::Ty::Rptr(ref lifetime, ref ty) => {
                if let &Some(ref lifetime) = lifetime {
                    idents.insert(lifetime.ident.clone());
                }

                ty.ty.collect_idents(idents)
            }
            syn::Ty::Tup(ref tys) => tys.iter().for_each(|v| v.collect_idents(idents)),
            syn::Ty::BareFn(ref bfn) => bfn.collect_idents(idents),
            syn::Ty::Array(ref ty, ref cexpr) => {
                ty.collect_idents(idents);
                cexpr.collect_idents(idents);
            }
            syn::Ty::Never
            | syn::Ty::Mac(_)
            | syn::Ty::TraitObject(_)
            | syn::Ty::ImplTrait(_)
            | syn::Ty::Infer => {}
        }
    }
}

impl CollectIdents for syn::ConstExpr {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::ConstExpr::Call(ref f, ref args) => {
                f.collect_idents(idents);
                args.iter().for_each(|v| v.collect_idents(idents));
            }
            syn::ConstExpr::Binary(_, ref c0, ref c1) | syn::ConstExpr::Index(ref c0, ref c1) => {
                c0.collect_idents(idents);
                c1.collect_idents(idents);
            }
            syn::ConstExpr::Unary(_, ref c) | syn::ConstExpr::Paren(ref c) => {
                c.collect_idents(idents)
            }
            syn::ConstExpr::Cast(ref c, ref ty) => {
                ty.collect_idents(idents);
                c.collect_idents(idents);
            }
            syn::ConstExpr::Path(ref p) => p.collect_idents(idents),
            syn::ConstExpr::Lit(_) | syn::ConstExpr::Other(_) => {}
        }
    }
}

impl CollectIdents for syn::BareFnTy {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        self.inputs.iter().for_each(|v| v.ty.collect_idents(idents));

        match self.output {
            syn::FunctionRetTy::Ty(ref ty) => ty.collect_idents(idents),
            syn::FunctionRetTy::Default => {}
        }
    }
}

impl CollectIdents for syn::Path {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        // If the path contains only one segment and is not a global path,
        // it could be a generic type parameter, so we add the ident.
        if self.segments.len() == 1 && !self.global {
            let last = self.segments.get(0).unwrap();
            idents.insert(last.ident.clone());
        }

        // If the path has more than one segment, it can not be a type parameter, because type
        // parameters are absolute without any preceding segments. So, only collect
        // the idents of the path parameters (aka type/lifetime parameters).
        self.segments
            .iter()
            .for_each(|s| s.parameters.collect_idents(idents));
    }
}

impl CollectIdents for syn::PathParameters {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::PathParameters::AngleBracketed(ref bracket) => {
                bracket.lifetimes.iter().for_each(|v| {
                    idents.insert(v.ident.clone());
                });
                bracket.types.iter().for_each(|v| v.collect_idents(idents));
                bracket
                    .bindings
                    .iter()
                    .for_each(|v| v.ty.collect_idents(idents));
            }
            syn::PathParameters::Parenthesized(ref parent) => {
                parent.inputs.iter().for_each(|v| v.collect_idents(idents));

                if let Some(ref output) = parent.output {
                    output.collect_idents(idents);
                }
            }
        }
    }
}

impl CollectIdents for syn::TyParamBound {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::TyParamBound::Trait(ref poly, _) => {
                poly.bound_lifetimes.iter().for_each(|l| {
                    l.collect_idents(idents);
                });

                poly.trait_ref.collect_idents(idents);
            }
            syn::TyParamBound::Region(ref lifetime) => {
                idents.insert(lifetime.ident.clone());
            }
        }
    }
}

impl CollectIdents for syn::TyParam {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        if let Some(ref default) = self.default {
            default.collect_idents(idents);
        }

        self.bounds.iter().for_each(|b| b.collect_idents(idents));
        idents.insert(self.ident.clone());
    }
}

impl CollectIdents for syn::LifetimeDef {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        self.bounds.iter().for_each(|b| {
            idents.insert(b.ident.clone());
        });
        idents.insert(self.lifetime.ident.clone());
    }
}

impl CollectIdents for syn::WherePredicate {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::WherePredicate::BoundPredicate(ref bound) => {
                bound.bounds.iter().for_each(|b| b.collect_idents(idents));
                bound
                    .bound_lifetimes
                    .iter()
                    .for_each(|l| l.collect_idents(idents));
                bound.bounded_ty.collect_idents(idents);
            }
            syn::WherePredicate::RegionPredicate(ref region) => {
                region.bounds.iter().for_each(|l| {
                    idents.insert(l.ident.clone());
                });
                idents.insert(region.lifetime.ident.clone());
            }
            syn::WherePredicate::EqPredicate(ref eq) => {
                eq.lhs_ty.collect_idents(idents);
                eq.rhs_ty.collect_idents(idents);
            }
        }
    }
}

impl State<phases::NoPhase> {
    /// Join the state with the new phase's extra data, creating a state in the
    /// new phase.
    pub fn join<P>(self, extra: P::StateExtra) -> State<P>
    where
        P: phases::Phase,
    {
        State {
            ident: self.ident,
            attrs: self.attrs,
            data: self.data,
            start: self.start,
            ready: self.ready,
            error: self.error,
            transitions: self.transitions,
            extra,
        }
    }
}
