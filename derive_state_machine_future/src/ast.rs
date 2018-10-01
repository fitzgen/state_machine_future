//! AST types for state machines and their states.

use std::collections::HashSet;

use darling;
use phases;
use syn;
use syn::punctuated::Punctuated;

/// A description of a state machine: its various states, which is the start
/// state, ready state, and error state.
#[derive(Debug, FromDeriveInput)]
#[darling(attributes(state_machine_future), supports(enum_any), forward_attrs(allow, cfg))]
pub struct StateMachine<P: phases::Phase> {
    pub ident: syn::Ident,
    pub vis: syn::Visibility,
    pub generics: syn::Generics,
    pub data: darling::ast::Data<State<P>, ()>,
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
    pub fields: darling::ast::Fields<syn::Field>,

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
        let states = self.data.take_enum().unwrap();
        let extra = self.extra;
        let machine = StateMachine {
            ident: self.ident,
            vis: self.vis,
            generics: self.generics,
            data: darling::ast::Data::Enum(vec![]),
            attrs: self.attrs,
            derive: self.derive,
            extra: (),
        };
        (machine, extra, states)
    }

    /// Get this state machine's states.
    pub fn states(&self) -> &[State<P>] {
        match self.data {
            darling::ast::Data::Enum(ref states) => states,
            darling::ast::Data::Struct(_) => unreachable!(),
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
            data: darling::ast::Data::Enum(states),
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
            fields: self.fields,
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

macro_rules! collect_idents {
    ($impl_type:ty, $($accessor:ident),+) => {
        impl CollectIdents for $impl_type {
            fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
                $(
                    self.$accessor.collect_idents(idents);
                )+
            }
        }
    };
}

collect_idents!(syn::AngleBracketedGenericArguments, args);
collect_idents!(syn::BareFnArg, ty);
collect_idents!(syn::Binding, ty);
collect_idents!(syn::BoundLifetimes, lifetimes);
collect_idents!(syn::ExprBinary, left, right);
collect_idents!(syn::ExprCall, func, args);
collect_idents!(syn::ExprCast, expr, ty);
collect_idents!(syn::ExprIndex, expr, index);
collect_idents!(syn::ExprParen, expr);
collect_idents!(syn::ExprPath, qself, path);
collect_idents!(syn::ExprUnary, expr);
collect_idents!(syn::LifetimeDef, lifetime, bounds);
collect_idents!(syn::ParenthesizedGenericArguments, inputs, output);
collect_idents!(syn::PathSegment, arguments);
collect_idents!(syn::PredicateEq, lhs_ty, rhs_ty);
collect_idents!(syn::PredicateLifetime, lifetime, bounds);
collect_idents!(syn::PredicateType, lifetimes, bounded_ty, bounds);
collect_idents!(syn::QSelf, ty);
collect_idents!(syn::TraitBound, lifetimes, path);
collect_idents!(syn::TypeArray, elem, len);
collect_idents!(syn::TypeBareFn, inputs, output);
collect_idents!(syn::TypeGroup, elem);
collect_idents!(syn::TypeParen, elem);
collect_idents!(syn::TypePath, qself, path);
collect_idents!(syn::TypePtr, elem);
collect_idents!(syn::TypeReference, lifetime, elem);
collect_idents!(syn::TypeSlice, elem);
collect_idents!(syn::TypeTuple, elems);

impl<T: CollectIdents> CollectIdents for Option<T> {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        self.iter().for_each(|e| e.collect_idents(idents));
    }
}

impl<T: CollectIdents, P> CollectIdents for Punctuated<T, P> {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        self.iter().for_each(|e| e.collect_idents(idents));
    }
}

impl CollectIdents for syn::Type {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::Type::Slice(ref slice) => slice.collect_idents(idents),
            syn::Type::Array(ref array) => array.collect_idents(idents),
            syn::Type::Ptr(ref ptr) => ptr.collect_idents(idents),
            syn::Type::Reference(ref reference) => reference.collect_idents(idents),
            syn::Type::BareFn(ref bare_fn) => bare_fn.collect_idents(idents),
            syn::Type::Tuple(ref tuple) => tuple.collect_idents(idents),
            syn::Type::Path(ref path) => path.collect_idents(idents),
            syn::Type::Paren(ref paren) => paren.collect_idents(idents),
            syn::Type::Group(ref group) => group.collect_idents(idents),
            syn::Type::Never(_)       |
            syn::Type::TraitObject(_) |
            syn::Type::ImplTrait(_)   |
            syn::Type::Infer(_)       |
            syn::Type::Macro(_)       |
            syn::Type::Verbatim(_)    => {},
        }
    }
}

impl CollectIdents for syn::ReturnType {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::ReturnType::Type(_, ref ty) => ty.collect_idents(idents),
            syn::ReturnType::Default => {},
        }
    }
}

impl CollectIdents for syn::Expr {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::Expr::Call(ref call) => call.collect_idents(idents),
            syn::Expr::Binary(ref binary) => binary.collect_idents(idents),
            syn::Expr::Unary(ref unary) => unary.collect_idents(idents),
            syn::Expr::Cast(ref cast) => cast.collect_idents(idents),
            syn::Expr::Index(ref index) => index.collect_idents(idents),
            syn::Expr::Path(ref path) => path.collect_idents(idents),
            syn::Expr::Paren(ref paren) => paren.collect_idents(idents),

            // NOTE: Because `darling` enables "syn/full" by default as a hack to make Racer
            // autocomplete work, we are facing a situation when covering `Lit` and `Field` is not
            // exhaustive enough, but we don't want to commit to "syn/full" as well as we don't use
            // anything beyond the default `syn` package.
            //
            // As a temporary solution, a catch-all placeholder is used here.
            //syn::Expr::Lit(_)   |
            //syn::Expr::Field(_) => {},
            _ => {},
        }
    }
}

impl CollectIdents for syn::Path {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        // If the path contains only one segment and is not a global path,
        // it could be a generic type parameter, so we add the ident.
        if self.segments.len() == 1 && self.leading_colon.is_none() {
            let last = &self.segments[0];
            idents.insert(last.ident.clone());
        }

        // If the path has more than one segment, it can not be a type parameter, because type
        // parameters are absolute without any preceding segments. So, only collect
        // the idents of the path parameters (aka type/lifetime parameters).
        self.segments.collect_idents(idents);
    }
}

impl CollectIdents for syn::PathArguments {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::PathArguments::None => {},
            syn::PathArguments::AngleBracketed(ref angle_bracketed) => angle_bracketed.collect_idents(idents),
            syn::PathArguments::Parenthesized(ref parenthesized) => parenthesized.collect_idents(idents),
        }
    }
}

impl CollectIdents for syn::GenericArgument {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::GenericArgument::Lifetime(ref lifetime) => lifetime.collect_idents(idents),
            syn::GenericArgument::Type(ref ty) => ty.collect_idents(idents),
            syn::GenericArgument::Binding(ref binding) => binding.collect_idents(idents),
            syn::GenericArgument::Const(_) => {},
        }
    }
}

impl CollectIdents for syn::TypeParamBound {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::TypeParamBound::Trait(ref trait_bound) => trait_bound.collect_idents(idents),
            syn::TypeParamBound::Lifetime(ref lifetime) => lifetime.collect_idents(idents),
        }
    }
}

impl CollectIdents for syn::Lifetime {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        idents.insert(self.ident.clone());
    }
}

impl CollectIdents for syn::TypeParam {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        idents.insert(self.ident.clone());
        self.bounds.collect_idents(idents);
        self.default.collect_idents(idents);
    }
}

impl CollectIdents for syn::WherePredicate {
    fn collect_idents(&self, idents: &mut HashSet<syn::Ident>) {
        match *self {
            syn::WherePredicate::Type(ref ty) => ty.collect_idents(idents),
            syn::WherePredicate::Lifetime(ref lifetime) => lifetime.collect_idents(idents),
            syn::WherePredicate::Eq(ref eq) => eq.collect_idents(idents),
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
            fields: self.fields,
            start: self.start,
            ready: self.ready,
            error: self.error,
            transitions: self.transitions,
            extra,
        }
    }
}
