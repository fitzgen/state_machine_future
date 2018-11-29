//! AST types for state machines and their states.

use darling;
use phases;
use syn;

/// A description of a state machine: its various states, which is the start
/// state, ready state, and error state.
#[derive(Debug, FromDeriveInput)]
#[darling(
    attributes(state_machine_future),
    supports(enum_any),
    forward_attrs(allow, cfg)
)]
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

    #[darling(default)]
    pub context: Option<syn::Ident>,
}

/// In individual state in a state machine.
#[derive(Debug, FromVariant)]
#[darling(
    attributes(state_machine_future, transitions, start, ready, error),
    forward_attrs(allow, doc, cfg)
)]
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
        F: FnMut(
            StateMachine<phases::NoPhase>,
            P::StateMachineExtra,
            Vec<State<P>>,
        ) -> StateMachine<Q>,
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
            context: self.context,
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
            context: self.context,
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
