//! Test using a generic context in a generic state machine where only
//! some of the type parameters overlap.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Future, Poll};
use state_machine_future::RentToOwn;

pub struct Context<T> {
    data: T,
}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context<U>")]
pub enum WithContext<T, U> {
    #[state_machine_future(start, transitions(Ready))]
    Start(T),

    #[state_machine_future(ready)]
    Ready(U),

    #[state_machine_future(error)]
    Error(()),
}

impl<T, U> PollWithContext<T, U> for WithContext<T, U> {
    fn poll_start<'s, 'c>(
        _: &'s mut RentToOwn<'s, Start<T>>,
        context: &'c mut RentToOwn<'c, Context<U>>,
    ) -> Poll<AfterStart<U>, ()> {
        transition!(Ready(context.take().data));
    }
}

#[test]
fn generic_sm_with_semigeneric_context() {
    let context = Context {
        data: 42,
    };

    assert_eq!(WithContext::start((), context).wait(), Ok(42));
}
