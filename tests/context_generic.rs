//! Test using a generic context in a non-generic state machine.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::RentToOwn;

#[allow(dead_code)]
pub struct Context<T> {
    data: T,
}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context<T>")]
pub enum WithContext {
    #[state_machine_future(start, transitions(Ready))]
    Start(),

    #[state_machine_future(ready)]
    Ready(()),

    #[state_machine_future(error)]
    Error(()),
}

impl PollWithContext for WithContext {
    fn poll_start<'s, 'c, T>(
        _: &'s mut RentToOwn<'s, Start>,
        _: &'c mut RentToOwn<'c, Context<T>>,
    ) -> Poll<AfterStart, ()> {
        unimplemented!()
    }
}

#[test]
fn generic_sm_with_nongeneric_context() {
    let context = Context { data: 42 };

    let _ = WithContext::start(context);
}
