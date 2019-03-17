//! Test using a non-generic context in a generic state machine.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::RentToOwn;

pub struct Context {}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context")]
pub enum WithContext<T> {
    #[state_machine_future(start, transitions(Ready))]
    Start(T),

    #[state_machine_future(ready)]
    Ready(()),

    #[state_machine_future(error)]
    Error(()),
}

impl<T> PollWithContext<T> for WithContext<T> {
    fn poll_start<'s, 'c>(
        _: &'s mut RentToOwn<'s, Start<T>>,
        _: &'c mut RentToOwn<'c, Context>,
    ) -> Poll<AfterStart, ()> {
        unimplemented!()
    }
}

#[test]
fn generic_sm_with_nongeneric_context() {
    let context = Context {};

    let _ = WithContext::start((), context);
}
