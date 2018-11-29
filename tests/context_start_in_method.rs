//! Test that we can access context type.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::RentToOwn;

pub struct Context {}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context")]
pub enum WithContext {
    #[state_machine_future(start, transitions(Ready))]
    Start,

    #[state_machine_future(ready)]
    Ready(()),

    #[state_machine_future(error)]
    Error(()),
}

impl PollWithContext for WithContext {
    fn poll_start<'s, 'c>(
        _: &'s mut RentToOwn<'s, Start>,
        _: &'c mut RentToOwn<'c, Context>,
    ) -> Poll<AfterStart, ()> {
        unimplemented!()
    }
}

#[test]
fn given_sm_with_no_start_args_only_takes_context() {
    let context = Context {};

    let _ = WithContext::start_in(Ready(()), context);
}
