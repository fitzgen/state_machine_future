//! Test that we can take a value from context when transitioning to ready.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Async;
use futures::Future;
use futures::Poll;
use state_machine_future::RentToOwn;

pub struct Context {
    value: String,
}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context")]
pub enum WithContext {
    #[state_machine_future(start, transitions(Ready))]
    Start,

    #[state_machine_future(ready)]
    Ready(String),

    #[state_machine_future(error)]
    Error(()),
}

impl PollWithContext for WithContext {
    fn poll_start<'s, 'c>(
        _: &'s mut RentToOwn<'s, Start>,
        context: &'c mut RentToOwn<'c, Context>,
    ) -> Poll<AfterStart, ()> {
        let context = context.take();

        let value = context.value;

        transition!(Ready(value))
    }
}

#[test]
fn given_sm_with_context_can_take_context_value_on_ready() {
    let context = Context {
        value: String::from("foo"),
    };

    let mut machine = WithContext::start(context);

    assert_eq!(machine.poll(), Ok(Async::Ready(String::from("foo"))));
}
