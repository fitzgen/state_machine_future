//! Test that we can access context type.

#[macro_use]
extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Async;
use futures::Future;
use futures::Poll;
use state_machine_future::RentToOwn;
use std::fmt::Debug;

pub struct ExternalSource<T> {
    pub value: T,
}

pub struct Context<T> {
    pub external_source: ExternalSource<T>,
    pub lazy_future: Option<Box<Future<Item = T, Error = ()>>>,
}

impl<T: Clone + 'static> Context<T> {
    fn load_from_external_source(&mut self) -> &mut Box<Future<Item = T, Error = ()>> {
        let value = &self.external_source.value;

        self.lazy_future
            .get_or_insert_with(|| Box::new(futures::future::ok(value.clone())))
    }
}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context", derive(Debug))]
pub enum WithContext<T: Clone + 'static> {
    #[state_machine_future(start, transitions(Ready))]
    Start(()),

    #[state_machine_future(ready)]
    Ready(T),

    #[state_machine_future(error)]
    Error(()),
}

impl<T: Clone + 'static> PollWithContext<T> for WithContext<T> {
    fn poll_start<'state, 'context>(
        _: &'state mut RentToOwn<'state, Start>,
        context: &'context mut RentToOwn<'context, Context<T>>,
    ) -> Poll<AfterStart<T>, ()> {

        let value = try_ready!(context.load_from_external_source().poll());

        transition!(Ready(value))
    }
}

#[test]
fn can_call_to_context() {

    let source = ExternalSource {
        value: String::from("foo"),
    };

    let context = Context { external_source: source, lazy_future: None };

    let mut machine = WithContext::start((), context);

    assert_eq!(machine.poll(), Ok(Async::Ready(String::from("foo"))));
}

fn check_debug<D: Debug>(_: D) {}

#[test]
fn given_sm_with_context_should_add_derives_to_states() {
    check_debug(Start(()));
    check_debug(Ready(10));
    check_debug(Error(()));
}

