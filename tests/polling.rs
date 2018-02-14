//! Test that we get the expected poll results.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Async, Future, Poll};
use state_machine_future::RentToOwn;

#[derive(StateMachineFuture)]
pub enum Machine {
    /// Choose which next state to go into depending on what start value is
    /// given.
    #[state_machine_future(start)]
    #[state_machine_future(transitions(NeverReady, AlwaysReady, Error))]
    Start(Result<Box<AfterStart>, usize>),

    /// This always returns NotReady.
    #[state_machine_future(transitions(Error))]
    NeverReady,

    /// This always returns Ready.
    #[state_machine_future(transitions(Ready))]
    AlwaysReady,

    #[state_machine_future(ready)]
    Ready(usize),

    #[state_machine_future(error)]
    Error(usize),
}

impl PollMachine for Machine {
    fn poll_start<'a>(start: &'a mut RentToOwn<'a, Start>) -> Poll<AfterStart, usize> {
        Ok(Async::Ready(*start.take().0?))
    }

    fn poll_never_ready<'a>(_: &'a mut RentToOwn<'a, NeverReady>) -> Poll<AfterNeverReady, usize> {
        Ok(Async::NotReady)
    }

    fn poll_always_ready<'a>(
        _: &'a mut RentToOwn<'a, AlwaysReady>,
    ) -> Poll<AfterAlwaysReady, usize> {
        Ok(Async::Ready(AfterAlwaysReady::Ready(Ready(1))))
    }
}

#[test]
fn direct_error() {
    let mut machine = Machine::start(Err(42));
    assert_eq!(machine.poll(), Err(42));

    // And its fused: never ready when polling again after we finished.
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
}

#[test]
fn indirect_error() {
    let mut machine = Machine::start(Ok(Box::new(AfterStart::Error(Error(42)))));
    assert_eq!(machine.poll(), Err(42));

    // And its fused: never ready when polling again after we finished.
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
}

#[test]
fn never_ready() {
    let mut machine = Machine::start(Ok(Box::new(AfterStart::NeverReady(NeverReady))));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
}

#[test]
fn always_ready() {
    let mut machine = Machine::start(Ok(Box::new(AfterStart::AlwaysReady(AlwaysReady))));
    assert_eq!(machine.poll(), Ok(Async::Ready(1)));

    // And its fused: never ready when polling again after we finished.
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
}
