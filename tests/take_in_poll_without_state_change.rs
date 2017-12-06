//! Test that if a state transition function takes the `RentToOwn` without
//! changing states, then we return `Ok(NotReady)` for perpetuity.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Async, Future, Poll};
use state_machine_future::RentToOwn;

#[derive(StateMachineFuture)]
pub enum Machine {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Ready))]
    Start,

    #[state_machine_future(ready, error)] Ready(usize),
}

impl PollMachine for Machine {
    fn poll_start<'a>(start: &'a mut RentToOwn<'a, Start>) -> Poll<AfterStart, usize> {
        // Take the state.
        let _ = start.take();

        // But don't transition to a new state.
        Ok(Async::NotReady)
    }
}

#[test]
fn taken_without_state_transition_is_never_ready() {
    let mut machine = Machine::start();

    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
    assert_eq!(machine.poll(), Ok(Async::NotReady));
}
