//! Test that the `AfterBlah` types implement `From<Successor>` for all
//! `Successor` typestates that come after the `Blah` typestate.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Async, Poll};
use state_machine_future::RentToOwn;

#[derive(StateMachineFuture)]
pub enum Machine {
    /// Choose which next state to go into depending on what start value is
    /// given.
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Ready))]
    Start,

    #[state_machine_future(ready)] Ready(usize),

    #[state_machine_future(error)] Error(usize),
}

impl PollMachine for Machine {
    fn poll_start<'a>(_: &'a mut RentToOwn<'a, Start>) -> Poll<AfterStart, usize> {
        Ok(Async::Ready(Ready(1).into()))
    }
}
