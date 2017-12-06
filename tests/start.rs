//! Test that the appropriat start method is generated for the original enum.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::{RentToOwn, StateMachineFuture};

#[derive(StateMachineFuture)]
pub enum Fsm {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Done))]
    Begin(usize, bool),

    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    Done(()),
}

impl PollFsm for Fsm {
    fn poll_begin<'a>(_: &'a mut RentToOwn<'a, Begin>) -> Poll<AfterBegin, ()> {
        unimplemented!()
    }
}

#[test]
fn fsm_has_appropriate_start_function() {
    fn check<S, F>(_: F)
    where
        S: StateMachineFuture,
        F: Fn(usize, bool) -> S::Future,
    {
    }

    check::<Fsm, _>(Fsm::start);
}

#[derive(StateMachineFuture)]
pub enum Fsm2 {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Done2))]
    Begin2 { x: bool, y: usize },

    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    Done2(()),
}

impl PollFsm2 for Fsm2 {
    fn poll_begin2<'a>(_: &'a mut RentToOwn<'a, Begin2>) -> Poll<AfterBegin2, ()> {
        unimplemented!()
    }
}

#[test]
fn fsm2_has_appropriate_start_function() {
    fn check<S, F>(_: F)
    where
        S: StateMachineFuture,
        F: Fn(bool, usize) -> S::Future,
    {
    }

    check::<Fsm2, _>(Fsm2::start);
}

#[derive(StateMachineFuture)]
pub enum Fsm3 {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Done3))]
    Begin3,

    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    Done3(()),
}

impl PollFsm3 for Fsm3 {
    fn poll_begin3<'a>(_: &'a mut RentToOwn<'a, Begin3>) -> Poll<AfterBegin3, ()> {
        unimplemented!()
    }
}

#[test]
fn fsm3_has_appropriate_start_function() {
    fn check<S, F>(_: F)
    where
        S: StateMachineFuture,
        F: Fn() -> S::Future,
    {
    }

    check::<Fsm3, _>(Fsm3::start);
}
