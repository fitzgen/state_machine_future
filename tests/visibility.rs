//! Test that we handle `pub`, `pub(crate)`, and non-`pub` state machines.

#![allow(dead_code)]

extern crate futures;
#[macro_use]
extern crate state_machine_future;

#[derive(StateMachineFuture)]
pub enum Pub {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    PubState(()),
}

#[derive(StateMachineFuture)]
pub(crate) enum PubCrate {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    PubCrateState(()),
}

#[derive(StateMachineFuture)]
enum NonPub {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    NonPubState(()),
}
