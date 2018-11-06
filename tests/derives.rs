//! Test that we add derive traits properly.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use std::fmt::Debug;

#[derive(StateMachineFuture)]
#[state_machine_future(derive(Debug))]
pub enum Debuggable {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    OnlyState(()),
}

fn check_debug<D: Debug>(_: D) {}

#[test]
fn state_derived_debug() {
    check_debug(OnlyState(()));
}

