//! Test that we can access context type.

#[macro_use]
extern crate state_machine_future;

use std::fmt::Debug;

pub struct Context {}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context", derive(Debug))]
pub enum WithContext {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    OnlyState(()),
}

fn check_debug<D: Debug>(_: D) {}

#[test]
fn given_sm_with_context_should_add_derives_to_states() {
    check_debug(OnlyState(()));
}
