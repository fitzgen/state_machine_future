//! Test that we handle different kinds of state enums correctly.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::RentToOwn;

#[derive(StateMachineFuture)]
pub enum Fsm {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Tuple))]
    Unit,

    #[state_machine_future(transitions(Struct))] Tuple(usize, bool),

    #[state_machine_future(transitions(Done))] Struct {
        x: usize,
        y: bool,
    },

    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    Done(()),
}

impl PollFsm for Fsm {
    fn poll_unit<'a>(unit: &'a mut RentToOwn<'a, Unit>) -> Poll<AfterUnit, ()> {
        match unit.take() {
            self::Unit => unimplemented!(),
        }
    }
    fn poll_tuple<'a>(tuple: &'a mut RentToOwn<'a, Tuple>) -> Poll<AfterTuple, ()> {
        match tuple.take() {
            Tuple(3, true) | Tuple(_, _) => unimplemented!(),
        }
    }
    fn poll_struct<'a>(st: &'a mut RentToOwn<'a, Struct>) -> Poll<AfterStruct, ()> {
        match st.take() {
            Struct { x: 3, y: true } | Struct { .. } => unimplemented!(),
        }
    }
}
