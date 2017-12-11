//! Test that we handle overlapping start/ready/error states properly.
//!
//! Here's the deal: we don't figure out which generics are used in which
//! variants, so it is up to you to add phantom datas as needed.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Future, Poll};
use state_machine_future::RentToOwn;
use std::fmt::Debug;
use std::marker::PhantomData;

#[derive(StateMachineFuture)]
pub enum Fsm<T : 'static, E>
where
    T: Default,
    E: Debug,
{
    /// The start state.
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Ready, Error))]
    Start(PhantomData<T>, PhantomData<E>),

    /// Some generic ready state.
    #[state_machine_future(ready)]
    Ready((T, PhantomData<E>)),

    /// Some generic error state.
    #[state_machine_future(error)]
    Error((E, PhantomData<T>)),
}

impl<T, E> PollFsm<T, E> for Fsm<T, E>
where
    T: Default + 'static,
    E: Debug,
{
    fn poll_start<'a>(
        _: &'a mut RentToOwn<'a, Start<T, E>>,
    ) -> Poll<AfterStart<T, E>, (E, PhantomData<T>)> {
        unimplemented!()
    }
}

#[test]
fn check_generic_start() {
    let _: Box<Future<Item = (usize, _), Error = (bool, _)>> =
        Box::new(Fsm::start(PhantomData, PhantomData));
}
