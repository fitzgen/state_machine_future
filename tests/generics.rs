//! Test that we handle generics properly.
//!
//! Here's the deal: we don't figure out which generics are used in which
//! variants, so it is up to you to add phantom datas as needed.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::{Future, Poll};
use state_machine_future::RentToOwn;
use std::fmt::Debug;
use std::io;
use std::marker::PhantomData;

pub trait ComplexTrait<'a, T> {
    fn data(&self) -> &'a T;
}

pub trait AssociatedTypesTrait {
    type Type;
}

pub struct StartType<'a, 'c, 'd: 'a, T, C>
where
    T: ComplexTrait<'c, C>,
    C: 'c,
    'c: 'd,
{
    _data: T,
    _phan: PhantomData<&'a C>,
    _phan2: PhantomData<&'c C>,
    _phan3: PhantomData<&'d C>,
}

#[derive(StateMachineFuture)]
pub enum Fsm<'a, 'c, 'd: 'a, T: 'static, E, C, D>
where
    T: ComplexTrait<'c, C>,
    E: Debug,
    C: 'c,
    'c: 'd,
    D: AssociatedTypesTrait<Type = E>,
{
    /// The start state.
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Complex, AssociatedType, Ready, Error))]
    Start(StartType<'a, 'c, 'd, T, C>, D),

    #[state_machine_future(transitions(Ready, Error))]
    Complex(&'a i32, &'d u32),

    #[state_machine_future(transitions(Ready, Error))]
    AssociatedType(D),

    /// Some generic ready state.
    #[state_machine_future(ready)]
    Ready(i32),

    /// Some generic error state.
    #[state_machine_future(error)]
    Error(E),
}

impl<'a, 'c, 'd: 'a, T, E, C, D> PollFsm<'a, 'c, 'd, T, E, C, D> for Fsm<'a, 'c, 'd, T, E, C, D>
where
    T: ComplexTrait<'c, C> + 'static,
    E: Debug,
    C: 'c,
    'c: 'd,
    D: AssociatedTypesTrait<Type = E>,
{
    fn poll_start<'b>(
        _: &'b mut RentToOwn<'b, Start<'a, 'c, 'd, T, E, C, D>>,
    ) -> Poll<AfterStart<'a, 'd, E, D>, E> {
        unimplemented!()
    }

    fn poll_complex<'b>(_: &'b mut RentToOwn<'b, Complex<'a, 'd>>) -> Poll<AfterComplex<E>, E> {
        unimplemented!()
    }

    fn poll_associated_type<'b>(
        _: &'b mut RentToOwn<'b, AssociatedType<E, D>>,
    ) -> Poll<AfterAssociatedType<E>, E> {
        unimplemented!()
    }
}

impl<'a> ComplexTrait<'a, i32> for i32 {
    fn data(&self) -> &'a i32 {
        unimplemented!()
    }
}

impl AssociatedTypesTrait for String {
    type Type = io::Error;
}

#[test]
fn check_generic_start() {
    let test = String::from("test");

    let _: Box<Future<Item = i32, Error = io::Error>> = Box::new(Fsm::start(
        StartType {
            _data: 0,
            _phan: Default::default(),
            _phan2: Default::default(),
            _phan3: Default::default(),
        },
        test,
    ));
}
