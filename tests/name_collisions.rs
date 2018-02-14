//! Test that the generated code is somewhat robust in the face of states with
//! names of types its using.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

#[derive(StateMachineFuture)]
pub enum Fsm {
    #[state_machine_future(start)]
    #[state_machine_future(transitions(Future))]
    Async,

    #[state_machine_future(transitions(Poll))]
    Future,

    #[state_machine_future(transitions(RentToOwn))]
    Poll,

    #[state_machine_future(transitions(StateMachineFuture))]
    RentToOwn,

    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    StateMachineFuture(()),
}

impl PollFsm for Fsm {
    fn poll_async<'a>(
        _: &'a mut state_machine_future::RentToOwn<'a, Async>,
    ) -> futures::Poll<AfterAsync, ()> {
        unimplemented!()
    }

    fn poll_future<'a>(
        _: &'a mut state_machine_future::RentToOwn<'a, Future>,
    ) -> futures::Poll<AfterFuture, ()> {
        unimplemented!()
    }

    fn poll_poll<'a>(
        _: &'a mut state_machine_future::RentToOwn<'a, Poll>,
    ) -> futures::Poll<AfterPoll, ()> {
        unimplemented!()
    }

    fn poll_rent_to_own<'a>(
        _: &'a mut state_machine_future::RentToOwn<'a, RentToOwn>,
    ) -> futures::Poll<AfterRentToOwn, ()> {
        unimplemented!()
    }
}
