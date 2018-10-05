//! Test case taken from
//! <https://github.com/fitzgen/state_machine_future/issues/25>.

extern crate futures;

#[macro_use]
extern crate state_machine_future;

use futures::{Future, Poll};
use state_machine_future::RentToOwn;

#[derive(StateMachineFuture)]
enum Foo<T>
where
    T: Future,
{
    #[state_machine_future(start, transitions(Finished))]
    Start(T),
    #[state_machine_future(ready)]
    Finished(T::Item),
    #[state_machine_future(error)]
    Failed(T::Error),
}

impl<T> PollFoo<T> for Foo<T>
where
    T: Future,
{
    fn poll_start<'a>(_state: &'a mut RentToOwn<'a, Start<T>>) -> Poll<AfterStart<T>, T::Error> {
        panic!()
    }
}
