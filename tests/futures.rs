#[macro_use] extern crate state_machine_future;
extern crate futures;

#[derive(Debug, PartialEq, Eq)]
struct MyItem;
#[derive(Debug, PartialEq, Eq)]
struct MyError;

#[derive(StateMachineFuture)]
enum MyStateMachine {
    #[state_machine_future(start, transitions(Counter))]
    Start,

    #[state_machine_future(transitions(Ready))]
    Counter(usize),

    #[state_machine_future(ready)]
    Ready(MyItem),

    #[state_machine_future(error)]
    Error(MyError),
}

impl FutureMyStateMachine for MyStateMachine {
    fn poll_start(_start: Start) -> SMPoll<AfterStart, Start, MyError> {
        transition!(AfterStart::Counter(Counter(3)))
    }
    fn poll_counter(counter: Counter) -> SMPoll<AfterCounter, Counter, MyError> {
        let value = counter.0;

        if value == 0 {
            transition!(AfterCounter::Ready(Ready(MyItem)))
        } else {
            not_ready!(Counter(value - 1))
        }
    }
}

use futures::Async;
use futures::future::Future;

#[test]
fn test_state_machine() {
    let mut sm = MyStateMachine::start();

    assert_eq!(sm.poll(), Ok(Async::NotReady));
    assert_eq!(sm.poll(), Ok(Async::NotReady));
    assert_eq!(sm.poll(), Ok(Async::NotReady));
    assert_eq!(sm.poll(), Ok(Async::Ready(MyItem)));
}

mod error {
    use super::{MyItem, MyError};
    use futures::{Poll, Async, Future};

    #[derive(StateMachineFuture)]
    enum ErrorMachine {
        #[state_machine_future(start, transitions(Fail))]
        Start,

        #[state_machine_future(transitions(Ready))]
        Fail,

        #[state_machine_future(ready)]
        Ready(MyItem),

        #[state_machine_future(error)]
        Error(MyError),
    }

    impl FutureErrorMachine for ErrorMachine {
        fn poll_start(_start: Start) -> SMPoll<AfterStart, Start, MyError> {
            transition!(AfterStart::Fail(Fail))
        }
        fn poll_fail(fail: Fail) -> SMPoll<AfterFail, Fail, MyError> {
            struct SubFut;

            impl Future for SubFut {
                type Item = MyItem;
                type Error = MyError;
                fn poll(&mut self) -> Poll<MyItem, MyError> {
                    Err(MyError)
                }
            }

            let mut subfut = SubFut;
            match subfut.poll()? {
                Async::Ready(item) => transition!(AfterFail::Ready(Ready(item))),
                Async::NotReady => not_ready!(fail),
            }
        }
    }

    #[test]
    fn test_error_machine() {
        let mut sm = ErrorMachine::start();

        assert_eq!(sm.poll(), Err(MyError));
    }
}
