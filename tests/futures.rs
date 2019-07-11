#[macro_use] extern crate state_machine_future;
extern crate futures;

#[derive(Debug, PartialEq, Eq)]
struct MyItem;
#[derive(Debug, PartialEq, Eq)]
enum MyError {
}

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
