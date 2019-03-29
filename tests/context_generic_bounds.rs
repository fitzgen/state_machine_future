//! Test using a generic context with type bounds in a non-generic state
//! machine.

extern crate futures;
#[macro_use]
extern crate state_machine_future;

use futures::Poll;
use state_machine_future::RentToOwn;

#[allow(dead_code)]
pub struct Context<T, U, V: Send> where T: Clone {
    data1: T,
    data2: U,
    data3: V,
}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "Context<T: Clone, U, V: Send>")]
pub enum WithContext {
    #[state_machine_future(start, transitions(Ready))]
    Start(),

    #[state_machine_future(ready)]
    Ready(()),

    #[state_machine_future(error)]
    Error(()),
}

impl PollWithContext for WithContext {
    fn poll_start<'s, 'c, T: Clone, U, V>(
        _: &'s mut RentToOwn<'s, Start>,
        context: &'c mut RentToOwn<'c, Context<T, U, V>>,
    ) -> Poll<AfterStart, ()> where V: Send {
        let _ = context.data1.clone();
        transition!(Ready(()))
    }
}

#[test]
fn nongeneric_sm_with_generic_context_with_bounds() {
    let context = Context {
        data1: 1,
        data2: "2",
        data3: '3',
    };

    let _ = WithContext::start(context);
}
