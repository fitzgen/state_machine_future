//! Test that we handle `pub`, `pub(self)`, `pub(super)`, `pub(crate)`,
//! `pub(in some::module)`, and non-`pub` state machines.

#![allow(dead_code)]

extern crate futures;
#[macro_use]
extern crate state_machine_future;

#[derive(StateMachineFuture)]
pub enum Pub {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    PubState(()),
}

#[derive(StateMachineFuture)]
pub(self) enum PubSelf {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    PubSelfState(()),
}

mod namespace {
    #[derive(StateMachineFuture)]
    pub(super) enum PubSuper {
        #[state_machine_future(start)]
        #[state_machine_future(ready)]
        #[state_machine_future(error)]
        PubSuperState(()),
    }
}

#[derive(StateMachineFuture)]
pub(crate) enum PubCrate {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    PubCrateState(()),
}

mod some {
    mod module {
        mod inner {
            #[derive(StateMachineFuture)]
            pub(in some::module) enum PubInSomeModule {
                #[state_machine_future(start)]
                #[state_machine_future(ready)]
                #[state_machine_future(error)]
                PubInSomeModuleState(()),
            }
        }
    }
}

#[derive(StateMachineFuture)]
enum NonPub {
    #[state_machine_future(start)]
    #[state_machine_future(ready)]
    #[state_machine_future(error)]
    NonPubState(()),
}
