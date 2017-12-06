//! Compile-Fail Tests

mod no_start_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod no_error_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }


    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),
    }
    ```
     */
}

mod no_ready_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }


    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Error))]
        Start,

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod more_than_one_start_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start1<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start1>
        ) -> Poll<AfterStart1, usize> {
            unimplemented!()
        }
        fn poll_start2<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start2>
        ) -> Poll<AfterStart2, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start1,

        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start2,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
    */
}

mod more_than_one_error_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }


    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error1(usize),

        #[state_machine_future(error)]
        Error2(usize),
    }
    ```
     */
}

mod more_than_one_ready_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready1))]
        Start,

        #[state_machine_future(ready)]
        Ready1(usize),

        #[state_machine_future(ready)]
        Ready2(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod no_transitions_on_non_ready_or_error_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        Start,

        #[state_machine_future(ready)]
        #[state_machine_future(error)]
        Ready(usize),
    }
    ```
     */
}

mod ready_state_with_multiple_fields {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize, usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}


mod error_state_with_multiple_fields {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize, usize),
    }
    ```
     */
}

mod ready_state_with_unit_style_variant {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready,

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod error_state_with_unit_style_variant {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }


    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error,
    }
    ```
     */
}

mod ready_state_with_struct_style_variant {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready { x: usize },

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod error_state_with_struct_style_variant {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error { x: usize },
    }
    ```
     */
}

mod ready_state_with_transitions {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        #[state_machine_future(transitions(Error))]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod error_state_with_transitions {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        #[state_machine_future(transitions(Ready))]
        Error(usize),
    }
    ```
     */
}

mod transition_to_unknown_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(UnknownState))]
        Start,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod unreachable_intermediate_state {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(Ready))]
        Start,

        #[state_machine_future(transitions(Unreachable2))]
        Unreachable1,

        #[state_machine_future(transitions(Ready))]
        Unreachable2,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}

mod reachable_state_without_path_to_ready_or_error {
    /*!
    ```compile_fail
    #[macro_use]
    extern crate state_machine_future;
    extern crate futures;
    use futures::*;
    fn main() {}
    impl PollMachine for Machine {
        fn poll_start<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, Start>
        ) -> Poll<AfterStart, usize> {
            unimplemented!()
        }
        fn poll_never_ready1<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, NeverReady1>
        ) -> Poll<AfterNeverReady1, usize> {
            unimplemented!()
        }
        fn poll_never_ready2<'a>(
            _: &'a mut state_machine_future::RentToOwn<'a, NeverReady2>
        ) -> Poll<AfterNeverReady2, usize> {
            unimplemented!()
        }
    }

    #[derive(StateMachineFuture)]
    pub enum Machine {
        #[state_machine_future(start)]
        #[state_machine_future(transitions(NeverReady1))]
        Start,

        #[state_machine_future(transitions(NeverReady2))]
        NeverReady1,

        #[state_machine_future(transitions(NeverReady1))]
        NeverReady2,

        #[state_machine_future(ready)]
        Ready(usize),

        #[state_machine_future(error)]
        Error(usize),
    }
    ```
     */
}
