/// Auxiliary macro for `poll_state_xy` functions to transition into a new state.
#[macro_export]
macro_rules! transition {
    ( $new_state:expr ) => {
        return ::state_machine_future::SMPoll::Ready($new_state);
    };
}

/// Auxiliary macro for `poll_state_xy` functions to declare not ready
#[macro_export]
macro_rules! not_ready {
    ( $new_state:expr ) => {
        return ::state_machine_future::SMPoll::NotReady($new_state);
    };
}

/// Auxiliary macro for `poll_state_xy` functions to return error
#[macro_export]
macro_rules! error {
    ( $new_state:expr ) => {
        return ::state_machine_future::SMPoll::Error($new_state);
    };
}
