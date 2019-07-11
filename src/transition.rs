/// Auxiliary macro for `poll_state_xy` functions to transition into a new state.
#[macro_export]
macro_rules! transition {
    ( $new_state:expr ) => {
        return Ok(::state_machine_future::SMAsync::Ready($new_state));
    };
}

/// Auxiliary macro for `poll_state_xy` functions to declare not ready
#[macro_export]
macro_rules! not_ready {
    ( $new_state:expr ) => {
        return Ok(::state_machine_future::SMAsync::NotReady($new_state));
    };
}
