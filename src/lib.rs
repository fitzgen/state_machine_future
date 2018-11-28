/*!

[![](https://docs.rs/state_machine_future/badge.svg)](https://docs.rs/state_machine_future/) [![](https://img.shields.io/crates/v/state_machine_future.svg)](https://crates.io/crates/state_machine_future) [![](https://img.shields.io/crates/d/state_machine_future.png)](https://crates.io/crates/state_machine_future) [![Build Status](https://travis-ci.org/fitzgen/state_machine_future.png?branch=master)](https://travis-ci.org/fitzgen/state_machine_future)

Easily create type-safe `Future`s from state machines — without the boilerplate.

`state_machine_future` type checks state machines and their state transitions,
and then generates `Future` implementations and typestate<sup>[0][]</sup>
boilerplate for you.

* [Introduction](#introduction)
* [Guide](#guide)
* [Example](#example)
* [Attributes](#attributes)
* [Macro](#macro)
* [Features](#features)
* [License](#license)
* [Contribution](#contribution)

## Introduction

Most of the time, using `Future` combinators like `map` and `then` are a great
way to describe an asynchronous computation. Other times, the most natural way
to describe the process at hand is a state machine.

When writing state machines in Rust, we want to *leverage the type system to
enforce that only valid state transitions may occur*. To do that, we want
*typestates*<sup>[0][]</sup>: types that represents each state in the state
machine, and methods whose signatures only permit valid state transitions. But
we *also* need an `enum` of every possible state, so we can treat the whole
state machine as a single entity, and implement `Future` for it. But this is
getting to be a *lot* of boilerplate...

Enter `#[derive(StateMachineFuture)]`.

With `#[derive(StateMachineFuture)]`, we describe the states and the possible
transitions between them, and then the custom derive generates:

* A typestate for each state in the state machine.

* A type for the whole state machine that implements `Future`.

* A concrete `start` method that constructs the state machine `Future` for you,
initialized to its start state.

* A state transition polling trait, with a `poll_zee_choo` method for each
non-final state `ZeeChoo`. This trait describes the state machine's valid
transitions, and its methods are called by `Future::poll`.

Then, all *we* need to do is implement the generated state transition polling
trait.

Additionally, `#[derive(StateMachineFuture)]` will statically prevent against
some footguns that can arise when writing state machines:

* Every state is reachable from the start state: *there are no useless states.*

* *There are no states which cannot reach a final state*. These states would
otherwise lead to infinite loops.

* *All state transitions are valid.* Attempting to make an invalid state
transition fails to type check, thanks to the generated typestates.

## Guide

Describe the state machine's states with an `enum` and add
`#[derive(StateMachineFuture)]` to it:

```ignore
#[derive(StateMachineFuture)]
enum MyStateMachine {
    // ...
}
```

There must be one **start** state, which is the initial state upon construction;
one **ready** state, which corresponds to `Future::Item`; and one **error**
state, which corresponds to `Future::Error`.

```ignore
#[derive(StateMachineFuture)]
enum MyStateMachine {
    #[state_machine_future(start)]
    Start,

    // ...

    #[state_machine_future(ready)]
    Ready(MyItem),

    #[state_machine_future(error)]
    Error(MyError),
}
```

Any other variants of the `enum` are intermediate states.

We define which state-to-state transitions are valid with
`#[state_machine_future(transitions(...))]`. This attribute annotates a state
variant, and lists which other states can be transitioned to immediately after
this state.

A final state (either **ready** or **error**) must be reachable from every
intermediate state and the **start** state. Final states are not allowed to have
transitions.

```ignore
#[derive(StateMachineFuture)]
enum MyStateMachine {
    #[state_machine_future(start, transitions(Intermediate))]
    Start,

    #[state_machine_future(transitions(Start, Ready))]
    Intermediate { x: usize, y: usize },

    #[state_machine_future(ready)]
    Ready(MyItem),

    #[state_machine_future(error)]
    Error(MyError),
}
```

From this state machine description, the custom derive generates boilerplate for
us.

For each state, the custom derive creates:

* A typestate for the state. The type's name matches the variant name, for
example the `Intermediate` state variant's typestate is also named `Intermediate`.
The kind of struct type generated matches the variant kind: a unit-style variant
results in a unit struct, a tuple-style variant results in a tuple struct, and a
struct-style variant results in a normal struct with fields.

| State `enum` Variant                              | Generated Typestate            |
| ------------------------------------------------- | ------------------------------ |
| `enum StateMachine { MyState, ... }`              | `struct MyState;`              |
| `enum StateMachine { MyState(bool, usize), ... }` | `struct MyState(bool, usize);` |
| `enum StateMachine { MyState { x: usize }, ... }` | `struct MyState { x: usize };` |

* An `enum` for the possible states that can come after this state. This `enum`
is named `AfterX` where `X` is the state's name. There is also a `From<Y>`
implementation for each `Y` state that can be transitioned to after `X`. For
example, the `Intermediate` state would get:

```ignore
enum AfterIntermediate {
    Start(Start),
    Ready(Ready),
}

impl From<Start> for AfterIntermediate {
    // ...
}

impl From<Ready> for AfterIntermediate {
    // ...
}
```

Next, for the state machine as a whole, the custom derive generates:

* A state machine `Future` type, which is essentially an `enum` of all the
different typestates. This type is named `BlahFuture` where `Blah` is the name
of the state machine description `enum`. In this example, where the state
machine description is named `MyStateMachine`, the generated state machine
future type would be named `MyStateMachineFuture`.

* A polling trait, `PollBordle` where `Bordle` is this state machine
description's name. For each non-final state `TootWasabi`, this trait has a
method, `poll_toot_wasabi`, which is like `Future::poll` but specialized to the
current state. Each method takes conditional ownership of its state (via
[`RentToOwn`][rent_to_own]) and returns a `futures::Poll<AfterThisState, Error>`
where `Error` is the state machine's error type. This signature *does not allow
invalid state transitions*, which makes attempting an illegal state transition
fail to type check. Here is the `MyStateMachine`'s polling trait, for example:

```ignore
trait PollMyStateMachine {
    fn poll_start<'a>(
        start: &'a mut RentToOwn<'a, Start>,
    ) -> Poll<AfterStart, Error>;

    fn poll_intermediate<'a>(
        intermediate: &'a mut RentToOwn<'a, Intermediate>,
    ) -> Poll<AfterIntermediate, Error>;
}
```

* An implementation of `Future` for that type. This implementation dispatches to
the appropriate polling trait method depending on what state the future is
in:

  * If the `Future` is in the `Start` state, then it uses `<MyStateMachine as
    PollMyStateMachine>::poll_start`.

  * If it is in the `Intermediate` state, then it uses `<MyStateMachine as
    PollMyStateMachine>::poll_intermediate`.

  * Etc...

* A concrete `start` method for the description type (so `MyStateMachine::start`
in this example) which constructs a new state machine `Future` type in its
**start** state for you. This method has a parameter for each field in the
**start** state variant.

| Start `enum` Variant            | Generated `start` Method                                            |
| ------------------------------- | ------------------------------------------------------------------- |
| `MyStart,`                      | `fn start() -> MyStateMachineFuture { ... }`                        |
| `MyStart(bool, usize),`         | `fn start(arg0: bool, arg1: usize) -> MyStateMachineFuture { ... }` |
| `MyStart { x: char, y: bool },` | `fn start(x: char, y: bool) -> MyStateMachineFuture { ... }`        |

Given all those generated types and traits, all we have to do is `impl PollBlah
for Blah` for our state machine `Blah`.

```ignore
impl PollMyStateMachine for MyStateMachine {
    fn poll_start<'a>(
        start: &'a mut RentToOwn<'a, Start>
    ) -> Poll<AfterStart, MyError> {
        // Call `try_ready!(start.inner.poll())` with any inner futures here.
        //
        // If we're ready to transition states, then we should return
        // `Ok(Async::Ready(AfterStart))`. If we are not ready to transition
        // states, return `Ok(Async::NotReady)`. If we encounter an error,
        // return `Err(...)`.
    }

    fn poll_intermediate<'a>(
        intermediate: &'a mut RentToOwn<'a, Intermediate>
    ) -> Poll<AfterIntermediate, MyError> {
        // Same deal as above...
    }
}
```

## Context

The state machine also allows to pass in a context that is available in every `poll_*` method
without having to explicitly include it in every one.

The context can be specified through the `context` argument of the `state_machine_future` attribute.
This will add parameters to the `start` method as well as to each `poll_*` method of the trait.

```
#[macro_use]
extern crate state_machine_future;
extern crate futures;

use futures::*;
use state_machine_future::*;

struct MyContext {

}

struct MyItem {

}

enum MyError {

}

#[derive(StateMachineFuture)]
#[state_machine_future(context = "MyContext")]
enum MyStateMachine {
    #[state_machine_future(start, transitions(Intermediate))]
    Start,

    #[state_machine_future(transitions(Start, Ready))]
    Intermediate { x: usize, y: usize },

    #[state_machine_future(ready)]
    Ready(MyItem),

    #[state_machine_future(error)]
    Error(MyError),
}

impl PollMyStateMachine for MyStateMachine {
    fn poll_start<'s, 'c>(
        start: &'s mut RentToOwn<'s, Start>,
        context: &'c mut RentToOwn<'c, MyContext>
    ) -> Poll<AfterStart, MyError> {

        // The `context` instance passed into `start` is available here.
        // It is a mutable reference, so are free to modify it.

        unimplemented!()
    }

    fn poll_intermediate<'s, 'c>(
        intermediate: &'s mut RentToOwn<'s, Intermediate>,
        context: &'c mut RentToOwn<'c, MyContext>
    ) -> Poll<AfterIntermediate, MyError> {

        // The `context` is available here as well.
        // It is the same instance. This means if `poll_start` modified it, those
        // changes will be visible to this method as well.

        unimplemented!()
    }
}

fn main() {
    let _ = MyStateMachine::start(MyContext { });
}
```

Same as for the state argument, the context can be taken through the `RentToOwn` type!
However, be aware that once you take the context, the state machine will **always** return
`Async::NotReady` **without** invoking the `poll_` methods anymore. The one exception to
this is when the state machine is in a ready or error state, where it will resolve normally
when polled if the context has been taken.

That's it!

## Example

Here is an example of a simple turn-based game played by two players over HTTP.

```
#[macro_use]
extern crate state_machine_future;

#[macro_use]
extern crate futures;

use futures::{Async, Future, Poll};
use state_machine_future::RentToOwn;
# pub struct Player;
# impl Player {
#     fn request_turn(&self) -> HttpTurnFuture { unimplemented!() }
# }
# pub struct HttpError;
# pub struct HttpInvitationFuture;
# impl Future for HttpInvitationFuture {
#     type Item = ();
#     type Error = HttpError;
#     fn poll(&mut self) -> Poll<(), HttpError> {
#         unimplemented!()
#     }
# }
# pub struct HttpTurnFuture;
# impl Future for HttpTurnFuture {
#     type Item = ();
#     type Error = HttpError;
#     fn poll(&mut self) -> Poll<(), HttpError> {
#         unimplemented!()
#     }
# }
# fn process_turn(_: ()) -> Option<GameResult> { unimplemented!() }
# fn main() {}

/// The result of a game.
pub struct GameResult {
    winner: Player,
    loser: Player,
}

/// Some kind of simple turn based game.
///
/// ```text
///              Invite
///                |
///                |
///                | accept invitation
///                |
///                |
///                V
///           WaitingForTurn --------+
///                |   ^             |
///                |   |             | receive turn
///                |   |             |
///                |   +-------------+
/// game concludes |
///                |
///                |
///                |
///                V
///            Finished
/// ```
#[derive(StateMachineFuture)]
enum Game {
    /// The game begins with an invitation to play from one player to another.
    ///
    /// Once the invited player accepts the invitation over HTTP, then we will
    /// switch states into playing the game, waiting to recieve each turn.
    #[state_machine_future(start, transitions(WaitingForTurn))]
    Invite {
        invitation: HttpInvitationFuture,
        from: Player,
        to: Player,
    },

    // We are waiting on a turn.
    //
    // Upon receiving it, if the game is now complete, then we go to the
    // `Finished` state. Otherwise, we give the other player a turn.
    #[state_machine_future(transitions(WaitingForTurn, Finished))]
    WaitingForTurn {
        turn: HttpTurnFuture,
        active: Player,
        idle: Player,
    },

    // The game is finished with a `GameResult`.
    //
    // The `GameResult` becomes the `Future::Item`.
    #[state_machine_future(ready)]
    Finished(GameResult),

    // Any state transition can implicitly go to this error state if we get an
    // `HttpError` while waiting on a turn or invitation acceptance.
    //
    // This `HttpError` is used as the `Future::Error`.
    #[state_machine_future(error)]
    Error(HttpError),
}

// Now, we implement the generated state transition polling trait for our state
// machine description type.

impl PollGame for Game {
    fn poll_invite<'a>(
        invite: &'a mut RentToOwn<'a, Invite>
    ) -> Poll<AfterInvite, HttpError> {
        // See if the invitation has been accepted. If not, this will early
        // return with `Ok(Async::NotReady)` or propagate any HTTP errors.
        try_ready!(invite.invitation.poll());

        // We're ready to transition into the `WaitingForTurn` state, so take
        // ownership of the `Invite` and then construct and return the new
        // state.
        let invite = invite.take();
        let waiting = WaitingForTurn {
            turn: invite.from.request_turn(),
            active: invite.from,
            idle: invite.to,
        };
        transition!(waiting)
    }

    fn poll_waiting_for_turn<'a>(
        waiting: &'a mut RentToOwn<'a, WaitingForTurn>
    ) -> Poll<AfterWaitingForTurn, HttpError> {
        // See if the next turn has arrived over HTTP. Again, this will early
        // return `Ok(Async::NotReady)` if the turn hasn't arrived yet, and
        // propagate any HTTP errors that we might encounter.
        let turn = try_ready!(waiting.turn.poll());

        // Ok, we have a new turn. Take ownership of the `WaitingForTurn` state,
        // process the turn and if the game is over, then transition to the
        // `Finished` state, otherwise swap which player we need a new turn from
        // and request the turn over HTTP.
        let waiting = waiting.take();
        if let Some(game_result) = process_turn(turn) {
            transition!(Finished(game_result))
        } else {
            let next_waiting = WaitingForTurn {
                turn: waiting.idle.request_turn(),
                active: waiting.idle,
                idle: waiting.active,
            };
            Ok(Async::Ready(next_waiting.into()))
        }
    }
}

# struct TokioHandle;
# impl TokioHandle { fn spawn<T>(&self, _: T) {} }
# fn get_some_player() -> Player { unimplemented!() }
# fn get_another_player() -> Player { unimplemented!() }
# fn invite(_: &Player, _: &Player) -> HttpInvitationFuture { unimplemented!() }
// To spawn a new `Game` as a `Future` on whatever executor we're using (for
// example `tokio`), we use `Game::start` to construct the `Future` in its start
// state and then pass it to the executor.
fn spawn_game(handle: TokioHandle) {
    let from = get_some_player();
    let to = get_another_player();
    let invitation = invite(&from, &to);
    let future = Game::start(invitation, from, to);
    handle.spawn(future)
}
```

## Attributes

This is a list of all of the attributes used by `state_machine_future`:

* `#[derive(StateMachineFuture)]`: Placed on an `enum` that describes a state
machine.

* `#[state_machine_future(derive(Clone, Debug, ...))]`: Placed on the `enum`
that describes the state machine. This attribute describes which
`#[derive(...)]`s to place on the generated `Future` type.

* `#[state_machine_future(start)]`: Used on a variant of the state machine
description `enum`. There must be exactly one variant with this attribute. This
describes the initial starting state. The generated `start` method has a
parameter for each field in this variant.

* `#[state_machine_future(ready)]`: Used on a variant of the state machine
description `enum`. There must be exactly one variant with this attribute. It
must be a tuple-style variant with one field, for example `Ready(MyItemType)`.
The generated `Future` implementation uses the field's type as `Future::Item`.

* `#[state_machine_future(error)]`: Used on a variant of the state machine
description `enum`. There must be exactly one variant with this attribute. It
must be a tuple-style variant with one field, for example `Error(MyError)`.  The
generated `Future` implementation uses the field's type as `Future::Error`.

* `#[state_machine_future(transitions(OtherState, AnotherState, ...))]`: Used on
a variant of the state machine description `enum`. Describes the states that
this one can transition to.

## Macro

An auxiliary macro is provided that helps reducing boilerplate code for state
transitions. So, the following code:

```Ok(Ready(NextState(1).into()))```

Can be reduced to:

```transition!(NextState(1))```

## Features

Here are the `cargo` features that you can enable:

* `debug_code_generation`: Prints the code generated by
`#[derive(StateMachineFuture)]` to `stdout` for debugging purposes.

## License

Licensed under either of

 * [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

 * [MIT license](http://opensource.org/licenses/MIT)

at your option.

## Contribution

See
[CONTRIBUTING.md](https://github.com/fitzgen/state_machine_future/blob/master/CONTRIBUTING.md)
for hacking.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.


[0]: https://en.wikipedia.org/wiki/Typestate_analysis

[rent_to_own]: https://crates.io/crates/rent_to_own

 */

#![deny(missing_docs)]
#![deny(missing_debug_implementations)]

extern crate futures;
extern crate rent_to_own;

// Re-export the custom derive. This allows people to depend only on this crate
// directly, not both this one and `derive_state_machine_future`.
#[allow(unused_imports)]
#[macro_use]
extern crate derive_state_machine_future;
pub use derive_state_machine_future::*;

mod compile_fail_tests;
#[macro_use]
mod transition;

// Helpers used by generated code. Not public API.
#[doc(hidden)]
pub mod export {
    pub use futures::{Async, Future, Poll};
}

/// Re-export of `rent_to_own::RentToOwn`.
pub type RentToOwn<'a, T> = rent_to_own::RentToOwn<'a, T>;

/// A trait that links an `enum` with `#[derive(StateMachineFuture)]` to its
/// generated type that implements `Future`.
pub trait StateMachineFuture {
    /// The generated `Future` type for this state machine.
    type Future: futures::Future;
}
