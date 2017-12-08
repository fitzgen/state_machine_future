# Contributing to `state_machine_future`

Hi! We'd love to have your contributions! If you want help or mentorship, reach
out to us in a GitHub issue, or ping `fitzgen`
in [#rust on irc.mozilla.org](irc://irc.mozilla.org#rust) and introduce
yourself.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Code of Conduct](#code-of-conduct)
- [Building](#building)
- [Testing](#testing)
- [Automatic code formatting](#automatic-code-formatting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Code of Conduct

We abide by the [Rust Code of Conduct][coc] and ask that you do as well.

[coc]: https://www.rust-lang.org/en-US/conduct.html

## Building

```
$ cargo build
```

## Testing

The tests require `cargo-readme` to be installed:
```
$ cargo install cargo-readme
```

To run all the tests:

```
$ cargo test
```

## Automatic code formatting

We use [`rustfmt`](https://github.com/rust-lang-nursery/rustfmt) to enforce a
consistent code style across the whole code base.

You can install the latest version of `rustfmt` with this command:

```
$ rustup update nightly
$ cargo install -f rustfmt-nightly
```

Ensure that `~/.cargo/bin` is on your path.

Once that is taken care of, you can (re)format all code by running this command:

```
$ cargo fmt
```

The code style is described in the `rustfmt.toml` file in top level of the repo.
