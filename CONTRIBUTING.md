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
$ rustup component add rustfmt-preview --toolchain nightly
```

Ensure that `~/.rustup/toolchains/$YOUR_HOST_TARGET/bin/` is on your `$PATH`.

Once that is taken care of, you can (re)format all code by running this command
from the root of the repository:

```
$ cargo +nightly fmt
```

## Pull Requests

All pull requests must be reviewed and approved of by at least one [team](#team)
member before merging.

## Team

* `fitzgen`
* `bkchr`

Larger, more nuanced decisions about design, architecture, breaking changes,
trade offs, etc are made by team consensus. In other words, decisions on things
that aren't straightforward improvements or bug fixes to things that already
exist in `state_machine_future`. If consensus can't be made, then `fitzgen` has
the last word.
