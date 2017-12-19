# Unreleased

Released YYYY/MM/DD

## Added

* TODO (or remove section if none)

## Changed

* TODO (or remove section if none)

## Deprecated

* TODO (or remove section if none)

## Removed

* TODO (or remove section if none)

## Fixed

* TODO (or remove section if none)

## Security

* TODO (or remove section if none)

--------------------------------------------------------------------------------

# 0.1.4

Released 2017/12/19

# Fixed

* Using non-`pub` types within a state no longer causes compilation errors
  related to type visibility. [#6][]

[#6]: https://github.com/fitzgen/state_machine_future/issues/6

# 0.1.3

Released 2017/12/12

## Fixed

* Bounds on generic type parameters that were "inline", rather than in a `where`
  clause, work with `#[derive(StateMachineFuture)] now. [#4][]

[#4]: https://github.com/fitzgen/state_machine_future/pull/4
