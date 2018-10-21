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

# 0.1.8

Released 2018/10/21

## Changed

* Updated darling and syn to latest version. Thanks [@hcpl][]. [#26][]

## Fixed

* Type parameter missing when used through a projection [#25][]

--------------------------------------------------------------------------------

# 0.1.7

Released 2018/06/04

## Added

* Added [@bkchr][] to the team! \o/

## Fixed

* Fixed potential "field is never used" warning from inside the generated code. [#19][]

[#19]: https://github.com/fitzgen/state_machine_future/pull/19
[@bkchr]: https://github.com/bkchr

--------------------------------------------------------------------------------

# 0.1.6

Released 2018/02/12

## Added

* Added the `transition!(..)` macro to make state transitions a little bit less
  boilerplate-y. [#16][]

[#16]: https://github.com/fitzgen/state_machine_future/pull/16

--------------------------------------------------------------------------------

# 0.1.5

Released 2018/1/4

## Added

* Support for heterogeneous usage of generic type- and lifetime-parameters
  across states. Previously, every state had to use every generic parameter
  (requiring `PhantomData` when it didn't need to use one). Now, states that
  don't need a generic parameter don't have to use it. [#10][]

[#10]: https://github.com/fitzgen/state_machine_future/pull/10

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
