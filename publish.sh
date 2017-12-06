#!/usr/bin/env bash

set -eux

cd $(dirname "$0")

pushd ./derive_state_machine_future
cargo publish --dry-run

popd
cargo publish --dry-run

pushd ./derive_state_machine_future
cargo publish

popd
cargo publish
