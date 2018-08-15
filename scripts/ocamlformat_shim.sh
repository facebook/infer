#!/bin/bash

# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Run ocamlformat, installing the required version via opam if necessary.

set -x
set -e

TARGET_VERSION=$(grep version .ocamlformat | cut -d ' ' -f 2)

if [ -z $TARGET_VERSION ]
then
  echo "Could not find .ocamlformat file containing version"
  exit 1
fi

if [ ! -x "$(command -v ocamlformat)" ] ||
   [ "$TARGET_VERSION" != "$(ocamlformat --version)" ]
then
    opam install "ocamlformat.$TARGET_VERSION"
fi

ocamlformat "$@"
