#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

## Wrapper around the bytecode versions of infer
# This is needed to find the dynamic libraries of our C stubs

if [ "$#" -lt 1 ]; then
  echo "usage: $0 BYTECODE_PROGRAM" >&2
  exit 1
fi

prog=$1
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SRC_DIR="$SCRIPT_DIR"/../infer/src
BUILD_MODE=${BUILD_MODE:-default}
DUNE_INSTALL_DIR=${INSTALL_DIR:-"$SRC_DIR/_build/install/$BUILD_MODE"}

export CAML_LD_LIBRARY_PATH="$DUNE_INSTALL_DIR"/lib/stublibs:"$CAML_LD_LIBRARY_PATH"

exec "$prog" "$@"
