#!/bin/bash

# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set -e
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_DIR="$SCRIPT_DIR/.."
INFER_BIN="$INFER_DIR/infer/bin"

if [ "$1" == "--xml" ]; then
    XML="--xml test.xml"
fi

TARGETS_TO_COMPILE=()
TARGETS_TO_TEST=()
if [ -e "$INFER_BIN/InferJava" ]; then
    TARGETS_TO_COMPILE+=('java')
    TARGETS_TO_TEST+=('java')
fi

platform=`uname`
if [ -e "$INFER_BIN/InferClang" ]; then
    TARGETS_TO_COMPILE+=('clang')
    TARGETS_TO_TEST+=('c' 'cpp')
    if command -v xcode-select > /dev/null; then
        TARGETS_TO_TEST+=('objc')
    fi
fi

# We must collect at least one target, or Infer must be recompiled
([ -z "$TARGETS_TO_TEST" ] || [ -z "$TARGETS_TO_COMPILE" ]) \
  && echo 'Infer is not compiled properly. Run make clean all' \
  && exit 1

cd $SCRIPT_DIR/..
./autogen.sh
./configure
make ${TARGETS_TO_COMPILE[@]}

# Must clean in order to force running the tests with the latest version
# of infer. There is no dependency between buck targets and infer.
buck clean
rm -rf test.xml

buck test ${TARGETS_TO_TEST[@]} $XML || exit 1

if [ -e "$INFER_BIN/InferJava" ]; then
    ./scripts/build_integration_tests.py
fi
