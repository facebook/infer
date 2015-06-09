#!/bin/bash

# Copyright 2013 - present Facebook.
# All Rights Reserved.

set -e
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_DIR="$SCRIPT_DIR/.."
INFER_BIN="$INFER_DIR/infer/bin"

if [ "$1" == "--xml" ]; then
    XML="--xml test.xml"
fi

platform=`uname`

TARGETS_TO_COMPILE=()
TARGETS_TO_TEST=()
if [ -e "$INFER_BIN/InferJava" ]; then
    TARGETS_TO_COMPILE+=('java')
    TARGETS_TO_TEST+=('java')
fi

if [ -e "$INFER_BIN/InferClang" ]; then
    TARGETS_TO_COMPILE=('clang')
    TARGETS_TO_TEST+=('c' 'cpp')
    if [ $platform == 'Darwin' ]; then
        TARGETS_TO_TEST+=('objc')
    fi
fi

# We must collect at least one target, or Infer must be recompiled
([ -z "$TARGETS_TO_TEST" ] || [ -z "$TARGETS_TO_COMPILE" ]) \
  && echo 'Infer is not compiled properly. Run make -C infer clean && make -C infer' \
  && exit 1

cd $SCRIPT_DIR/..
make -C infer ${TARGETS_TO_COMPILE[@]}

# Must clean in order to force running the tests with the latest version
# of infer. There is no dependency between buck targets and infer.
buck clean
rm -rf test.xml

buck test ${TARGETS_TO_TEST[@]} $XML || exit 1
