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

XML=
if [ "$1" == "--xml" ]; then
    XML="_xml"
fi

make -j -C "$INFER_DIR" test_build test$XML
