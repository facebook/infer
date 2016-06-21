#!/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# go to the root of the repo -- assumes we are in .travis/
cd "${SCRIPT_DIR}/.."

echo 'Running `infer -- javac examples/Hello.java`'
infer -- javac examples/Hello.java | grep -q 'NULL_DEREFERENCE: *1'
echo
