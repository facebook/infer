#!/bin/bash

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# go to the root of the repo -- assumes we are in .travis/
cd "${SCRIPT_DIR}/.."

echo 'Running `infer -- javac examples/Hello.java`'
infer -- javac examples/Hello.java | grep -q 'NULL_DEREFERENCE: *1'
echo
