#!/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -x

# assumes we are in .travis/
cd "${SCRIPT_DIR}/../docker"

docker build -t infer .

echo 'Running `infer -- javac /infer/examples/Hello.java`'
docker run -t infer \
  /bin/bash -c \
    'infer -- javac /infer/examples/Hello.java | grep -q "NULL_DEREFERENCE: *1"'
echo

echo 'Running `infer -- clang -c /infer/examples/hello.c`'
docker run -t infer \
  /bin/bash -c \
    'infer -- clang -c /infer/examples/hello.c | grep -q "NULL_DEREFERENCE: *1"'
echo
