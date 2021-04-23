#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -o pipefail
set -u

basedir=$(dirname "${BASH_SOURCE[0]}")
out=$("${basedir}"/erlang.escript "$@")
exit_code=$?
echo "$out"
if [ $exit_code = 0 ]; then
    sh -c "$out"
    exit $?
fi
exit $exit_code
