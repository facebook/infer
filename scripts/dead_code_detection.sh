#!/bin/bash

# Copyright (c) 2018 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


# Dead code detection for infer's source code. See comments in infer/src/deadcode/Makefile.

set -e
set -o pipefail
set -u

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

make -C "$SCRIPT_DIR"/../infer/src/deadcode
