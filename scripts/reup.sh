#!/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


# re-format reason code

set -e
set -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

base=`basename $0`
tmpfile=`mktemp -t "${base}.XXXX"`

"$SCRIPT_DIR/refmt.sh" --parse re --print re "$@" > "$tmpfile"
mv "$tmpfile" "${@: -1}"
