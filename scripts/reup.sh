#/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


# re-format reason code

base=`basename $0`
TMPFILE=`mktemp -t ${base}` || exit 1
refmt -print-width 100 -heuristics-file unary.txt -parse re -print re $1 > $TMPFILE
mv $TMPFILE $1
