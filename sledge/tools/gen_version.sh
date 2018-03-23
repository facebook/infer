#!/bin/bash

#
# Copyright (c) 2018 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.
#

# usage: gen_version.sh <file> [<version>]

FILE="$1"

if [[ ! -z "$2" ]]; then
    # second arg passed when called from opam
    VERSION="$2"
else
    # second arg omitted when called from src/jbuild
    if [[ ! "%%VERSION%%" == "%%"*"%%" ]]; then
        # file has been watermarked when building distrib archive
        VERSION="%%VERSION%%"
    else
        # file has not been watermarked when building in dev git tree
        VERSION=$(git describe --tags --dirty --always)
    fi
fi

(test -e $FILE || touch $FILE);

sed -e "s|%%VERSION%[%]|$VERSION|g" $FILE.in | diff $FILE - | patch --silent $FILE
