#!/bin/bash


# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# usage: gen_version.sh <file> [<version>]

FILE="$1"

if [[ ! -z "$2" ]]; then
    # second arg passed when called from opam
    VERSION="$2"
else
    # second arg omitted when called from src/dune
    if [[ ! "%%VERSION%%" == "%%"*"%%" ]]; then
        # file has been watermarked when building distrib archive
        VERSION="%%VERSION%%"
    else
        # file has not been watermarked when building in dev git tree
        VERSION=$(git describe --dirty --always)
    fi
fi

sed -e "s|%%VERSION%[%]|$VERSION|g" $FILE.in > $FILE
