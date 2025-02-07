#!/bin/sh

set -e
set -x

if test "$#" != 1; then
    echo "Usage: $0 <prefix>"
    exit 1
fi

PREFIX=$1

dune install "--prefix=$PREFIX" --release
