#!/bin/sh

# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set -e

# try to pull submodules if we are in a git repo
# might fail if git is not installed (how did you even checkout the
# repo in the first place?)
if test -d '.git' && [ -z "$SKIP_SUBMODULES" ] ; then
  printf 'git repository detected, updating submodule... '
  git submodule update --init > /dev/null
  printf 'done\n'
else
  echo 'no git repository detected; not updating git submodules'
fi

ACINCLUDE="acinclude.m4"
printf "generating $ACINCLUDE..."
cat m4/*.m4 > "$ACINCLUDE"
printf " done\n"

printf "generating ./configure script..."
autoreconf -fi
printf " done\n"

echo ""
echo "you may now run the following commands to build Infer:"
echo ""
echo "  ./configure"
echo "  make"
echo ""
echo 'run `./configure --help` for more options'
