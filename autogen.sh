#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# make sure we run from the root of the repo
pushd "$SCRIPT_DIR" > /dev/null

# We need to record the date that the documentation was last modified to put in our man
# pages. Unfortunately that information is only available reliably from `git`, which we don't have
# access to from other distributions of the infer source code. Such source distributions should
# distribute the "configure" script too. The idea is to bake this date inside "configure" so that
# it's available at build time. We do that by generating an m4 macro that hardcodes the date we
# compute in this script for "configure" to find.
MAN_LAST_MODIFIED_M4=m4/__GENERATED__ac_check_infer_man_last_modified.m4
printf 'generating %s' "$MAN_LAST_MODIFIED_M4... "
if test -d '.git' ; then
  # date at which the man pages were last modified, to record in the manpages themselves
  MAN_FILES=(
      infer/src/base/CommandLineOption.ml
      infer/src/base/Config.ml
  )
  MAN_DATE=$(git log -n 1 --pretty=format:%cd --date=short -- "${MAN_FILES[@]}")
  INFER_MAN_LAST_MODIFIED=${INFER_MAN_LAST_MODIFIED:-$MAN_DATE}
else
  echo 'no git repository detected; setting last modified date to today'
  # best effort: get today's date
  INFER_MAN_LAST_MODIFIED=${INFER_MAN_LAST_MODIFIED:-$(date +%Y-%m-%d)}
fi

printf "AC_DEFUN([AC_CHECK_INFER_MAN_LAST_MODIFIED],\n" > "$MAN_LAST_MODIFIED_M4"
printf "[INFER_MAN_LAST_MODIFIED=%s\n" "$INFER_MAN_LAST_MODIFIED" >> "$MAN_LAST_MODIFIED_M4"
printf " AC_SUBST([INFER_MAN_LAST_MODIFIED])\n" >> "$MAN_LAST_MODIFIED_M4"
printf "])\n" >> "$MAN_LAST_MODIFIED_M4"
printf 'done\n'

# older versions of `autoreconf` only support including macros via acinclude.m4
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
