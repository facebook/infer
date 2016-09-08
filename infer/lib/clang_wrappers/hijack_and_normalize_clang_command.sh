#!/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Given the more-or-less raw arguments passed to clang as arguments,
# this normalizes them via `clang -###` if needed to call the script
# that actually attaches the plugin on each source file. Unless we
# don't want to attach the plugin, in which case just run the original
# command.

#### Configuration ####
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# script to run our own clang
CLANG_COMPILER="${SCRIPT_DIR}/filter_args_and_run_fcp_clang"
# script to attach the plugin to clang -cc1 commands and run InferClang
CLANG_CC1_CAPTURE="${SCRIPT_DIR}/attach_plugin_and_run_clang_frontend.sh"
# path to Apple's clang
APPLE_CLANG="$FCP_APPLE_CLANG"

# Main
if [ "${0%++}" != "$0" ]; then XX="++"; fi

# Skip -cc1as commands
if [ "$1" = "-cc1as" ]; then
  STATUS=0
# Normalize clang command if not -cc1 already. -cc1 is always the first argument if present.
elif [ "$1" = "-cc1" ]; then
    "$CLANG_CC1_CAPTURE" "$@"
    STATUS=$?
else
    # Run `clang -###` to get one compile command per source file.
    # Slow since it spawns clang as a separate process
    #
    # Generate a command containing all the commands in the output of `clang -###`. These are
    # the lines that start with ' "/absolute/path/to/binary"'.
    #
    # In that command, replace /absolute/path/to/clang with our own wrapper, but only for the
    # core compiler commands (those that start with "-cc1"). This means we'll capture all
    # compilation commands (one per source file), without interfering with non-compiler commands
    # (as they run with absolute paths, so they won't get captured again further down the line).
    #
    # Fail on errors: if we detect an error in the output of `clang -###`, we add the line
    # `echo <error>; exit 1` to the generated command. This is because `clang -###` pretty much
    # never fails, but warns of failures on stderr instead.
    CC_COMMAND=$("$CLANG_COMPILER$XX" -### "$@" 2>&1 | \
        # only keep lines that are commands or errors
        grep -e '^\([[:space:]]\"\|clang: error:\)' | \
        # replace -cc1 commands with our clang wrapper
        sed -e "s#^[[:space:]]\"\([^\"]*\)\" \"-cc1\" \(.*\)\$# \"$CLANG_CC1_CAPTURE\" \"-cc1\" \2#g" | \
        # do not run if language is assembler or assembler-with-cpp
        grep -v -- '"-x" "assembler' | \
        # do not run -cc1as commands
        grep -v -- '"-cc1as"' | \
        # replace error messages by failures
        sed -e 's#^\(^clang: error:.*$\)#echo "\1"; exit 1#g' | \
        # add trailing ; to each line
        sed -e 's/$/;/g')
    if [ -n "$CC_COMMAND" ]; then
        eval $CC_COMMAND
    else
        # No command to execute after -###, this is fishy, let's execute the original command
        # instead.
        #
        # In particular, this can happen when the user tries to run `infer -- clang -c
        # file_that_does_not_exist.c`. In this case, this will fail with the appropriate error
        # message from clang instead of silently analyzing 0 files.
        "$CLANG_COMPILER$XX" "$@"
    fi
    STATUS=$?
fi

# run Apple clang if required (and if any)
if [ -n "$APPLE_CLANG" ]; then
    "$APPLE_CLANG$XX" "$@" || exit $?
fi

exit $STATUS
