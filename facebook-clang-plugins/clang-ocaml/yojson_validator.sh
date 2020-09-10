#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Script to validate Yojson outputs w.r.t. ATD specifications.
# This works by running a given 'converter' to parse and pretty-print the outputs, then observing the difference with gunzip + pretty-print.

if [ "$2" == "--std" ]; then
    CONVERTER=("$1" --std --pretty)
    YDUMP=(ydump -std)
    shift 2
else
    CONVERTER=("$1" --pretty)
    YDUMP=(ydump)
    shift
fi

while [ -n "$1" ]
do
    if [[ "$1" == *.gz ]]; then
        DUMP_CMD=(gunzip -c "$1")
    else
        DUMP_CMD=(cat "$1")
    fi

    if ! diff -q <("${DUMP_CMD[@]}" | "${YDUMP[@]}") <("${CONVERTER[@]}" "$1" /dev/stdout) >/dev/null 2>&1; then
        echo "The file '$1' does not respect the ATD format implemented by $CONVERTER."
        echo "Here is the command that shows the problem:"
        echo -n "  diff <("
        printf '%s ' "${DUMP_CMD[@]}"
        echo -n "| "
        printf '"%s" ' "${YDUMP[@]}"
        echo -n ") <("
        printf '"%s" ' "${CONVERTER[@]}" "$1"
        echo "/dev/stdout)"
        exit 2
    fi

    shift;
done
