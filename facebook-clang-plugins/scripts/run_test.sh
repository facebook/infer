# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#!/bin/bash

TESTFILE="$1"
shift
TESTNAME=`basename "$TESTFILE"`

OUTFILE="$TESTFILE.out"
EXPFILE="$TESTFILE.exp"
DIFFFILE="$TESTFILE.diff"

if [[ $VERBOSE > 0 ]]
then
    DEST=/dev/stdout
else
    DEST=/dev/null
fi

if $* 2>&1 | tee "$OUTFILE" > $DEST
then
    if diff <(cat "$EXPFILE" 2> /dev/null || true) "$OUTFILE" > "$DIFFFILE"
    then
        echo "[+] $TESTNAME succeeded"
        rm "$DIFFFILE"
        exit 0
    else
        echo "[-] $TESTNAME failed (unexpected output)"
        printf '\033[1;31m\n'
        if [[ $LIMIT > 0 ]]
        then
            tail -n $LIMIT "$DIFFFILE"
        else
            cat "$DIFFFILE"
        fi
        printf '\033[0m\n'
        rm "$DIFFFILE"
        exit 1
    fi
else
    echo "[-] $TESTNAME failed (error $?)"
    exit 2
fi
