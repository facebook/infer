#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# wrap execution of sledge with time and memory limits
# and add status entries to report in case of an unexpected exit

# usage: wrap.sh <timeout(sec)> <memout(GB)> <command> <testdir/testname>

set -u

timeout=$1
memout=$(( $2*1024*1024 ))
command=${@: 3: $#-3}
test=${@: -1}

unknown_error () {
  echo "((name $test)(entry(Status(UnknownError \"$1\"))))" >> $test.sexp
}

timeout () {
  echo "((name $test)(entry(Status Timeout)))" >> $test.sexp
}

memout () {
  echo "((name $test)(entry(Status Memout)))" >> $test.sexp
}

(
  ulimit -t $timeout -v $memout
  $command -report $test.sexp $test 1> $test.out 2> $test.err
) &> /dev/null
status=$?

case $status in
  ( 0 | 1 ) ;;
  ( 132 ) unknown_error "illegal instruction" ;;
  ( 136 ) unknown_error "floating-point exception" ;;
  ( 139 ) unknown_error "segmentation violation" ;;
  ( 127 | 134 ) memout ;;
  ( 137 | 152 ) timeout ;;
  ( * ) unknown_error "exit $status" ;;
esac

(test -f $test.sexp && grep -q "Status" $test.sexp) || unknown_error "exit $status"

exit $status
