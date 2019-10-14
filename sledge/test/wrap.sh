#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# wrap execution of sledge with time and memory limits
# and add RESULT status to output in case of an unexpected exit

# usage: wrap.sh <timeout(sec)> <memout(MB)> <command> <testdir/testname>

set -u

timeout=$1
memout=$(( $2*1024 ))
command=${@: 3: $#-3}
test=${@: -1}

testdir=$(dirname $test)
testname=$(basename $test)

cd $testdir
(
  ulimit -t $timeout -v $memout
  $command $testname 1> $testname.out 2> $testname.err
)
status=$?
case $status in
  ( 132 ) echo -e "RESULT: illegal instruction" >> $testname.out ;;
  ( 136 ) echo -e "RESULT: floating-point exception" >> $testname.out ;;
  ( 139 ) echo -e "RESULT: segmentation violation" >> $testname.out ;;
  ( 127 ) echo -e "RESULT: MEMOUT" >> $testname.out ;;
  ( 137 | 152 ) echo -e "RESULT: TIMEOUT" >> $testname.out ;;
  ( * ) ;;
esac
if ! grep -q 'RESULT:' $testname.out; then
  echo -e "RESULT: Internal error: "$status >> $testname.out
fi
exit $status
