#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

sledge=$(dirname $PWD)/install/$(basename $PWD)/bin/sledge

line=$(\
  $sledge help -recursive -expand \
    | grep -n "== subcommands ===" \
    | cut -d : -f1,1)

line=$(($line+1))

$sledge help -recursive

$sledge h -r -e \
  | tail -n +$line \
  | sed -e "/^$/d;s/  \(.*\)  .*/\1/g" \
  | while read cmd; do \
      printf "\n====== sledge $cmd ======\n\n"; \
      $sledge $cmd -help; \
    done
