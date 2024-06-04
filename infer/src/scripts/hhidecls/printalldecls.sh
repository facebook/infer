#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Run hhidecls on all .hhi files and output the resulting Textual
# to stdout. Exclude the /facebook directory.


find -path ./facebook -prune -o -name '*.hhi' -print | while read -r file; do
  printf "// decls from %s\n" "$file" ;
  hackc facts "$file" | hhidecls ;
  done
