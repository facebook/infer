#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Usage: set_libso_path.sh [LIBSO_DIR] [TARGET]
#
# This changes MacOSX's executable [TARGET] to use shared libraries in
# [LIBSO_DIR] when rpath has been set to [LIBSO_DIR].

set -e
set -o pipefail
set -u

LIBSO_DIR=$1
TARGET=$2

TMP=$( mktemp )
trap "rm $TMP" EXIT

otool -L "$TARGET" | tail -n +2 > "$TMP"
while IFS='' read -r line || [[ -n "$line" ]]; do
  LIB_PATH=$( echo $line | awk '{print $1}' )
  LIB_FILE=$( basename "${LIB_PATH}" )
  if [ -f "${LIBSO_DIR}/${LIB_FILE}" ]; then
    install_name_tool -change "${LIB_PATH}" "@rpath/${LIB_FILE}" "$TARGET"
  fi
done < "$TMP"
