#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Download llvm, clang and needed libraries

set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SHASUM=${SHASUM:-shasum -a 256}
PATCH=${PATCH:-patch}

LLVM_VER="17.0.1"
LLVM_URL="https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VER}/llvm-project-${LLVM_VER}.src.tar.xz"
LLVM_SHA="b0e42aafc01ece2ca2b42e3526f54bebc4b1f1dc8de6e34f46a0446a13e882b9"
LLVM_FILE="llvm-project.src.tar.xz"
CLANG_PREBUILD_PATCHES=(
    "$SCRIPT_DIR/err_ret_local_block.patch"
    "$SCRIPT_DIR/mangle_suppress_errors.patch"
    "$SCRIPT_DIR/AArch64SVEACLETypes.patch"
)

mkdir -p "${SCRIPT_DIR}/download"
pushd "${SCRIPT_DIR}/download" >/dev/null

if ! echo "${LLVM_SHA}  ${LLVM_FILE}" | $SHASUM -c ; then
  echo "wrong checksum for $LLVM_FILE, downloading it again"
  rm -f "$LLVM_FILE"
fi

if [ ! -f "${LLVM_FILE}" ]; then
    curl -L "${LLVM_URL}" --output "${LLVM_FILE}"
fi
echo "${LLVM_SHA}  ${LLVM_FILE}" | $SHASUM -c

rm -rf "llvm-project-${LLVM_VER}.src" "llvm-project"

tar xf "${LLVM_FILE}"
mv "llvm-project-${LLVM_VER}.src" "llvm-project"

if [ "$SKIP_PATCH" != "yes" ]; then
    # apply prebuild patch
    for PATCH_FILE in "${CLANG_PREBUILD_PATCHES[@]}"; do
        "$PATCH" --force -p 1 < "$PATCH_FILE"
    done
fi

popd >/dev/null
