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

LLVM_VER="9.0.0"

LLVM_FILE="llvm-${LLVM_VER}.src.tar.xz"
CLANG_FILE="cfe-${LLVM_VER}.src.tar.xz"
COMPILER_RT_FILE="compiler-rt-${LLVM_VER}.src.tar.xz"
LIBCXX_FILE="libcxx-${LLVM_VER}.src.tar.xz"
LIBCXXABI_FILE="libcxxabi-${LLVM_VER}.src.tar.xz"
OPENMP_FILE="openmp-${LLVM_VER}.src.tar.xz"

LLVM_URL="https://releases.llvm.org/${LLVM_VER}/${LLVM_FILE}"
CLANG_URL="https://releases.llvm.org/${LLVM_VER}/${CLANG_FILE}"
COMPILER_RT_URL="https://releases.llvm.org/${LLVM_VER}/${COMPILER_RT_FILE}"
LIBCXX_URL="https://releases.llvm.org/${LLVM_VER}/${LIBCXX_FILE}"
LIBCXXABI_URL="https://releases.llvm.org/${LLVM_VER}/${LIBCXXABI_FILE}"
OPENMP_URL="https://releases.llvm.org/${LLVM_VER}/${OPENMP_FILE}"

LLVM_SHA="d6a0565cf21f22e9b4353b2eb92622e8365000a9e90a16b09b56f8157eabfe84"
CLANG_SHA="7ba81eef7c22ca5da688fdf9d88c20934d2d6b40bfe150ffd338900890aa4610"
COMPILER_RT_SHA="56e4cd96dd1d8c346b07b4d6b255f976570c6f2389697347a6c3dcb9e820d10e"
LIBCXX_SHA="3c4162972b5d3204ba47ac384aa456855a17b5e97422723d4758251acf1ed28c"
LIBCXXABI_SHA="675041783565c906ac2f7f8b2bc5c40f14d871ecfa8ade34855aa18de95530e9"
OPENMP_SHA="9979eb1133066376cc0be29d1682bc0b0e7fb541075b391061679111ae4d3b5b"

FILES=(
    $LLVM_FILE
    $CLANG_FILE
    $COMPILER_RT_FILE
    $LIBCXX_FILE
    $LIBCXXABI_FILE
    $OPENMP_FILE
)

URLS=(
    $LLVM_URL
    $CLANG_URL
    $COMPILER_RT_URL
    $LIBCXX_URL
    $LIBCXXABI_URL
    $OPENMP_URL
)

SHAS=(
    $LLVM_SHA
    $CLANG_SHA
    $COMPILER_RT_SHA
    $LIBCXX_SHA
    $LIBCXXABI_SHA
    $OPENMP_SHA
)

mkdir -p "${SCRIPT_DIR}/download"
pushd "${SCRIPT_DIR}/download" >/dev/null

for i in ${!URLS[@]}; do
    if [ ! -f "${FILES[$i]}" ]; then
        curl "${URLS[$i]}" --output "${FILES[$i]}"
    fi
    echo "${SHAS[$i]}  ${FILES[$i]}" | $SHASUM -c
done

rm -rf "llvm-${LLVM_VER}.src" "llvm"

tar xf "${LLVM_FILE}"
cd "llvm-${LLVM_VER}.src/tools"
tar xf "../../${CLANG_FILE}"
mv "cfe-${LLVM_VER}.src" clang
cd ../projects
tar xf "../../${COMPILER_RT_FILE}"
mv "compiler-rt-${LLVM_VER}.src" compiler-rt
tar xf "../../${LIBCXX_FILE}"
mv "libcxx-${LLVM_VER}.src" libcxx
tar xf "../../${LIBCXXABI_FILE}"
mv "libcxxabi-${LLVM_VER}.src" libcxxabi
tar xf "../../${OPENMP_FILE}"
mv "openmp-${LLVM_VER}.src" openmp
cd ../..
mv "llvm-${LLVM_VER}.src" llvm

popd >/dev/null
