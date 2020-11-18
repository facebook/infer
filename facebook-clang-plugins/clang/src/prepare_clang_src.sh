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

LLVM_VER="10.0.1"

GITHUB_URL="https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_VER}"

LLVM_FILE="llvm-${LLVM_VER}.src.tar.xz"
CLANG_FILE="clang-${LLVM_VER}.src.tar.xz"
COMPILER_RT_FILE="compiler-rt-${LLVM_VER}.src.tar.xz"
LIBCXX_FILE="libcxx-${LLVM_VER}.src.tar.xz"
LIBCXXABI_FILE="libcxxabi-${LLVM_VER}.src.tar.xz"
OPENMP_FILE="openmp-${LLVM_VER}.src.tar.xz"

LLVM_URL="${GITHUB_URL}/${LLVM_FILE}"
CLANG_URL="${GITHUB_URL}/${CLANG_FILE}"
COMPILER_RT_URL="${GITHUB_URL}/${COMPILER_RT_FILE}"
LIBCXX_URL="${GITHUB_URL}/${LIBCXX_FILE}"
LIBCXXABI_URL="${GITHUB_URL}/${LIBCXXABI_FILE}"
OPENMP_URL="${GITHUB_URL}/${OPENMP_FILE}"

LLVM_SHA="c5d8e30b57cbded7128d78e5e8dad811bff97a8d471896812f57fa99ee82cdf3"
CLANG_SHA="f99afc382b88e622c689b6d96cadfa6241ef55dca90e87fc170352e12ddb2b24"
COMPILER_RT_SHA="d90dc8e121ca0271f0fd3d639d135bfaa4b6ed41e67bd6eb77808f72629658fa"
LIBCXX_SHA="def674535f22f83131353b3c382ccebfef4ba6a35c488bdb76f10b68b25be86c"
LIBCXXABI_SHA="a97ef810b2e9fb70e8f7e317b74e646ed4944f488b02ac5ddd9c99e385381a7b"
OPENMP_SHA="d19f728c8e04fb1e94566c8d76aef50ec926cd2f95ef3bf1e0a5de4909b28b44"

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
        curl -L "${URLS[$i]}" --output "${FILES[$i]}"
    fi
    echo "${SHAS[$i]}  ${FILES[$i]}" | $SHASUM -c
done

rm -rf "llvm-${LLVM_VER}.src" "llvm"

tar xf "${LLVM_FILE}"
cd "llvm-${LLVM_VER}.src/tools"
tar xf "../../${CLANG_FILE}"
mv "clang-${LLVM_VER}.src" clang
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
