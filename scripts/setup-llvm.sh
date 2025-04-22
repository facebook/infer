#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Set up LLVM dependencies for the LLVM frontend

set -e
set -o pipefail
set -u
set -x

PLATFORM=$(uname)
INFER_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"
llvm_opam_version="18-shared"
srcdir="$INFER_ROOT"/dependencies/llvm/src
patches=(
    add-debug-info.patch
    add-LLVMGetAtomicRMWBinOp.patch
    fix-null-global_initializer.patch
)


clean_slate () {
    rm -fr "$srcdir"
    opam remove llvm --yes
    opam pin remove llvm --no-action
}

download_and_patch_opam_llvm_src () {
    opam source --dir="$srcdir" llvm."$llvm_opam_version"
    pushd "$srcdir"
    patch -p1 < "$INFER_ROOT"/dependencies/llvm/add-sledge-patches-opam.patch
    for patch in "${patches[@]}"; do
        ln -s "$INFER_ROOT"/dependencies/llvm/patches/"$patch" .
    done
    popd
}

install_opam_llvm () {
    opam pin add llvm."$llvm_opam_version" "$srcdir" --no-action
    opam install llvm."$llvm_opam_version" --yes
}

clean_slate
download_and_patch_opam_llvm_src
install_opam_llvm
