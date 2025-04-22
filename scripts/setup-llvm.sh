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
CLANG_INSTALL=$(realpath "$INFER_ROOT"/facebook-clang-plugins/clang/install)
llvm_opam_version="20-static-infer"

clean_slate () {
    opam remove llvm --yes
    opam remove conf-llvm --yes
    opam pin remove llvm --no-action
    opam pin remove conf-llvm --no-action
    opam repo remove local-llvm --all
}

install_opam_llvm () {
    opam repo add local-llvm "$INFER_ROOT"/dependencies/llvm/opam-repository
    # cd to the root of the infer repo so that opam can find the infer-specific patches listed in
    # the opam file as paths relative to the infer repo root
    pushd "$INFER_ROOT"
    PATH="$CLANG_INSTALL/bin:$PATH" OPAM_USER_PATH_RO="$CLANG_INSTALL" opam install llvm."$llvm_opam_version" --yes --assume-depext
    popd
}

clean_slate
install_opam_llvm
