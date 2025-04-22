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
llvm_opam_version="18-shared-2"

clean_slate () {
    opam remove llvm --yes
    opam pin remove llvm --no-action
    opam repo remove local-llvm --all
}

install_opam_llvm () {
    opam repo add local-llvm "$INFER_ROOT"/dependencies/llvm/opam-repository
    # cd to the root of the infer repo so that opam can find the infer-specific patches listed in
    # the opam file as paths relative to the infer repo root
    pushd "$INFER_ROOT"
    opam install llvm."$llvm_opam_version" --yes
    popd
}

clean_slate
install_opam_llvm
