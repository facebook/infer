#!/usr/bin/env bash

# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_ROOT="$SCRIPT_DIR/../"
PLATFORM="$(uname)"

function usage() {
  echo "Usage: $0 [targets]"
  echo
  echo " targets:"
  echo "    all      build everything"
  echo "    clang    build clang support (C/Objective-C)"
  echo "    java     build Java support"
  echo
  echo " examples:"
  echo "    $0 all           # build Java and C/Objective-C support"
  echo "    $0 java clang    # equivalent way of doing the above"
  echo "    $0 java          # build Java support only"
}

if [[ $# == 0 ]]; then
  usage
  exit 1
fi

BUILD_JAVA=0
BUILD_CLANG=0
while [[ $# > 0 ]]; do
  opt_key="$1"
  case $opt_key in
    all)
      BUILD_CLANG=1
      BUILD_JAVA=1
      shift
      continue
      ;;
    clang)
      BUILD_CLANG=1
      shift
      continue
      ;;
    java)
      BUILD_JAVA=1
      shift
      continue
      ;;
    -h|--help)
      usage
      exit 0
     ;;
     *)
      usage
      exit 1
  esac
  shift
done


check_installed () {
  local CMD=$1
  if ! which $1 >/dev/null 2>&1; then
    echo "dependency not found: $CMD"
    exit 1
  fi
}

check_version () {
  local CMD=$1
  local VERSION=$2
  if ! $1 2>&1 | grep -e "$VERSION" >/dev/null 2>&1; then
    echo "version mismatch: the output of \"$CMD\" does not match \"$VERSION\""
    exit 1
  fi
}

set -x

check_installed opam

opam switch 4.01.0 -y
opam install -y \
     atdgen.1.6.0 \
     extlib.1.5.4 \
     javalib.2.3.1 \
     sawja.1.5.1

# Java-specific dependencies
if [ "$BUILD_JAVA" == "1" ]; then
  check_installed javac
  check_version "javac -version" "\b1\.[78]"
fi

# OSX-specific dependencies
if [ "$PLATFORM" == "Darwin" ]; then
  check_installed xcodebuild
  check_version "xcodebuild -version" "\(\b6\.[1-9]\+\|\b7\.[0-9]\+\)"
fi

# prepare build targets
TARGETS=""
if [ "$BUILD_JAVA" = "1" ]; then
  TARGETS+=" java"
fi
if [ "$BUILD_CLANG" = "1" ]; then
  TARGETS+=" clang"
  if [ ! -f ".release" ]; then
    ./facebook-clang-plugins/clang/setup.sh
    ./compile-fcp.sh
  fi
fi

make -C infer clean $TARGETS
