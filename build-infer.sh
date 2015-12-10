#!/usr/bin/env bash

# Convenience script to build Infer when using opam

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
  echo "Usage: $0 [-y] [targets]"
  echo
  echo " targets:"
  echo "   all      build everything (default)"
  echo "   clang    build C and Objective-C analyzer"
  echo "   java     build Java analyzer"
  echo
  echo " options:"
  echo "   -h,--help   show this message"
  echo "   -y,--yes    automatically agree to everything"
  echo
  echo " examples:"
  echo "    $0               # build Java and C/Objective-C analyzers"
  echo "    $0 java clang    # equivalent way of doing the above"
  echo "    $0 java          # build only the Java analyzer"
}

# arguments
BUILD_CLANG=0
BUILD_JAVA=0
INTERACTIVE=yes

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
    -y|--yes)
      INTERACTIVE=no
      shift
      continue
     ;;
     *)
      usage
      exit 1
  esac
  shift
done

# if no arguments then build both clang and Java
if [ "$BUILD_CLANG" = "0" ] && [ "$BUILD_JAVA" = "0" ]; then
  BUILD_CLANG=1
  BUILD_JAVA=1
fi

# enable --yes option for some commands in non-interactive mode
YES=
if [ "$INTERACTIVE" = "no" ]; then
  YES=--yes
fi

check_installed () {
  local cmd=$1
  if ! which $cmd >/dev/null 2>&1; then
    echo "dependency not found: $cmd"
    exit 1
  fi
}

echo "initializing opam... "
check_installed opam

# opam is noisy
opam init --no-setup --yes > /dev/null
eval $(opam config env)

echo "preparing build... "
if [ ! -f .release ]; then
  ./autogen.sh > /dev/null
elif [ ! -d .git ] && [ ! -f .release ]; then
  git submodule update --init --recursive
fi

TARGETS=""
if [ "$BUILD_JAVA" = "1" ]; then
  TARGETS+=" java"
fi
if [ "$BUILD_CLANG" = "1" ]; then
  TARGETS+=" clang"
  if [ ! -f .release ]; then
    if ! facebook-clang-plugins/clang/setup.sh --only-check-install; then
      echo ""
      echo "  Warning: you are not using a release of Infer. The C and"
      echo "  Objective-C analyses require a custom clang to be compiled"
      echo "  now. This step takes ~30-60 minutes, possibly more."
      echo ""
      echo "  To speed this along, you are encouraged to use a release of"
      echo "  Infer instead:"
      echo ""
      echo "  http://fbinfer.com/docs/getting-started.html"
      echo ""
      echo "  If you are only interested in analyzing Java programs, simply"
      echo "  run this script with only the \"java\" argument:"
      echo ""
      echo "  $0 java"
      echo ""

      confirm="n"
      printf "Are you sure you want to compile clang? (y/N) "
      if [ "$INTERACTIVE" = "no" ]; then
          confirm="y"
          echo "$confirm"
      else
          read confirm
      fi

      if [ "x$confirm" != "xy" ]; then
          exit 0
      fi

      ./facebook-clang-plugins/clang/setup.sh
    fi
    ./compile-fcp.sh
  fi
fi

echo
echo "  *************************"
echo "  **                     **"
echo "  **   Building Infer    **"
echo "  **                     **"
echo "  *************************"
echo

./configure
make $TARGETS || (echo "compilation failure; try running `make clean`"; exit 1)
