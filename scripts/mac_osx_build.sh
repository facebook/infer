#!/usr/bin/env bash

#  Copyright (c) 2015, Facebook, Inc.
#  All rights reserved.
#
#  This source code is licensed under the BSD-style license found in the
#  LICENSE file in the root directory of this source tree. An additional grant
#  of patent rights can be found in the PATENTS file in the same directory.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_ROOT="$SCRIPT_DIR/../"

function usage() {
  echo "Usage: $0 [options]"
  echo
  echo " options:"
  echo "    --java-only   only build Java support (default: false)"
  echo "    --no-init     do not initialize OPAM"
  echo
  echo " examples:"
  echo "    $0                # build Java and C/Objective-C support"
  echo "    $0  --java-only   # build Java support"
  exit 1
}

JAVA_ONLY=
NO_INIT=
while [[ $# > 0 ]]; do
  opt_key="$1"
  case $opt_key in
    -j|--java-only)
      JAVA_ONLY=1
      shift
      continue
      ;;
    -n|--no-init)
      NO_INIT=1
      shift
      continue
      ;;
    -h|--help)
      usage
  esac
  shift
done

function brew_install() {
  if [[ -n "$(brew list | grep $1)" ]]; then
    echo "$1 is already installed. skipping."
  else
    echo "installing $1"
    brew install --build-bottle --build-from-source $1 || brew upgrade --build-from-source $@
  fi
}

function opam_install() {
  if [[ -n "$(opam list | grep -e "$1\s*$2")" ]]; then
    echo "$1 is already installed. skipping."
  else
    echo "installing $1"
    opam install $1.$2 -y
  fi
}

function main() {
  platform=`uname`
  if [ $platform != "Darwin" ]; then
    echo "This setup script works only on MacOS"
    exit 1
  fi

  type brew >/dev/null 2>&1 || {
    echo "could not find homebrew. please install it from http://brew.sh/";
    exit 1
  }

  brew_install opam
  opam init --comp=4.01.0 -y
  opam_install sawja 1.5
  opam_install atdgen 1.5.0
  opam_install javalib 2.3
  opam_install extlib 1.5.4
  if [ ! $NO_INIT ]; then
    opam config env
  fi

  if [ $JAVA_ONLY ]; then
    cd $INFER_ROOT
    make -C infer java
  else
    $INFER_ROOT/scripts/check_clang_plugin_version.sh $INFER_ROOT/../facebook-clang-plugin
    if [ $? -ne 0 ]; then
      echo "facebook-clang-plugin is not up-to-date. installing..."
      $INFER_ROOT/update-fcp.sh
      $INFER_ROOT/../facebook-clang-plugin/clang/setup.sh
      $INFER_ROOT/compile-fcp.sh
    fi

    cd $INFER_ROOT
    make -C infer
  fi
}

main
