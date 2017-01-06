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
NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"

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
BUILD_CLANG=no
BUILD_JAVA=no
INTERACTIVE=yes
ORIG_ARGS="$*"

while [[ $# > 0 ]]; do
  opt_key="$1"
  case $opt_key in
    all)
      BUILD_CLANG=yes
      BUILD_JAVA=yes
      shift
      continue
      ;;
    clang)
      BUILD_CLANG=yes
      shift
      continue
      ;;
    java)
      BUILD_JAVA=yes
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
if [ "$BUILD_CLANG" = "no" ] && [ "$BUILD_JAVA" = "no" ]; then
  BUILD_CLANG=yes
  BUILD_JAVA=yes
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

setup_opam () {
    OCAML_VERSION="4.02.3"

    opam init --compiler=$OCAML_VERSION -j $NCPU --no-setup --yes

    OPAMSWITCH=infer-$OCAML_VERSION
    opam switch set -j $NCPU $OPAMSWITCH --alias-of $OCAML_VERSION
}

add_opam_git_pin () {
    PACKAGE_NAME=$1
    REPO_URL=$2
    PIN_HASH=$3

    if [ "$(opam show -f pinned "$PACKAGE_NAME")" != "git ($PIN_HASH)" ]; then
        opam pin add --yes --no-action "$PACKAGE_NAME" "$REPO_URL"
    fi
}

install_opam_deps () {
    # trick to avoid rsync'inc the whole directory to opam since we are only interested in
    # installing the dependencies
    INFER_DEPS_DIR=$(mktemp -d infer-deps-XXXX)
    cp opam "$INFER_DEPS_DIR"
    # give unique name to the package to force opam to recheck the dependencies are all installed
    opam pin add --yes --no-action "$INFER_DEPS_DIR" "$INFER_DEPS_DIR"
    opam install -j $NCPU --yes --deps-only "$INFER_DEPS_DIR"
    opam pin remove "$INFER_DEPS_DIR"
    rm -fr "$INFER_DEPS_DIR"
}

echo "initializing opam... "
check_installed opam
setup_opam
eval $(SHELL=bash opam config env --switch=$OPAMSWITCH)
echo "installing infer dependencies... "
install_opam_deps

echo "preparing build... "
if [ ! -f .release ]; then
  if [ "$BUILD_CLANG" = "no" ]; then
    SKIP_SUBMODULES=true ./autogen.sh > /dev/null
  else
    ./autogen.sh > /dev/null
  fi
fi

if [ "$BUILD_CLANG" = "no" ]; then
  INFER_CONFIGURE_OPTS+=" --disable-c-analyzers"
fi
if [ "$BUILD_JAVA" = "no" ]; then
  INFER_CONFIGURE_OPTS+=" --disable-java-analyzers"
fi

./configure $INFER_CONFIGURE_OPTS

if [ "$BUILD_CLANG" = "yes" ] && ! facebook-clang-plugins/clang/setup.sh --only-check-install; then
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
fi

make -j $NCPU all || (
  echo
  echo '  compilation failure; you can try running'
  echo
  echo '    make clean'
  echo "    $0 $ORIG_ARGS"
  echo
  exit 1)
