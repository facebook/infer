#!/usr/bin/env bash

# Convenience script to build Infer when using opam

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -o pipefail
set -u

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_ROOT="$SCRIPT_DIR"
DEPENDENCIES_DIR="$INFER_ROOT/facebook/dependencies"
PLATFORM="$(uname)"
SANDCASTLE=${SANDCASTLE:-}
NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
INFER_OPAM_DEFAULT_SWITCH="ocaml-variants.4.11.1+flambda"
INFER_OPAM_DEFAULT_COMPILER="$INFER_OPAM_DEFAULT_SWITCH"
INFER_OPAM_SWITCH=${INFER_OPAM_SWITCH:-$INFER_OPAM_DEFAULT_SWITCH}
INFER_OPAM_COMPILER=${INFER_OPAM_COMPILER:-$INFER_OPAM_DEFAULT_COMPILER}

function usage() {
  echo "Usage: $0 [-y] [targets]"
  echo
  echo " targets:"
  echo "   all      build everything (default)"
  echo "   clang    build C and Objective-C analyzer"
  echo "   java     build Java analyzer"
  echo
  echo " options:"
  echo "   -h,--help             show this message"
  echo "   --no-opam-lock        do not use the opam.locked file and let opam resolve dependencies"
  echo "   --only-setup-opam     initialize opam, install the opam dependencies of infer, and exit"
  echo "   --user-opam-switch    use the current opam switch to install infer (default: $INFER_OPAM_DEFAULT_SWITCH)"
  echo "   -y,--yes              automatically agree to everything"
  echo
  echo " examples:"
  echo "    $0               # build Java and C/Objective-C analyzers"
  echo "    $0 java clang    # equivalent way of doing the above"
  echo "    $0 java          # build only the Java analyzer"
}

# arguments
BUILD_CLANG=${BUILD_CLANG:-no}
BUILD_JAVA=${BUILD_JAVA:-no}
INFER_CONFIGURE_OPTS=${INFER_CONFIGURE_OPTS:-""}
INTERACTIVE=${INTERACTIVE:-yes}
JOBS=${JOBS:-$NCPU}
ONLY_SETUP_OPAM=${ONLY_SETUP_OPAM:-no}
USE_OPAM_LOCK=${USE_OPAM_LOCK:-yes}
USER_OPAM_SWITCH=no

ORIG_ARGS="$*"

while [[ $# -gt 0 ]]; do
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
    --no-opam-lock)
      USE_OPAM_LOCK=no
      shift
      continue
     ;;
    --user-opam-switch)
      USER_OPAM_SWITCH=yes
      shift
      continue
     ;;
    --only-setup-opam)
      ONLY_SETUP_OPAM=yes
      shift
      continue
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
if [ "$BUILD_CLANG" == "no" ] && [ "$BUILD_JAVA" == "no" ]; then
  BUILD_CLANG=yes
  BUILD_JAVA=yes
fi

# enable --yes option for some commands in non-interactive mode
YES=
if [ "$INTERACTIVE" == "no" ]; then
  YES=--yes
fi
# --yes by default for opam commands except if we are using the user's opam switch
if [ "$INTERACTIVE" == "no" ] || [ "$USER_OPAM_SWITCH" == "no" ]; then
    export OPAMYES=true
fi

setup_opam () {
    opam var root 1>/dev/null 2>/dev/null || opam init --reinit --bare --no-setup &&
    opam_retry opam_switch_create_if_needed "$INFER_OPAM_SWITCH" "$INFER_OPAM_COMPILER" &&
    opam switch set "$INFER_OPAM_SWITCH"
}

install_opam_deps () {
    local locked=
    if [ "$USE_OPAM_LOCK" == yes ]; then
        locked=--locked
    fi
    opam install --deps-only infer "$INFER_ROOT" $locked &&
    if [ -n "$SANDCASTLE" ]; then
        opam pin list | grep yojson || opam pin add yojson "${DEPENDENCIES_DIR}/yojson-1.7.0fix"
    fi
}

echo "initializing opam... " >&2
. "$INFER_ROOT"/scripts/opam_utils.sh
if [ "$USER_OPAM_SWITCH" == "no" ]; then
    setup_opam
fi
eval $(SHELL=bash opam env)
echo >&2
echo "installing infer dependencies; this can take up to 30 minutes... " >&2
opam_retry install_opam_deps

if [ "$ONLY_SETUP_OPAM" == "yes" ]; then
  exit 0
fi

echo "preparing build... " >&2
./autogen.sh > /dev/null

if [ "$BUILD_CLANG" == "no" ]; then
  INFER_CONFIGURE_OPTS+=" --disable-c-analyzers"
fi
if [ "$BUILD_JAVA" == "no" ]; then
  INFER_CONFIGURE_OPTS+=" --disable-java-analyzers"
fi

./configure $INFER_CONFIGURE_OPTS

if [ "$BUILD_CLANG" == "yes" ]; then
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
    if [ "$INTERACTIVE" == "no" ]; then
        confirm="y"
        echo "$confirm"
    else
        read confirm
    fi

    if [ "x$confirm" != "xy" ]; then
        exit 0
    fi

    # only run this script if we are definitely building clang
    facebook-clang-plugins/clang/src/prepare_clang_src.sh
  fi
fi

make -j "$JOBS" opt || (
  echo >&2
  echo '  compilation failure; you can try running' >&2
  echo >&2
  echo '    make clean' >&2
  echo "    '$0' $ORIG_ARGS" >&2
  echo >&2
  exit 1)

echo
echo "*** Success! Infer is now built in '$SCRIPT_DIR/infer/bin/'."
echo '*** Install infer on your system with `make install`.'
echo
echo '*** If you plan to hack on infer, check out CONTRIBUTING.md to setup your dev environment.'
