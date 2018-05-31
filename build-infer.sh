#!/usr/bin/env bash

# Convenience script to build Infer when using opam

# Copyright (c) 2015-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -o pipefail
set -u

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_ROOT="$SCRIPT_DIR"
INFER_DEPS_DIR="$INFER_ROOT/dependencies/infer-deps"
PLATFORM="$(uname)"
NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION_DEFAULT="4.06.1+flambda"
INFER_OPAM_SWITCH_DEFAULT=infer-"$OCAML_VERSION_DEFAULT"


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
  echo "   --no-opam-lock        do not use the opam.lock file and let opam resolve dependencies"
  echo "   --only-setup-opam     initialize opam, install the opam dependencies of infer, and exit"
  echo "   --opam-switch         specify the opam switch where to install infer (default: $INFER_OPAM_SWITCH_DEFAULT)"
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
INFER_OPAM_SWITCH=${INFER_OPAM_SWITCH:-$INFER_OPAM_SWITCH_DEFAULT}
INTERACTIVE=${INTERACTIVE:-yes}
JOBS=${JOBS:-$NCPU}
OCAML_VERSION=${OCAML_VERSION:-$OCAML_VERSION_DEFAULT}
ONLY_SETUP_OPAM=${ONLY_SETUP_OPAM:-no}
OPAM_LOCK_URL=${OPAM_LOCK_URL:-https://github.com/rgrinberg/opam-lock}
USE_OPAM_LOCK=${USE_OPAM_LOCK:-yes}

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
    --no-opam-lock)
      USE_OPAM_LOCK=no
      shift
      continue
     ;;
    --opam-switch)
      shift
      [[ $# > 0 ]] || (usage; exit 1)
      INFER_OPAM_SWITCH="$1"
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
if [ "$BUILD_CLANG" = "no" ] && [ "$BUILD_JAVA" = "no" ]; then
  BUILD_CLANG=yes
  BUILD_JAVA=yes
fi

# enable --yes option for some commands in non-interactive mode
YES=
if [ "$INTERACTIVE" = "no" ]; then
  YES=--yes
fi
# --yes by default for opam commands
export OPAMYES=1

check_installed () {
  local cmd="$1"
  if ! which "$cmd" >/dev/null 2>&1; then
    echo "dependency not found: '$cmd'" >&2
    exit 1
  fi
}

opam_retry () {
  "$@" || ( \
    echo >&2; \
    printf '*** `%s` failed\n' "$*" >&2; \
    echo '*** Updating opam then retrying' >&2; \
    opam update && \
    "$@" || ( \
      echo >&2; \
      printf '*** ERROR: `%s` failed\n' "$*" >&2; \
      exit 1 \
    ) \
  ) \
}

setup_opam () {
    opam_retry opam init --compiler="$OCAML_VERSION" -j "$JOBS" --no-setup
    if [ "$INFER_OPAM_SWITCH" = "$INFER_OPAM_SWITCH_DEFAULT" ]; then
        opam_retry opam switch set -j "$JOBS" "$INFER_OPAM_SWITCH" --alias-of "$OCAML_VERSION"
    else
        opam_retry opam switch set -j "$JOBS" "$INFER_OPAM_SWITCH"
    fi
}

# Install and record the infer dependencies in opam. The main trick is to install the
# $INFER_DEPS_DIR directory instead of the much larger infer repository. That directory contains
# just enough to pretend it installs infer.
install_infer-deps () {
    # remove previous infer-deps pin, which might have conflicting dependencies
    opam pin remove infer-deps --no-action
    INFER_TMP_DEPS_DIR="$(mktemp -d "$INFER_ROOT"/dependencies/infer-deps-XXXX)"
    INFER_TMP_PACKAGE_NAME="$(basename "$INFER_TMP_DEPS_DIR")"
    cp -a "$INFER_DEPS_DIR"/* "$INFER_TMP_DEPS_DIR"
    # give unique name to the package to force opam to recheck the dependencies are all installed
    opam pin add --no-action "$INFER_TMP_PACKAGE_NAME" "$INFER_TMP_DEPS_DIR"
    opam install -j "$JOBS" --deps-only "$INFER_TMP_PACKAGE_NAME"
    opam pin remove "$INFER_TMP_PACKAGE_NAME"
    rm -fr "$INFER_TMP_DEPS_DIR"
    # pin infer so that opam doesn't violate its package constraints when the user does
    # "opam upgrade"
    opam pin add infer-deps "$INFER_DEPS_DIR"
}

install_locked_deps() {
    if ! opam lock 2> /dev/null; then
        echo "opam-lock not found in the current switch, installing from '$OPAM_LOCK_URL'..." >&2
        opam pin add -k git lock "$OPAM_LOCK_URL"
    fi
    opam lock --install < "$INFER_ROOT"/opam.lock
}

install_opam_deps() {
    if [ "$USE_OPAM_LOCK" = yes ]; then
        install_locked_deps
    else
        install_infer-deps
    fi
}


echo "initializing opam... " >&2
check_installed opam
setup_opam
eval $(SHELL=bash opam config env --switch="$INFER_OPAM_SWITCH")
echo >&2
echo "installing infer dependencies; this can take up to 30 minutes... " >&2
opam_retry install_opam_deps

if [ "$ONLY_SETUP_OPAM" = "yes" ]; then
  exit 0
fi

echo "preparing build... " >&2
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

make -j "$JOBS" || (
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
