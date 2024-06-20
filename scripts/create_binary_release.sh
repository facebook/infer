#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -o pipefail
set -u
set -x

if [ "$#" != "1" ]; then
  set +x
  echo "Usage: $0 version_tag" 1>&2
  echo 1>&2
  echo "Example: $0 v0.11.0" 1>&2
  exit 1
fi

VERSION=$1

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$SCRIPT_DIR"/..
NCPUS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)"
PLATFORM=$(uname)
if [ "$PLATFORM" == 'Darwin' ]; then
    RELEASE_NAME=infer-osx-$(uname -m)-"$VERSION"
else
    RELEASE_NAME=infer-linux-$(uname -m)-"$VERSION"
fi
RELEASE_TARBALL="$RELEASE_NAME".tar.xz

# configurable stuff
DRYRUN=${DRYRUN:-no}
JOBS=${JOBS:-$NCPUS}

pushd "$ROOT_DIR"
rm -fr "$RELEASE_NAME"

./build-infer.sh --only-setup-opam
eval $(opam env)
touch .release
./autogen.sh
./configure \
    --prefix="/$RELEASE_NAME"

make -j "$JOBS" \
    install-with-libs \
    BUILD_MODE=opt \
    DESTDIR="$ROOT_DIR" \
    libdir_relative_to_bindir=../lib
popd

if [ "$DRYRUN" = "no" ]; then
    installed_version="$(./"$RELEASE_NAME"/bin/infer --version | head -1 | cut -d ' ' -f 3)"
    if [ "$installed_version" != "$VERSION" ]; then
        set +x
        printf "Infer reports the wrong version number: got '%s' but expected '%s'\n" \
               "$installed_version" "$VERSION" 1>&2
        exit 1
    fi

    # trick so that the String-Who-Must-Not-Be-Named doesn't appear verbatim in the script
    FBDASHONLY=$(printf "%s%s" 'FB-O' 'NLY')
    if grep -Ir "$FBDASHONLY" "$RELEASE_NAME"; then
        set +x
        echo "Found files marked $FBDASHONLY" 1>&2
        exit 1
    fi
fi

tar cJf "$RELEASE_TARBALL" "$RELEASE_NAME"
rm -fr "$RELEASE_NAME"

# special GitHub sauce for later steps to find the tarball
echo
echo "::set-output name=tarball-path::$RELEASE_TARBALL"
