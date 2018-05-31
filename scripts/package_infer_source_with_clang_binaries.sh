#!/bin/bash

# Copyright (c) 2013-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -x
set -e

VERSION=$1

if [ "$#" != "1" ]; then
  echo "Usage: $0 version_tag"
  echo
  echo "Example: $0 v0.11.0"
  exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SCRIPT_NAME="$(basename "${BASH_SOURCE[0]}")"
ROOT_INFER_DIR="$SCRIPT_DIR"/..
CLANG_PLUGIN_DIR="$ROOT_INFER_DIR"/facebook-clang-plugins
CLANG_PREFIX="$CLANG_PLUGIN_DIR"/clang/install
PLATFORM=`uname`
INFER_SOURCE="$ROOT_INFER_DIR"/infer-source

if [ "$PLATFORM" == 'Darwin' ]; then
    RELEASE_NAME=infer-osx-"$VERSION"
else
    RELEASE_NAME=infer-linux64-"$VERSION"
fi
RELEASE_TARBALL="$RELEASE_NAME".tar.xz
PKG_DIR="$ROOT_INFER_DIR"/"$RELEASE_NAME"
PKG_PLUGIN_DIR="$PKG_DIR"/facebook-clang-plugins
PKG_CLANG_PREFIX="$PKG_PLUGIN_DIR"/clang/install


# Build infer and facebook-clang-plugins
cd "$ROOT_INFER_DIR"
# This assumes the current commit is the one with the release bump
./build-infer.sh --yes
find "$CLANG_PREFIX"/{bin,lib} -type f -print0 | xargs -0 strip -x || true

# Get a copy of the github repo
if ! git -C "$INFER_SOURCE" status > /dev/null; then
  git clone https://github.com/facebook/infer.git "$INFER_SOURCE"
fi
pushd "$INFER_SOURCE"
# fetch new tags
git tag -d "$VERSION" || true
git fetch origin
# update master
git checkout "$VERSION" || exit 1
git submodule update --init
git clean -xfd
popd

# Copy infer source
rm -fr "$PKG_DIR"
mkdir -p "$PKG_DIR"
rsync -a \
  --exclude="**/.git" \
  --exclude="**/.gitmodules" \
  --exclude="**/.gitignore" \
  --exclude="facebook-clang-plugins/clang/src/clang-*.tar.*" \
  "$INFER_SOURCE"/ "$PKG_DIR"/
touch "$PKG_DIR"/.release
rsync -a "$ROOT_INFER_DIR"/configure "$PKG_DIR"/configure

mkdir -pv "$PKG_CLANG_PREFIX"/{bin,lib,include}
mkdir -pv "$PKG_PLUGIN_DIR"/libtooling/build
mkdir -pv "$PKG_PLUGIN_DIR"/clang-ocaml/build
rsync -a "$CLANG_PLUGIN_DIR"/{CONTRIBUTING.md,LICENSE,LLVM-LICENSE,README.md} "$PKG_PLUGIN_DIR"
rsync -a "$CLANG_PREFIX"/bin/clang* "$PKG_CLANG_PREFIX"/bin/
rsync -a --exclude '*.a' "$CLANG_PREFIX"/lib/ "$PKG_CLANG_PREFIX"/lib/
rsync -a "$CLANG_PREFIX"/include/ "$PKG_CLANG_PREFIX"/include/
rsync -a "$CLANG_PLUGIN_DIR"/libtooling/build/ "$PKG_PLUGIN_DIR"/libtooling/build/
rsync -a "$CLANG_PLUGIN_DIR"/clang-ocaml/build/ "$PKG_PLUGIN_DIR"/clang-ocaml/build/

# no clang source in release versions, so remove the corresponding
# checksum check
grep -v -e '\bsrc/clang-.*\.tar\.*' \
  < "$CLANG_PLUGIN_DIR"/clang/installed.version \
  > "$PKG_PLUGIN_DIR"/clang/installed.version

# trick so that the String-Who-Must-Not-Be-Named doesn't appear verbatim in the script
FBDASHONLY=$(printf "%s%s" 'FB-O' 'NLY')
if grep -Ir "$FBDASHONLY" "$PKG_DIR"; then
    echo "Found files marked $FBONLY"
    exit 1
fi

cd "$ROOT_INFER_DIR" && tar cJf "$RELEASE_TARBALL" "$RELEASE_NAME"
