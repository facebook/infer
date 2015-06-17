#!/bin/bash

# Copyright 2013 - present Facebook.
# All Rights Reserved.

set -x
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INFER_DIR=$SCRIPT_DIR/../infer
CLANG_PLUGIN_DIR=$SCRIPT_DIR/../../facebook-clang-plugin
platform=`uname`

# Build Infer
cd $INFER_DIR && make

VERSION=$($INFER_DIR/bin/infer --version 2>&1 | head -1 | awk '{print $3}')
if [ $platform == 'Darwin' ]; then
    BINARY_DIR=infer-osx-$VERSION
else
    BINARY_DIR=infer-linux64-$VERSION
fi
BINARY_TARBALL=$BINARY_DIR.tar.xz

# Build package.
PKG_DIR=$SCRIPT_DIR/../$BINARY_DIR

# Start with infer
mkdir -p $PKG_DIR/infer/infer/{annotations,bin,lib}
mkdir -p $PKG_DIR/infer/examples
mkdir -p $PKG_DIR/infer/scripts
cp -r $INFER_DIR/annotations/* $PKG_DIR/infer/infer/annotations
cp -r $INFER_DIR/bin/* $PKG_DIR/infer/infer/bin
cp -r $INFER_DIR/lib/* $PKG_DIR/infer/infer/lib
cp -r $INFER_DIR/../examples/* $PKG_DIR/infer/examples
cp $INFER_DIR/../{CONTRIBUTING.md,LICENSE,PATENTS,README.md,INSTALL.md} $PKG_DIR/infer/
cp -r $INFER_DIR/../scripts/* $PKG_DIR/infer/scripts
# don't include pyc files into the release
find $PKG_DIR -name "*.pyc" | xargs rm

# Add facebook-clang-plugin
PKG_PLUGIN_DIR=$PKG_DIR/facebook-clang-plugin
mkdir -p $PKG_PLUGIN_DIR/clang/{bin,lib,include}
mkdir -p $PKG_PLUGIN_DIR/libtooling/build
cp $CLANG_PLUGIN_DIR/{CONTRIBUTING.md,LICENSE,LLVM-LICENSE,PATENTS,README.md} $PKG_PLUGIN_DIR
cp -r $CLANG_PLUGIN_DIR/clang/bin/clang* $PKG_PLUGIN_DIR/clang/bin
cp -r $CLANG_PLUGIN_DIR/clang/lib/* $PKG_PLUGIN_DIR/clang/lib
cp -r $CLANG_PLUGIN_DIR/clang/include/* $PKG_PLUGIN_DIR/clang/include
rm $PKG_PLUGIN_DIR/clang/lib/*.a

cp -r $CLANG_PLUGIN_DIR/libtooling/build/* $PKG_PLUGIN_DIR/libtooling/build

cd $PKG_DIR/.. && tar cJf $BINARY_TARBALL $BINARY_DIR

# Cleanup.
rm -rf $PKG_DIR

# vim: sw=2 ts=2 et:
