#!/bin/bash
set -e
set -x

# This script installs the facebook-clang-plugins
#
# TODO (t5939566): ADD INSTRUCTIONS ON HOW TO CUSTOMIZE THE ENVVARS FOR
# THE INSTALLATION OF THE PLUGINS.

INFER_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PLUGIN_DIR="$INFER_ROOT/../facebook-clang-plugin"
CLANG_EXEC="$PLUGIN_DIR/clang/bin/clang"

# check if clang is available
if ! $CLANG_EXEC  --version 2>&1 | grep -q '3\.6'; then
    echo "The required version of clang has not been found in $CLANG_EXEC" && exit 1
fi

# install facebook-clang-plugins
pushd "$PLUGIN_DIR"
# prepare flags for the compilation on the Linux platform
platform="$(uname)"
if [ "$platform" == 'Linux' ]; then
    export SDKPATH=""
    export PATH="$PLUGIN_DIR/clang/bin:$PATH"
    [ -z "$CC" ] && export CC="$PLUGIN_DIR/clang/bin/clang"
    [ -z "$CXX" ] && export CXX="$PLUGIN_DIR/clang/bin/clang++"
    [ -z "$CFLAGS" ] && export CFLAGS="-std=c++11 -fPIC"
    [ -z "$LDFLAGS" ] && export LDFLAGS="-shared"
    [ -z "$CLANG_PREFIX" ] && export CLANG_PREFIX="$PLUGIN_DIR/clang"
    [ -z "$LLVM_INCLUDES" ] && export LLVM_INCLUDES="$PLUGIN_DIR/clang/include"
    [ -z "$CLANG_INCLUDES" ] && export CLANG_INCLUDES="$LLVM_INCLUDES $CLANG_PREFIX/include"
fi

# compile
make clean
make -C clang-ocaml clean
make
make -C clang-ocaml all build/clang_ast_proj.ml build/clang_ast_proj.mli
popd

# check YojsonASTExporter works with clang
echo "int main() { return 0; }" | \
    $CLANG_EXEC -o /dev/null -x c \
        -Xclang -load -Xclang $PLUGIN_DIR/libtooling/build/FacebookClangPlugin.dylib \
        -Xclang -plugin -Xclang YojsonASTExporter -c - > /dev/null \
|| { echo "$CLANG_EXEC and the facebook-clang-plugins are not working.";
     echo "Check you're using the right revision of clang, then retry"; exit 1; }
