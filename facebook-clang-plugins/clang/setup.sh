#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Simple installation script for llvm/clang.

set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLANG_RELATIVE_SRC="src/download/llvm-project/llvm"
CLANG_SRC="${CLANG_SRC:-$SCRIPT_DIR/$CLANG_RELATIVE_SRC}"
CLANG_PREFIX="$SCRIPT_DIR/install"
CLANG_INSTALLED_VERSION_FILE="$SCRIPT_DIR/installed.version"
PATCHELF=${PATCHELF:-patchelf}
PLATFORM=$(uname)
PLATFORM_ENV=${PLATFORM_ENV:-}
STRIP=${STRIP:-strip}
CMAKE=${CMAKE:-cmake}
ZLIB=${ZLIB:-$CLANG_SRC}

NCPUS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)"
JOBS="${JOBS:-$(($NCPUS>=8?$NCPUS/4:2))}"

SHASUM=${SHASUM:-shasum -a 256}

usage () {
    echo "Usage: $0 [-chr]"
    echo
    echo " options:"
    echo "    -c,--only-check-install    check if recompiling clang is needed"
    echo "    -h,--help                  show this message"
    echo "    -n,--ninja                 use ninja for building"
    echo "    -p,--clang-hash            print the installed clang hash"
    echo "    -r,--only-record-install   do not install clang but pretend we did"
    echo "    -s,--sequential-link       only use one process for linking (ninja only)"
}

clang_hash () {
    if [ "$CLANG_HASH_USE_GIT" = "yes" ]; then
        HASH=$(git -C "$CLANG_SRC" rev-parse HEAD)
        echo "$HASH"
    else
        pushd "$SCRIPT_DIR" > /dev/null
        HASH=$($SHASUM setup.sh src/prepare_clang_src.sh | $SHASUM)
        printf "%s" "$HASH" | cut -d ' ' -f 1
        popd > /dev/null
    fi
}

check_installed () {
    pushd "$SCRIPT_DIR" > /dev/null
    HASH=$(clang_hash)
    RESULT=1
    if [ -f "$CLANG_INSTALLED_VERSION_FILE" ]; then
        FILE_HASH=$(cat "$CLANG_INSTALLED_VERSION_FILE")
        if [ "$HASH" == "$FILE_HASH" ]; then
            RESULT=0
        fi
    fi
    popd > /dev/null
    return $RESULT
}

record_installed () {
    pushd "$SCRIPT_DIR" > /dev/null
    HASH=$(clang_hash)
    echo $HASH > "$CLANG_INSTALLED_VERSION_FILE"
    popd > /dev/null
}

ONLY_CHECK=
ONLY_RECORD=
PRINT_CLANG_HASH=
USE_NINJA=
SEQUENTIAL_LINK=

while [[ $# -gt 0 ]]; do
    opt_key="$1"
    case $opt_key in
        -p|--clang-hash)
            PRINT_CLANG_HASH=yes
            shift
            continue
            ;;
        -c|--only-check-install)
            ONLY_CHECK=yes
            shift
            continue
            ;;
        -r|--only-record-install)
            ONLY_RECORD=yes
            shift
            continue
            ;;
        -n|--ninja)
            USE_NINJA=yes
            shift
            continue
            ;;
        -s|--sequential-link)
            SEQUENTIAL_LINK=yes
            shift
            continue
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            usage
            exit 2
    esac
    shift
done

if [ "$PRINT_CLANG_HASH" = "yes" ]; then
    clang_hash
    exit 0
fi

if [ "$ONLY_RECORD" = "yes" ]; then
    record_installed
    exit 0
fi

if check_installed; then
    # already installed
    if [ "$ONLY_CHECK" = "yes" ]; then
        exit 0
    fi
    echo "Clang is already installed according to $CLANG_INSTALLED_VERSION_FILE"
    echo "Nothing to do, exiting."
    exit 0
else
    if [ "$ONLY_CHECK" = "yes" ]; then
        exit 1
    fi
fi

set -x

if [[ x"$MAKEFLAGS" != x ]]; then
    echo "WARNING: MAKEFLAGS var was set to: $MAKEFLAGS"
    echo "         which may (and will!) interfere with the setup"
    echo "         ... ignoring MAKEFLAGS."
    unset MAKEFLAGS
fi

if [[ x"$DESTDIR" != x ]]; then
    echo "WARNING: DESTDIR var was set to: $DESTDIR"
    echo "         but the setup script is not designed to handle"
    echo "         relocation of the installation."
    echo "         ... ignoring DESTDIR."
    unset DESTDIR
fi

if [[ "$PLATFORM" = "Linux" ]] && [[ -n "${PLATFORM_ENV}" ]] ; then
    CXXFLAGS="$CXXFLAGS -DHAVE_RPC_XDR_H=0 -D_GLIBCXX_INCLUDE_NEXT_C_HEADERS -Wl,-rpath-link,${PLATFORM_ENV}/lib"
fi

CMAKE_ARGS=(
  -DCMAKE_BUILD_TYPE=Release
  -DCMAKE_C_FLAGS="$CFLAGS $CMAKE_C_FLAGS"
  -DCMAKE_CXX_FLAGS="$CXXFLAGS $CPPFLAGS $CMAKE_CXX_FLAGS"
  -DCMAKE_INSTALL_PREFIX="$CLANG_PREFIX"
  -DLLVM_BUILD_TOOLS=Off
  -DLLVM_ENABLE_ASSERTIONS=Off
  -DLLVM_ENABLE_EH=On
  -DLLVM_ENABLE_RTTI=On
  -DLLVM_BUILD_DOCS=Off
  -DLLVM_INCLUDE_BENCHMARKS=Off
  -DLLVM_INCLUDE_EXAMPLES=Off
  -DLLVM_INCLUDE_TESTS=Off
  -DLLVM_TARGETS_TO_BUILD="X86;AArch64;ARM;Mips"
)

if [ "$PLATFORM" = "Darwin" ]; then
    CMAKE_ARGS+=(
      -DCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS $CMAKE_SHARED_LINKER_FLAGS"
      -DLLVM_BUILD_LLVM_DYLIB=ON
    )
else
    CMAKE_ARGS+=(
      -DCMAKE_SHARED_LINKER_FLAGS="$LDFLAGS $CMAKE_SHARED_LINKER_FLAGS -lstdc++ -fPIC"
    )
fi

if [[ "$PLATFORM" = "Linux" ]] && [[ -n "${PLATFORM_ENV}" ]] ; then
    # Please note that this case only applies to infer/master platform builds
    CMAKE_ARGS+=(
        -DLLVM_ENABLE_PROJECTS="clang"
        -DLLVM_ENABLE_RUNTIMES="compiler-rt;libcxx;libcxxabi;libunwind"
        -DZLIB_INCLUDE_DIR="$ZLIB/include"
        # We disable some tools to avoid adding -isystem /usr/include in platform builds. It is good
        # enough for now, but if we want to use them, we should give proper include directories as
        # we did for zlib above.
        -DLLVM_ENABLE_LIBXML2=Off
        -DLLVM_ENABLE_TERMINFO=Off
        -DLLVM_ENABLE_Z3_SOLVER=Off
        -DLLVM_ENABLE_ZSTD=Off
    )
else
    CMAKE_ARGS+=(
        -DLLVM_ENABLE_PROJECTS="clang;openmp"
        -DLLVM_ENABLE_RUNTIMES="compiler-rt;libcxx;libcxxabi;libunwind"
    )
fi

if [ "$USE_NINJA" = "yes" ]; then
    CMAKE_GENERATOR="Ninja"
    BUILD_BIN="ninja"
    # Do not set a 'j' build default for Ninja (let Ninja decide)
    BUILD_ARGS=""
else
    CMAKE_GENERATOR="Unix Makefiles"
    BUILD_BIN="make"
    BUILD_ARGS="-j $JOBS"
fi

if [ "$SEQUENTIAL_LINK" = "yes" ]; then
    if [[ x"$USE_NINJA" = x ]]; then
        echo "Linking with a single process is only supported with the Ninja generator."
        echo "Unable to proceed, exiting."
        exit 1
    fi
    # For Ninja, the compile jobs is the number of CPUs *not* $JOBS
    CMAKE_ARGS+=(
        -DCMAKE_JOB_POOLS:STRING="compile=$NCPUS;link=1"
        -DCMAKE_JOB_POOL_COMPILE:STRING="compile"
        -DCMAKE_JOB_POOL_LINK:STRING="link"
    )
fi

# start the installation
if [ ! -d "$CLANG_SRC" ]; then
    echo "Clang src (${CLANG_SRC}) missing, please run src/prepare_clang_src.sh"
    exit 1
fi

if [ -n "$CLANG_TMP_DIR" ]; then
    TMP=$CLANG_TMP_DIR
else
    TMP=`mktemp -d ${TMPDIR-/tmp}/clang-setup.XXXXXX`
fi
pushd "$TMP"

mkdir -p build
pushd build

# workaround install issue with ocaml llvm bindings and ocamldoc
mkdir -p docs/ocamldoc/html

$CMAKE -G "$CMAKE_GENERATOR" "$CLANG_SRC" "${CMAKE_ARGS[@]}" $CLANG_CMAKE_ARGS

$BUILD_BIN $BUILD_ARGS

echo "testing clang build"
./bin/clang --version

# "uninstall" previous clang
rm -fr "$CLANG_PREFIX"

$BUILD_BIN $BUILD_ARGS install

popd # build
popd # $TMP

# On Linux, copy __config_site to install directory. This way we don't need additional -I statements
CONFIG_SITE="$CLANG_PREFIX/include/x86_64-unknown-linux-gnu/c++/v1/__config_site"
if [[ "$PLATFORM" = "Linux" ]] && [[ -f "$CONFIG_SITE" ]]; then
    cp -f "$CONFIG_SITE" "$CLANG_PREFIX/include/c++/v1/__config_site"
fi

# delete libs not needed by Infer
if [ "$KEEP_LIBS" != "yes" ]; then
    rm -v "$CLANG_PREFIX"/lib/libclang*
    rm -v "$CLANG_PREFIX"/lib/libLLVM*
fi

# brutally strip everything, ignore errors
set +e
find "$CLANG_PREFIX"/{bin,lib} -type f -exec "$STRIP" -x \{\} \;

if [[ "$PLATFORM" = "Linux" ]] && [[ -n "${PLATFORM_ENV}" ]]; then
    # patch binaries to use platform_env rpath, ignore errors
    find "$CLANG_PREFIX"/{bin,lib} -type f -exec "$PATCHELF" --set-rpath "${PLATFORM_ENV}/lib" \{\} \;
fi
set -e

echo "testing installed clang"
"$CLANG_PREFIX"/bin/clang --version

echo "deleting temp dir '$CLANG_TMP_DIR'..."
if [ -n "$CLANG_TMP_DIR" ]; then
    rm -rf "$TMP/*"
else
    rm -rf "$TMP"
fi

record_installed
