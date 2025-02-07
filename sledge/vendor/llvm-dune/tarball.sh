#!/bin/sh

set -e
set -x

if test "$(dirname $0)" != '.'; then
    echo "The script must be executed from its current directory."
    exit 1
fi

if test "$#" -ne 0; then
    echo "Usage: $0"
    exit 1
fi

# Clean up directory
git clean -xdf

# Double-check we have the submodule up-to-date
git submodule update --init

# Remove everything other than what we need in dune-project/
find llvm-project \
  '!' -path 'llvm-project/llvm/bindings/ocaml/*' \
  '!' -path 'llvm-project/llvm/bindings/ocaml' \
  '!' -path 'llvm-project/llvm/bindings' \
  '!' -path 'llvm-project/llvm' \
  '!' -path 'llvm-project' \
  -delete

VERSION=$(cat VERSION)

mkdir "llvm-dune-full-minified-${VERSION}"
cp -r $(git ls-files) "llvm-dune-full-minified-${VERSION}/"
tar cvzf "llvm-dune-full-minified-${VERSION}.tar.gz" "llvm-dune-full-minified-${VERSION}"
