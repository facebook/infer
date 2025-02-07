#!/bin/sh -ex

if test "$#" != 1; then
    echo "Usage: $0 <prefix>"
    exit 1
fi

PREFIX=$1

dune install "--prefix=$PREFIX" --release

for pkg in $(basename -s .opam *.opam); do 
    case "$pkg" in
    llvm)
        cp META.llvm.in "$PREFIX/lib/llvm/META";;
    llvm_*)
        target=$(echo "$pkg" | cut -d_ -f2-)
        sed "s/@TARGET@/$target/g" META.llvm_TARGET.in > "$PREFIX/lib/llvm_$target/META";;
    *)
        echo "Something went wrong while processing $pkg. Please report."
        exit 1;;
    esac
done
