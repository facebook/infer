#!/usr/bin/env bash
set -e
for ocamlversion in `opam switch -i -s`; do
    opam switch $ocamlversion
    eval `opam config env`
    ./configure
    make clean
    make
    make tests
    ./configure --disable-magic
    make clean
    make
    make tests
done
