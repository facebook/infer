#!/usr/bin/env bash
set -e

opam config exec --switch=4.07.0+rc1 -- \
  ocamlfind ocamlc -g -package unix,compiler-libs.common -linkpkg \
    interface_generator.ml -o interface_generator

for module in \
    Pervasives Arg Array ArrayLabels Buffer Bytes BytesLabels Callback Char \
    Complex Digest Ephemeron Filename Float Format Gc Genlex Hashtbl Int32 \
    Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj Oo \
    Option Parsing Printexc Printf Queue Random Result Scanf Seq Set Sort \
    Spacetime Stack StdLabels Stream String StringLabels Sys Uchar Weak; do
    target=stdcompat__`echo $module | tr A-Z a-z`.mli.in
    echo $module
    ./interface_generator $module \
        ~/.opam/4.07.0+rc1/bin/ocaml \
        ~/.opam/4.06.1/bin/ocaml \
        ~/.opam/4.05.0/bin/ocaml \
        ~/.opam/4.04.2/bin/ocaml \
        ~/.opam/4.03.0/bin/ocaml \
        ~/.opam/4.02.3/bin/ocaml \
        ~/.opam/4.01.0/bin/ocaml \
        ~/.opam/4.00.1/bin/ocaml \
        ~/.opam/3.12.1/bin/ocaml \
        ~/.opam/3.11.2/bin/ocaml >$target
done
