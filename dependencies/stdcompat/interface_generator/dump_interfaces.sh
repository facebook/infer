#!/usr/bin/env bash
set -e

# 3.11.2 3.12.1 4.00.1
#   4.01.0 4.02.3 4.03.0 4.04.2 4.05.0 4.06.1 \
#   4.07.0 \
#   4.08 4.09 4.10 4.11 4.12 4.13 4.14 

for ocaml_version in 5.2 5.1 5.0; do
  target_dir=../interfaces/$ocaml_version
#  ocaml_version=ocaml-variants.4.10.0+beta2
#  target_dir=../interfaces/4.10.0
  mkdir -p $target_dir
  for module in Stack \
       Stdlib Atomic Arg Array ArrayLabels Bool Buffer Bytes BytesLabels Callback Char \
       Complex Digest Domain Either Ephemeron Filename Float Format Fun Gc Hashtbl Int32 \
       Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj \
       Oo Option Parsing Printexc Printf Queue Random Result Scanf Seq Set \
       StdLabels String StringLabels Sys Uchar Weak In_channel Out_channel \
       Unit ; do
    target=$target_dir/`echo ${module:0:1} | tr A-Z a-z`${module:1}.mli
    echo $target
    opam exec --switch=$ocaml_version -- \
         ./interface_dumper $module ocaml >$target
#    [ `stat --format="%s" $target` -gt 1 ] || rm $target
#    [ `stat -f%z $target` -gt 1 ] || rm $target
  done
done
