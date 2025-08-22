(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let read_file filename = In_channel.with_file filename ~f:In_channel.input_all

let () =
  if Array.length (Sys.get_argv ()) <> 3 then
    prerr_endline "Usage: ast_compare <file1.py> <file2.py>"
  else
    let file1 = (Sys.get_argv ()).(1) in
    let file2 = (Sys.get_argv ()).(2) in
    let src1 = read_file file1 in
    let src2 = read_file file2 in
    if PythonCompareWithoutTypeAnnot.compare src1 src2 () then
      print_endline "ASTs are equal (ignoring type annotations)."
    else print_endline "ASTs differ."
