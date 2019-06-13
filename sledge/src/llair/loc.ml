(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}
[@@deriving compare, equal, hash, sexp]

let none = {dir= ""; file= ""; line= 0; col= 0}
let is_none loc = compare loc none = 0

let mk ?(dir = none.dir) ?(file = none.file) ?(col = none.col) ~line =
  {dir; file; line; col}

let pp fs {dir; file; line; col} =
  Format.pp_print_string fs dir ;
  if not (String.is_empty dir) then
    Format.pp_print_string fs Filename.dir_sep ;
  Format.pp_print_string fs file ;
  if not (String.is_empty file) then Format.pp_print_char fs ':' ;
  if line > 0 then (
    Format.pp_print_int fs line ;
    Format.pp_print_char fs ':' ) ;
  if col > 0 then (
    Format.pp_print_int fs col ;
    Format.pp_print_char fs ':' )
