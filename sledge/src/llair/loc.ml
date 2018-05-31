(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}
[@@deriving compare, sexp]

let none = {dir= ""; file= ""; line= 0; col= 0}

let mk ?(dir= none.dir) ?(file= none.file) ?(col= none.col) ~line =
  {dir; file; line; col}

let fmt ff {dir; file; line; col} =
  Format.pp_print_string ff dir ;
  if not (String.is_empty dir) then
    Format.pp_print_string ff Filename.dir_sep ;
  Format.pp_print_string ff file ;
  if not (String.is_empty file) then Format.pp_print_char ff ':' ;
  if line > 0 then (
    Format.pp_print_int ff line ;
    Format.pp_print_char ff ':' ) ;
  if col > 0 then (
    Format.pp_print_int ff col ;
    Format.pp_print_char ff ':' )
