(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}
[@@deriving compare, equal, sexp]

let none = {dir= ""; file= ""; line= 0; col= 0}

let mk ~dir ~file ~line ~col =
  let dir = Option.get_or dir ~default:none.dir in
  let file = Option.get_or file ~default:none.file in
  let line = Option.get_or line ~default:none.line in
  let col = Option.get_or col ~default:none.col in
  {dir; file; line; col}

let root = ref None

let pp fs ({dir; file; line; col} as loc) =
  if not (equal loc none) then Format.pp_print_string fs "; " ;
  ( if not (String.is_empty dir) then
    let dir_file =
      if String.is_empty file then Fpath.v dir
      else Fpath.append (Fpath.v dir) (Fpath.v file)
    in
    let relative =
      let* root = !root in
      let+ relative = Fpath.relativize ~root:(Fpath.v root) dir_file in
      relative
    in
    Fpath.pp fs (Option.value relative ~default:dir_file) ) ;
  if not (String.is_empty file) then Format.pp_print_char fs ':' ;
  if line > 0 then (
    Format.pp_print_int fs line ;
    Format.pp_print_char fs ':' ) ;
  if col > 0 then (
    Format.pp_print_int fs col ;
    Format.pp_print_char fs ':' )

let pp fs l = Format.pp_print_as fs 0 (Format.asprintf "%a" pp l)
