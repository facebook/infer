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

let mk ?(dir = none.dir) ?(file = none.file) ?(col = none.col) ~line =
  let dir = if String.is_empty dir then dir else Filename.realpath dir in
  {dir; file; line; col}

let root = ref (Filename.realpath (Sys.getcwd ()))

let pp fs ({dir; file; line; col} as loc) =
  if not (equal loc none) then Format.pp_print_string fs "; " ;
  if not (String.is_empty dir) then (
    let dir =
      Option.value_map ~f:Fpath.to_string
        (Fpath.relativize ~root:(Fpath.v !root) (Fpath.v dir))
        ~default:dir
    in
    Format.pp_print_string fs dir ;
    Format.pp_print_string fs Filename.dir_sep ) ;
  Format.pp_print_string fs file ;
  if not (String.is_empty file) then Format.pp_print_char fs ':' ;
  if line > 0 then (
    Format.pp_print_int fs line ;
    Format.pp_print_char fs ':' ) ;
  if col > 0 then (
    Format.pp_print_int fs col ;
    Format.pp_print_char fs ':' )

let pp fs l = Format.pp_print_as fs 0 (Format.asprintf "%a" pp l)
