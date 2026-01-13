(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = LineAdded of int | LineRemoved of int

val append_removed_line : int option -> t list -> t list

val append_added_line : int option -> t list -> t list

val append_removed_lines : int option -> int option -> t list -> t list

val append_added_lines : int option -> int option -> t list -> t list

type explicit

val pp_explicit : F.formatter -> explicit -> unit

val dummy_explicit : explicit

val write_json :
  previous_file:string -> current_file:string -> out_path:string -> explicit list -> unit

val gen_explicit_diffs :
  previous_content:string -> current_content:string -> t list -> explicit list
