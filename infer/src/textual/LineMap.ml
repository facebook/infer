(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type loc = {line: int; column: int}

type t = loc array

let new_line_marker = "// .line "

let new_column_marker = "// .column "

let read_value ~marker line =
  let line = String.strip line in
  String.chop_prefix line ~prefix:marker |> Option.bind ~f:int_of_string_opt


let detect_new_line line = read_value ~marker:new_line_marker line

let detect_new_column line = read_value ~marker:new_column_marker line

let create content : t =
  let lines = String.split content ~on:'\n' in
  let line_map = Array.create {line= 0; column= -1} ~len:(List.length lines) in
  let cur_line = ref 1 in
  let cur_column = ref (-1) in
  List.iteri lines ~f:(fun i line ->
      line_map.(i) <- {line= !cur_line; column= !cur_column} ;
      Option.iter (detect_new_line line) ~f:(fun line_num ->
          cur_line := line_num ;
          cur_column := -1 ) ;
      Option.iter (detect_new_column line) ~f:(fun column_num -> cur_column := column_num) ) ;
  line_map


let find line_map = function
  | i when i < 0 && i >= Array.length line_map ->
      None
  | i ->
      Some line_map.(i)


let pp fmt line_map =
  let pp_loc fmt {line; column} =
    F.fprintf fmt "line: %a" Int.pp line ;
    if column > 0 then F.fprintf fmt ", column: %a" Int.pp column
  in
  Array.iteri line_map ~f:(fun textual_line loc ->
      F.fprintf fmt "Textual line: %a, Original %a\n" Int.pp textual_line pp_loc loc )
