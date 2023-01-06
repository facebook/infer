(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int array

let new_line_marker = "// .line "

let detect_new_line line =
  let line = String.strip line in
  match String.chop_prefix line ~prefix:new_line_marker with
  | Some line_num ->
      int_of_string_opt line_num
  | _ ->
      None


let create content : t =
  let lines = String.split content ~on:'\n' in
  let line_map = Array.create 0 ~len:(List.length lines) in
  let cur_line = ref 1 in
  List.iteri lines ~f:(fun i line ->
      match detect_new_line line with
      | Some line_num ->
          line_map.(i) <- !cur_line ;
          cur_line := line_num
      | _ ->
          line_map.(i) <- !cur_line ) ;
  line_map


let find line_map = function
  | i when i < 0 && i >= Array.length line_map ->
      None
  | i ->
      Some line_map.(i)


let pp fmt line_map =
  Array.iteri line_map ~f:(fun textual_line original_line ->
      F.fprintf fmt "Textual line: %a, Original line: %a\n" Int.pp textual_line Int.pp original_line )
