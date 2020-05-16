(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Map a file name to an array of string, one for each line in the file. *)
type t = (SourceFile.t, string array) Hashtbl.t

let create () = Hashtbl.create 1

let read_file fname =
  let cin = In_channel.create fname in
  let lines = ref [] in
  try
    while true do
      let line_raw = In_channel.input_line_exn cin in
      let line =
        let len = String.length line_raw in
        if len > 0 && Char.equal line_raw.[len - 1] '\r' then
          String.sub line_raw ~pos:0 ~len:(len - 1)
        else line_raw
      in
      lines := line :: !lines
    done ;
    assert false (* execution never reaches here *)
  with End_of_file ->
    In_channel.close cin ;
    Array.of_list_rev !lines


let file_data (hash : t) fname =
  try Some (Hashtbl.find hash fname)
  with Caml.Not_found -> (
    try
      let lines_arr = read_file (SourceFile.to_abs_path fname) in
      Hashtbl.add hash fname lines_arr ;
      Some lines_arr
    with exn when SymOp.exn_not_failure exn -> None )


let from_file_linenum hash fname linenum =
  match file_data hash fname with
  | Some lines_arr when linenum > 0 && linenum <= Array.length lines_arr ->
      Some lines_arr.(linenum - 1)
  | _ ->
      None


let from_loc hash loc = from_file_linenum hash loc.Location.file loc.Location.line

let iteri hash fname ~f =
  file_data hash fname |> Option.iter ~f:(Array.iteri ~f:(fun linenum line -> f (linenum + 1) line))
