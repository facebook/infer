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
  try In_channel.read_lines ~fix_win_eol:true fname |> Array.of_list |> Option.some
  with Sys_error _ -> None


let file_data (hash : t) fname =
  match Hashtbl.find_opt hash fname with
  | None ->
      let open IOption.Let_syntax in
      let+ lines_arr = read_file (SourceFile.to_abs_path fname) in
      Hashtbl.add hash fname lines_arr ;
      lines_arr
  | res ->
      res


let from_file_linenum hash fname linenum =
  match file_data hash fname with
  | Some lines_arr when linenum > 0 && linenum <= Array.length lines_arr ->
      Some lines_arr.(linenum - 1)
  | _ ->
      None


let from_loc hash loc = from_file_linenum hash loc.Location.file loc.Location.line

let iteri hash fname ~f =
  file_data hash fname |> Option.iter ~f:(Array.iteri ~f:(fun linenum line -> f (linenum + 1) line))
