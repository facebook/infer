(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
module F = Format
module L = Logging

let multilink_file_name = "multilink.txt"

type t = string String.Table.t

let add multilinks fname = String.Table.set multilinks ~key:(Filename.basename fname) ~data:fname

let create () : t = String.Table.create ~size:1 ()

(* Cache of multilinks files read from disk *)
let multilink_files_cache = String.Table.create ~size:1 ()

let reset_cache () = String.Table.clear multilink_files_cache

let read ~dir : t option =
  let multilink_fname = Filename.concat dir multilink_file_name in
  match Utils.read_file multilink_fname with
  | Error _ ->
      None
  | Ok lines ->
      let links = create () in
      List.iter
        ~f:(fun line -> String.Table.set links ~key:(Filename.basename line) ~data:line)
        lines ;
      String.Table.set multilink_files_cache ~key:dir ~data:links ;
      Some links


(* Write a multilink file in the given directory *)
let write multilinks ~dir =
  let fname = Filename.concat dir multilink_file_name in
  let outc = Out_channel.create fname in
  String.Table.iteri
    ~f:(fun ~key:_ ~data:src -> Out_channel.output_string outc (src ^ "\n"))
    multilinks ;
  Out_channel.close outc


let lookup ~dir =
  try Some (String.Table.find_exn multilink_files_cache dir) with
  | Not_found_s _ | Caml.Not_found ->
      read ~dir


let resolve fname =
  let fname_s = DB.filename_to_string fname in
  if Sys.file_exists fname_s = `Yes then fname
  else
    let base = Filename.basename fname_s in
    let dir = Filename.dirname fname_s in
    match lookup ~dir with
    | None ->
        fname
    | Some links -> (
      try DB.filename_from_string (String.Table.find_exn links base) with
      | Not_found_s _ | Caml.Not_found ->
          fname )
