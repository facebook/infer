(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type compilation_data = {
  dir : string;
  command : string;
  args : string;
}

type t = compilation_data SourceFile.Map.t ref
let empty () = ref SourceFile.Map.empty

let get_size database = SourceFile.Map.cardinal !database

let iter database f = SourceFile.Map.iter f !database

let find database key = SourceFile.Map.find key !database

let parse_command_and_arguments command_and_arguments =
  let regexp = Str.regexp "[^\\][ ]" in
  let index = Str.search_forward regexp command_and_arguments 0 in
  let command = Str.string_before command_and_arguments (index+1) in
  let arguments = Str.string_after command_and_arguments (index+1) in
  command, arguments

(** Parse the compilation database json file into the compilationDatabase
    map. The json file consists of an array of json objects that contain the file
    to be compiled, the directory to be compiled in, and the compilation command as a list
    and as a string. We pack this information into the compilationDatabase map, and remove the
    clang invocation part, because we will use a clang wrapper. *)
let decode_json_file (database : t) json_format =
  let json_path = match json_format with | `Raw x | `Escaped x -> x in
  let to_string s = match json_format with
    | `Raw _ ->
        s
    | `Escaped _ ->
        Utils.with_process_in (Printf.sprintf "/bin/sh -c 'printf \"%%s\" %s'" s) input_line
        |> fst in
  Logging.out "parsing compilation database from %s@\n" json_path;
  let exit_format_error () =
    failwith ("Json file doesn't have the expected format") in
  let json = Yojson.Basic.from_file json_path in
  let get_dir el =
    match el with
    | ("directory", `String dir) -> Some (to_string dir)
    | _ -> None in
  let get_file el =
    match el with
    | ("file", `String file) -> Some (to_string file)
    | _ -> None in
  let get_cmd el =
    match el with
    | ("command", `String cmd) -> Some cmd
    | _ -> None in
  let rec parse_json json =
    match json with
    | `List arguments ->
        IList.iter parse_json arguments
    | `Assoc l ->
        let dir = match List.find_map ~f:get_dir l with
          | Some dir -> dir
          | None -> exit_format_error () in
        let file = match List.find_map ~f:get_file l with
          | Some file -> file
          | None -> exit_format_error () in
        let cmd = match List.find_map ~f:get_cmd l with
          | Some cmd -> cmd
          | None -> exit_format_error () in
        let command, args = parse_command_and_arguments cmd in
        let compilation_data = { dir; command; args;} in
        let abs_file = if Filename.is_relative file then dir ^/ file else file in
        let source_file = SourceFile.from_abs_path abs_file in
        database := SourceFile.Map.add source_file compilation_data !database
    | _ -> exit_format_error () in
  parse_json json

let from_json_files db_json_files =
  let db = empty () in
  IList.iter (decode_json_file db) db_json_files;
  Logging.out "created database with %d entries@\n" (get_size db);
  db
