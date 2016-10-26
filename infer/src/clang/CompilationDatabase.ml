(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

type compilation_data = {
  dir : string;
  command : string;
  args : string list;
}

type t = compilation_data StringMap.t ref
let empty () = ref StringMap.empty

let get_size database = StringMap.cardinal !database

let iter database f = StringMap.iter f !database

let find database key = StringMap.find key !database

(** Parse the compilation database json file into the compilationDatabase
    map. The json file consists of an array of json objects that contain the file
    to be compiled, the directory to be compiled in, and the compilation command as a list
    and as a string. We pack this information into the compilationDatabase map, and remove the
    clang invocation part, because we will use a clang wrapper. *)
let decode_json_file (database : t) should_add_file json_path =
  let json = Yojson.Basic.from_file json_path in
  let parse_argument compilation_argument =
    match compilation_argument with
    | `String arg -> arg
    | _ -> failwith ("Json file doesn't have the expected format") in
  let rec parse_json json =
    match json with
    | `List arguments ->
        IList.iter parse_json arguments
    | `Assoc [ ("directory", `String dir);
               ("file", `String file_path);
               ("arguments", `List compilation_arguments);
               ("command", `String _) ] ->
        (match IList.map parse_argument compilation_arguments with
         | [] -> failwith ("Command cannot be empty")
         | command :: args when should_add_file file_path ->
             let compilation_data = { dir; command; args;} in
             database := StringMap.add file_path compilation_data !database
         | _ -> ())
    | _ ->
        failwith ("Json file doesn't have the expected format") in
  parse_json json
