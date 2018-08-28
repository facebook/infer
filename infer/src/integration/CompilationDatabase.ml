(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type compilation_data = {directory: string; executable: string; escaped_arguments: string list}

type t = compilation_data SourceFile.Map.t ref

let empty () = ref SourceFile.Map.empty

let get_size database = SourceFile.Map.cardinal !database

let filter_compilation_data database ~f =
  SourceFile.Map.filter (fun s _ -> f s) !database |> SourceFile.Map.bindings


let parse_command_and_arguments command_and_arguments =
  let regexp = Str.regexp "[^\\][ ]" in
  let index = Str.search_forward regexp command_and_arguments 0 in
  let command = Str.string_before command_and_arguments (index + 1) in
  let arguments = Str.string_after command_and_arguments (index + 1) in
  (command, [arguments])


(** Parse the compilation database json file into the compilationDatabase
    map. The json file consists of an array of json objects that contain the file
    to be compiled, the directory to be compiled in, and the compilation command as a list
    and as a string. We pack this information into the compilationDatabase map, and remove the
    clang invocation part, because we will use a clang wrapper. *)
let decode_json_file (database : t) json_format =
  let json_path = match json_format with `Raw x | `Escaped x -> x in
  let unescape_path s =
    match json_format with
    | `Raw _ ->
        s
    | `Escaped _ ->
        Utils.with_process_in
          (Printf.sprintf "/bin/sh -c 'printf \"%%s\" %s'" s)
          In_channel.input_line_exn
        |> fst
  in
  L.(debug Capture Quiet) "parsing compilation database from %s@\n" json_path ;
  let exit_format_error error =
    L.(die ExternalError) ("Json file doesn't have the expected format: " ^^ error)
  in
  let parse_command json =
    let directory = ref None in
    let file = ref None in
    let command = ref None in
    let one_field = function
      | "directory", `String dir ->
          directory := Some (unescape_path dir)
      | "directory", json ->
          exit_format_error
            "the value of the \"directory\" field is not a string; found '%s' instead"
            (Yojson.Basic.to_string json)
      | "file", `String f ->
          file := Some (unescape_path f)
      | "file", json ->
          exit_format_error "the value of the \"file\" field is not a string; found '%s' instead"
            (Yojson.Basic.to_string json)
      | "command", `String cmd ->
          (* prefer "arguments" when available *)
          if Option.is_none !command then command := Some (parse_command_and_arguments cmd)
      | "command", json ->
          exit_format_error
            "the value of the \"command\" field is not a string; found '%s' instead"
            (Yojson.Basic.to_string json)
      | "arguments", `List args -> (
          let args =
            List.map args ~f:(function
              | `String argument ->
                  argument
              | _ ->
                  exit_format_error
                    "the value of the \"arguments\" field is not a list of strings in command %s"
                    (Yojson.Basic.to_string json) )
          in
          match args with
          | [] ->
              exit_format_error
                "the value of the \"arguments\" field is an empty list in command %s"
                (Yojson.Basic.to_string json)
          | cmd :: args ->
              command := Some (cmd, List.map ~f:Escape.escape_shell args) )
      | "arguments", json ->
          exit_format_error
            "the value of the \"arguments\" field is not a list; found '%s' instead"
            (Yojson.Basic.to_string json)
      | "output", _ ->
          ()
      | _, _ (* be generous and allow anything else too *) ->
          ()
    in
    match json with
    | `Assoc fields ->
        List.iter ~f:one_field fields ;
        let directory =
          match !directory with
          | Some directory ->
              directory
          | None ->
              exit_format_error "no \"directory\" entry found in command %s"
                (Yojson.Basic.to_string json)
        in
        let file =
          match !file with
          | Some file ->
              file
          | None ->
              exit_format_error "no \"file\" entry found in command %s"
                (Yojson.Basic.to_string json)
        in
        let executable, escaped_arguments =
          match !command with
          | Some x ->
              x
          | None ->
              exit_format_error "no \"command\" or \"arguments\" entry found in command %s"
                (Yojson.Basic.to_string json)
        in
        let compilation_data = {directory; executable; escaped_arguments} in
        let abs_file = if Filename.is_relative file then directory ^/ file else file in
        let source_file = SourceFile.from_abs_path abs_file in
        database := SourceFile.Map.add source_file compilation_data !database
    | _ ->
        exit_format_error "Compilation database entry is not an object: %s"
          (Yojson.Basic.to_string json)
  in
  match Yojson.Basic.from_file json_path with
  | `List commands ->
      List.iter ~f:parse_command commands
  | _ as json ->
      exit_format_error "Compilation database is not a list of commands: %s"
        (Yojson.Basic.to_string json)


let from_json_files db_json_files =
  let db = empty () in
  List.iter ~f:(decode_json_file db) db_json_files ;
  L.(debug Capture Quiet) "created database with %d entries@\n" (get_size db) ;
  db
