(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for parsing stack traces and using them to guide Infer analysis *)

open! Utils

module F = Format

type frame = {
  class_str : string;
  method_str : string;
  file_str : string;
  line_num : int;
}

type t = {
  exception_name: string;
  frames: frame list;
}

let make exception_name frames = { exception_name; frames; }

let make_frame class_str method_str file_str line_num =
  { class_str; method_str; file_str; line_num; }

let frame_matches_location frame_obj loc =
  let lfname = DB.source_file_to_string loc.Location.file in
  Utils.string_is_suffix frame_obj.file_str lfname &&
  frame_obj.line_num = loc.Location.line

let parse_stack_frame frame_str =
  (* separate the qualified method name and the parenthesized text/line number*)
  ignore(Str.string_match (Str.regexp "\t*at \\(.*\\)(\\(.*\\))") frame_str 0);
  let qualified_procname = Str.matched_group 1 frame_str in
  let file_and_line = Str.matched_group 2 frame_str in
  (* separate the class name from the method name *)
  ignore(Str.string_match (Str.regexp "\\(.*\\)\\.\\(.*\\)") qualified_procname 0);
  let class_str = Str.matched_group 1 qualified_procname in
  let method_str = Str.matched_group 2 qualified_procname in
  (* Native methods don't have debugging info *)
  if string_equal file_and_line "Native Method" then
    make_frame class_str method_str "Native Method" (-1)
  else begin
    (* separate the filename and line number *)
    ignore(Str.string_match (Str.regexp "\\(.*\\):\\([0-9]+\\)") file_and_line 0);
    let file_str = Str.matched_group 1 file_and_line in
    let line_num = int_of_string (Str.matched_group 2 file_and_line) in
    make_frame class_str method_str file_str line_num
  end

let parse_exception_line exception_line =
  ignore(Str.string_match
           (Str.regexp "Exception in thread \"\\(.*\\)\" \\(.*\\)")
           exception_line
           0);
  let exception_name = Str.matched_group 2 exception_line in
  exception_name

let of_string s =
  let lines = Str.split (Str.regexp "\n") s in
  match lines with
  | exception_line :: trace ->
      let exception_name = parse_exception_line exception_line in
      let parsed = IList.map parse_stack_frame trace in
      make exception_name parsed
  | [] -> failwith "Empty stack trace"

let of_json json =
  let exception_name_key = "exception_type" in
  let frames_key = "stack_trace" in
  let extract_json_member key =
    match Yojson.Basic.Util.member key json with
    | `Null -> failwith ("Missing key in supplied JSON \
                          data: " ^ key)
    | item -> item in
  let exception_name =
    Yojson.Basic.Util.to_string (extract_json_member exception_name_key) in
  let frames =
    Yojson.Basic.Util.to_list (extract_json_member frames_key)
    |> IList.map Yojson.Basic.Util.to_string
    |> IList.map String.trim
    |> IList.filter (fun s -> s <> "")
    |> IList.map parse_stack_frame in
  make exception_name frames

let of_json_file filename =
  match Utils.read_optional_json_file filename with
  | Ok json -> of_json json
  | Error msg -> failwith (Printf.sprintf "Could not read or parse the supplied JSON \
                                           stacktrace file %s :\n %s" filename msg)
