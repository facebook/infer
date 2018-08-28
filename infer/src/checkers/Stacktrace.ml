(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module for parsing stack traces and using them to guide Infer analysis *)

open! IStd
module L = Logging

type frame = {class_str: string; method_str: string; file_str: string; line_num: int option}

type t = {exception_name: string; frames: frame list}

let new_line_regexp = Str.regexp "\n"

(* Pre-compute the regular expression matchers used to parse: *)
(* Stack frames into (procedure, location) tuples. *)
let frame_regexp = Str.regexp "\t*at \\(.*\\)(\\(.*\\))"

(* procedures into class and method name. *)
let procname_regexp = Str.regexp "\\(.*\\)\\.\\(.*\\)"

(* locations into file and line number information. *)
let file_and_line_regexp = Str.regexp "\\(.*\\):\\([0-9]+\\)"

(* exception information lines into thread id and exception type *)
let exception_regexp = Str.regexp "Exception in thread \"\\(.*\\)\" \\(.*\\)"

let make exception_name frames = {exception_name; frames}

let make_frame class_str method_str file_str line_num = {class_str; method_str; file_str; line_num}

let frame_matches_location frame_obj loc =
  let lfname =
    if SourceFile.is_invalid loc.Location.file then None
    else Some (SourceFile.to_string loc.Location.file)
  in
  let matches_file =
    Option.value_map lfname ~default:false ~f:(String.is_suffix ~suffix:frame_obj.file_str)
  in
  let matches_line =
    match frame_obj.line_num with None -> false | Some line -> Int.equal line loc.Location.line
  in
  matches_file && matches_line


let parse_stack_frame frame_str =
  (* separate the qualified method name and the parenthesized text/line number*)
  ignore (Str.string_match frame_regexp frame_str 0) ;
  let qualified_procname = Str.matched_group 1 frame_str in
  let file_and_line = Str.matched_group 2 frame_str in
  (* separate the class name from the method name *)
  ignore (Str.string_match procname_regexp qualified_procname 0) ;
  let class_str = Str.matched_group 1 qualified_procname in
  let method_str = Str.matched_group 2 qualified_procname in
  (* Native methods don't have debugging info *)
  if String.equal file_and_line "Native Method" then
    make_frame class_str method_str "Native Method" None
  else
    (* Separate the filename and line number.
       note that a few methods might not have line number information,
       for those, file_and_line includes only the filename. *)
    let is_file_line = Str.string_match file_and_line_regexp file_and_line 0 in
    let file_str, line_num =
      if is_file_line then
        ( Str.matched_group 1 file_and_line
        , Some (int_of_string (Str.matched_group 2 file_and_line)) )
      else (file_and_line, None)
    in
    make_frame class_str method_str file_str line_num


let parse_exception_line exception_line =
  ignore (Str.string_match exception_regexp exception_line 0) ;
  let exception_name = Str.matched_group 2 exception_line in
  exception_name


let of_string s =
  let lines = Str.split new_line_regexp s in
  match lines with
  | exception_line :: trace ->
      let exception_name = parse_exception_line exception_line in
      let parsed = List.map ~f:parse_stack_frame trace in
      make exception_name parsed
  | [] ->
      L.(die UserError) "Empty stack trace"


let of_json filename json =
  let exception_name_key = "exception_type" in
  let frames_key = "stack_trace" in
  let extract_json_member key =
    match Yojson.Basic.Util.member key json with
    | `Null ->
        L.(die UserError) "Missing key in supplied JSON data: %s (in file %s)" key filename
    | item ->
        item
  in
  let exception_name = Yojson.Basic.Util.to_string (extract_json_member exception_name_key) in
  let frames =
    Yojson.Basic.Util.to_list (extract_json_member frames_key)
    |> List.map ~f:Yojson.Basic.Util.to_string
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun s -> s <> "")
    |> List.map ~f:parse_stack_frame
  in
  make exception_name frames


let of_json_file filename =
  try of_json filename (Yojson.Basic.from_file filename) with
  | Sys_error msg | Yojson.Json_error msg ->
      L.(die UserError)
        "Could not read or parse the supplied JSON stacktrace file %s :@\n %s" filename msg
