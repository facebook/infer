(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for parsing stack traces and using them to guide Infer analysis *)

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

let parse_stack_frame frame_str =
  (* separate the qualified method name and the parenthesized text/line number*)
  ignore(Str.string_match (Str.regexp "\t*at \\(.*\\)(\\(.*\\))") frame_str 0);
  let qualified_procname = Str.matched_group 1 frame_str in
  let file_and_line = Str.matched_group 2 frame_str in
  (* separate the class name from the method name *)
  ignore(Str.string_match (Str.regexp "\\(.*\\)\\.\\(.*\\)") qualified_procname 0);
  let class_str = Str.matched_group 1 qualified_procname in
  let method_str = Str.matched_group 2 qualified_procname in
  (* separate the filename and line number *)
  ignore(Str.string_match (Str.regexp "\\(.*\\):\\([0-9]+\\)") file_and_line 0);
  let file_str = Str.matched_group 1 file_and_line in
  let line_num = int_of_string (Str.matched_group 2 file_and_line) in
  make_frame class_str method_str file_str line_num

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
