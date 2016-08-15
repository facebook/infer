(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

let frame_id_of_stackframe frame =
  let loc_str = match frame.Stacktrace.line_num with
    | None -> frame.Stacktrace.file_str
    | Some line -> F.sprintf "%s:%d" frame.Stacktrace.file_str line in
  F.sprintf
    "%s.%s(%s)"
    frame.Stacktrace.class_str
    frame.Stacktrace.method_str
    loc_str

let frame_id_of_summary stacktree =
  let short_name = IList.hd
      (Str.split (Str.regexp "(") stacktree.Stacktree_j.method_name) in
  match stacktree.Stacktree_j.location with
  | None ->
      failwith "Attempted to take signature of a frame without location \
                information. This is undefined."
  | Some { line = Some line_num; file } ->
      F.sprintf "%s(%s:%d)" short_name (Filename.basename file) line_num
  | Some { file } ->
      F.sprintf "%s(%s)" short_name (Filename.basename file)

let stracktree_of_frame frame =
  { Stacktree_j.method_name = F.sprintf
        "%s.%s"
        frame.Stacktrace.class_str
        frame.Stacktrace.method_str;
    location = Some { Stacktree_j.location_type = "call_site";
                      file = frame.Stacktrace.file_str;
                      line = frame.Stacktrace.line_num;
                      blame_range = [] };
    callees = [];
  }

(** k = 1 implementation, where k is the number of levels of calls inlined *)
let stitch_summaries stacktrace_file summary_files out_file =
  let stacktrace = Stacktrace.of_json_file stacktrace_file in
  let summaries = IList.map
      (Ag_util.Json.from_file Stacktree_j.read_stacktree)
      summary_files in
  let summary_map = IList.fold_left
      (fun acc stacktree ->
         StringMap.add (frame_id_of_summary stacktree) stacktree acc)
      StringMap.empty
      summaries in
  let expand_stack_frame frame =
    (* TODO: Implement k > 1 case *)
    let frame_id = frame_id_of_stackframe frame in
    if StringMap.exists (fun key _ -> key = frame_id) summary_map then
      StringMap.find frame_id summary_map
    else
      stracktree_of_frame frame in
  let expanded_frames = IList.map expand_stack_frame stacktrace.frames in
  let crashcontext = { Stacktree_j.stack = expanded_frames} in
  Ag_util.Json.to_file Stacktree_j.write_crashcontext_t out_file crashcontext

let collect_all_summaries root_out_dir stacktrace_file =
  let out_dir = Filename.concat root_out_dir "crashcontext" in
  DB.create_dir out_dir;
  let out_file = Filename.concat out_dir "crashcontext.json" in
  let path_regexp = Str.regexp ".*crashcontext/.*\\..*\\.json" in
  let path_matcher path =  Str.string_match path_regexp path 0 in
  let method_summaries =
    DB.paths_matching root_out_dir path_matcher in
  stitch_summaries stacktrace_file method_summaries out_file;
