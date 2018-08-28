(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let frame_id_of_stackframe frame =
  let loc_str =
    match frame.Stacktrace.line_num with
    | None ->
        frame.Stacktrace.file_str
    | Some line ->
        F.sprintf "%s:%d" frame.Stacktrace.file_str line
  in
  F.sprintf "%s.%s(%s)" frame.Stacktrace.class_str frame.Stacktrace.method_str loc_str


let frame_id_of_summary stacktree =
  let short_name = List.hd_exn (Str.split (Str.regexp "(") stacktree.Stacktree_j.method_name) in
  match stacktree.Stacktree_j.location with
  | None ->
      L.(die InternalError)
        "Attempted to take signature of a frame without location information. This is undefined."
  | Some {line= Some line_num; file} ->
      F.sprintf "%s(%s:%d)" short_name (Filename.basename file) line_num
  | Some {file} ->
      F.sprintf "%s(%s)" short_name (Filename.basename file)


let stracktree_of_frame frame =
  { Stacktree_j.method_name=
      F.sprintf "%s.%s" frame.Stacktrace.class_str frame.Stacktrace.method_str
  ; location=
      Some
        { Stacktree_j.location_type= "call_site"
        ; file= frame.Stacktrace.file_str
        ; line= frame.Stacktrace.line_num
        ; blame_range= [] }
  ; callees= [] }


(** k = 1 implementation, where k is the number of levels of calls inlined *)
let stitch_summaries stacktrace_file summary_files out_file =
  let stacktrace = Stacktrace.of_json_file stacktrace_file in
  let summaries =
    List.map ~f:(Atdgen_runtime.Util.Json.from_file Stacktree_j.read_stacktree) summary_files
  in
  let summary_map =
    List.fold
      ~f:(fun acc stacktree ->
        String.Map.set ~key:(frame_id_of_summary stacktree) ~data:stacktree acc )
      ~init:String.Map.empty summaries
  in
  let expand_stack_frame frame =
    (* TODO: Implement k > 1 case *)
    let frame_id = frame_id_of_stackframe frame in
    if String.Map.existsi ~f:(fun ~key ~data:_ -> String.equal key frame_id) summary_map then
      String.Map.find_exn summary_map frame_id
    else stracktree_of_frame frame
  in
  let expanded_frames = List.map ~f:expand_stack_frame stacktrace.frames in
  let crashcontext = {Stacktree_j.stack= expanded_frames} in
  Atdgen_runtime.Util.Json.to_file Stacktree_j.write_crashcontext_t out_file crashcontext


let collect_all_summaries root_summaries_dir stacktrace_file stacktraces_dir =
  let method_summaries =
    Utils.directory_fold
      (fun summaries path ->
        (* check if the file is a JSON file under the crashcontext dir *)
        if
          Sys.is_directory path <> `Yes
          && Filename.check_suffix path "json"
          && String.is_suffix ~suffix:"crashcontext" (Filename.dirname path)
        then path :: summaries
        else summaries )
      [] root_summaries_dir
  in
  let pair_for_stacktrace_file =
    match stacktrace_file with
    | None ->
        None
    | Some file ->
        let crashcontext_dir = Config.results_dir ^/ "crashcontext" in
        Utils.create_dir crashcontext_dir ;
        Some (file, crashcontext_dir ^/ "crashcontext.json")
  in
  let trace_file_regexp = Str.regexp "\\(.*\\)\\.json" in
  let pairs_for_stactrace_dir =
    match stacktraces_dir with
    | None ->
        []
    | Some s -> (
        let dir = DB.filename_from_string s in
        let trace_file_matcher path =
          let path_str = DB.filename_to_string path in
          Str.string_match trace_file_regexp path_str 0
        in
        let trace_fold stacktrace_file acc =
          let stacktrace_file_str = DB.filename_to_string stacktrace_file in
          let out_file = Str.matched_group 1 stacktrace_file_str ^ ".crashcontext.json" in
          (stacktrace_file_str, out_file) :: acc
        in
        try DB.fold_paths_matching ~dir ~p:trace_file_matcher ~init:[] ~f:trace_fold
        with
        (* trace_fold runs immediately after trace_file_matcher in the
           DB.fold_paths_matching statement below, so we don't need to
           call Str.string_match again. *)
        | Caml.Not_found
        -> assert false )
  in
  let input_output_file_pairs =
    match pair_for_stacktrace_file with
    | None ->
        pairs_for_stactrace_dir
    | Some pair ->
        pair :: pairs_for_stactrace_dir
  in
  let process_stacktrace (stacktrace_file, out_file) =
    stitch_summaries stacktrace_file method_summaries out_file
  in
  List.iter ~f:process_stacktrace input_output_file_pairs


let crashcontext_epilogue ~in_buck_mode =
  (* if we are the top-level process, then find the output directory and
     collect all crashcontext summaries under it in a single
     crashcontext.json file.
     Important: Note that when running under buck, this is not the final
     infer-out/ directory, but instead it is buck-out/, which contains the
     infer output directories for every buck target. *)
  let root_summaries_dir =
    if in_buck_mode then
      let buck_out = match Config.buck_out with Some dir -> dir | None -> "buck-out" in
      Config.project_root ^/ buck_out
    else Config.results_dir
  in
  collect_all_summaries root_summaries_dir Config.stacktrace Config.stacktraces_dir


let pp_stacktree fmt st = Format.pp_print_string fmt (Stacktree_j.string_of_stacktree st)
