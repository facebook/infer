(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(* example build report json output
   [
     {
     "success" : true,
     "results" : {
       "//annotations:annotations_infer" : {
         "success" : true,
         "type" : "BUILT_LOCALLY",
         "output" : "buck-out/gen/annotations/annotations_infer/infer_out"
       },
       "//module2:module2_infer" : {
         "success" : true,
         "type" : "BUILT_LOCALLY",
         "output" : "buck-out/gen/module2/module2_infer/infer_out"
       },
       "//module1:module1_infer" : {
         "success" : true,
         "type" : "BUILT_LOCALLY",
         "output" : "buck-out/gen/module1/module1_infer/infer_out"
       },
       "//module3:module1_infer" : {
         "success" : "SUCCESS",
         "type" : "BUILT_LOCALLY",
         "outputs" : {
           "DEFAULT" : [ "buck-out/gen/module1/module3_infer/infer_out" ]
         }
       }
     },
     "failures" : { }
   }%
   ]
*)

(** Read the build report json file buck produced, and parse into a list of pairs
    [(target, output-path)]. NB contrary to what buck documentation says, the output path is always
    present even when the target is locally cached. *)
let read_and_parse_report build_report =
  let get_json_field fieldname = function
    | `Assoc fields ->
        List.Assoc.find fields ~equal:String.equal fieldname
    | _ ->
        None
  in
  let parse_target (target, json) =
    let path_opt =
      match get_json_field "output" json with
      | Some (`String str) ->
          Some str
      | _ -> (
        match get_json_field "outputs" json |> Option.bind ~f:(get_json_field "DEFAULT") with
        | Some (`List [`String str]) ->
            Some str
        | _ ->
            None )
    in
    match path_opt with
    | None ->
        L.debug Capture Quiet "Could not parse target json: %s@." (Yojson.Basic.to_string json) ;
        None
    | Some path ->
        Some (target, path)
  in
  let parse_results = function
    | `Assoc results ->
        (* NB this will simply skip unparseable targets *)
        List.filter_map results ~f:parse_target |> Option.some
    | _ ->
        None
  in
  Yojson.Basic.from_file build_report |> get_json_field "results" |> Option.bind ~f:parse_results


(** Function for processing paths in a buck build report and generating an [infer-deps.txt] file.
    Given a pair [(buck_target, output_path)],

    - if [output_path] contains a capture DB, then generate the appropriate deps line;
    - if [output_path] contains an [infer-deps.txt] file, expand and inline it;
    - if [output_path] is a dummy target used in the combined genrule integration for clang targets,
      read its contents, parse them as an output directory path and apply the above two tests to
      that *)
let expand_target ~root acc (target, target_path) =
  let inline acc path =
    Utils.with_file_in path ~f:(In_channel.fold_lines ~init:acc ~f:(fun acc line -> line :: acc))
  in
  let expand_dir acc (target, target_path) =
    (* invariant: [target_path] is absolute *)
    let db_file = ResultsDirEntryName.get_path ~results_dir:target_path CaptureDB in
    if ISys.file_exists db_file then
      (* we found a capture DB so add this as a target line *)
      Printf.sprintf "%s\t-\t%s" target target_path :: acc
    else
      let infer_deps_file =
        ResultsDirEntryName.get_path ~results_dir:target_path CaptureDependencies
      in
      if ISys.file_exists infer_deps_file then
        (* we found an [infer_deps.txt] file so inline in *)
        inline acc infer_deps_file
      else (
        (* don't know what to do with this directory *)
        L.internal_error "Didn't find capture DB or infer-deps file in path %s.@\n" target_path ;
        acc )
  in
  let target_path = if Filename.is_absolute target_path then target_path else root ^/ target_path in
  match Sys.is_directory target_path with
  | `Yes ->
      expand_dir acc (target, target_path)
  | _ when String.is_suffix target_path ~suffix:ResultsDirEntryName.infer_deps_file_name ->
      (* direct path of an [infer-deps.txt] file, inline *)
      inline acc target_path
  | _ -> (
    (* assume path is an intermediate genrule output containing the
       output path of the underlying capture target *)
    match Utils.read_file target_path with
    | Ok [new_target_path] ->
        expand_dir acc (target, new_target_path)
    | Ok _ ->
        L.internal_error "Couldn't parse intermediate deps file %s@." target_path ;
        acc
    | Error error ->
        L.internal_error "Error %s@\nCouldn't read intermediate deps file %s@." error target_path ;
        acc )


let parse_infer_deps ~root ~build_report_file =
  match read_and_parse_report build_report_file with
  | None ->
      L.die InternalError "Couldn't parse buck build report: %s@." build_report_file
  | Some target_path_list ->
      List.fold target_path_list ~init:[] ~f:(expand_target ~root)
      |> List.dedup_and_sort ~compare:String.compare
