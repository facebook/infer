(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
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
        L.internal_error "Could not parse target json: %s@." (Yojson.Basic.to_string json) ;
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
let expand_target acc (target, target_path) =
  let expand_dir acc (target, target_path) =
    (* invariant: [target_path] is absolute *)
    let db_file = ResultsDirEntryName.get_path ~results_dir:target_path CaptureDB in
    match Sys.file_exists db_file with
    | `Yes ->
        (* there is a capture DB at this path, so terminate expansion and generate deps line *)
        let line = Printf.sprintf "%s\t-\t%s" target target_path in
        line :: acc
    | `No | `Unknown -> (
        (* no capture DB was found, so look for, and inline, an [infer-deps.txt] file *)
        let infer_deps =
          ResultsDirEntryName.get_path ~results_dir:target_path CaptureDependencies
        in
        match Sys.file_exists infer_deps with
        | `Yes ->
            Utils.with_file_in infer_deps
              ~f:(In_channel.fold_lines ~init:acc ~f:(fun acc line -> line :: acc))
        | `No | `Unknown ->
            L.internal_error "No capture DB or infer-deps file in %s@." target_path ;
            acc )
  in
  let target_path =
    if Filename.is_absolute target_path then target_path else Config.project_root ^/ target_path
  in
  match Sys.is_directory target_path with
  | `Yes ->
      (* output path is directory, so should contain either a capture DB or an [infer-deps.txt] file *)
      expand_dir acc (target, target_path)
  | `No | `Unknown -> (
    (* output path is not a directory, so assume it's an intermediate genrule output containing the
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


let infer_deps_of_build_report build_report =
  match read_and_parse_report build_report with
  | None ->
      L.die InternalError "Couldn't parse buck build report: %s@." build_report
  | Some target_path_list ->
      let infer_deps_lines =
        List.fold target_path_list ~init:[] ~f:expand_target
        |> List.dedup_and_sort ~compare:String.compare
      in
      let infer_deps = ResultsDir.get_path CaptureDependencies in
      Utils.with_file_out infer_deps ~f:(fun out_channel ->
          Out_channel.output_lines out_channel infer_deps_lines )


let capture build_cmd =
  let prog, buck_cmd = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  L.progress "Querying buck for java flavor capture targets...@." ;
  let time0 = Mtime_clock.counter () in
  let BuckFlavors.{command; rev_not_targets; targets} =
    BuckFlavors.add_flavors_to_buck_arguments JavaFlavor ~extra_flavors:[] buck_cmd
  in
  L.progress "Found %d java flavor capture targets in %a.@." (List.length targets) Mtime.Span.pp
    (Mtime_clock.count time0) ;
  let all_args = List.rev_append rev_not_targets targets in
  let build_report_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "buck_build_report" ".json"
  in
  let updated_buck_cmd =
    (* make buck tell us where in buck-out are the capture directories for merging *)
    (prog :: command :: "--build-report" :: build_report_file :: Buck.config JavaFlavor)
    @ Config.buck_build_args_no_inline
    @ Buck.store_args_in_file ~identifier:"java_flavor_build" all_args
  in
  L.(debug Capture Quiet)
    "Processed buck command '%a'@." (Pp.seq F.pp_print_string) updated_buck_cmd ;
  if List.is_empty targets then L.external_warning "WARNING: found no buck targets to analyze.@."
  else
    let time0 = Mtime_clock.counter () in
    Buck.wrap_buck_call ~label:"build" updated_buck_cmd |> ignore ;
    infer_deps_of_build_report build_report_file ;
    L.progress "Java flavor capture took %a.@." Mtime.Span.pp (Mtime_clock.count time0) ;
    ResultsDir.RunState.set_merge_capture true ;
    ()
