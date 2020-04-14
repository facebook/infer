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
        L.internal_error "Could not parse target json: %s" (Yojson.Basic.to_string json) ;
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


let infer_deps_of_build_report build_report =
  match read_and_parse_report build_report with
  | None ->
      L.die InternalError "Couldn't parse buck build report: %s@." build_report
  | Some target_path_list ->
      let out_line out_channel (target, target_output_path) =
        Printf.fprintf out_channel "%s\t-\t%s\n" target (Config.project_root ^/ target_output_path)
      in
      let infer_deps = Config.(results_dir ^/ buck_infer_deps_file_name) in
      Utils.with_file_out infer_deps ~f:(fun out_channel ->
          List.iter target_path_list ~f:(out_line out_channel) )


let run_buck_capture cmd =
  let shell_cmd =
    List.map ~f:Escape.escape_shell cmd
    |> String.concat ~sep:" "
    |> fun cmd -> Printf.sprintf "%s 2>&1" cmd
  in
  let path_var = "PATH" in
  let new_path =
    Sys.getenv path_var
    |> Option.value_map ~default:Config.bin_dir ~f:(fun old_path -> Config.bin_dir ^ ":" ^ old_path)
  in
  let env = `Extend [(path_var, new_path)] in
  let ({stdin; stdout; stderr; pid} : Unix.Process_info.t) =
    Unix.create_process_env ~prog:"sh" ~args:["-c"; shell_cmd] ~env ()
  in
  let buck_stdout = Unix.in_channel_of_descr stdout in
  Utils.with_channel_in buck_stdout ~f:(L.progress "BUCK: %s@.") ;
  Unix.close stdin ;
  Unix.close stderr ;
  In_channel.close buck_stdout ;
  match Unix.waitpid pid with
  | Ok () ->
      ()
  | Error _ as err ->
      L.(die ExternalError)
        "*** Buck genrule capture failed to execute: %s@\n***@."
        (Unix.Exit_or_signal.to_string_hum err)


let capture build_cmd =
  let prog, buck_cmd = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  L.progress "Querying buck for genrule capture targets...@." ;
  let time0 = Mtime_clock.counter () in
  let command, args, targets =
    Buck.parse_command_and_targets JavaGenruleMaster ~filter_kind:`Yes buck_cmd
  in
  L.progress "Found %d genrule capture targets in %a.@." (List.length targets) Mtime.Span.pp
    (Mtime_clock.count time0) ;
  let all_args = List.rev_append args targets in
  let build_report_file = Filename.temp_file ~in_dir:Config.temp_dir "buck_build_report" ".json" in
  let updated_buck_cmd =
    (* make buck tell us where in buck-out are the capture directories for merging *)
    (prog :: command :: "--build-report" :: build_report_file :: Buck.buck_config JavaGenruleMaster)
    @ List.rev_append Config.buck_build_args_no_inline (Buck.store_args_in_file all_args)
  in
  L.(debug Capture Quiet)
    "Processed buck command '%a'@." (Pp.seq F.pp_print_string) updated_buck_cmd ;
  if List.is_empty targets then L.external_warning "WARNING: found no buck targets to analyze.@."
  else
    let time0 = Mtime_clock.counter () in
    run_buck_capture updated_buck_cmd ;
    infer_deps_of_build_report build_report_file ;
    L.progress "Genrule capture took %a.@." Mtime.Span.pp (Mtime_clock.count time0) ;
    RunState.set_merge_capture true ;
    RunState.store ()
