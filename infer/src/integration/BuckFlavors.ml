(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CLOpt = CommandLineOption
module L = Logging
module F = Format

type flavored_arguments = {command: string; rev_not_targets: string list; targets: string list}

(* buck1 only *)
let add_flavors_to_buck_arguments buck_mode ~extra_flavors original_buck_args =
  let command, rev_not_targets, targets =
    Buck.parse_command_and_targets buck_mode V1 original_buck_args
  in
  let targets =
    List.rev_map targets ~f:(fun t ->
        Buck.Target.(
          t |> of_string |> add_flavor_v1 ~extra_flavors buck_mode Config.command |> to_string ) )
  in
  {command; rev_not_targets; targets}


let capture_buck_args build_report_file =
  ("--build-report" :: build_report_file :: (if Config.keep_going then ["--keep-going"] else []))
  @ (match Config.load_average with Some l -> ["-L"; Float.to_string l] | None -> [])
  @ Buck.config Clang V1 @ Config.buck_build_args


let run_buck_build prog buck_build_args =
  L.debug Capture Verbose "%s %s@." prog (List.to_string ~f:Fn.id buck_build_args) ;
  let infer_args =
    Option.fold (Sys.getenv CommandLineOption.args_env_var) ~init:"--fcp-syntax-only"
      ~f:(fun acc arg -> Printf.sprintf "%s%c%s" acc CommandLineOption.env_var_sep arg )
  in
  let extend_env = [(CommandLineOption.args_env_var, infer_args)] in
  Buck.wrap_buck_call ~extend_env ~label:"build" V1 (prog :: buck_build_args) |> ignore


let get_all_infer_deps_under_buck_out () =
  Utils.fold_folders ~init:[] ~path:(Config.project_root ^/ Config.buck_out) ~f:(fun acc dir ->
      if
        String.is_substring dir ~substring:"infer-out"
        && ISys.file_exists (ResultsDirEntryName.get_path ~results_dir:dir AnalysisDB)
      then Printf.sprintf "\t\t%s" dir :: acc
      else acc )
  |> List.dedup_and_sort ~compare:String.compare


let capture build_cmd =
  let prog, buck_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  (* let children infer processes know that they are inside Buck *)
  let infer_args_with_buck =
    String.concat
      ~sep:(String.of_char CLOpt.env_var_sep)
      (Option.to_list (Sys.getenv CLOpt.args_env_var) @ ["--buck"])
  in
  Unix.putenv ~key:CLOpt.args_env_var ~data:infer_args_with_buck ;
  let {command; rev_not_targets; targets} =
    add_flavors_to_buck_arguments Clang ~extra_flavors:[] buck_args
  in
  let infer_deps_lines =
    if List.is_empty targets then []
    else
      let all_args = List.rev_append rev_not_targets targets in
      let updated_buck_cmd =
        command
        :: ( Config.buck_build_args_no_inline
           @ Buck.store_args_in_file ~identifier:"clang_flavor_build" all_args )
      in
      L.debug Capture Quiet "Processed buck command '%a'@\n" (Pp.seq F.pp_print_string)
        updated_buck_cmd ;
      let prog, buck_build_cmd = (prog, updated_buck_cmd) in
      if Config.keep_going && not Config.continue_capture then
        Process.create_process_and_wait ~prog ~args:["clean"] ;
      let build_report_file =
        Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "buck_build_report" ".json"
      in
      run_buck_build prog (buck_build_cmd @ capture_buck_args build_report_file) ;
      if Config.buck_merge_all_deps then get_all_infer_deps_under_buck_out ()
      else BuckBuildReport.parse_infer_deps ~root:Config.project_root ~build_report_file
  in
  let infer_deps = ResultsDir.get_path CaptureDependencies in
  Utils.with_file_out infer_deps ~f:(fun out_channel ->
      Out_channel.output_lines out_channel infer_deps_lines )
