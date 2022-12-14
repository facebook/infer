(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let capture_buck2_args build_report_file =
  ("--build-report" :: build_report_file :: (if Config.keep_going then ["--keep-going"] else []))
  @ Config.buck2_build_args


let run_buck2_build prog buck2_build_args =
  L.debug Capture Verbose "%s %s@." prog (List.to_string ~f:Fn.id buck2_build_args) ;
  let infer_args =
    Option.fold (Sys.getenv CommandLineOption.args_env_var) ~init:"--fcp-syntax-only"
      ~f:(fun acc arg -> Printf.sprintf "%s%c%s" acc CommandLineOption.env_var_sep arg)
  in
  let extend_env = [(CommandLineOption.args_env_var, infer_args)] in
  Buck.wrap_buck_call ~extend_env V2 ~label:"build" (prog :: buck2_build_args) |> ignore


let capture build_cmd =
  let prog, buck2_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  let command, rev_not_targets, targets = Buck.parse_command_and_targets Clang V2 buck2_args in
  let targets =
    List.rev_map targets ~f:(fun target_and_platform ->
        String.split ~on:' ' target_and_platform
        |> List.hd
        |> Option.value ~default:target_and_platform )
  in
  if not (List.is_empty targets) then (
    let all_args = List.rev_append rev_not_targets targets in
    let buck2_build_cmd =
      command
      :: ( Config.buck2_build_args_no_inline
         @ Buck.store_args_in_file ~identifier:"clang_buck2_build" all_args )
    in
    L.debug Capture Quiet "Processed buck2 command '%a'@\n" (Pp.seq F.pp_print_string)
      buck2_build_cmd ;
    ResultsDir.RunState.set_merge_capture true ;
    if Config.keep_going && not Config.continue_capture then
      Process.create_process_and_wait ~prog ~args:["clean"] ;
    let build_report_file =
      Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "buck2_build_report" ".json"
    in
    run_buck2_build prog (buck2_build_cmd @ capture_buck2_args build_report_file) ;
    let infer_deps_lines =
      BuckBuildReport.parse_infer_deps ~root:Config.buck2_root ~build_report_file
    in
    let infer_deps = ResultsDir.get_path CaptureDependencies in
    Utils.with_file_out infer_deps ~f:(fun out_channel ->
        Out_channel.output_lines out_channel infer_deps_lines ) )
