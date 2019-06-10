(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let write_infer_deps infile =
  let out_line out_channel line =
    match String.split ~on:' ' line with
    | [target; target_output_path] ->
        Printf.fprintf out_channel "%s\t-\t%s\n" target (Config.project_root ^/ target_output_path)
    | _ ->
        L.die ExternalError "Couldn't parse buck target output: %s@." line
  in
  let infer_deps = Config.(results_dir ^/ buck_infer_deps_file_name) in
  Utils.with_file_out infer_deps ~f:(fun out_channel ->
      Utils.with_file_in infile ~f:(In_channel.iter_lines ~f:(out_line out_channel)) )


let run_buck_capture cmd =
  let buck_output_file = Filename.temp_file "buck_output" ".log" in
  let shell_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
  let shell_cmd_redirected = Printf.sprintf "%s >'%s'" shell_cmd buck_output_file in
  match Utils.with_process_in shell_cmd_redirected Utils.consume_in |> snd with
  | Ok () ->
      write_infer_deps buck_output_file ; Unix.unlink buck_output_file
  | Error _ as err ->
      L.(die ExternalError)
        "*** Buck genrule capture failed to execute: %s@\n***@."
        (Unix.Exit_or_signal.to_string_hum err)


let capture build_cmd =
  let prog, buck_cmd = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  L.progress "Querying buck for genrule capture targets...@." ;
  let time0 = Mtime_clock.counter () in
  let command, args, targets =
    Buck.parse_command_and_targets ~filter_kind:`Yes ~dep_depth:(Some None) buck_cmd
  in
  L.progress "Found %d genrule capture targets in %a.@." (List.length targets) Mtime.Span.pp
    (Mtime_clock.count time0) ;
  let all_args = List.rev_append args targets in
  let updated_buck_cmd =
    (* make buck tell us where in buck-out are the capture directories for merging *)
    (prog :: command :: "--show-output" :: Lazy.force Buck.buck_config)
    @ List.rev_append Config.buck_build_args_no_inline (Buck.store_args_in_file all_args)
  in
  L.(debug Capture Quiet)
    "Processed buck command '%a'@." (Pp.seq F.pp_print_string) updated_buck_cmd ;
  let time0 = Mtime_clock.counter () in
  run_buck_capture updated_buck_cmd ;
  L.progress "Genrule capture took %a.@." Mtime.Span.pp (Mtime_clock.count time0) ;
  RunState.set_merge_capture true ;
  RunState.store ()
