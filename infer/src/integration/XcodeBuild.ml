(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let capture ~prog ~args =
  let apple_clang =
    Process.create_process_and_wait_with_output ~prog:"xcrun" ~args:["--find"; "clang"] ReadStdout
    |> String.strip
  in
  let xcode_version =
    Process.create_process_and_wait_with_output ~prog:"xcodebuild" ~args:["-version"] ReadStdout
  in
  let apple_clang_version =
    Process.create_process_and_wait_with_output ~prog:apple_clang ~args:["--version"] ReadStdout
  in
  L.environment_info "Xcode version: %s@." xcode_version ;
  L.environment_info "clang version: %s@." apple_clang_version ;
  let args =
    List.append args
      [ Printf.sprintf "CC=%s" Config.wrappers_dir ^/ "clang"
      ; Printf.sprintf "CPLUSPLUS=%s" Config.wrappers_dir ^/ "clang++"
      ; "GCC_PRECOMPILE_PREFIX_HEADER=NO" ]
  in
  let infer_args =
    Option.fold (Sys.getenv CommandLineOption.args_env_var)
      ~init:(Printf.sprintf "%s%c%s" "--fcp-apple-clang" CommandLineOption.env_var_sep apple_clang)
      ~f:(fun acc arg -> Printf.sprintf "%s%c%s" acc CommandLineOption.env_var_sep arg )
  in
  L.debug Capture Verbose "%s [%s] [%s]@." prog (String.concat ~sep:"," args) infer_args ;
  let {Unix.Process_info.stdin; stdout; stderr; pid} =
    Unix.create_process_env ~prog ~args
      ~env:(`Extend [(CommandLineOption.args_env_var, infer_args)])
      ()
  in
  let stdout_chan = Unix.in_channel_of_descr stdout in
  let stderr_chan = Unix.in_channel_of_descr stderr in
  Unix.close stdin ;
  Utils.with_channel_in stdout_chan ~f:(L.progress "XCODEBUILD: %s@.") ;
  Utils.with_channel_in stderr_chan ~f:(L.progress "XCODEBUILD: %s@.") ;
  match Unix.waitpid pid with
  | Ok () ->
      In_channel.close stdout_chan ;
      In_channel.close stderr_chan
  | Error _ as err ->
      L.die ExternalError "*** capture failed to execute: %s"
        (Unix.Exit_or_signal.to_string_hum err)
