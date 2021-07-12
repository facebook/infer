(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let capture ~build_cmd =
  let raw_prog, ndk_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
  (* env vars *)
  let path_opt = Sys.getenv "PATH" in
  let new_env =
    [ ("INFER_OLD_PATH", Option.value path_opt ~default:"")
    ; ("PATH", Option.fold path_opt ~init:Config.wrappers_dir ~f:(Printf.sprintf "%s:%s"))
    ; ("INFER_RESULTS_DIR", Config.results_dir) ]
  in
  (* args *)
  let raw_prog_name = Filename.basename raw_prog in
  let prog =
    if List.exists Config.clang_exe_aliases ~f:(String.equal raw_prog_name) then raw_prog_name
    else raw_prog
  in
  let args =
    "NDK_TOOLCHAIN_VERSION=clang" :: "TARGET_CC=clang" :: "TARGET_CXX=clang" :: "TARGET_LD=ld"
    :: ndk_args
  in
  (* run ndk_build *)
  L.(debug Capture Verbose)
    "Running command %s %s with env %s@." prog (String.concat ~sep:" " args)
    (List.to_string ~f:(fun (a, b) -> a ^ "=" ^ b) new_env) ;
  let {Unix.Process_info.stdin; stdout; stderr; pid} =
    Unix.create_process_env ~prog ~args ~env:(`Extend new_env) ()
  in
  let ndk_stderr = Unix.in_channel_of_descr stderr in
  Utils.with_channel_in ndk_stderr ~f:(L.progress "NDKBUILD: %s@.") ;
  Unix.close stdin ;
  Unix.close stdout ;
  In_channel.close ndk_stderr ;
  match Unix.waitpid pid with
  | Ok () ->
      ()
  | Error _ as err ->
      L.die ExternalError "ndkbuild capture failed to execute: %s"
        (Unix.Exit_or_signal.to_string_hum err)
        ()
