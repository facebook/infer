(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

type compiler = Java | Javac [@@deriving compare]

let compile compiler build_prog build_args =
  let prog, prog_args =
    match (compiler, Config.java_jar_compiler) with
    | _, None ->
        (build_prog, ["-J-Duser.language=en"])
    | Java, Some jar ->
        (build_prog, ["-jar"; jar])
    | _, Some jar ->
        (* fall back to java in PATH to avoid passing -jar to javac *)
        ("java", ["-jar"; jar])
  in
  let cli_args, file_args =
    let args = "-verbose" :: "-g" :: "-d" :: Config.javac_classes_out :: build_args in
    List.partition_tf args ~f:(fun arg ->
        (* As mandated by javac, argument files must not contain certain arguments. *)
        String.is_prefix ~prefix:"-J" arg || String.is_prefix ~prefix:"@" arg )
  in
  (* Pass non-special args via a file to avoid exceeding the command line size limit. *)
  let args_file =
    let file = Filename.temp_file "args_" "" in
    let quoted_file_args =
      List.map file_args ~f:(fun arg ->
          if String.contains arg '\'' then arg else F.sprintf "'%s'" arg )
    in
    Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc quoted_file_args) ;
    file
  in
  let cli_file_args = cli_args @ ["@" ^ args_file] in
  let args = prog_args @ cli_file_args in
  L.(debug Capture Quiet) "Current working directory: '%s'@." (Sys.getcwd ()) ;
  let verbose_out_file = Filename.temp_file "javac" ".out" in
  let try_run cmd error_k =
    let shell_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
    let shell_cmd_redirected = Printf.sprintf "%s 2>'%s'" shell_cmd verbose_out_file in
    L.(debug Capture Quiet) "Trying to execute: %s@." shell_cmd_redirected ;
    match Utils.with_process_in shell_cmd_redirected In_channel.input_all with
    | log, Error err -> (
      match error_k with
      | Some k ->
          L.(debug Capture Quiet)
            "*** Failed: %s!@\n%s@."
            (Unix.Exit_or_signal.to_string_hum (Error err))
            log ;
          k ()
      | None ->
          let verbose_errlog = Utils.with_file_in verbose_out_file ~f:In_channel.input_all in
          L.(die UserError)
            "@\n\
             *** Failed to execute compilation command: %s@\n\
             *** Command: %s@\n\
             *** Output:@\n\
             %s%s@\n\
             *** Infer needs a working compilation command to run.@."
            (Unix.Exit_or_signal.to_string_hum (Error err))
            shell_cmd log verbose_errlog )
    | exception exn ->
        IExn.reraise_if exn ~f:(fun () ->
            match error_k with
            | Some k ->
                L.(debug Capture Quiet) "*** Failed: %a!@\n" Exn.pp exn ;
                k () ;
                false
            | None ->
                true )
    | log, Ok () ->
        L.(debug Capture Quiet) "*** Success. Logs:@\n%s" log
  in
  let fallback () = try_run ("javac" :: cli_file_args) None in
  try_run (prog :: args) (Some fallback) ;
  verbose_out_file


let no_source_file args =
  let not_source_file arg =
    let stripped_arg = String.strip ~drop:(fun char -> Char.equal char '\"') arg in
    not (String.is_suffix ~suffix:".java" stripped_arg)
  in
  List.for_all args ~f:(fun arg ->
      (* expand arg files *)
      match String.chop_prefix ~prefix:"@" arg with
      | None ->
          not_source_file arg
      | Some arg_file ->
          List.for_all ~f:not_source_file (In_channel.read_lines arg_file) )


let capture compiler ~prog ~args =
  match (compiler, Config.capture_blacklist) with
  (* Simulates Buck support for compilation commands with no source file *)
  | _ when Config.buck_cache_mode && no_source_file args ->
      ()
  | Javac, Some blacklist
    when let re = Str.regexp blacklist in
         List.exists ~f:(fun arg -> Str.string_match re arg 0) args ->
      ()
  | _ ->
      let verbose_out_file = compile compiler prog args in
      if not (InferCommand.equal Config.command Compile) then
        JMain.from_verbose_out verbose_out_file ;
      if not Config.debug_mode then Unix.unlink verbose_out_file
