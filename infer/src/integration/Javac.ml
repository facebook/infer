(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    let file = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "javac_args" "" in
    let quoted_file_args =
      List.map file_args ~f:(fun arg ->
          if String.contains arg '\'' then arg
          else
            (* javac reads a backslash in a quoted string as an escape character *)
            F.sprintf "'%s'" (String.substr_replace_all arg ~pattern:"\\" ~with_:"\\\\") )
    in
    Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc quoted_file_args) ;
    file
  in
  let cli_file_args = cli_args @ ["@" ^ args_file] in
  let args = prog_args @ cli_file_args in
  L.(debug Capture Quiet) "Current working directory: '%s'@." (Stdlib.Sys.getcwd ()) ;
  let verbose_out_file =
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "javac" ".out"
  in
  let run_and_capture_stdout cmd =
    let stdout_read, stdout_write = Unix.pipe () in
    let stdout_in = Unix.in_channel_of_descr stdout_read in
    let f () =
      let pid =
        let stderr_fd =
          IUnix.openfile verbose_out_file ~mode:[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o600
        in
        Exception.try_finally
          ~f:(fun () ->
            UnixLabels.create_process ~prog:(List.hd_exn cmd) ~args:(Array.of_list cmd)
              ~stdin:Unix.stdin ~stdout:stdout_write ~stderr:stderr_fd )
          ~finally:(fun () ->
            (* the read below only ends once every write end is closed *)
            Unix.close stdout_write ;
            Unix.close stderr_fd )
      in
      let log = In_channel.input_all stdout_in in
      (IUnix.waitpid (IUnix.Pid.of_int pid), log)
    in
    Exception.try_finally ~f ~finally:(fun () -> In_channel.close stdout_in)
  in
  let try_run cmd error_k =
    L.(debug Capture Quiet) "Trying to execute: %a@." Pp.cli_args cmd ;
    match run_and_capture_stdout cmd with
    | Error err, log -> (
      match error_k with
      | Some k ->
          L.(debug Capture Quiet)
            "*** Failed: %s!@\n%s@."
            (IUnix.Exit_or_signal.to_string_hum (Error err))
            log ;
          k ()
      | None ->
          let verbose_errlog = Utils.with_file_in verbose_out_file ~f:In_channel.input_all in
          L.(die UserError)
            "@\n\
             *** Failed to execute compilation command: %s@\n\
             *** Command: %a@\n\
             *** Output:@\n\
             %s%s@\n\
             *** Infer needs a working compilation command to run.@."
            (IUnix.Exit_or_signal.to_string_hum (Error err))
            Pp.cli_args cmd log verbose_errlog )
    | exception exn ->
        IExn.reraise_if exn ~f:(fun () ->
            match error_k with
            | Some k ->
                L.(debug Capture Quiet) "*** Failed: %a!@\n" Exn.pp exn ;
                k () ;
                false
            | None ->
                true )
    | Ok (), log ->
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


let call_infer_javac_capture ~javac_args =
  let prog = Config.infer_binary in
  let args =
    "capture" :: "--continue" :: "--" :: "javac"
    :: List.filter_map javac_args ~f:(fun arg ->
        if String.equal "-Werror" arg then None
        else if String.is_substring arg ~substring:"-g:" then Some "-g"
        else Some arg )
  in
  L.debug Capture Verbose "%s %s@." prog (String.concat ~sep:" " args) ;
  Process.create_process_and_wait ~prog ~args ()


let capture compiler ~prog ~args =
  (* Simulates Buck support for compilation commands with no source file *)
  if not (Config.buck_cache_mode && no_source_file args) then (
    let verbose_out_file = compile compiler prog args in
    if not (InferCommand.equal Config.command Compile) then JMain.from_verbose_out verbose_out_file ;
    if not Config.debug_mode then Unix.unlink verbose_out_file )
