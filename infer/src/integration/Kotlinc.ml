(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

let compile prog args =
  let cli_args, file_args =
    let args = "-d" :: Config.javac_classes_out :: args in
    List.partition_tf args ~f:(fun arg ->
        (* Similarly to javac, argument files must not contain certain arguments. *)
        String.is_prefix ~prefix:"-J" arg || String.is_prefix ~prefix:"@" arg )
  in
  (* Pass non-special args via a file to avoid exceeding the command line size limit. *)
  let args_file =
    let file = Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "kotlinc_args" "" in
    let quoted_file_args =
      List.map file_args ~f:(fun arg ->
          if String.contains arg '\'' then arg else F.sprintf "'%s'" arg )
    in
    Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc quoted_file_args) ;
    file
  in
  let args = cli_args @ ["@" ^ args_file] in
  L.debug Capture Quiet "Current working directory: '%s'@." (Sys.getcwd ()) ;
  let run cmd =
    let shell_cmd = List.map ~f:Escape.escape_shell cmd |> String.concat ~sep:" " in
    L.debug Capture Quiet "Trying to execute: %s@." shell_cmd ;
    match Utils.with_process_in shell_cmd In_channel.input_all with
    | log, Error err ->
        L.(die UserError)
          "@\n\
           *** Failed to execute compilation command: %s@\n\
           *** Command: %s@\n\
           *** Output:@\n\
           *** Infer needs a working compilation command to run.@."
          (Unix.Exit_or_signal.to_string_hum (Error err))
          shell_cmd log
    | log, Ok () ->
        L.(debug Capture Quiet) "*** Success. Logs:@\n%s" log
  in
  run (prog :: args)


let kotlin_sources_from_args args =
  let is_kotlin_source_file arg =
    let stripped_arg = String.strip ~drop:(fun char -> Char.equal char '\"') arg in
    String.is_suffix ~suffix:".kt" stripped_arg
  in
  let source_files = ref [] in
  List.iter args ~f:(fun arg ->
      (* Expand arg files *)
      match String.chop_prefix ~prefix:"@" arg with
      | None ->
          if is_kotlin_source_file arg then source_files := arg :: !source_files
      | Some arg_file ->
          List.iter (In_channel.read_lines arg_file) ~f:(fun arg ->
              if is_kotlin_source_file arg then source_files := arg :: !source_files ) ) ;
  !source_files


let capture ~prog ~args =
  compile prog args ;
  if not (InferCommand.equal Config.command Compile) then
    let sources = kotlin_sources_from_args args in
    JMain.from_arguments Config.javac_classes_out ~sources
