(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module CLOpt = CommandLineOption
module L = Logging

let capture_text =
  if Config.equal_analyzer Config.analyzer Config.Linters then "linting" else "translating"

let create_files_stack compilation_database should_capture_file =
  let stack = Stack.create () in
  let add_to_stack file _ = if should_capture_file file then Stack.push stack file in
  CompilationDatabase.iter compilation_database add_to_stack ; stack

let swap_command cmd =
  let plusplus = "++" in
  let clang = "clang" in
  let clangplusplus = "clang++" in
  if String.is_suffix ~suffix:plusplus cmd then Config.wrappers_dir ^/ clangplusplus
  else Config.wrappers_dir ^/ clang

let run_compilation_file compilation_database file =
  try
    let compilation_data = CompilationDatabase.find compilation_database file in
    let wrapper_cmd = swap_command compilation_data.command in
    let arg_file =
      ClangQuotes.mk_arg_file "cdb_clang_args_" ClangQuotes.EscapedNoQuotes [compilation_data.args]
    in
    let args = [("@" ^ arg_file)] in
    let env =
      `Extend
        [ ( CLOpt.args_env_var
          , String.concat ~sep:(String.of_char CLOpt.env_var_sep)
              (Option.to_list (Sys.getenv CLOpt.args_env_var) @ ["--fcp-syntax-only"]) ) ]
    in
    (Some compilation_data.dir, wrapper_cmd, args, env)
  with Not_found ->
    Process.print_error_and_exit "Failed to find compilation data for %a@\n%!" SourceFile.pp file

let run_compilation_database compilation_database should_capture_file =
  let number_of_files = CompilationDatabase.get_size compilation_database in
  L.(debug Capture Quiet) "Starting %s %d files@\n%!" capture_text number_of_files ;
  L.progress "Starting %s %d files@\n%!" capture_text number_of_files ;
  let jobs_stack = create_files_stack compilation_database should_capture_file in
  let capture_text_upper = String.capitalize capture_text in
  let job_to_string file = Format.asprintf "%s %a" capture_text_upper SourceFile.pp file in
  let fail_on_failed_job =
    if Config.linters_ignore_clang_failures then false
    else
      match Config.buck_compilation_database with
      | Some `NoDeps
       -> Config.clang_frontend_do_lint
      | _
       -> false
  in
  Process.run_jobs_in_parallel ~fail_on_failed_job jobs_stack
    (run_compilation_file compilation_database) job_to_string

(** Computes the compilation database files. *)
let get_compilation_database_files_buck ~prog ~args =
  match Buck.add_flavors_to_buck_command args with
  | build :: args_with_flavor
   -> (
      let build_args = build :: "--config" :: "*//cxx.pch_enabled=false" :: args_with_flavor in
      Process.create_process_and_wait ~prog ~args:build_args ;
      (* The option --keep-going is not accepted in the command buck targets *)
      let args_with_flavor_no_keep_going =
        List.filter ~f:(fun s -> not (String.equal s "--keep-going")) args_with_flavor
      in
      let buck_targets_shell =
        prog :: "targets" :: "--show-output" :: args_with_flavor_no_keep_going
        |> Utils.shell_escape_command
      in
      let output, exit_or_signal =
        Utils.with_process_in buck_targets_shell In_channel.input_lines
      in
      match exit_or_signal with
      | Error _ as status
       -> failwithf "*** command failed:@\n*** %s@\n*** %s@." buck_targets_shell
            (Unix.Exit_or_signal.to_string_hum status)
      | Ok () ->
        match output with
        | []
         -> L.external_error "There are no files to process, exiting@." ; exit 0
        | lines
         -> L.(debug Capture Quiet)
              "Reading compilation database from:@\n%s@\n" (String.concat ~sep:"\n" lines) ;
            (* this assumes that flavors do not contain spaces *)
            let split_regex = Str.regexp "#[^ ]* " in
            let scan_output compilation_database_files line =
              match Str.bounded_split split_regex line 2 with
              | [_; filename]
               -> `Raw filename :: compilation_database_files
              | _
               -> failwithf "Failed to parse `buck targets --show-output ...` line of output:@\n%s"
                    line
            in
            List.fold ~f:scan_output ~init:[] lines )
  | _
   -> let cmd = String.concat ~sep:" " (prog :: args) in
      Process.print_error_and_exit "Incorrect buck command: %s. Please use buck build <targets>"
        cmd

(** Compute the compilation database files. *)
let get_compilation_database_files_xcodebuild ~prog ~args =
  let tmp_file = Filename.temp_file "cdb" ".json" in
  let xcodebuild_prog, xcodebuild_args = (prog, prog :: args) in
  let xcpretty_prog = "xcpretty" in
  let xcpretty_args =
    [xcpretty_prog; "--report"; "json-compilation-database"; "--output"; tmp_file]
  in
  L.(debug Capture Quiet)
    "Running %s | %s@\n@." (List.to_string ~f:Fn.id xcodebuild_args)
    (List.to_string ~f:Fn.id xcpretty_args) ;
  let producer_status, consumer_status =
    Process.pipeline ~producer_prog:xcodebuild_prog ~producer_args:xcodebuild_args
      ~consumer_prog:xcpretty_prog ~consumer_args:xcpretty_args
  in
  match (producer_status, consumer_status) with
  | Ok (), Ok ()
   -> [`Escaped tmp_file]
  | _
   -> L.(die ExternalError) "There was an error executing the build command"

let capture_files_in_database ~changed_files compilation_database =
  let filter_changed =
    match changed_files with
    | None
     -> fun _ -> true
    | Some changed_files_set
     -> fun source_file -> SourceFile.Set.mem source_file changed_files_set
  in
  run_compilation_database compilation_database filter_changed

let capture_file_in_database compilation_database source_file =
  run_compilation_database compilation_database (SourceFile.equal source_file)
