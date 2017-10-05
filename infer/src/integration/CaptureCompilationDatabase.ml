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

type cmd = {cwd: string; prog: string; args: string}

let create_cmd (compilation_data: CompilationDatabase.compilation_data) =
  let swap_command cmd =
    if String.is_suffix ~suffix:"++" cmd then Config.wrappers_dir ^/ "clang++"
    else Config.wrappers_dir ^/ "clang"
  in
  let arg_file =
    ClangQuotes.mk_arg_file "cdb_clang_args_" ClangQuotes.EscapedNoQuotes [compilation_data.args]
  in
  {cwd= compilation_data.dir; prog= swap_command compilation_data.command; args= arg_file}

(* A sentinel is a file which indicates that a failure occurred in another infer process.
   Because infer processes run in parallel but do not share any memory, we use the
   filesystem to signal failures across processes. *)
let sentinel_exists sentinel_opt =
  let file_exists sentinel = PVariant.( = ) (Sys.file_exists sentinel) `Yes in
  Option.value_map ~default:false sentinel_opt ~f:file_exists

let invoke_cmd ~fail_sentinel cmd =
  let create_sentinel_if_needed () =
    let create_empty_file fname = Utils.with_file_out ~f:(fun _ -> ()) fname in
    Option.iter fail_sentinel ~f:create_empty_file
  in
  if sentinel_exists fail_sentinel then L.progress "E%!"
  else
    try
      let pid =
        let prog = cmd.prog in
        let argv = [prog; ("@" ^ cmd.args); "-fsyntax-only"] in
        Spawn.(spawn ~cwd:(Path cmd.cwd) ~prog ~argv ())
      in
      match Unix.waitpid (Pid.of_int pid) with
      | Ok ()
       -> L.progress ".%!"
      | Error _
       -> L.progress "!%!" ; create_sentinel_if_needed ()
    with exn ->
      let trace = Printexc.get_backtrace () in
      L.external_error "@\nException caught:@\n%a.@\n%s@\n" Exn.pp exn trace ;
      L.progress "X%!" ;
      create_sentinel_if_needed ()

let run_compilation_database compilation_database should_capture_file =
  let compilation_data =
    CompilationDatabase.filter_compilation_data compilation_database ~f:should_capture_file
  in
  let number_of_jobs = List.length compilation_data in
  let capture_text =
    if Config.equal_analyzer Config.analyzer Config.Linters then "linting" else "translating"
  in
  L.(debug Capture Quiet) "Starting %s %d files@\n%!" capture_text number_of_jobs ;
  L.progress "Starting %s %d files@\n%!" capture_text number_of_jobs ;
  let sequence = Parmap.L (List.map ~f:create_cmd compilation_data) in
  let fail_sentinel_fname = Config.results_dir ^/ Config.linters_failed_sentinel_filename in
  let fail_sentinel =
    if Config.linters_ignore_clang_failures then None
    else
      match Config.buck_compilation_database with
      | Some NoDeps when Config.clang_frontend_do_lint
       -> Some fail_sentinel_fname
      | Some NoDeps | Some Deps _ | None
       -> None
  in
  Utils.rmtree fail_sentinel_fname ;
  let chunksize = min (List.length compilation_data / Config.jobs + 1) 10 in
  Parmap.pariter ~ncores:Config.jobs ~chunksize (invoke_cmd ~fail_sentinel) sequence ;
  L.progress "@." ;
  L.(debug Analysis Medium) "Ran %d jobs" number_of_jobs ;
  if sentinel_exists fail_sentinel then (
    L.progress
      "Failure detected, capture did not finish successfully. Use `--linters-ignore-clang-failures` to ignore compilation errors. Terminating@." ;
    L.exit 1 )

(** Computes the compilation database files. *)
let get_compilation_database_files_buck ~prog ~args =
  let all_buck_args = Buck.inline_argument_files args in
  let targets, no_targets = List.partition_tf ~f:Buck.is_target_string all_buck_args in
  let targets =
    match Config.buck_compilation_database with
    | Some Deps depth
     -> Buck.get_dependency_targets_and_add_flavors targets ~depth
    | _
     -> Buck.add_flavors_to_buck_command targets
  in
  match no_targets with
  | "build" :: no_targets_no_build
   -> (
      let targets_in_file = Buck.store_targets_in_file targets in
      let build_args = no_targets @ ["--config"; "*//cxx.pch_enabled=false"; targets_in_file] in
      Logging.(debug Linters Quiet)
        "Processed buck command is : 'buck %s'@\n" (String.concat ~sep:" " build_args) ;
      Process.create_process_and_wait ~prog ~args:build_args ;
      let buck_targets_shell =
        List.append [prog; "targets"; "--show-output"; targets_in_file] no_targets_no_build
        |> Utils.shell_escape_command
      in
      let output, exit_or_signal =
        Utils.with_process_in buck_targets_shell In_channel.input_lines
      in
      match exit_or_signal with
      | Error _ as status
       -> L.(die ExternalError)
            "*** command failed:@\n*** %s@\n*** %s@." buck_targets_shell
            (Unix.Exit_or_signal.to_string_hum status)
      | Ok () ->
        match output with
        | []
         -> L.external_error "There are no files to process, exiting@." ; L.exit 0
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
               -> L.(die ExternalError)
                    "Failed to parse `buck targets --show-output ...` line of output:@\n%s" line
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
