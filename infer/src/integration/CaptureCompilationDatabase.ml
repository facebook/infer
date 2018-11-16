(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let create_cmd (source_file, (compilation_data : CompilationDatabase.compilation_data)) =
  let swap_executable cmd =
    if String.is_suffix ~suffix:"++" cmd then Config.wrappers_dir ^/ "clang++"
    else Config.wrappers_dir ^/ "clang"
  in
  let arg_file =
    ClangQuotes.mk_arg_file "cdb_clang_args" ClangQuotes.EscapedNoQuotes
      compilation_data.escaped_arguments
  in
  ( source_file
  , { CompilationDatabase.directory= compilation_data.directory
    ; executable= swap_executable compilation_data.executable
    ; escaped_arguments= ["@" ^ arg_file; "-fsyntax-only"] @ List.rev Config.clang_extra_flags } )


let invoke_cmd (source_file, (cmd : CompilationDatabase.compilation_data)) =
  let argv = cmd.executable :: cmd.escaped_arguments in
  ( match Spawn.spawn ~cwd:(Path cmd.directory) ~prog:cmd.executable ~argv () with
  | pid ->
      !ProcessPoolState.update_status (Mtime_clock.now ()) (SourceFile.to_string source_file) ;
      Unix.waitpid (Pid.of_int pid)
      |> Result.map_error ~f:(fun unix_error ->
             Unix.Exit_or_signal.to_string_hum (Error unix_error) )
  | exception Unix.Unix_error (err, f, arg) ->
      Error (F.asprintf "%s(%s): %s@." f arg (Unix.Error.message err)) )
  |> function
  | Ok () ->
      ()
  | Error error ->
      let log_or_die fmt =
        if Config.linters_ignore_clang_failures || Config.keep_going then L.debug Capture Quiet fmt
        else L.die ExternalError fmt
      in
      log_or_die "Error running compilation for '%a': %a:@\n%s@." SourceFile.pp source_file
        Pp.cli_args argv error


let run_compilation_database compilation_database should_capture_file =
  let compilation_data =
    CompilationDatabase.filter_compilation_data compilation_database ~f:should_capture_file
  in
  let number_of_jobs = List.length compilation_data in
  L.(debug Capture Quiet)
    "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  L.progress "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  let compilation_commands = List.map ~f:create_cmd compilation_data in
  let runner = Tasks.Runner.create ~jobs:Config.jobs ~f:invoke_cmd in
  Tasks.Runner.run runner ~tasks:compilation_commands ;
  L.progress "@." ;
  L.(debug Analysis Medium) "Ran %d jobs" number_of_jobs


(** Computes the compilation database files. *)
let get_compilation_database_files_buck ~prog ~args =
  let dep_depth =
    match Config.buck_compilation_database with Some (Deps depth) -> Some depth | _ -> None
  in
  match
    Buck.add_flavors_to_buck_arguments ~filter_kind:`Yes ~dep_depth
      ~extra_flavors:Config.append_buck_flavors args
  with
  | {command= "build" as command; rev_not_targets; targets} ->
      let targets_args = Buck.store_args_in_file targets in
      let build_args =
        (command :: List.rev_append rev_not_targets (List.rev Config.buck_build_args_no_inline))
        @ (* Infer doesn't support C++ modules nor precompiled headers yet (T35656509) *)
          "--config" :: "*//cxx.pch_enabled=false" :: "--config" :: "*//cxx.modules_default=false"
          :: "--config" :: "*//cxx.modules=False" :: targets_args
      in
      Logging.(debug Linters Quiet)
        "Processed buck command is: 'buck %a'@\n" (Pp.seq F.pp_print_string) build_args ;
      Process.create_process_and_wait ~prog ~args:build_args ;
      let buck_targets_shell =
        prog :: "targets"
        :: List.rev_append
             (Buck.filter_compatible `Targets rev_not_targets)
             (List.rev Config.buck_build_args_no_inline)
        @ ("--show-output" :: targets_args)
      in
      let on_target_lines = function
        | [] ->
            L.(die ExternalError) "There are no files to process, exiting"
        | lines ->
            L.(debug Capture Quiet)
              "Reading compilation database from:@\n%a@\n"
              (Pp.seq ~sep:"\n" F.pp_print_string)
              lines ;
            (* this assumes that flavors do not contain spaces *)
            let split_regex = Str.regexp "#[^ ]* " in
            let scan_output compilation_database_files line =
              match Str.bounded_split split_regex line 2 with
              | [_; filename] ->
                  `Raw filename :: compilation_database_files
              | _ ->
                  L.(die ExternalError)
                    "Failed to parse `buck targets --show-output ...` line of output:@\n%s" line
            in
            List.fold ~f:scan_output ~init:[] lines
      in
      Utils.with_process_lines
        ~debug:L.(debug Capture Quiet)
        ~cmd:buck_targets_shell ~tmp_prefix:"buck_targets_" ~f:on_target_lines
  | _ ->
      Process.print_error_and_exit "Incorrect buck command: %s %a. Please use buck build <targets>"
        prog (Pp.seq F.pp_print_string) args


(** Compute the compilation database files. *)
let get_compilation_database_files_xcodebuild ~prog ~args =
  let tmp_file = Filename.temp_file "cdb" ".json" in
  let xcodebuild_prog, xcodebuild_args = (prog, prog :: args) in
  let xcpretty_prog = "xcpretty" in
  let xcpretty_args =
    [xcpretty_prog; "--report"; "json-compilation-database"; "--output"; tmp_file]
  in
  L.(debug Capture Quiet)
    "Running %s | %s@\n@."
    (List.to_string ~f:Fn.id xcodebuild_args)
    (List.to_string ~f:Fn.id xcpretty_args) ;
  let producer_status, consumer_status =
    Process.pipeline ~producer_prog:xcodebuild_prog ~producer_args:xcodebuild_args
      ~consumer_prog:xcpretty_prog ~consumer_args:xcpretty_args
  in
  match (producer_status, consumer_status) with
  | Ok (), Ok () ->
      [`Escaped tmp_file]
  | _ ->
      L.(die ExternalError) "There was an error executing the build command"


let capture_files_in_database ~changed_files compilation_database =
  let filter_changed =
    match changed_files with
    | None ->
        fun _ -> true
    | Some changed_files_set ->
        fun source_file -> SourceFile.Set.mem source_file changed_files_set
  in
  run_compilation_database compilation_database filter_changed


let capture_file_in_database compilation_database source_file =
  run_compilation_database compilation_database (SourceFile.equal source_file)
