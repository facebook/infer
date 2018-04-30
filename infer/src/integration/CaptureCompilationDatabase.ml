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
module L = Logging

let create_cmd (compilation_data: CompilationDatabase.compilation_data) =
  let swap_executable cmd =
    if String.is_suffix ~suffix:"++" cmd then Config.wrappers_dir ^/ "clang++"
    else Config.wrappers_dir ^/ "clang"
  in
  let arg_file =
    ClangQuotes.mk_arg_file "cdb_clang_args" ClangQuotes.EscapedNoQuotes
      compilation_data.escaped_arguments
  in
  { CompilationDatabase.directory= compilation_data.directory
  ; executable= swap_executable compilation_data.executable
  ; escaped_arguments= ["@" ^ arg_file; "-fsyntax-only"] }


(* A sentinel is a file which indicates that a failure occurred in another infer process.
   Because infer processes run in parallel but do not share any memory, we use the
   filesystem to signal failures across processes. *)
let sentinel_exists sentinel_opt =
  let file_exists sentinel = PolyVariantEqual.( = ) (Sys.file_exists sentinel) `Yes in
  Option.value_map ~default:false sentinel_opt ~f:file_exists


let invoke_cmd ~fail_sentinel (cmd: CompilationDatabase.compilation_data) =
  let create_sentinel_if_needed () =
    let create_empty_file fname = Utils.with_file_out ~f:(fun _ -> ()) fname in
    Option.iter fail_sentinel ~f:create_empty_file
  in
  if sentinel_exists fail_sentinel then L.progress "E%!"
  else
    try
      let pid =
        let open Spawn in
        spawn ~cwd:(Path cmd.directory) ~prog:cmd.executable
          ~argv:(cmd.executable :: cmd.escaped_arguments) ()
      in
      match Unix.waitpid (Pid.of_int pid) with
      | Ok () ->
          L.progress ".%!"
      | Error _ ->
          L.progress "!%!" ; create_sentinel_if_needed ()
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
  L.(debug Capture Quiet)
    "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  L.progress "Starting %s %d files@\n%!" Config.clang_frontend_action_string number_of_jobs ;
  let sequence = Parmap.L (List.map ~f:create_cmd compilation_data) in
  let fail_sentinel_fname = Config.results_dir ^/ Config.linters_failed_sentinel_filename in
  let fail_sentinel =
    if Config.linters_ignore_clang_failures then None
    else
      match Config.buck_compilation_database with
      | Some NoDeps when Config.linters ->
          Some fail_sentinel_fname
      | Some NoDeps | Some (Deps _) | None ->
          None
  in
  Utils.rmtree fail_sentinel_fname ;
  let chunksize = min ((List.length compilation_data / Config.jobs) + 1) 10 in
  Parmap.pariter ~ncores:Config.jobs ~chunksize (invoke_cmd ~fail_sentinel) sequence ;
  L.progress "@." ;
  L.(debug Analysis Medium) "Ran %d jobs" number_of_jobs ;
  if sentinel_exists fail_sentinel then (
    L.progress
      "Failure detected, capture did not finish successfully. Use \
       `--linters-ignore-clang-failures` to ignore compilation errors. Terminating@." ;
    L.exit 1 )


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
        command
        :: List.rev_append rev_not_targets
             ("--config" :: "*//cxx.pch_enabled=false" :: targets_args)
      in
      Logging.(debug Linters Quiet)
        "Processed buck command is: 'buck %a'@\n" (Pp.seq F.pp_print_string) build_args ;
      Process.create_process_and_wait ~prog ~args:build_args ;
      let buck_targets_shell =
        prog
        :: "targets"
        :: List.rev_append
             (Buck.filter_compatible `Targets rev_not_targets)
             ("--show-output" :: targets_args)
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
