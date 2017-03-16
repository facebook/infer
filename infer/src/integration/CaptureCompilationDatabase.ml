(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module CLOpt = CommandLineOption
module F = Format

let capture_text =
  if Config.equal_analyzer Config.analyzer Config.Linters then "linting"
  else "translating"

(** Read the files to compile from the changed files index. *)
let should_capture_file_from_index () =
  match SourceFile.changed_files_set with
  | None ->
      (match Config.changed_files_index with
       | Some index ->
           Process.print_error_and_exit "Error reading the changed files index %s.\n%!" index
       | None -> function _ -> true)
  | Some files_set ->
      function source_file -> SourceFile.Set.mem source_file files_set

let create_files_stack compilation_database should_capture_file =
  let stack = Stack.create () in
  let add_to_stack file _ = if should_capture_file file then
      Stack.push stack file in
  CompilationDatabase.iter compilation_database add_to_stack;
  stack

let swap_command cmd =
  let plusplus = "++" in
  let clang = "clang" in
  let clangplusplus = "clang++" in
  if String.is_suffix ~suffix:plusplus cmd then
    Config.wrappers_dir ^/ clangplusplus
  else
    Config.wrappers_dir ^/ clang

let run_compilation_file compilation_database file =
  try
    let compilation_data = CompilationDatabase.find compilation_database file in
    let wrapper_cmd = swap_command compilation_data.command in
    let arg_file =
      ClangQuotes.mk_arg_file
        "cdb_clang_args_" ClangQuotes.EscapedNoQuotes [compilation_data.args] in
    let args = ["@" ^ arg_file] in
    let env =
      `Extend [
        (CLOpt.args_env_var,
         String.concat ~sep:(String.of_char CLOpt.env_var_sep)
           (Option.to_list (Sys.getenv CLOpt.args_env_var) @ ["--fcp-syntax-only"]))] in
    (Some compilation_data.dir, wrapper_cmd, args, env)
  with Not_found ->
    Process.print_error_and_exit "Failed to find compilation data for %a \n%!"
      SourceFile.pp file

let run_compilation_database compilation_database should_capture_file =
  let number_of_files = CompilationDatabase.get_size compilation_database in
  Logging.out "Starting %s %d files \n%!" capture_text number_of_files;
  Logging.stdout "Starting %s %d files \n%!" capture_text number_of_files;
  let jobs_stack = create_files_stack compilation_database should_capture_file in
  let capture_text_upper = String.capitalize capture_text in
  let job_to_string =
    fun file -> Format.asprintf "%s %a" capture_text_upper SourceFile.pp file in
  let fail_on_failed_job =
    if Config.linters_ignore_clang_failures then false
    else
      match Config.buck_compilation_database with
      | Some `NoDeps -> Config.clang_frontend_do_lint
      | _ -> false in
  Process.run_jobs_in_parallel ~fail_on_failed_job jobs_stack
    (run_compilation_file compilation_database) job_to_string

(** Computes the compilation database files. *)
let get_compilation_database_files_buck () =
  let cmd = List.rev_append Config.rest (List.rev Config.buck_build_args) in
  match Buck.add_flavors_to_buck_command cmd with
  | buck :: build :: args_with_flavor -> (
      let build_args = build :: "--config" :: "*//cxx.pch_enabled=false" :: args_with_flavor in
      Process.create_process_and_wait ~prog:buck ~args:build_args;
      let buck_targets_shell =
        buck :: "targets" :: "--show-output" :: args_with_flavor
        |> List.map ~f:(Printf.sprintf "'%s'")
        |> String.concat ~sep:" " in
      try
        match fst @@ Utils.with_process_in buck_targets_shell In_channel.input_lines with
        | [] -> Logging.stdout "There are no files to process, exiting."; exit 0
        | lines ->
            Logging.out "Reading compilation database from:@\n%s@\n"
              (String.concat ~sep:"\n" lines);
            (* this assumes that flavors do not contain spaces *)
            let split_regex = Str.regexp "#[^ ]* " in
            let scan_output compilation_database_files line =
              match Str.bounded_split split_regex line 2 with
              | _::filename::[] ->
                  `Raw filename::compilation_database_files
              | _ ->
                  failwithf
                    "Failed to parse `buck targets --show-output ...` line of output:@\n%s" line in
            List.fold ~f:scan_output ~init:[] lines
      with Unix.Unix_error (err, _, _) ->
        Process.print_error_and_exit
          "Cannot execute %s\n%!"
          (buck_targets_shell ^ " " ^ (Unix.error_message err))
    )
  | _ ->
      let cmd = String.concat ~sep:" " cmd in
      Process.print_error_and_exit "Incorrect buck command: %s. Please use buck build <targets>" cmd

(** Compute the compilation database files. *)
let get_compilation_database_files_xcodebuild () =
  let prog_args = List.rev Config.rest in
  let temp_dir = Config.results_dir ^/ "clang" in
  Utils.create_dir temp_dir;
  let tmp_file = Filename.temp_file ~in_dir:temp_dir "cdb" ".json" in
  let xcodebuild_prog, xcodebuild_args =
    match prog_args with
    | prog :: _ -> (prog, prog_args)
    | [] -> failwith("Build command cannot be empty") in
  let xcpretty_prog = "xcpretty" in
  let xcpretty_args =
    [xcpretty_prog; "--report"; "json-compilation-database"; "--output"; tmp_file] in
  Logging.out "Running %s | %s\n@." (List.to_string ~f:Fn.id xcodebuild_args)
    (List.to_string ~f:Fn.id xcpretty_args);
  let producer_status, consumer_status =
    Process.pipeline
      ~producer_prog:xcodebuild_prog ~producer_args:xcodebuild_args
      ~consumer_prog:xcpretty_prog ~consumer_args:xcpretty_args in
  match producer_status, consumer_status with
  | Ok (), Ok () -> [`Escaped tmp_file]
  | _ ->
      Logging.stderr "There was an error executing the build command";
      exit 1

let capture_files_in_database compilation_database =
  run_compilation_database compilation_database (should_capture_file_from_index ())

let capture_file_in_database compilation_database source_file =
  run_compilation_database compilation_database (SourceFile.equal source_file)
