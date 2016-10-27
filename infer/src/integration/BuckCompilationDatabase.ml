(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

let capture_text =
  if Config.analyzer = Some Config.Linters then "linting"
  else "translating"

let replace_header_file_with_source_file file_path =
  let file_path = DB.source_file_to_abs_path file_path in
  let possible_file_replacements file_path =
    IList.map (fun suffix -> (Filename.chop_extension file_path) ^ suffix ) [".m"; ".mm"] in
  let file =
    if Filename.check_suffix file_path ".h" || Filename.check_suffix file_path ".hh" then
      try
        IList.find Sys.file_exists (possible_file_replacements file_path)
      with Not_found ->
        Logging.out "Couldn't find any replacement source file for file %s " file_path;
        file_path
    else file_path in
  DB.abs_source_file_from_path file

(** Read the files to compile from the changed files index. *)
let read_files_to_compile () =
  let changed_files = DB.SourceFileSet.empty in
  match DB.read_changed_files_index with
  | None ->
      (match Config.changed_files_index with
       | Some index ->
           Process.print_error_and_exit "Error reading the changed files index %s.\n%!" index
       | None -> changed_files)
  | Some lines ->
      IList.fold_left
        (fun changed_files line ->
           let file = replace_header_file_with_source_file (DB.abs_source_file_from_path line) in
           DB.SourceFileSet.add file changed_files)
        changed_files lines

(** The buck targets are assumed to start with //, aliases are not supported. *)
let check_args_for_targets args =
  if not (IList.exists (Utils.string_is_prefix "//") args) then
    let args_s = String.concat " " args in
    Process.print_error_and_exit
      "Error reading buck command %s. Please, pass buck targets, aliases are not allowed.\n%!"
      args_s

let add_flavor_to_targets args =
  let flavor =
    match Config.use_compilation_database with
    | Some `Deps -> "#uber-compilation-database"
    | Some `NoDeps -> "#compilation-database"
    | _ -> assert false (* cannot happen *) in
  let process_arg arg =
    (* Targets are assumed to start with //, aliases are not allowed *)
    if Utils.string_is_prefix "//" arg then arg ^ flavor
    else arg in
  IList.map process_arg args

let create_files_stack compilation_database =
  let stack = Stack.create () in
  let add_to_stack file _ =
    Stack.push file stack in
  CompilationDatabase.iter compilation_database add_to_stack;
  stack

let swap_command cmd =
  let clang = "clang" in
  let clangplusplus = "clang++" in
  if Utils.string_is_suffix clang cmd then
    Config.wrappers_dir // clang
  else if Utils.string_is_suffix clangplusplus cmd then
    Config.wrappers_dir // clangplusplus
  else
    (* The command in the compilation database json emitted by buck can only be clang or clang++ *)
    failwithf "Unexpected command name in Buck compilation database: %s" cmd

let run_compilation_file compilation_database file =
  try
    let compilation_data = CompilationDatabase.find compilation_database file in
    let wrapper_cmd = swap_command compilation_data.command in
    let arg_file =
      ClangQuotes.mk_arg_file
        "cdb_clang_args_" ClangQuotes.EscapedNoQuotes [compilation_data.args] in
    let args = Array.of_list [wrapper_cmd; "@" ^ arg_file] in
    let env = Array.append
        (Unix.environment())
        (Array.of_list ["FCP_RUN_SYNTAX_ONLY=1"]) in
    (Some compilation_data.dir, wrapper_cmd, args, env)
  with Not_found ->
    Process.print_error_and_exit "Failed to find compilation data for %s \n%!" file

let run_compilation_database compilation_database =
  let number_of_files = CompilationDatabase.get_size compilation_database in
  Logging.out "Starting %s %d files \n%!" capture_text number_of_files;
  Logging.stdout "Starting %s %d files \n%!" capture_text number_of_files;
  let jobs_stack = create_files_stack compilation_database in
  let capture_text_upper = String.capitalize capture_text in
  let job_to_string = fun file -> capture_text_upper ^ " " ^ file in
  Process.run_jobs_in_parallel jobs_stack (run_compilation_file compilation_database) job_to_string

let should_add_file_to_cdb changed_files file_path =
  match Config.changed_files_index with
  | Some _ -> DB.SourceFileSet.mem (DB.source_file_from_string file_path) changed_files
  | None -> true

(** Computes the compilation database files. *)
let get_compilation_database_files () =
  let cmd = IList.rev_append Config.rest (IList.rev Config.buck_build_args) in
  match cmd with
  | buck :: build :: args ->
      (check_args_for_targets args;
       let args_with_flavor = add_flavor_to_targets args in
       let buck_build = Array.of_list
           (buck :: build :: "--config" :: "*//cxx.pch_enabled=false" :: args_with_flavor) in
       Process.create_process_and_wait buck_build;
       let buck_targets_list = buck :: "targets" :: "--show-output" :: args_with_flavor in
       let buck_targets = String.concat " " buck_targets_list in
       try
         match fst @@ Utils.with_process_in buck_targets Std.input_list with
         | [] -> Logging.stdout "There are no files to process, exiting."; exit 0
         | lines ->
             Logging.out "Reading compilation database from:@\n%s@\n" (String.concat "\n" lines);
             let scan_output compilation_database_files chan =
               Scanf.sscanf chan "%s %s"
                 (fun target file -> StringMap.add target file compilation_database_files) in
             (* Map from targets to json output *)
             let compilation_database_files = IList.fold_left scan_output StringMap.empty lines in
             IList.map (snd) (StringMap.bindings compilation_database_files)
       with Unix.Unix_error (err, _, _) ->
         Process.print_error_and_exit
           "Cannot execute %s\n%!"
           (buck_targets ^ " " ^ (Unix.error_message err)))
  | _ ->
      let cmd = String.concat " " cmd in
      Process.print_error_and_exit "Incorrect buck command: %s. Please use buck build <targets>" cmd

let () =
  let changed_files = read_files_to_compile () in
  let db_json_files =
    match Config.clang_compilation_database with
    | Some file -> [file]
    | None ->
        if Option.is_some Config.use_compilation_database then
          get_compilation_database_files ()
        else failwith(
            "Either the option clang_compilation_database or the option \
             use_compilation_database should be passed to this module ") in
  let compilation_database = CompilationDatabase.empty () in
  IList.iter
    (CompilationDatabase.decode_json_file
       compilation_database (should_add_file_to_cdb changed_files)) db_json_files;

  create_dir (Config.results_dir // Config.clang_build_output_dir_name);
  run_compilation_database compilation_database
