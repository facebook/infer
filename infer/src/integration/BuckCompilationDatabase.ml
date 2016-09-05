(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils
module YBU = Yojson.Basic.Util

type compilation_data = {
  dir : string;
  command : string;
  args : string list;
}

let capture_text =
  if Config.analyzer = Some Config.Linters then "linting"
  else "translating"

let swap_command cmd =
  let clang = "clang" in
  let clangplusplus = "clang++" in
  if Utils.string_is_suffix clang cmd then
    Config.wrappers_dir // clang
  else if Utils.string_is_suffix clangplusplus cmd then
    Config.wrappers_dir // clangplusplus
  else assert false
(* The command in the compilation database json
   emited by buck can only be clang or clang++ *)

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
           DB.SourceFileSet.add (DB.abs_source_file_from_path line) changed_files)
        changed_files lines

(** Add file to compilation database, only if it is in changed_files. *)
let add_file_to_compilation_database file_path cmd_options changed_files compilation_database =
  let add_file =
    match Config.changed_files_index with
    | Some _ -> DB.SourceFileSet.mem (DB.source_file_from_string file_path) changed_files
    | None -> true in
  if add_file then
    compilation_database := StringMap.add file_path cmd_options !compilation_database

(** We have to replace the .o files because the path in buck-out doesn't exist at this point.
    Moreover, in debug mode we create debug files in the place where the .o files are created,
    so having all that in the results directory is convenient for finding the files and for
    scanning the directory for running clang_frontend_stats. *)
let replace_clang_args arg =
  if Filename.check_suffix arg ".o" then
    let dir = Config.results_dir // Config.clang_build_output_dir_name in
    let abbrev_source_file = DB.source_file_encoding (DB.source_file_from_string arg) in
    dir // abbrev_source_file
  else arg

(* Doing this argument manipulation here rather than in the wrappers because it seems to
   be needed only with this integration.*)
let remove_clang_arg arg args =
  if (arg = "-include-pch") || (Filename.check_suffix arg ".gch")
  then args else arg :: args

(** Parse the compilation database json file into the compilationDatabase
    map. The json file consists of an array of json objects that contain the file
    to be compiled, the directory to be compiled in, and the compilation command as a list
    and as a string. We pack this information into the compilationDatabase map, and remove the
    clang invocation part, because we will use a clang wrapper. *)
let decode_compilation_database changed_files compilation_database _ path =
  let collect_arguments compilation_argument args =
    match compilation_argument with
    | `String arg ->
        let arg' = replace_clang_args arg in
        remove_clang_arg arg' args
    | _ -> failwith ("Json file doesn't have the expected format") in
  let json = Yojson.Basic.from_file path in
  let rec parse_json json =
    match json with
    | `List arguments ->
        IList.iter parse_json arguments
    | `Assoc [ ("directory", `String dir);
               ("file", `String file_path);
               ("arguments", `List compilation_arguments);
               ("command", `String _) ] ->
        (match IList.fold_right collect_arguments compilation_arguments [] with
         | [] -> failwith ("Command cannot be empty")
         | cmd :: args ->
             let wrapper_cmd = swap_command cmd in
             let compilation_data = { dir; command = wrapper_cmd; args;} in
             add_file_to_compilation_database file_path compilation_data changed_files
               compilation_database)
    | _ ->
        failwith ("Json file doesn't have the expected format") in
  parse_json json

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
  StringMap.iter add_to_stack !compilation_database;
  stack

let run_compilation_file compilation_database file =
  try
    let compilation_data = StringMap.find file !compilation_database in
    Unix.chdir compilation_data.dir;
    let args = Array.of_list compilation_data.args in
    let env = Array.append
        (Unix.environment())
        (Array.of_list [
            "INFER_RESULTS_DIR="^Config.results_dir;
            "FCP_RUN_SYNTAX_ONLY=1"]) in
    Process.exec_command compilation_data.command args env
  with Not_found ->
    Process.print_error_and_exit "Failed to find compilation data for %s \n%!" file

let run_compilation_database compilation_database =
  let number_of_files = StringMap.cardinal !compilation_database in
  Logging.out "Starting %s %d files \n%!" capture_text number_of_files;
  Logging.stdout "Starting %s %d files \n%!" capture_text number_of_files;
  let jobsStack = create_files_stack compilation_database in
  let capture_text_upper = String.capitalize capture_text in
  let job_to_string = fun file -> capture_text_upper ^ " " ^ file in
  Process.run_jobs_in_parallel jobsStack (run_compilation_file compilation_database) job_to_string


(** Computes the compilation database: a map from a file path to info to compile the file, i.e.
    the dir where the compilation should be executed and the arguments to clang.*)
let get_compilation_database changed_files =
  let cmd = IList.rev Config.buck_build_args in
  match cmd with
  | buck :: build :: args ->
      (check_args_for_targets args;
       let args_with_flavor = add_flavor_to_targets args in
       let buck_build = Array.of_list (buck :: build :: args_with_flavor) in
       Process.create_process_and_wait buck_build;
       let buck_targets_list = buck :: "targets" :: "--show-output" :: args_with_flavor in
       let buck_targets = String.concat " " buck_targets_list in
       try
         match Utils.with_process_in buck_targets Std.input_list with
         | [] -> Logging.stdout "There are no files to process, exiting."; exit 0
         | lines ->
             let scan_output compilation_database_files chan =
               Scanf.sscanf chan "%s %s"
                 (fun target file -> StringMap.add target file compilation_database_files) in
             (* Map from targets to json output *)
             let compilation_database_files = IList.fold_left scan_output StringMap.empty lines in
             let compilation_database = ref StringMap.empty in
             StringMap.iter
               (decode_compilation_database changed_files compilation_database)
               compilation_database_files;
             compilation_database
       with Unix.Unix_error (err, _, _) ->
         Process.print_error_and_exit
           "Cannot execute %s\n%!"
           (buck_targets ^ " " ^ (Unix.error_message err)))
  | _ ->
      let cmd = String.concat " " cmd in
      Process.print_error_and_exit "Incorrect buck command: %s. Please use buck build <targets>" cmd

let () =
  let changed_files = read_files_to_compile () in
  let compilation_database = get_compilation_database changed_files in
  DB.create_dir (Config.results_dir // Config.clang_build_output_dir_name);
  run_compilation_database compilation_database
