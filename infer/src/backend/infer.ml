(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Top-level driver that orchestrates build system integration, frontends, backend, and
    reporting *)

module L = Logging

let rec rmtree name =
  match Unix.opendir name with
  | dir -> (
      match Unix.readdir dir with
      | entry when entry = Filename.current_dir_name || entry = Filename.parent_dir_name ->
          ()
      | entry ->
          rmtree entry
      | exception End_of_file ->
          Unix.closedir dir ;
          Unix.rmdir name
    )
  | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Unix.unlink name
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()

(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json = DB.Results_dir.(path_to_filename Abs_root ["report.json"]) in
  match read_file (DB.filename_to_string issues_json) with
  | Some lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat "" lines in
      if issues <> [] then exit Config.fail_on_issue_exit_code
  | None -> ()

(* permissions used for created files *)
let file_perm = 0o0666

let create_results_dir () =
  create_path (Config.results_dir // Config.captured_dir_name) ;
  create_path (Config.results_dir // Config.sources_dir_name) ;
  create_path (Config.results_dir // Config.specs_dir_name)

let touch_start_file () =
  let start = Config.results_dir // Config.start_filename in
  let flags =
    Unix.O_CREAT :: Unix.O_WRONLY :: (if Config.continue_capture then [Unix.O_EXCL] else []) in
  (* create new file, or open existing file for writing to update modified timestamp *)
  try Unix.close (Unix.openfile start flags file_perm)
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

type build_mode =
  | Analyze | Ant | Buck | ClangCompilationDatabase | Gradle | Java | Javac | Make | Mvn | Ndk
  | Xcode

let build_mode_of_string path =
  match Filename.basename path with
  | "analyze" -> Analyze
  | "ant" -> Ant
  | "buck" -> Buck
  | "clang-compilation-database" -> ClangCompilationDatabase
  | "gradle" | "gradlew" -> Gradle
  | "java" -> Java
  | "javac" -> Javac
  | "cc" | "clang" | "clang++" | "cmake" | "configure" | "g++" | "gcc" | "make" | "waf" -> Make
  | "mvn" -> Mvn
  | "ndk-build" -> Ndk
  | "xcodebuild" -> Xcode
  | cmd -> failwithf "Unsupported build command %s" cmd

let remove_results_dir build_mode =
  if not (build_mode = Analyze || Config.buck || Config.reactive_mode) then
    rmtree Config.results_dir

let run_command cmd_list after_wait =
  let cmd = Array.of_list cmd_list in
  let pid = Unix.create_process cmd.(0) cmd Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  let exit_code = match status with Unix.WEXITED i -> i | _ -> 1 in
  after_wait exit_code ;
  if exit_code <> 0 then (
    L.err "Failed to execute: %s@\n" (String.concat " " cmd_list) ;
    exit exit_code
  )

let capture build_cmd build_mode =
  let analyze_cmd = "analyze" in
  let is_analyze_cmd cmd =
    match cmd with
    | [cmd] when cmd = analyze_cmd -> true
    | _ -> false in
  let build_cmd =
    match build_mode with
    | Buck when Option.is_some Config.use_compilation_database ->
        let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_buck () in
        CaptureCompilationDatabase.capture_files_in_database json_cdb;
        [analyze_cmd]
    | ClangCompilationDatabase ->
        (match Config.rest with
         | arg::_ -> CaptureCompilationDatabase.capture_files_in_database [arg]
         | _ -> failwith("Errror parsing arguments. Please, pass the compilation \
                          database json file as in \
                          infer -- clang-compilation-database file.json."));
        [analyze_cmd]
    | _ ->  build_cmd in
  let in_buck_mode = build_mode = Buck in
  let infer_py = Config.lib_dir // "python" // "infer.py" in
  run_command (
    infer_py ::
    Config.anon_args @
    ["--analyzer";
     IList.assoc (=) Config.analyzer
       (IList.map (fun (n,a) -> (a,n)) Config.string_to_analyzer)] @
    (match Config.blacklist with
     | Some s when in_buck_mode && not (is_analyze_cmd build_cmd) -> ["--blacklist-regex"; s]
     | _ -> []) @
    (if not Config.create_harness then [] else
       ["--android-harness"]) @
    (if not Config.buck then [] else
       ["--buck"]) @
    (match Config.java_jar_compiler with None -> [] | Some p ->
        ["--java-jar-compiler"; p]) @
    (match IList.rev Config.buck_build_args with
     | args when in_buck_mode ->
         IList.map (fun arg -> ["--Xbuck"; "'" ^ arg ^ "'"]) args |> IList.flatten
     | _ -> []) @
    (if not Config.continue_capture then [] else
       ["--continue"]) @
    (if not Config.debug_mode then [] else
       ["--debug"]) @
    (if not Config.debug_exceptions then [] else
       ["--debug-exceptions"]) @
    (if Config.filtering then [] else
       ["--no-filtering"]) @
    (if not Config.flavors || not in_buck_mode || is_analyze_cmd build_cmd then [] else
       ["--use-flavors"]) @
    "-j" :: (string_of_int Config.jobs) ::
    (Option.map_default (fun l -> ["-l"; string_of_float l]) [] Config.load_average) @
    (if not Config.pmd_xml then [] else
       ["--pmd-xml"]) @
    ["--project-root"; Config.project_root] @
    (if not Config.reactive_mode then [] else
       ["--reactive"]) @
    "--out" :: Config.results_dir ::
    (match Config.xcode_developer_dir with None -> [] | Some d ->
        ["--xcode-developer-dir"; d]) @
    (if Config.rest = [] then [] else
       ("--" :: build_cmd))
  ) (fun exit_code ->
      if exit_code = Config.infer_py_argparse_error_exit_code then
        (* swallow infer.py argument parsing error *)
        Config.print_usage_exit ()
    )

let analyze = function
  | Buck when Config.use_compilation_database = None ->
      (* In Buck mode when compilation db is not used, analysis is invoked either from capture or a
         separate Analyze invocation is necessary, depending on the buck flavor used. *)
      ()
  | Java | Javac ->
      (* In Java and Javac modes, analysis is invoked from capture. *)
      ()
  | Analyze | Ant | Buck | ClangCompilationDatabase | Gradle | Make | Mvn | Ndk | Xcode ->
      if not (Sys.file_exists Config.(results_dir // captured_dir_name)) then (
        L.err "There was nothing to analyze, exiting" ;
        Config.print_usage_exit ()
      );
      (match Config.analyzer with
       | Infer | Eradicate | Checkers | Tracing | Crashcontext | Quandary ->
           (* Still handled by infer.py through capture function above *)
           ()
       | Linters ->
           (* Still handled by infer.py through capture function above *)
           ()
       | Capture | Compile ->
           (* Still handled by infer.py through capture function above *)
           ()
      )

let epilogue build_mode =
  if Config.is_originator then (
    StatsAggregator.generate_files () ;
    if Config.analyzer = Config.Crashcontext then
      Crashcontext.crashcontext_epilogue ~in_buck_mode:(build_mode = Buck);
    if Config.fail_on_bug then
      fail_on_issue_epilogue ();
  )

let () =
  let build_cmd = IList.rev Config.rest in
  let build_mode = match build_cmd with path :: _ -> build_mode_of_string path | [] -> Analyze in
  remove_results_dir build_mode ;
  create_results_dir () ;
  if Config.is_originator then L.out "%s@\n" Config.version_string ;
  touch_start_file () ;
  capture build_cmd build_mode ;
  analyze build_mode ;
  epilogue build_mode
