(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Top-level driver that orchestrates build system integration, frontends, backend, and
    reporting *)

module CLOpt = CommandLineOption
module L = Logging
module F = Format


let rec rmtree name =
  match Unix.((lstat name).st_kind) with
  | S_DIR ->
      let dir = Unix.opendir name in
      let rec rmdir dir =
        match Unix.readdir dir with
        | entry ->
            if not (entry = Filename.current_dir_name || entry = Filename.parent_dir_name) then (
              rmtree (name ^/ entry)
            );
            rmdir dir
        | exception End_of_file ->
            Unix.closedir dir ;
            Unix.rmdir name in
      rmdir dir
  | _ ->
      Unix.unlink name
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()


type build_system =
  | BAnalyze | BAnt | BBuck | BGradle | BJava | BJavac | BMake | BMvn | BNdk | BXcode

(* List of ([build system], [executable name]). Several executables may map to the same build
   system. In that case, the first one in the list will be used for printing, eg, in which mode
   infer is running. *)
let build_system_exe_assoc = [
  BAnalyze, "analyze"; BAnt, "ant"; BBuck, "buck"; BGradle, "gradle"; BGradle, "gradlew";
  BJava, "java"; BJavac, "javac";
  (* NOTE: "make/cc" is not a valid exe name and thus will never be matched, we only use it for
     printing *)
  BMake, "make/cc"; BMake, "cc"; BMake, "clang"; BMake, "clang++"; BMake, "cmake";
  BMake, "configure"; BMake, "g++"; BMake, "gcc"; BMake, "make"; BMake, "waf";
  BMvn, "mvn"; BMvn, "mvnw"; BNdk, "ndk-build"; BXcode, "xcodebuild";
]

let build_system_of_exe_name name =
  try
    List.Assoc.find_exn (List.Assoc.inverse build_system_exe_assoc) name
  with Not_found ->
    invalid_argf "Unsupported build command %s" name

let string_of_build_system build_system =
  List.Assoc.find_exn build_system_exe_assoc build_system

(* based on the build_system and options passed to infer, we run in different driver modes *)
type driver_mode =
  | Analyze
  | BuckGenrule of string
  | BuckCompilationDB
  | ClangCompilationDB of string list
  | Javac of Javac.compiler * string * string list
  | Maven of string * string list
  | PythonCapture of build_system * string list
  | XcodeXcpretty

let pp_driver_mode fmt driver_mode =
  let log_argfile_arg fname =
    try
      F.fprintf fmt "-- Contents of '%s'@\n" fname;
      In_channel.iter_lines ~f:(F.fprintf fmt "%s@\n") (In_channel.create fname);
      F.fprintf fmt "-- /Contents of '%s'@." fname;
    with exn ->
      F.fprintf fmt "  Error reading file '%s':@\n  %a@." fname Exn.pp exn in
  match driver_mode with
  | Analyze | BuckGenrule _ | BuckCompilationDB | ClangCompilationDB _  | PythonCapture (_,_)
  | XcodeXcpretty ->
      (* these are pretty boring, do not log anything *)
      ()
  | Javac (_, prog, args) ->
      F.fprintf fmt "Javac driver mode:@\nprog = %s@\n" prog;
      let log_arg arg =
        F.fprintf fmt "Arg: %s@\n" arg;
        (* "@fname" means that fname is an arg file containing additional arguments to pass to
           javac. *)
        String.chop_prefix ~prefix:"@" arg
        |>
        (* Sometimes these argfiles go away at the end of the build and we cannot inspect them after
           the fact, so log them now. *)
        Option.iter ~f:log_argfile_arg in
      List.iter ~f:log_arg args
  | Maven (prog, args) ->
      F.fprintf fmt "Maven driver mode:@\nprog = %s@\n" prog;
      List.iter ~f:(F.fprintf fmt "Arg: %s@\n") args

let remove_results_dir () =
  rmtree Config.results_dir

let create_results_dir () =
  Unix.mkdir_p (Config.results_dir ^/ Config.attributes_dir_name) ;
  Unix.mkdir_p (Config.results_dir ^/ Config.captured_dir_name) ;
  Unix.mkdir_p (Config.results_dir ^/ Config.specs_dir_name)

let clean_results_dir () =
  let dirs = ["classnames"; "filelists"; "multicore"; "sources"] in
  let suffixes = [".cfg"; ".cg"] in
  let rec clean name =
    match Unix.opendir name with
    | dir -> (
        let rec cleandir dir =
          match Unix.readdir dir with
          | entry ->
              if (IList.exists (String.equal entry) dirs) then (
                rmtree (name ^/ entry)
              ) else if not (entry = Filename.current_dir_name
                             || entry = Filename.parent_dir_name) then (
                clean (name ^/ entry)
              );
              cleandir dir
          | exception End_of_file ->
              Unix.closedir dir in
        cleandir dir
      )
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
        if IList.exists (Filename.check_suffix name) suffixes then
          Unix.unlink name
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        () in
  clean Config.results_dir


let register_perf_stats_report () =
  let stats_dir = Filename.concat Config.results_dir Config.backend_stats_dir_name in
  let stats_base = Config.perf_stats_prefix ^ ".json" in
  let stats_file = Filename.concat stats_dir stats_base in
  Unix.mkdir_p stats_dir;
  PerfStats.register_report_at_exit stats_file


let touch_start_file () =
  let start = Config.results_dir ^/ Config.start_filename in
  let flags =
    Unix.O_CREAT :: Unix.O_WRONLY :: (if Config.continue_capture then [Unix.O_EXCL] else []) in
  (* create new file, or open existing file for writing to update modified timestamp *)
  try Unix.close (Unix.openfile ~perm:0o0666 ~mode:flags start)
  with Unix.Unix_error (Unix.EEXIST, _, _) -> ()


let run_command ~prog ~args cleanup =
  Unix.waitpid (Unix.fork_exec ~prog ~args:(prog :: args) ())
  |> fun status
  -> cleanup status
   ; ok_exn (Unix.Exit_or_signal.or_error status)

let check_xcpretty () =
  match Unix.system "xcpretty --version" with
  | Ok () -> ()
  | Error _ ->
      L.stderr
        "@.xcpretty not found in the path. Please, install xcpretty \
         for a more robust integration with xcodebuild. Otherwise use the option \
         --no-xcpretty.@.@.";
      exit 1

let capture_with_compilation_database db_files =
  let root = Unix.getcwd () in
  Config.clang_compilation_db_files := IList.map (Utils.filename_to_absolute ~root) db_files;
  let compilation_database = CompilationDatabase.from_json_files db_files in
  CaptureCompilationDatabase.capture_files_in_database compilation_database

let capture = function
  | Analyze->
      ()
  | BuckCompilationDB ->
      L.stdout "Capturing using Buck's compilation database...@\n";
      let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_buck () in
      capture_with_compilation_database json_cdb
  | BuckGenrule path ->
      L.stdout "Capturing for Buck genrule compatibility...@\n";
      JMain.main (lazy (JClasspath.load_from_arguments path))
  | ClangCompilationDB db_files ->
      L.stdout "Capturing using compilation database...@\n";
      capture_with_compilation_database db_files
  | Javac (compiler, prog, args) ->
      L.stdout "Capturing in javac mode...@.";
      Javac.capture compiler ~prog ~args
  | Maven (prog, args) ->
      L.stdout "Capturing in maven mode...@.";
      Maven.capture ~prog ~args
  | PythonCapture (build_system, build_cmd) ->
      L.stdout "Capturing in %s mode...@." (string_of_build_system build_system);
      let in_buck_mode = build_system = BBuck in
      let infer_py = Config.lib_dir ^/ "python" ^/ "infer.py" in
      let args =
        List.rev_append Config.anon_args (
          ["--analyzer";
           IList.assoc (=) Config.analyzer
             (IList.map (fun (n,a) -> (a,n)) Config.string_to_analyzer)] @
          (match Config.blacklist with
           | Some s when in_buck_mode -> ["--blacklist-regex"; s]
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
          (if not Config.debug_mode then [] else
             ["--debug"]) @
          (if not Config.debug_exceptions then [] else
             ["--debug-exceptions"]) @
          (if Config.filtering then [] else
             ["--no-filtering"]) @
          (if not Config.flavors || not in_buck_mode then [] else
             ["--use-flavors"]) @
          "-j" :: (string_of_int Config.jobs) ::
          (match Config.load_average with None -> [] | Some l ->
              ["-l"; string_of_float l]) @
          (if not Config.pmd_xml then [] else
             ["--pmd-xml"]) @
          ["--project-root"; Config.project_root] @
          (if not Config.reactive_mode then [] else
             ["--reactive"]) @
          "--out" :: Config.results_dir ::
          (match Config.xcode_developer_dir with None -> [] | Some d ->
              ["--xcode-developer-dir"; d]) @
          "--" ::
          if in_buck_mode && Config.flavors then
            Buck.add_flavors_to_buck_command build_cmd
          else build_cmd
        ) in
      run_command ~prog:infer_py ~args
        (fun status ->
           if status = Result.Error (`Exit_non_zero Config.infer_py_argparse_error_exit_code) then
             (* swallow infer.py argument parsing error *)
             Config.print_usage_exit ()
        )
  | XcodeXcpretty ->
      L.stdout "Capturing using xcpretty...@\n";
      check_xcpretty ();
      let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_xcodebuild () in
      capture_with_compilation_database json_cdb

let run_parallel_analysis () =
  let multicore_dir = Config.results_dir ^/ Config.multicore_dir_name in
  rmtree multicore_dir ;
  Unix.mkdir_p multicore_dir ;
  InferAnalyze.main (multicore_dir ^/ "Makefile") ;
  run_command
    ~prog:"make" ~args:(
    "-C" :: multicore_dir ::
    "-k" ::
    "-j" :: (string_of_int Config.jobs) ::
    (Option.value_map ~f:(fun l -> ["-l"; string_of_float l]) ~default:[] Config.load_average) @
    (if Config.debug_mode then [] else ["-s"])
  ) (fun _ -> ())

let execute_analyze () =
  if Config.jobs = 1 || Config.cluster_cmdline <> None then
    InferAnalyze.main ""
  else
    run_parallel_analysis ()

let report () =
  let report_csv = Some (Config.results_dir ^/ "report.csv") in
  let report_json = Some (Config.results_dir ^/ "report.json") in
  InferPrint.main ~report_csv ~report_json ;
  match Config.report_hook with
  | None -> ()
  | Some prog ->
      let if_some key opt args = match opt with None -> args | Some arg -> key :: arg :: args in
      let if_true key opt args = if not opt then args else key :: args in
      let args =
        if_some "--issues-csv" report_csv @@
        if_some "--issues-json" report_json @@
        if_some "--issues-txt" Config.bugs_txt @@
        if_some "--issues-xml" Config.bugs_xml @@
        if_true "--pmd-xml" Config.pmd_xml [
          "--project-root"; Config.project_root;
          "--results-dir"; Config.results_dir
        ] in
      if is_error (Unix.waitpid (Unix.fork_exec ~prog ~args:(prog :: args) ())) then
        L.stderr "** Error running the reporting script:@\n**   %s %s@\n** See error above@."
          prog (String.concat ~sep:" " args)

let analyze driver_mode =
  let should_analyze, should_report = match driver_mode, Config.analyzer with
    | PythonCapture (BBuck, _), _ ->
        (* In Buck mode when compilation db is not used, analysis is invoked either from capture or
           a separate Analyze invocation is necessary, depending on the buck flavor used. *)
        false, false
    | _ when Config.maven ->
        (* Called from Maven, only do capture. *)
        false, false
    | _, (Capture | Compile) ->
        false, false
    | _, (Infer | Eradicate | Checkers | Tracing | Crashcontext | Quandary | Threadsafety | Bufferoverrun) ->
        true, true
    | _, Linters ->
        false, true in
  if (should_analyze || should_report) &&
     (Sys.file_exists Config.(results_dir ^/ captured_dir_name)) <> `Yes then (
    L.stderr "There was nothing to analyze, exiting@." ;
    exit 1
  );
  if should_analyze then execute_analyze ();
  if should_report then report ()

(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json = DB.Results_dir.(path_to_filename Abs_root ["report.json"]) in
  match Utils.read_file (DB.filename_to_string issues_json) with
  | Some lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat ~sep:"" lines in
      if issues <> [] then exit Config.fail_on_issue_exit_code
  | None -> ()

let log_infer_args driver_mode =
  L.out "INFER_ARGS = %s@\n" (Option.value (Sys.getenv CLOpt.args_env_var) ~default:"<not found>");
  List.iter ~f:(L.out "anon arg: %s@\n") Config.anon_args;
  List.iter ~f:(L.out "rest arg: %s@\n") Config.rest;
  L.out "Project root = %s@\n" Config.project_root;
  L.out "CWD = %s@\n" (Sys.getcwd ());
  L.out "Driver mode:@\n%a@." pp_driver_mode driver_mode

let driver_mode_of_build_cmd build_cmd =
  match build_cmd with
  | [] ->
      if not (List.is_empty !Config.clang_compilation_db_files) then
        ClangCompilationDB !Config.clang_compilation_db_files
      else
        Analyze
  | prog :: args ->
      match build_system_of_exe_name (Filename.basename prog) with
      | BAnalyze ->
          Analyze
      | BBuck when Config.use_compilation_database <> None ->
          BuckCompilationDB
      | BJava ->
          Javac (Javac.Java, prog, args)
      | BJavac ->
          Javac (Javac.Javac, prog, args)
      | BMvn ->
          Maven (prog, args)
      | BXcode when Config.xcpretty ->
          XcodeXcpretty
      | BAnt | BBuck | BGradle | BMake | BNdk | BXcode as build_system ->
          PythonCapture (build_system, build_cmd)

let get_driver_mode () =
  match Config.generated_classes with
  | _ when Config.maven ->
      (* infer is pretending to be javac in the Maven integration *)
      let build_args = match Array.to_list Sys.argv with
        | _::args -> args
        | [] -> [] in
      Javac (Javac.Javac, "javac", build_args)
  | Some path ->
      BuckGenrule path
  | None ->
      driver_mode_of_build_cmd (IList.rev Config.rest)

let () =
  let driver_mode = get_driver_mode () in
  if not (driver_mode = Analyze || Config.(buck || continue_capture || maven || reactive_mode)) then
    remove_results_dir () ;
  create_results_dir () ;
  (* re-set log files, as default files were in results_dir removed above *)
  L.set_log_file_identifier Config.current_exe None ;
  if Config.print_builtins then Builtin.print_and_exit () ;
  if Config.is_originator then L.do_out "%s@\n" Config.version_string ;
  if Config.debug_mode || Config.stats_mode then log_infer_args driver_mode ;
  (* infer might be called from a Makefile and itself uses `make` to run the analysis in parallel,
     but cannot communicate with the parent make command. Since infer won't interfere with them
     anyway, pretend that we are not called from another make to prevent make falling back to a
     mono-threaded execution. *)
  Unix.unsetenv "MAKEFLAGS";
  register_perf_stats_report () ;
  touch_start_file () ;
  capture driver_mode ;
  analyze driver_mode ;
  if Config.is_originator then (
    StatsAggregator.generate_files () ;
    let in_buck_mode = match driver_mode with | PythonCapture (BBuck, _) -> true | _ -> false in
    if Config.analyzer = Config.Crashcontext then
      Crashcontext.crashcontext_epilogue ~in_buck_mode;
    if in_buck_mode then
      clean_results_dir () ;
    if Config.fail_on_bug then
      fail_on_issue_epilogue () ;
  )
