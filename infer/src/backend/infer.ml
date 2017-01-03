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
  match Unix.opendir name with
  | dir -> (
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
    )
  | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
      Unix.unlink name
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()


type build_mode =
  | Analyze | Ant | Buck | Gradle | Java | Javac | Make | Mvn | Ndk | Xcode

let build_mode_of_string path =
  match Filename.basename path with
  | "analyze" -> Analyze
  | "ant" -> Ant
  | "buck" -> Buck
  | "gradle" | "gradlew" -> Gradle
  | "java" -> Java
  | "javac" -> Javac
  | "cc" | "clang" | "clang++" | "cmake" | "configure" | "g++" | "gcc" | "make" | "waf" -> Make
  | "mvn" -> Mvn
  | "ndk-build" -> Ndk
  | "xcodebuild" -> Xcode
  | cmd -> failwithf "Unsupported build command %s" cmd

let string_of_build_mode = function
  | Analyze -> "analyze"
  | Ant -> "ant"
  | Buck -> "buck"
  | Gradle -> "gradle"
  | Java -> "java"
  | Javac -> "javac"
  | Make -> "make/cc"
  | Mvn -> "maven"
  | Ndk -> "ndk-build"
  | Xcode -> "xcodebuild"


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

let run_javac build_mode build_cmd =
  let build_prog, build_args =
    match build_cmd with
    | prog :: args -> (prog, args)
    | [] -> invalid_arg "run_java: build command cannot be empty" in
  let prog, prog_args =
    match build_mode, Config.java_jar_compiler with
    | _, None -> (build_prog, ["-J-Duser.language=en"])
    | Java, Some jar -> (build_prog, ["-jar"; jar])
    | _, Some jar -> (* fall back to java in PATH to avoid passing -jar to javac *)
        ("java", ["-jar"; jar]) in
  let cli_args, file_args =
    let args =
      "-verbose" :: "-g" ::
      if List.exists build_args ~f:(function "-d" | "-classes_out" -> true | _ -> false)
      then build_args
      else "-d" :: Config.javac_classes_out :: build_args in
    List.partition_tf args ~f:(fun arg ->
        (* As mandated by javac, argument files must not contain certain arguments. *)
        String.is_prefix ~prefix:"-J" arg || String.is_prefix ~prefix:"@" arg) in
  (* Pass non-special args via a file to avoid exceeding the command line size limit. *)
  let args_file =
    let file = Filename.temp_file "args_" "" in
    let quoted_file_args =
      List.map file_args ~f:(fun arg ->
          if String.contains arg '\'' then arg else F.sprintf "'%s'" arg) in
    Out_channel.with_file file ~f:(fun oc -> Out_channel.output_lines oc quoted_file_args) ;
    file in
  let cli_file_args = cli_args @ ["@" ^ args_file] in
  let args = prog_args @ cli_file_args in
  let verbose_out_file = Filename.temp_file "javac_" ".out" in
  Unix.with_file verbose_out_file ~mode:[Unix.O_WRONLY] ~f:(
    fun verbose_out_fd ->
      try
        Unix_.fork_redirect_exec_wait ~prog ~args ~stderr:verbose_out_fd ()
      with exn ->
      try
        Unix_.fork_redirect_exec_wait ~prog:"javac" ~args:cli_file_args ~stderr:verbose_out_fd ()
      with _ ->
        L.stderr "Failed to execute: %s %s@\nSee contents of %s.@\n"
          prog (String.concat ~sep:" " args) verbose_out_file ;
        raise exn
  );
  verbose_out_file

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

let capture build_cmd build_mode =
  match build_mode, Config.generated_classes with
  | _, Some path ->
      L.stdout "Capturing for Buck genrule compatibility...@\n";
      JMain.main (lazy (JClasspath.load_from_arguments path))
  | Analyze, _ when not (List.is_empty !Config.clang_compilation_db_files) ->
      capture_with_compilation_database !Config.clang_compilation_db_files
  | Analyze, _ ->
      ()
  | Buck, _ when Config.use_compilation_database <> None ->
      L.stdout "Capturing using Buck's compilation database...@\n";
      let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_buck () in
      capture_with_compilation_database json_cdb
  | (Java | Javac), _ ->
      let verbose_out_file = run_javac build_mode build_cmd in
      if Config.analyzer <> Config.Compile then
        JMain.main (lazy (JClasspath.load_from_verbose_output verbose_out_file)) ;
      Unix.unlink verbose_out_file
  | Xcode, _ when Config.xcpretty ->
      L.stdout "Capturing using xcpretty...@\n";
      check_xcpretty ();
      let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_xcodebuild () in
      capture_with_compilation_database json_cdb
  | build_mode, _ ->
      L.stdout "Capturing in %s mode...@." (string_of_build_mode build_mode);
      let in_buck_mode = build_mode = Buck in
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

let analyze = function
  | Buck when Config.use_compilation_database = None ->
      (* In Buck mode when compilation db is not used, analysis is invoked either from capture or a
         separate Analyze invocation is necessary, depending on the buck flavor used. *)
      ()
  | _ ->
      if (Sys.file_exists Config.(results_dir ^/ captured_dir_name)) <> `Yes then (
        L.stderr "There was nothing to analyze, exiting" ;
        Config.print_usage_exit ()
      );
      (match Config.analyzer with
       | Infer | Eradicate | Checkers | Tracing | Crashcontext | Quandary | Threadsafety ->
           execute_analyze () ;
           report ()
       | Linters ->
           report ()
       | Capture | Compile ->
           ()
      )

(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json = DB.Results_dir.(path_to_filename Abs_root ["report.json"]) in
  match Utils.read_file (DB.filename_to_string issues_json) with
  | Some lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat ~sep:"" lines in
      if issues <> [] then exit Config.fail_on_issue_exit_code
  | None -> ()

let () =
  let build_cmd = IList.rev Config.rest in
  let build_mode = match build_cmd with path :: _ -> build_mode_of_string path | [] -> Analyze in
  if not (build_mode = Analyze || Config.(buck || continue_capture || reactive_mode)) then
    remove_results_dir () ;
  create_results_dir () ;
  (* re-set log files, as default files were in results_dir removed above *)
  L.set_log_file_identifier Config.current_exe None ;
  if Config.print_builtins then Builtin.print_and_exit () ;
  if Config.is_originator then L.do_out "%s@\n" Config.version_string ;
  (* infer might be called from a Makefile and itself uses `make` to run the analysis in parallel,
     but cannot communicate with the parent make command. Since infer won't interfere with them
     anyway, pretend that we are not called from another make to prevent make falling back to a
     mono-threaded execution. *)
  Unix.unsetenv "MAKEFLAGS";
  register_perf_stats_report () ;
  touch_start_file () ;
  capture build_cmd build_mode ;
  analyze build_mode ;
  if Config.is_originator then (
    StatsAggregator.generate_files () ;
    if Config.analyzer = Config.Crashcontext then
      Crashcontext.crashcontext_epilogue ~in_buck_mode:(build_mode = Buck);
    if build_mode = Buck then
      clean_results_dir () ;
    if Config.fail_on_bug then
      fail_on_issue_epilogue () ;
  )
