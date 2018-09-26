(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open PolyVariantEqual

(** entry points for top-level functionalities such as capture, analysis, and reporting *)

module CLOpt = CommandLineOption
module L = Logging
module F = Format

(* based on the build_system and options passed to infer, we run in different driver modes *)
type mode =
  | Analyze
  | BuckGenrule of string
  | BuckCompilationDB of string * string list
  | Clang of Clang.compiler * string * string list
  | ClangCompilationDB of [`Escaped of string | `Raw of string] list
  | Javac of Javac.compiler * string * string list
  | Maven of string * string list
  | PythonCapture of Config.build_system * string list
  | XcodeXcpretty of string * string list
[@@deriving compare]

let equal_mode = [%compare.equal: mode]

let pp_mode fmt = function
  | Analyze ->
      F.fprintf fmt "Analyze driver mode"
  | BuckGenrule prog ->
      F.fprintf fmt "BuckGenRule driver mode:@\nprog = '%s'" prog
  | BuckCompilationDB (prog, args) ->
      F.fprintf fmt "BuckCompilationDB driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args
        args
  | ClangCompilationDB _ ->
      F.fprintf fmt "ClangCompilationDB driver mode"
  | PythonCapture (bs, args) ->
      F.fprintf fmt "PythonCapture driver mode:@\nbuild system = '%s'@\nargs = %a"
        (Config.string_of_build_system bs)
        Pp.cli_args args
  | XcodeXcpretty (prog, args) ->
      F.fprintf fmt "XcodeXcpretty driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Javac (_, prog, args) ->
      F.fprintf fmt "Javac driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Maven (prog, args) ->
      F.fprintf fmt "Maven driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Clang (_, prog, args) ->
      F.fprintf fmt "Clang driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args


(* A clean command for each driver mode to be suggested to the user
   in case nothing got captured. *)
let clean_compilation_command mode =
  match mode with
  | BuckCompilationDB (prog, _) | Clang (_, prog, _) ->
      Some (prog ^ " clean")
  | XcodeXcpretty (prog, args) ->
      Some (String.concat ~sep:" " (List.append (prog :: args) ["clean"]))
  | _ ->
      None


let register_perf_stats_report stats_type =
  let rtime_span, initial_times = (Mtime_clock.counter (), Unix.times ()) in
  PerfStats.register_report (PerfStats.Time (rtime_span, initial_times)) stats_type


(* Clean up the results dir to select only what's relevant to go in the Buck cache. In particular,
   get rid of non-deterministic outputs.*)
let clean_results_dir () =
  if not Config.flavors then
    (* we do not need to keep the capture data in Buck/Java mode *)
    ResultsDatabase.reset_capture_tables () ;
  ResultsDatabase.db_canonicalize () ;
  (* make sure we are done with the database *)
  ResultsDatabase.db_close () ;
  (* In Buck flavors mode we keep all capture data, but in Java mode we keep only the tenv *)
  let should_delete_dir =
    let dirs_to_delete =
      let open Config in
      let common_list =
        [ backend_stats_dir_name
        ; classnames_dir_name
        ; frontend_stats_dir_name
        ; multicore_dir_name
        ; reporting_stats_dir_name ]
      in
      if flavors then common_list else captured_dir_name :: common_list
    in
    List.mem ~equal:String.equal dirs_to_delete
  in
  let should_delete_file =
    let files_to_delete =
      [ Config.log_file
      ; (* some versions of sqlite do not clean up after themselves *)
        ResultsDatabase.database_filename ^ "-shm"
      ; ResultsDatabase.database_filename ^ "-wal" ]
    in
    let suffixes_to_delete = [".txt"; ".csv"; ".json"] in
    fun name ->
      (* Keep the JSON report and the JSON costs report *)
      (not
         (List.exists
            ~f:(String.equal (Filename.basename name))
            [Config.report_json; Config.costs_report_json]))
      && ( List.mem ~equal:String.equal files_to_delete (Filename.basename name)
         || List.exists ~f:(Filename.check_suffix name) suffixes_to_delete )
  in
  let rec delete_temp_results name =
    let rec cleandir dir =
      match Unix.readdir_opt dir with
      | Some entry ->
          if should_delete_dir entry then Utils.rmtree (name ^/ entry)
          else if
            not
              ( String.equal entry Filename.current_dir_name
              || String.equal entry Filename.parent_dir_name )
          then delete_temp_results (name ^/ entry) ;
          cleandir dir (* next entry *)
      | None ->
          Unix.closedir dir
    in
    match Unix.opendir name with
    | dir ->
        cleandir dir
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
        if should_delete_file name then Unix.unlink name ;
        ()
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        ()
  in
  delete_temp_results Config.results_dir


let reset_duplicates_file () =
  let start = Config.results_dir ^/ Config.duplicates_filename in
  let delete () = Unix.unlink start in
  let create () =
    Unix.close (Unix.openfile ~perm:0o0666 ~mode:[Unix.O_CREAT; Unix.O_WRONLY] start)
  in
  if Sys.file_exists start = `Yes then delete () ;
  create ()


let command_error_handling ~always_die ~prog ~args = function
  | Ok _ ->
      ()
  | Error _ as status ->
      let log =
        if (not always_die) && Config.keep_going then
          (* Log error and proceed past the failure when keep going mode is on *)
          L.external_error
        else L.die InternalError
      in
      log "%a:@\n  %s" Pp.cli_args (prog :: args) (Unix.Exit_or_signal.to_string_hum status)


let run_command ~prog ~args ?(cleanup = command_error_handling ~always_die:false ~prog ~args) () =
  Unix.waitpid (Unix.fork_exec ~prog ~argv:(prog :: args) ())
  |> fun status ->
  cleanup status ;
  ok_exn (Unix.Exit_or_signal.or_error status)


let check_xcpretty () =
  match Unix.system "xcpretty --version" with
  | Ok () ->
      ()
  | Error _ ->
      L.user_error
        "@\n\
         xcpretty not found in the path. Please consider installing xcpretty for a more robust \
         integration with xcodebuild. Otherwise use the option --no-xcpretty.@\n\
         @."


let capture_with_compilation_database db_files =
  let root = Unix.getcwd () in
  Config.clang_compilation_dbs :=
    List.map db_files ~f:(function
      | `Escaped fname ->
          `Escaped (Utils.filename_to_absolute ~root fname)
      | `Raw fname ->
          `Raw (Utils.filename_to_absolute ~root fname) ) ;
  let compilation_database = CompilationDatabase.from_json_files db_files in
  CaptureCompilationDatabase.capture_files_in_database compilation_database


let capture ~changed_files = function
  | Analyze ->
      ()
  | BuckCompilationDB (prog, args) ->
      L.progress "Capturing using Buck's compilation database...@." ;
      let json_cdb = CaptureCompilationDatabase.get_compilation_database_files_buck ~prog ~args in
      capture_with_compilation_database ~changed_files json_cdb
  | BuckGenrule path ->
      L.progress "Capturing for Buck genrule compatibility...@." ;
      JMain.from_arguments path
  | Clang (compiler, prog, args) ->
      if CLOpt.is_originator then L.progress "Capturing in make/cc mode...@." ;
      Clang.capture compiler ~prog ~args
  | ClangCompilationDB db_files ->
      L.progress "Capturing using compilation database...@." ;
      capture_with_compilation_database ~changed_files db_files
  | Javac (compiler, prog, args) ->
      if CLOpt.is_originator then L.progress "Capturing in javac mode...@." ;
      Javac.capture compiler ~prog ~args
  | Maven (prog, args) ->
      L.progress "Capturing in maven mode...@." ;
      Maven.capture ~prog ~args
  | PythonCapture (build_system, build_cmd) ->
      register_perf_stats_report PerfStats.TotalFrontend ;
      L.progress "Capturing in %s mode...@." (Config.string_of_build_system build_system) ;
      let in_buck_mode = Config.equal_build_system build_system BBuck in
      let infer_py = Config.lib_dir ^/ "python" ^/ "infer.py" in
      let args =
        List.rev_append Config.anon_args
          ( ( if not (List.is_empty Config.buck_blacklist) then
              ["--blacklist-regex"; "(" ^ String.concat ~sep:")|(" Config.buck_blacklist ^ ")"]
            else [] )
          @ (if not Config.continue_capture then [] else ["--continue"])
          @ ( match Config.force_integration with
            | None ->
                []
            | Some tool ->
                ["--force-integration"; Config.string_of_build_system tool] )
          @ ( match Config.java_jar_compiler with
            | None ->
                []
            | Some p ->
                ["--java-jar-compiler"; p] )
          @ ( match List.rev Config.buck_build_args with
            | args when in_buck_mode ->
                List.map ~f:(fun arg -> ["--Xbuck"; "'" ^ arg ^ "'"]) args |> List.concat
            | _ ->
                [] )
          @ (if not Config.debug_mode then [] else ["--debug"])
          @ (if Config.filtering then [] else ["--no-filtering"])
          @ (if (not Config.flavors) || not in_buck_mode then [] else ["--use-flavors"])
          @ "-j" :: string_of_int Config.jobs
            :: (match Config.load_average with None -> [] | Some l -> ["-l"; string_of_float l])
          @ (if not Config.pmd_xml then [] else ["--pmd-xml"])
          @ ["--project-root"; Config.project_root]
          @ (if not Config.quiet then [] else ["--quiet"])
          @ "--out" :: Config.results_dir
            ::
            ( match Config.xcode_developer_dir with
            | None ->
                []
            | Some d ->
                ["--xcode-developer-dir"; d] )
          @ "--"
            ::
            ( if in_buck_mode && Config.flavors then (
              (* let children infer processes know that they are inside Buck *)
              let infer_args_with_buck =
                String.concat
                  ~sep:(String.of_char CLOpt.env_var_sep)
                  (Option.to_list (Sys.getenv CLOpt.args_env_var) @ ["--buck"])
              in
              Unix.putenv ~key:CLOpt.args_env_var ~data:infer_args_with_buck ;
              let prog, buck_args = (List.hd_exn build_cmd, List.tl_exn build_cmd) in
              let {Buck.command; rev_not_targets; targets} =
                Buck.add_flavors_to_buck_arguments ~filter_kind:`Auto ~dep_depth:None
                  ~extra_flavors:[] buck_args
              in
              let all_args = List.rev_append rev_not_targets targets in
              let updated_buck_cmd =
                [prog; command]
                @ List.rev_append Config.buck_build_args_no_inline
                    (Buck.store_args_in_file all_args)
              in
              Logging.(debug Capture Quiet)
                "Processed buck command '%a'@\n" (Pp.seq F.pp_print_string) updated_buck_cmd ;
              updated_buck_cmd )
            else build_cmd ) )
      in
      if in_buck_mode && Config.flavors then ( RunState.set_merge_capture true ; RunState.store () ) ;
      run_command ~prog:infer_py ~args
        ~cleanup:(function
          | Error (`Exit_non_zero exit_code)
            when Int.equal exit_code Config.infer_py_argparse_error_exit_code ->
              (* swallow infer.py argument parsing error *)
              Config.print_usage_exit ()
          | status ->
              command_error_handling ~always_die:true ~prog:infer_py ~args status)
        () ;
      PerfStats.get_reporter PerfStats.TotalFrontend ()
  | XcodeXcpretty (prog, args) ->
      L.progress "Capturing using xcodebuild and xcpretty...@." ;
      check_xcpretty () ;
      let json_cdb =
        CaptureCompilationDatabase.get_compilation_database_files_xcodebuild ~prog ~args
      in
      capture_with_compilation_database ~changed_files json_cdb


(* shadowed for tracing *)
let capture ~changed_files mode =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"capture" ())) ;
  capture ~changed_files mode ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let execute_analyze ~changed_files =
  register_perf_stats_report PerfStats.TotalBackend ;
  InferAnalyze.main ~changed_files ;
  PerfStats.get_reporter PerfStats.TotalBackend ()


(* shadowed for tracing *)
let execute_analyze ~changed_files =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"analyze" ())) ;
  execute_analyze ~changed_files ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let report ?(suppress_console = false) () =
  let report_json = Config.(results_dir ^/ report_json) in
  InferPrint.main ~report_json:(Some report_json) ;
  (* Post-process the report according to the user config. By default, calls report.py to create a
     human-readable report.

     Do not bother calling the report hook when called from within Buck. *)
  match (Config.buck_cache_mode, Config.report_hook) with
  | true, _ | false, None ->
      ()
  | false, Some prog ->
      let if_true key opt args = if not opt then args else key :: args in
      let bugs_txt = Option.value ~default:(Config.results_dir ^/ "bugs.txt") Config.issues_txt in
      let args =
        if_true "--pmd-xml" Config.pmd_xml
        @@ if_true "--quiet"
             (Config.quiet || suppress_console)
             [ "--issues-json"
             ; report_json
             ; "--issues-txt"
             ; bugs_txt
             ; "--project-root"
             ; Config.project_root
             ; "--results-dir"
             ; Config.results_dir ]
      in
      if is_error (Unix.waitpid (Unix.fork_exec ~prog ~argv:(prog :: args) ())) then
        L.external_error
          "** Error running the reporting script:@\n**   %s %s@\n** See error above@." prog
          (String.concat ~sep:" " args)


(* shadowed for tracing *)
let report ?suppress_console () =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"report" ())) ;
  report ?suppress_console () ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let error_nothing_to_analyze mode =
  let clean_command_opt = clean_compilation_command mode in
  let nothing_to_compile_msg = "Nothing to compile." in
  let please_run_capture_msg =
    match mode with Analyze -> " Have you run `infer capture`?" | _ -> ""
  in
  ( match clean_command_opt with
  | Some clean_command ->
      L.user_warning "%s%s Try running `%s` first.@." nothing_to_compile_msg please_run_capture_msg
        clean_command
  | None ->
      L.user_warning "%s%s Try cleaning the build first.@." nothing_to_compile_msg
        please_run_capture_msg ) ;
  L.progress "There was nothing to analyze.@."


let analyze_and_report ?suppress_console_report ~changed_files mode =
  let should_analyze, should_report =
    match (Config.command, mode) with
    | _, PythonCapture (BBuck, _) when not Config.flavors ->
        (* In Buck mode when compilation db is not used, analysis is invoked from capture if buck flavors are not used *)
        (false, false)
    | _ when Config.infer_is_clang || Config.infer_is_javac ->
        (* Called from another integration to do capture only. *)
        (false, false)
    | (Capture | Compile | Events | Explore | Report | ReportDiff), _ ->
        (false, false)
    | (Analyze | Diff | Run), _ ->
        (true, true)
  in
  let should_merge =
    match mode with
    | PythonCapture (BBuck, _) when Config.flavors && InferCommand.equal Run Config.command ->
        (* if doing capture + analysis of buck with flavors, we always need to merge targets before the analysis phase *)
        true
    | Analyze ->
        RunState.get_merge_capture ()
    | _ ->
        (* else rely on the command line value *) Config.merge
  in
  if should_merge then (
    MergeCapture.merge_captured_targets () ;
    RunState.set_merge_capture false ;
    RunState.store () ) ;
  if should_analyze then
    if SourceFiles.is_empty () && Config.capture then error_nothing_to_analyze mode
    else execute_analyze ~changed_files ;
  if should_report && Config.report then report ?suppress_console:suppress_console_report ()


(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json =
    DB.Results_dir.(path_to_filename Abs_root [Config.report_json]) |> DB.filename_to_string
  in
  match Utils.read_file issues_json with
  | Ok lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat ~sep:"" lines in
      if issues <> [] then L.exit Config.fail_on_issue_exit_code
  | Error error ->
      L.internal_error "Failed to read report file '%s': %s@." issues_json error ;
      ()


let assert_supported_mode required_analyzer requested_mode_string =
  let analyzer_enabled =
    match required_analyzer with
    | `Clang ->
        Version.clang_enabled
    | `Java ->
        Version.java_enabled
    | `Xcode ->
        Version.clang_enabled && Version.xcode_enabled
  in
  if not analyzer_enabled then
    let analyzer_string =
      match required_analyzer with
      | `Clang ->
          "clang"
      | `Java ->
          "java"
      | `Xcode ->
          "clang and xcode"
    in
    L.(die UserError)
      "Unsupported build mode: %s@\n\
       Infer was built with %s analyzers disabled.@ Please rebuild infer with %s enabled.@."
      requested_mode_string analyzer_string analyzer_string


let assert_supported_build_system build_system =
  match (build_system : Config.build_system) with
  | BAnt | BGradle | BJava | BJavac | BMvn ->
      Config.string_of_build_system build_system |> assert_supported_mode `Java
  | BClang | BMake | BNdk ->
      Config.string_of_build_system build_system |> assert_supported_mode `Clang
  | BXcode ->
      Config.string_of_build_system build_system |> assert_supported_mode `Xcode
  | BBuck ->
      let analyzer, build_string =
        if Config.flavors then (`Clang, "buck with flavors")
        else if Option.is_some Config.buck_compilation_database then
          (`Clang, "buck compilation database")
        else (
          if Config.reactive_mode then
            L.user_error
              "WARNING: The reactive analysis mode is not compatible with the Buck integration \
               for Java" ;
          (`Java, Config.string_of_build_system build_system) )
      in
      assert_supported_mode analyzer build_string


let mode_of_build_command build_cmd =
  match build_cmd with
  | [] ->
      if not (List.is_empty !Config.clang_compilation_dbs) then (
        assert_supported_mode `Clang "clang compilation database" ;
        ClangCompilationDB !Config.clang_compilation_dbs )
      else Analyze
  | prog :: args -> (
      let build_system =
        match Config.force_integration with
        | Some build_system when CLOpt.is_originator ->
            build_system
        | _ ->
            Config.build_system_of_exe_name (Filename.basename prog)
      in
      assert_supported_build_system build_system ;
      match (build_system : Config.build_system) with
      | BBuck when Option.is_some Config.buck_compilation_database ->
          BuckCompilationDB (prog, List.append args (List.rev Config.buck_build_args))
      | BClang ->
          Clang (Clang.Clang, prog, args)
      | BMake ->
          Clang (Clang.Make, prog, args)
      | BJava ->
          Javac (Javac.Java, prog, args)
      | BJavac ->
          Javac (Javac.Javac, prog, args)
      | BMvn ->
          Maven (prog, args)
      | BXcode when Config.xcpretty ->
          XcodeXcpretty (prog, args)
      | BBuck when (not Config.flavors) && Config.reactive_mode ->
          L.die UserError "The Buck Java integration does not support --reactive@."
      | BBuck
        when Option.is_none Config.buck_compilation_database && Config.flavors && Config.linters ->
          L.user_warning
            "WARNING: the linters require --buck-compilation-database to be set.@ Alternatively, \
             set --no-linters to disable them and this warning.@." ;
          PythonCapture (BBuck, build_cmd)
      | (BAnt | BBuck | BGradle | BNdk | BXcode) as build_system ->
          PythonCapture (build_system, build_cmd) )


let mode_from_command_line =
  lazy
    ( match Config.generated_classes with
    | _ when Config.infer_is_clang ->
        let prog, args =
          match Array.to_list Sys.argv with prog :: args -> (prog, args) | [] -> assert false
          (* Sys.argv is never empty *)
        in
        Clang (Clang.Clang, prog, args)
    | _ when Config.infer_is_javac ->
        let build_args = match Array.to_list Sys.argv with _ :: args -> args | [] -> [] in
        Javac (Javac.Javac, "javac", build_args)
    | Some path ->
        assert_supported_mode `Java "Buck genrule" ;
        BuckGenrule path
    | None ->
        mode_of_build_command (List.rev Config.rest) )


let run_prologue mode =
  if CLOpt.is_originator then (
    L.environment_info "%a@\n" Config.pp_version () ;
    PerfStats.register_report_at_exit PerfStats.Driver ) ;
  if Config.debug_mode then L.environment_info "Driver mode:@\n%a@." pp_mode mode ;
  if CLOpt.is_originator then (
    if Config.dump_duplicate_symbols then reset_duplicates_file () ;
    (* infer might be called from a Makefile and itself uses `make` to run the analysis in parallel,
     but cannot communicate with the parent make command. Since infer won't interfere with them
     anyway, pretend that we are not called from another make to prevent make falling back to a
     mono-threaded execution. *)
    Unix.unsetenv "MAKEFLAGS" ;
    (* disable the Buck daemon as changes in the Buck or infer config may be missed otherwise *)
    Unix.putenv ~key:"NO_BUCKD" ~data:"1" ) ;
  ()


let run_epilogue mode =
  if CLOpt.is_originator then (
    let in_buck_mode = match mode with PythonCapture (BBuck, _) -> true | _ -> false in
    if Config.developer_mode then StatsAggregator.generate_files () ;
    if Config.crashcontext then Crashcontext.crashcontext_epilogue ~in_buck_mode ;
    if Config.fail_on_bug then fail_on_issue_epilogue () ;
    () ) ;
  if Config.buck_cache_mode then clean_results_dir () ;
  ()


let read_config_changed_files () =
  match Config.changed_files_index with
  | None ->
      None
  | Some index -> (
    match Utils.read_file index with
    | Ok lines ->
        Some (SourceFile.changed_sources_from_changed_files lines)
    | Error error ->
        L.external_error "Error reading the changed files index '%s': %s@." index error ;
        None )
