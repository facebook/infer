(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  | AnalyzeJson
  | Ant of {prog: string; args: string list}
  | BuckClangFlavor of {build_cmd: string list}
  | BuckCompilationDB of {deps: BuckMode.clang_compilation_db_deps; prog: string; args: string list}
  | BuckGenrule of {prog: string}
  | BuckJavaFlavor of {build_cmd: string list}
  | Clang of {compiler: Clang.compiler; prog: string; args: string list}
  | ClangCompilationDB of {db_files: [`Escaped of string | `Raw of string] list}
  | Gradle of {prog: string; args: string list}
  | Javac of {compiler: Javac.compiler; prog: string; args: string list}
  | Maven of {prog: string; args: string list}
  | NdkBuild of {build_cmd: string list}
  | XcodeBuild of {prog: string; args: string list}
  | XcodeXcpretty of {prog: string; args: string list}

let is_analyze_mode = function Analyze -> true | _ -> false

let pp_mode fmt = function
  | Analyze ->
      F.fprintf fmt "Analyze driver mode"
  | AnalyzeJson ->
      F.fprintf fmt "Analyze json mode"
  | Ant {prog; args} ->
      F.fprintf fmt "Ant driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | BuckClangFlavor {build_cmd} ->
      F.fprintf fmt "BuckClangFlavor driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | BuckCompilationDB {deps; prog; args} ->
      F.fprintf fmt "BuckCompilationDB driver mode:@\nprog = '%s'@\nargs = %a@\ndeps = %a" prog
        Pp.cli_args args BuckMode.pp_clang_compilation_db_deps deps
  | BuckGenrule {prog} ->
      F.fprintf fmt "BuckGenRule driver mode:@\nprog = '%s'" prog
  | BuckJavaFlavor {build_cmd} ->
      F.fprintf fmt "BuckJavaFlavor driver mode:@\nbuild command = %a" Pp.cli_args build_cmd
  | Clang {prog; args} ->
      F.fprintf fmt "Clang driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | ClangCompilationDB _ ->
      F.fprintf fmt "ClangCompilationDB driver mode"
  | Gradle {prog; args} ->
      F.fprintf fmt "Gradle driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Javac {prog; args} ->
      F.fprintf fmt "Javac driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Maven {prog; args} ->
      F.fprintf fmt "Maven driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | NdkBuild {build_cmd} ->
      F.fprintf fmt "NdkBuild driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | XcodeBuild {prog; args} ->
      F.fprintf fmt "XcodeBuild driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | XcodeXcpretty {prog; args} ->
      F.fprintf fmt "XcodeXcpretty driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args


(* A clean command for each driver mode to be suggested to the user
   in case nothing got captured. *)
let clean_compilation_command mode =
  match mode with
  | BuckCompilationDB {prog} | Clang {prog} ->
      Some (prog ^ " clean")
  | XcodeXcpretty {prog; args} ->
      Some (String.concat ~sep:" " (List.append (prog :: args) ["clean"]))
  | _ ->
      None


let reset_duplicates_file () =
  let start = ResultsDir.get_path DuplicateFunctions in
  let delete () = Unix.unlink start in
  let create () =
    Unix.close (Unix.openfile ~perm:0o0666 ~mode:[Unix.O_CREAT; Unix.O_WRONLY] start)
  in
  if Sys.file_exists start = `Yes then delete () ;
  create ()


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


let capture ~changed_files = function
  | Analyze | AnalyzeJson ->
      ()
  | Ant {prog; args} ->
      L.progress "Capturing in ant mode...@." ;
      Ant.capture ~prog ~args
  | BuckClangFlavor {build_cmd} ->
      L.progress "Capturing in buck mode...@." ;
      BuckFlavors.capture build_cmd
  | BuckCompilationDB {deps; prog; args} ->
      L.progress "Capturing using Buck's compilation database...@." ;
      let db_files =
        CaptureCompilationDatabase.get_compilation_database_files_buck deps ~prog ~args
      in
      CaptureCompilationDatabase.capture ~changed_files ~db_files
  | BuckGenrule {prog} ->
      L.progress "Capturing for Buck genrule compatibility...@." ;
      JMain.from_arguments prog
  | BuckJavaFlavor {build_cmd} ->
      L.progress "Capturing for BuckJavaFlavor integration...@." ;
      BuckJavaFlavor.capture build_cmd
  | Clang {compiler; prog; args} ->
      if CLOpt.is_originator then L.progress "Capturing in make/cc mode...@." ;
      Clang.capture compiler ~prog ~args
  | ClangCompilationDB {db_files} ->
      L.progress "Capturing using compilation database...@." ;
      CaptureCompilationDatabase.capture ~changed_files ~db_files
  | Gradle {prog; args} ->
      L.progress "Capturing in gradle mode...@." ;
      Gradle.capture ~prog ~args
  | Javac {compiler; prog; args} ->
      if CLOpt.is_originator then L.progress "Capturing in javac mode...@." ;
      Javac.capture compiler ~prog ~args
  | Maven {prog; args} ->
      L.progress "Capturing in maven mode...@." ;
      Maven.capture ~prog ~args
  | NdkBuild {build_cmd} ->
      L.progress "Capturing in ndk-build mode...@." ;
      NdkBuild.capture ~build_cmd
  | XcodeBuild {prog; args} ->
      L.progress "Capturing in xcodebuild mode...@." ;
      XcodeBuild.capture ~prog ~args
  | XcodeXcpretty {prog; args} ->
      L.progress "Capturing using xcodebuild and xcpretty...@." ;
      check_xcpretty () ;
      let db_files =
        CaptureCompilationDatabase.get_compilation_database_files_xcodebuild ~prog ~args
      in
      CaptureCompilationDatabase.capture ~changed_files ~db_files


(* shadowed for tracing *)
let capture ~changed_files mode =
  GCStats.log_f ~name:"capture" Capture
  @@ fun () ->
  ScubaLogging.execute_with_time_logging "capture"
  @@ fun () ->
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"capture" ())) ;
  capture ~changed_files mode ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let execute_analyze ~changed_files =
  GCStats.log_f ~name:"analysis_scheduler" Analysis
  @@ fun () ->
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"analyze" ())) ;
  InferAnalyze.main ~changed_files ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


let execute_analyze_json () =
  match (Config.cfg_json, Config.tenv_json) with
  | Some cfg_json, Some tenv_json ->
      InferAnalyzeJson.analyze_json cfg_json tenv_json
  | _, _ ->
      L.user_warning
        "** Missing cfg or tenv json files. Provide them as arguments throught '--cfg-json' and \
         '--tenv-json' **\n" ;
      ()


let report ?(suppress_console = false) () =
  let issues_json = ResultsDir.get_path ReportJson in
  let costs_json = ResultsDir.get_path ReportCostsJson in
  let config_impact_json = ResultsDir.get_path ReportConfigImpactJson in
  JsonReports.write_reports ~issues_json ~costs_json ~config_impact_json ;
  (* Post-process the report according to the user config. By default, calls report.py to create a
     human-readable report.

     Do not bother calling the report hook when called from within Buck. *)
  if not Config.buck_cache_mode then (
    (* Create a dummy bugs.txt file for backwards compatibility. TODO: Stop doing that one day. *)
    Utils.with_file_out (Config.results_dir ^/ "bugs.txt") ~f:(fun outc ->
        Out_channel.output_string outc "The contents of this file have moved to report.txt.\n" ) ;
    TextReport.create_from_json
      ~quiet:(Config.quiet || suppress_console)
      ~console_limit:Config.report_console_limit ~report_txt:(ResultsDir.get_path ReportText)
      ~report_json:issues_json ;
    if Config.pmd_xml then
      XMLReport.write ~xml_path:(ResultsDir.get_path ReportXML) ~json_path:issues_json ) ;
  if Config.(test_determinator && process_clang_ast) then
    TestDeterminator.merge_test_determinator_results () ;
  ()


(* shadowed for tracing *)
let report ?suppress_console () =
  GCStats.log_f ~name:"report" Analysis
  @@ fun () ->
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
    | _, BuckClangFlavor _ when not (Option.exists ~f:BuckMode.is_clang_flavors Config.buck_mode) ->
        (* In Buck mode when compilation db is not used, analysis is invoked from capture if buck flavors are not used *)
        (false, false)
    | _ when Config.infer_is_clang || Config.infer_is_javac ->
        (* Called from another integration to do capture only. *)
        (false, false)
    | (Capture | Compile | Debug | Explore | Help | Report | ReportDiff), _ ->
        (false, false)
    | (Analyze | AnalyzeJson | Run), _ ->
        (true, true)
  in
  let should_analyze = should_analyze && Config.capture in
  let should_merge =
    match mode with
    | _ when Config.merge ->
        (* [--merge] overrides other behaviors *)
        true
    | BuckClangFlavor _
      when Option.exists ~f:BuckMode.is_clang_flavors Config.buck_mode
           && InferCommand.equal Run Config.command ->
        (* if doing capture + analysis of buck with flavors, we always need to merge targets before the analysis phase *)
        true
    | Analyze | BuckJavaFlavor _ | Gradle _ ->
        ResultsDir.RunState.get_merge_capture ()
    | AnalyzeJson ->
        false
    | _ ->
        false
  in
  let analyze_json = match mode with AnalyzeJson -> true | _ -> false in
  if should_merge then (
    if Config.export_changed_functions then MergeCapture.merge_changed_functions () ;
    MergeCapture.merge_captured_targets () ;
    ResultsDir.RunState.set_merge_capture false ) ;
  if should_analyze then
    if analyze_json then execute_analyze_json ()
    else if SourceFiles.is_empty () && Config.capture then error_nothing_to_analyze mode
    else (
      execute_analyze ~changed_files ;
      if Config.starvation_whole_program then StarvationGlobalAnalysis.whole_program_analysis () ) ;
  if should_report && Config.report then report ?suppress_console:suppress_console_report ()


let analyze_and_report ?suppress_console_report ~changed_files mode =
  ScubaLogging.execute_with_time_logging "analyze_and_report" (fun () ->
      analyze_and_report ?suppress_console_report ~changed_files mode )


(** as the Config.fail_on_bug flag mandates, exit with error when an issue is reported *)
let fail_on_issue_epilogue () =
  let issues_json = ResultsDir.get_path ReportJson in
  match Utils.read_file issues_json with
  | Ok lines ->
      let issues = Jsonbug_j.report_of_string @@ String.concat ~sep:"" lines in
      if not (List.is_empty issues) then L.exit Config.fail_on_issue_exit_code
  | Error error ->
      L.internal_error "Failed to read report file '%s': %s@." issues_json error ;
      ()


let assert_supported_mode required_analyzer requested_mode_string =
  let analyzer_enabled =
    match required_analyzer with
    | `Clang ->
        Version.clang_enabled
    | `ClangJava ->
        Version.clang_enabled && Version.java_enabled
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
      | `ClangJava ->
          "clang & java"
      | `Java ->
          "java"
      | `Xcode ->
          "clang and xcode"
    in
    L.(die UserError)
      "Unsupported build mode: %s@\n\
       Infer was built with %s analyzers disabled.@ Please rebuild infer with %s enabled.@."
      requested_mode_string analyzer_string analyzer_string


let error_no_buck_mode_specified () =
  L.die UserError
    "`buck` command detected on the command line but no Buck integration has been selected. Please \
     specify `--buck-clang`, `--buck-java`, or `--buck-compilation-database`. See `infer capture \
     --help` for more information."


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
        match Config.buck_mode with
        | None ->
            error_no_buck_mode_specified ()
        | Some ClangFlavors ->
            (`Clang, "buck with flavors")
        | Some (ClangCompilationDB _) ->
            (`Clang, "buck compilation database")
        | Some JavaFlavor ->
            (`Java, Config.string_of_build_system build_system)
      in
      assert_supported_mode analyzer build_string


let mode_of_build_command build_cmd (buck_mode : BuckMode.t option) =
  match build_cmd with
  | [] ->
      if not (List.is_empty Config.clang_compilation_dbs) then (
        assert_supported_mode `Clang "clang compilation database" ;
        ClangCompilationDB {db_files= Config.clang_compilation_dbs} )
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
      match ((build_system : Config.build_system), buck_mode) with
      | BAnt, _ ->
          Ant {prog; args}
      | BBuck, None ->
          error_no_buck_mode_specified ()
      | BBuck, Some (ClangCompilationDB deps) ->
          BuckCompilationDB {deps; prog; args= List.append args Config.buck_build_args}
      | BBuck, Some ClangFlavors when Config.is_checker_enabled Linters ->
          L.user_warning
            "WARNING: the linters require --buck-compilation-database to be set.@ Alternatively, \
             set --no-linters to disable them and this warning.@." ;
          BuckClangFlavor {build_cmd}
      | BBuck, Some JavaFlavor ->
          BuckJavaFlavor {build_cmd}
      | BBuck, Some ClangFlavors ->
          BuckClangFlavor {build_cmd}
      | BClang, _ ->
          Clang {compiler= Clang.Clang; prog; args}
      | BGradle, _ ->
          Gradle {prog; args}
      | BJava, _ ->
          Javac {compiler= Javac.Java; prog; args}
      | BJavac, _ ->
          Javac {compiler= Javac.Javac; prog; args}
      | BMake, _ ->
          Clang {compiler= Clang.Make; prog; args}
      | BMvn, _ ->
          Maven {prog; args}
      | BNdk, _ ->
          NdkBuild {build_cmd}
      | BXcode, _ when Config.xcpretty ->
          XcodeXcpretty {prog; args}
      | BXcode, _ ->
          XcodeBuild {prog; args} )


let mode_from_command_line =
  lazy
    ( match Config.generated_classes with
    | _ when Config.infer_is_clang ->
        let prog, args =
          match Array.to_list (Sys.get_argv ()) with
          | prog :: args ->
              (prog, args)
          | [] ->
              assert false
          (* Sys.argv is never empty *)
        in
        Clang {compiler= Clang.Clang; prog; args}
    | _ when Config.infer_is_javac ->
        let build_args =
          match Array.to_list (Sys.get_argv ()) with _ :: args -> args | [] -> []
        in
        Javac {compiler= Javac.Javac; prog= "javac"; args= build_args}
    | Some path ->
        assert_supported_mode `Java "Buck genrule" ;
        BuckGenrule {prog= path}
    | None ->
        mode_of_build_command Config.rest Config.buck_mode )


let run_prologue mode =
  if CLOpt.is_originator then L.environment_info "%a@\n" Config.pp_version () ;
  if Config.debug_mode then L.environment_info "Driver mode:@\n%a@." pp_mode mode ;
  if CLOpt.is_originator && Config.dump_duplicate_symbols then reset_duplicates_file () ;
  ()


let run_prologue mode =
  ScubaLogging.execute_with_time_logging "run_prologue" (fun () -> run_prologue mode)


let run_epilogue () =
  if CLOpt.is_originator then (
    if Config.fail_on_bug then fail_on_issue_epilogue () ;
    () ) ;
  if Config.buck_cache_mode then ResultsDir.scrub_for_caching () ;
  ()


let run_epilogue () =
  GCStats.log ~name:"main_process_full" Analysis (GCStats.get ~since:ProgramStart) ;
  ScubaLogging.execute_with_time_logging "run_epilogue" run_epilogue


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
