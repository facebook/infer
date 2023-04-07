(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** entry points for top-level functionalities such as capture, analysis, and reporting *)

module L = Logging
module F = Format

(* based on the build_system and options passed to infer, we run in different driver modes *)
type mode =
  | Analyze
  | Ant of {prog: string; args: string list}
  | Buck2Clang of {build_cmd: string list}
  | Buck2Java of {build_cmd: string list}
  | BuckClangFlavor of {build_cmd: string list}
  | BuckCompilationDB of {deps: BuckMode.clang_compilation_db_deps; prog: string; args: string list}
  | BuckErlang of {prog: string; args: string list}
  | BuckGenrule of {prog: string}
  | BuckJavaFlavor of {build_cmd: string list}
  | BxlClang of {build_cmd: string list}
  | BxlClangFile
  | Clang of {compiler: Clang.compiler; prog: string; args: string list}
  | ClangCompilationDB of {db_files: [`Escaped of string | `Raw of string] list}
  | Gradle of {prog: string; args: string list}
  | Javac of {compiler: Javac.compiler; prog: string; args: string list}
  | JsonSIL of {cfg_json: string; tenv_json: string}
  | Maven of {prog: string; args: string list}
  | NdkBuild of {build_cmd: string list}
  | Rebar3 of {args: string list}
  | Erlc of {args: string list}
  | Hackc of {prog: string; args: string list}
  | Textual of {textualfiles: string list}
  | XcodeBuild of {prog: string; args: string list}
  | XcodeXcpretty of {prog: string; args: string list}

let is_analyze_mode = function Analyze -> true | _ -> false

let is_compatible_with_textual_generation = function Javac _ -> true | _ -> false

let pp_mode fmt = function
  | Analyze ->
      F.fprintf fmt "Analyze driver mode"
  | Ant {prog; args} ->
      F.fprintf fmt "Ant driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Buck2Clang {build_cmd} ->
      F.fprintf fmt "Buck2/Clang driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | Buck2Java {build_cmd} ->
      F.fprintf fmt "Buck2/Java driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | BuckClangFlavor {build_cmd} ->
      F.fprintf fmt "BuckClangFlavor driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | BuckCompilationDB {deps; prog; args} ->
      F.fprintf fmt "BuckCompilationDB driver mode:@\nprog = '%s'@\nargs = %a@\ndeps = %a" prog
        Pp.cli_args args BuckMode.pp_clang_compilation_db_deps deps
  | BuckErlang {prog; args} ->
      F.fprintf fmt "BuckErlang driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | BuckGenrule {prog} ->
      F.fprintf fmt "BuckGenRule driver mode:@\nprog = '%s'" prog
  | BuckJavaFlavor {build_cmd} ->
      F.fprintf fmt "BuckJavaFlavor driver mode:@\nbuild command = %a" Pp.cli_args build_cmd
  | BxlClang {build_cmd} ->
      F.fprintf fmt "BxlClang driver mode:@\nbuild command = %a" Pp.cli_args build_cmd
  | BxlClangFile ->
      F.fprintf fmt "BxlClang file driver mode"
  | Clang {prog; args} ->
      F.fprintf fmt "Clang driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | ClangCompilationDB _ ->
      F.fprintf fmt "ClangCompilationDB driver mode"
  | Gradle {prog; args} ->
      F.fprintf fmt "Gradle driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Javac {prog; args} ->
      F.fprintf fmt "Javac driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | JsonSIL {cfg_json; tenv_json} ->
      F.fprintf fmt "Json driver mode:@\ncfg_json= '%s'@\ntenv_json = %s" cfg_json tenv_json
  | Maven {prog; args} ->
      F.fprintf fmt "Maven driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | NdkBuild {build_cmd} ->
      F.fprintf fmt "NdkBuild driver mode: build_cmd = %a" Pp.cli_args build_cmd
  | Rebar3 {args} ->
      F.fprintf fmt "Rebar3 driver mode:@\nargs = %a" Pp.cli_args args
  | Erlc {args} ->
      F.fprintf fmt "Erlc driver mode:@\nargs = %a" Pp.cli_args args
  | Hackc {prog; args} ->
      F.fprintf fmt "Hackc driver mode:@\nprog = '%s'@\nargs = %a" prog Pp.cli_args args
  | Textual {textualfiles} -> (
    match textualfiles with
    | [] ->
        ()
    | _ :: _ ->
        F.fprintf fmt "Textual capture mode:@\nfiles = %a" Pp.cli_args textualfiles )
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
  let create () = Unix.close (Unix.openfile ~perm:0o0666 ~mode:[O_CREAT; O_WRONLY] start) in
  if ISys.file_exists start then delete () ;
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


let capture ~changed_files mode =
  ( if not (List.is_empty Config.merge_capture) then (
      let expanded_args = Utils.inline_argument_files Config.merge_capture in
      let infer_deps_file = ResultsDir.get_path CaptureDependencies in
      List.map expanded_args ~f:(fun dir -> Printf.sprintf "-\t-\t%s" dir)
      |> Out_channel.write_lines infer_deps_file ;
      () )
    else
      let dolifiles = Config.capture_doli in
      ( match dolifiles with
      | [] ->
          ()
      | _ :: _ ->
          let dfiles =
            List.map dolifiles ~f:(fun x -> TextualParser.TextualFile.StandaloneFile x)
          in
          TextualParser.capture ~capture:DoliCapture dfiles ) ;
      match mode with
      | Analyze ->
          ()
      | Ant {prog; args} ->
          L.progress "Capturing in ant mode...@." ;
          Ant.capture ~prog ~args
      | Buck2Clang {build_cmd} ->
          L.progress "Capturing in buck2/clang mode...@." ;
          Buck2Clang.capture build_cmd
      | Buck2Java {build_cmd} ->
          L.progress "Capturing in buck2/java mode...@." ;
          Buck2Java.capture build_cmd
      | BuckClangFlavor {build_cmd} ->
          L.progress "Capturing in buck mode...@." ;
          BuckFlavors.capture build_cmd
      | BuckCompilationDB {deps; prog; args} ->
          L.progress "Capturing using Buck's compilation database...@." ;
          let db_files =
            CaptureCompilationDatabase.get_compilation_database_files_buck deps ~prog ~args
          in
          CaptureCompilationDatabase.capture ~changed_files ~db_files
      | BuckErlang {prog; args} ->
          L.progress "Capturing Erlang using Buck...@." ;
          Erlang.capture_buck ~command:prog ~args
      | BuckGenrule {prog} ->
          L.progress "Capturing for Buck genrule compatibility...@." ;
          JMain.from_arguments prog
      | BuckJavaFlavor {build_cmd} ->
          L.progress "Capturing for BuckJavaFlavor integration...@." ;
          BuckJavaFlavor.capture build_cmd
      | BxlClang {build_cmd} ->
          L.progress "Capturing in bxl/clang mode...@." ;
          BxlClang.capture build_cmd
      | BxlClangFile ->
          L.progress "Capturing in bxl/clang file mode...@." ;
          BxlClang.file_capture ()
      | Clang {compiler; prog; args} ->
          if Config.is_originator then L.progress "Capturing in make/cc mode...@." ;
          Clang.capture compiler ~prog ~args
      | ClangCompilationDB {db_files} ->
          L.progress "Capturing using compilation database...@." ;
          CaptureCompilationDatabase.capture ~changed_files ~db_files
      | Gradle {prog; args} ->
          L.progress "Capturing in gradle mode...@." ;
          Gradle.capture ~prog ~args
      | Javac {compiler; prog; args} ->
          if Config.is_originator then L.progress "Capturing in javac mode...@." ;
          Javac.capture compiler ~prog ~args
      | JsonSIL {cfg_json; tenv_json} ->
          L.progress "Capturing using JSON mode...@." ;
          CaptureSILJson.capture ~cfg_json ~tenv_json
      | Maven {prog; args} ->
          L.progress "Capturing in maven mode...@." ;
          Maven.capture ~prog ~args
      | NdkBuild {build_cmd} ->
          L.progress "Capturing in ndk-build mode...@." ;
          NdkBuild.capture ~build_cmd
      | Rebar3 {args} ->
          L.progress "Capturing in rebar3 mode...@." ;
          Erlang.capture ~command:"rebar3" ~args
      | Erlc {args} ->
          L.progress "Capturing in erlc mode...@." ;
          Erlang.capture ~command:"erlc" ~args
      | Hackc {prog; args} ->
          L.progress "Capturing in hackc mode...@." ;
          Hack.capture ~prog ~args
      | Textual {textualfiles} ->
          let tfiles =
            List.map textualfiles ~f:(fun x -> TextualParser.TextualFile.StandaloneFile x)
          in
          TextualParser.capture ~capture:TextualCapture tfiles
      | XcodeBuild {prog; args} ->
          L.progress "Capturing in xcodebuild mode...@." ;
          XcodeBuild.capture ~prog ~args
      | XcodeXcpretty {prog; args} ->
          L.progress "Capturing using xcodebuild and xcpretty...@." ;
          check_xcpretty () ;
          let db_files =
            CaptureCompilationDatabase.get_compilation_database_files_xcodebuild ~prog ~args
          in
          CaptureCompilationDatabase.capture ~changed_files ~db_files ) ;
  let should_merge =
    match mode with
    | Buck2Clang _
    | Buck2Java _
    | BuckClangFlavor _
    | BuckJavaFlavor _
    | BxlClang _
    | BxlClangFile
    | Gradle _ ->
        true
    | _ ->
        not (List.is_empty Config.merge_capture)
  in
  if should_merge then (
    if Config.export_changed_functions then MergeCapture.merge_changed_functions () ;
    let root =
      match mode with
      | Buck2Clang _ | BxlClang _ | BxlClangFile ->
          Config.buck2_root
      | _ ->
          Config.project_root
    in
    MergeCapture.merge_captured_targets ~root )


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


let report () =
  let issues_json = ResultsDir.get_path ReportJson in
  let costs_json = ResultsDir.get_path ReportCostsJson in
  let config_impact_json = ResultsDir.get_path ReportConfigImpactJson in
  JsonReports.write_reports ~issues_json ~costs_json ~config_impact_json ;
  (* Post-process the report according to the user config.
     Do not bother calling the report hook when called from within Buck. *)
  if not Config.buck_cache_mode then (
    TextReport.create_from_json ~quiet:Config.quiet ~console_limit:Config.report_console_limit
      ~report_txt:(ResultsDir.get_path ReportText) ~report_json:issues_json ;
    if Config.pmd_xml then
      XMLReport.write ~xml_path:(ResultsDir.get_path ReportXML) ~json_path:issues_json ;
    if Config.sarif then
      SarifReport.create_from_json ~report_sarif:(ResultsDir.get_path ReportSarif)
        ~report_json:issues_json ;
    if Config.is_checker_enabled Checker.Datalog then
      DatalogFacts.create_from_json
        ~datalog_dir:(ResultsDir.get_path DatalogFacts)
        ~report_json:issues_json ;
    () ) ;
  if Config.export_changed_functions then TestDeterminator.merge_changed_functions_results () ;
  if Config.(test_determinator && process_clang_ast) then
    TestDeterminator.merge_test_determinator_results () ;
  ()


(* shadowed for tracing *)
let report () =
  GCStats.log_f ~name:"report" Analysis
  @@ fun () ->
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"report" ())) ;
  report () ;
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


let analyze_and_report ~changed_files mode =
  match (Config.command, mode) with
  | _, BuckClangFlavor _ when not (Option.exists ~f:BuckMode.is_clang Config.buck_mode) ->
      (* In Buck mode when compilation db is not used, analysis is invoked from capture if buck flavors are not used *)
      ()
  | _ when Config.infer_is_clang || Config.infer_is_javac ->
      (* Called from another integration to do capture only. *) ()
  | (Capture | Compile | Debug | Explore | Help | Report | ReportDiff), _ ->
      ()
  | (Analyze | Run), _ when Config.invalidate_only ->
      ()
  | (Analyze | Run), _ ->
      if Config.capture then
        if SourceFiles.is_empty () then error_nothing_to_analyze mode
        else (
          execute_analyze ~changed_files ;
          if Config.starvation_whole_program then StarvationGlobalAnalysis.whole_program_analysis () ;
          if Config.shrink_analysis_db then DBWriter.shrink_analysis_db () ) ;
      if Config.report then report ()


let analyze_and_report ~changed_files mode =
  ScubaLogging.execute_with_time_logging "analyze_and_report" (fun () ->
      analyze_and_report ~changed_files mode )


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
    | `Erlang ->
        Version.erlang_enabled
    | `Hack ->
        Version.hack_enabled
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
      | `Erlang ->
          "erlang"
      | `Hack ->
          "hack"
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
     specify `--buck-clang`, `--buck-java`, `--buck-erlang`, or `--buck-compilation-database`. See \
     `infer capture --help` for more information."


let assert_supported_build_system build_system =
  match (build_system : Config.build_system) with
  | BAnt | BGradle | BJava | BJavac | BMvn ->
      Config.string_of_build_system build_system |> assert_supported_mode `Java
  | BClang | BMake | BNdk ->
      Config.string_of_build_system build_system |> assert_supported_mode `Clang
  | BRebar3 ->
      Config.string_of_build_system build_system |> assert_supported_mode `Erlang
  | BErlc ->
      Config.string_of_build_system build_system |> assert_supported_mode `Erlang
  | BHackc ->
      Config.string_of_build_system build_system |> assert_supported_mode `Hack
  | BXcode ->
      Config.string_of_build_system build_system |> assert_supported_mode `Xcode
  | BBuck ->
      let analyzer, build_string =
        match Config.buck_mode with
        | None ->
            error_no_buck_mode_specified ()
        | Some Clang ->
            (`Clang, "buck with flavors")
        | Some (ClangCompilationDB _) ->
            (`Clang, "buck compilation database")
        | Some Java ->
            (`Java, Config.string_of_build_system build_system)
        | Some Erlang ->
            L.die UserError "Unsupported buck2 integration."
      in
      assert_supported_mode analyzer build_string
  | BBuck2 ->
      let analyzer, build_string =
        match Config.buck_mode with
        | None ->
            error_no_buck_mode_specified ()
        | Some Clang ->
            (`Clang, Config.string_of_build_system build_system)
        | Some Erlang ->
            (`Erlang, Config.string_of_build_system build_system)
        | Some Java ->
            (`Java, Config.string_of_build_system build_system)
        | Some (ClangCompilationDB _) ->
            L.die UserError "Unsupported buck2 integration."
      in
      assert_supported_mode analyzer build_string


let mode_of_build_command build_cmd (buck_mode : BuckMode.t option) =
  match build_cmd with
  | [] when Config.bxl_file_capture ->
      BxlClangFile
  | [] -> (
      let textualfiles = Config.capture_textual in
      match (Config.clang_compilation_dbs, textualfiles) with
      | _ :: _, _ :: _ ->
          L.die UserError "Both --clang-compilation-dbs and --capture-textual are set."
      | _ :: _, [] ->
          assert_supported_mode `Clang "clang compilation database" ;
          ClangCompilationDB {db_files= Config.clang_compilation_dbs}
      | [], _ :: _ ->
          Textual {textualfiles}
      | [], [] -> (
        match (Config.cfg_json, Config.tenv_json) with
        | Some cfg_json, Some tenv_json ->
            JsonSIL {cfg_json; tenv_json}
        | _ ->
            Analyze ) )
  | prog :: args -> (
      let build_system =
        match Config.force_integration with
        | Some build_system when Config.is_originator ->
            build_system
        | _ ->
            Config.build_system_of_exe_name (Filename.basename prog)
      in
      assert_supported_build_system build_system ;
      match (build_system : Config.build_system) with
      | BAnt ->
          Ant {prog; args}
      | BBuck -> (
        match buck_mode with
        | None ->
            error_no_buck_mode_specified ()
        | Some (ClangCompilationDB deps) ->
            BuckCompilationDB {deps; prog; args= List.append args Config.buck_build_args}
        | Some Clang when Config.process_clang_ast ->
            L.user_warning
              "WARNING: the clang AST can only be processed when --buck-compilation-database is \
               set.@ Alternatively, set --no-process-clang-ast to disable this warning.@." ;
            BuckClangFlavor {build_cmd}
        | Some Java ->
            BuckJavaFlavor {build_cmd}
        | Some Clang ->
            BuckClangFlavor {build_cmd}
        | Some buck_mode ->
            L.die UserError "%a not supported in buck1.@." BuckMode.pp buck_mode )
      | BBuck2 -> (
        match buck_mode with
        | None ->
            error_no_buck_mode_specified ()
        | Some Erlang ->
            BuckErlang {prog; args}
        | Some Clang when Config.buck2_use_bxl ->
            BxlClang {build_cmd}
        | Some Clang ->
            Buck2Clang {build_cmd}
        | Some Java ->
            Buck2Java {build_cmd}
        | Some buck_mode ->
            L.die UserError "%a is not supported with buck2.@." BuckMode.pp buck_mode )
      | BClang ->
          Clang {compiler= Clang.Clang; prog; args}
      | BGradle ->
          Gradle {prog; args}
      | BJava ->
          Javac {compiler= Javac.Java; prog; args}
      | BJavac ->
          Javac {compiler= Javac.Javac; prog; args}
      | BMake ->
          Clang {compiler= Clang.Make; prog; args}
      | BMvn ->
          Maven {prog; args}
      | BNdk ->
          NdkBuild {build_cmd}
      | BRebar3 ->
          Rebar3 {args}
      | BErlc ->
          Erlc {args}
      | BHackc ->
          Hackc {prog; args}
      | BXcode when Config.xcpretty ->
          XcodeXcpretty {prog; args}
      | BXcode ->
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
  if Config.is_originator then L.environment_info "%a@\n" Config.pp_version () ;
  if Config.debug_mode then L.environment_info "Driver mode:@\n%a@." pp_mode mode ;
  if Config.is_originator && Config.dump_duplicate_symbols then reset_duplicates_file () ;
  ()


let run_prologue mode =
  ScubaLogging.execute_with_time_logging "run_prologue" (fun () -> run_prologue mode)


let run_epilogue () =
  if Config.is_originator then (
    if Config.fail_on_bug then fail_on_issue_epilogue () ;
    () ) ;
  if Config.buck_cache_mode then ResultsDir.scrub_for_caching () ;
  ()


let run_epilogue () =
  GCStats.log ~name:"main_process_full" Analysis (GCStats.get ~since:ProgramStart) ;
  ScubaLogging.execute_with_time_logging "run_epilogue" run_epilogue
