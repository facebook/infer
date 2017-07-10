(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Configuration values: either constant, determined at compile time, or set at startup
    time by system calls, environment variables, or command line options *)

module CLOpt = CommandLineOption
module F = Format

type analyzer =
  | BiAbduction | CaptureOnly | CompileOnly | Eradicate | Checkers | Crashcontext | Linters
[@@deriving compare]

let equal_analyzer = [%compare.equal : analyzer]

let string_to_analyzer = [
  "capture", CaptureOnly; "checkers", Checkers; "compile", CompileOnly;
  "crashcontext", Crashcontext; "eradicate", Eradicate; "infer", BiAbduction; "linters", Linters;
]

let string_of_analyzer a =
  List.find_exn ~f:(fun (_, a') -> equal_analyzer a a') string_to_analyzer |> fst

let clang_frontend_action_symbols = [
  ("lint", `Lint);
  ("capture", `Capture);
  ("lint_and_capture", `Lint_and_capture);
]

type language = Clang | Java [@@deriving compare]

let equal_language = [%compare.equal : language]

let string_of_language = function
  | Java -> "Java"
  | Clang -> "C_CPP"


let ml_bucket_symbols = [
  ("all", `MLeak_all);
  ("cf", `MLeak_cf);
  ("arc", `MLeak_arc);
  ("narc", `MLeak_no_arc);
  ("cpp", `MLeak_cpp);
  ("unknown_origin", `MLeak_unknown);
]

let issues_fields_symbols = [
  ("bug_class", `Issue_field_bug_class);
  ("kind", `Issue_field_kind);
  ("bug_type", `Issue_field_bug_type);
  ("qualifier", `Issue_field_qualifier);
  ("severity", `Issue_field_severity);
  ("visibility", `Issue_field_visibility);
  ("line", `Issue_field_line);
  ("column", `Issue_field_column);
  ("procedure", `Issue_field_procedure);
  ("procedure_id", `Issue_field_procedure_id);
  ("procedure_start_line", `Issue_field_procedure_start_line);
  ("file", `Issue_field_file);
  ("bug_trace", `Issue_field_bug_trace);
  ("key", `Issue_field_key);
  ("hash", `Issue_field_hash);
  ("line_offset", `Issue_field_line_offset);
  ("procedure_id_without_crc", `Issue_field_procedure_id_without_crc);
  ("qualifier_contains_potential_exception_note",
   `Issue_field_qualifier_contains_potential_exception_note);
]

type os_type = Unix | Win32 | Cygwin


(** Constant configuration values *)

(** If true, a precondition with e.g. index 3 in an array does not require the caller to
    have index 3 too this mimics what happens with direct access to the array without a
    procedure call, where the index is simply materialized if not there *)
let allow_missing_index_in_proc_call = true

let anonymous_block_num_sep = "______"

let anonymous_block_prefix = "__objc_anonymous_block_"

let assign = "<\"Assign\">"

let attributes_dir_name = "attributes"

let backend_stats_dir_name = "backend_stats"

(** If true, a procedure call succeeds even when there is a bound error this mimics what
    happens with a direct array access where an error is produced and the analysis
    continues *)
let bound_error_allowed_in_procedure_call = true

let buck_generated_folder = "buck-out/gen"

let buck_infer_deps_file_name = "infer-deps.txt"

let buck_results_dir_name = "infer"

let captured_dir_name = "captured"

let checks_disabled_by_default = [
  "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL";
]

let clang_initializer_prefix = "__infer_globals_initializer_"

(** Experimental: if true do some specialized analysis of concurrent constructs. *)
let csl_analysis = true

let default_failure_name = "ASSERTION_FAILURE"

let default_in_zip_results_dir = "infer"

(** Dotty output filename **)
let dotty_output = "icfg.dot"

let duplicates_filename = "duplicates.txt"

(** exit code to use for the --fail-on-issue option *)
let fail_on_issue_exit_code = 2

let frontend_stats_dir_name = "frontend_stats"

let global_tenv_filename = "global.tenv"

(** If true, treat calls to no-arg getters as idempotent w.r.t non-nullness *)
let idempotent_getters = true

(** If true, changes to code are checked at the procedure level; if false, at the file
    level *)
let incremental_procs = true

(** Our Python script does its own argument parsing and will fail with this error on failure *)
let infer_py_argparse_error_exit_code = 22

let ivar_attributes = "ivar_attributes"

let lint_dotty_dir_name = "lint_dotty"

let lint_issues_dir_name = "lint_issues"

(** letters used in the analysis output *)
let log_analysis_file = "F"
let log_analysis_procedure = "."
let log_analysis_wallclock_timeout = "T"
let log_analysis_symops_timeout = "S"
let log_analysis_recursion_timeout = "R"
let log_analysis_crash = "C"

let log_dir_name = "log"

let manual_buck_compilation_db = "BUCK COMPILATION DATABASE OPTIONS"
let manual_buck_flavors = "BUCK FLAVORS OPTIONS"
let manual_buck_java = "BUCK FOR JAVA OPTIONS"
let manual_buffer_overrun = "BUFFER OVERRUN OPTIONS"
let manual_clang = "CLANG OPTIONS"
let manual_clang_linters = "CLANG LINTERS OPTIONS"
let manual_crashcontext = "CRASHCONTEXT OPTIONS"
let manual_generic = Cmdliner.Manpage.s_options
let manual_internal = "INTERNAL OPTIONS"
let manual_java = "JAVA OPTIONS"
let manual_quandary = "QUANDARY CHECKER OPTIONS"
let manual_siof = "SIOF CHECKER OPTIONS"
let manual_threadsafety = "THREADSAFETY CHECKER OPTIONS"

(** Maximum level of recursion during the analysis, after which a timeout is generated *)
let max_recursion = 5

(** Flag to tune the level of applying the meet operator for
    preconditions during the footprint analysis:
    0 = do not use the meet
    1 = use the meet to generate new preconditions *)
let meet_level = 1

let multicore_dir_name = "multicore"

let nsnotification_center_checker_backend = false

let perf_stats_prefix = "perf_stats"

let proc_stats_filename = "proc_stats.json"

let property_attributes = "property_attributes"

let report_condition_always_true_in_clang = false

(** If true, sanity-check inferred preconditions against Nullable annotations and report
    inconsistencies *)
let report_nullable_inconsistency = true

let reporting_stats_dir_name = "reporting_stats"

(** If true, compact summaries before saving *)
let save_compact_summaries = true

(** If true enables printing proposition compatible for the SMT project *)
let smt_output = false

let source_file_extentions = [".java"; ".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"]

let specs_dir_name = "specs"

let specs_files_suffix = ".specs"

let start_filename = ".start"

(** If true performs taint analysis *)
let taint_analysis = true

(** Enable detailed tracing information during array abstraction *)
let trace_absarray = false

(** If true, optimize based on locality using reachability *)
let undo_join = true

let unsafe_unret = "<\"Unsafe_unretained\">"

let use_jar_cache = true

let weak = "<\"Weak\">"

let whitelisted_cpp_methods = [
  "std::move";
  "std::forward";
  "std::min";
  "std::max";
  "std::swap";
  "google::CheckNotNull";
]

let whitelisted_cpp_classes = [
  "std::__less";
  "std::__wrap_iter"; (* libc++ internal name of vector iterator *)
  "__gnu_cxx::__normal_iterator"; (* libstdc++ internal name of vector iterator *)
]

type dynamic_dispatch_policy = [
  | `None
  | `Interface
  | `Sound
  | `Lazy
]

(** Compile time configuration values *)

let pp_version fmt () =
  F.fprintf fmt "Infer version %s@\nCopyright 2009 - present Facebook. All Rights Reserved."
    Version.versionString

let version_string = F.asprintf "%a" pp_version ()

(** System call configuration values *)

(** Initial time of the analysis, i.e. when this module is loaded, gotten from
    Unix.time *)
let initial_analysis_time = Unix.time ()

let clang_exe_aliases = [
  (* this must be kept in sync with the clang-like symlinks in [wrappers_dir] (see below) *)
  "c++"; "cc"; "clang"; "clang++"; "g++"; "gcc";
]

let initial_command =
  (* Sys.executable_name tries to do clever things which we must avoid, use argv[0] instead *)
  let exe_basename = Filename.basename Sys.argv.(0) in
  let is_clang = List.mem ~equal:String.equal clang_exe_aliases in
  match CommandDoc.command_of_exe_name exe_basename with
  | Some _ as command -> command
  | None when is_clang exe_basename -> Some CLOpt.Clang
  | None -> None

let bin_dir =
  (* Resolve symlinks to get to the real executable, which is located in [bin_dir]. *)
  Filename.dirname (Utils.realpath Sys.executable_name)

let lib_dir =
  bin_dir ^/ Filename.parent_dir_name ^/ "lib"

let etc_dir =
  bin_dir ^/ Filename.parent_dir_name ^/ "etc"

(** Path to lib/specs to retrieve the default models *)
let models_dir =
  lib_dir ^/ specs_dir_name

let models_jar =
  lib_dir ^/ "java" ^/ "models.jar"

let models_src_dir =
  let root = Unix.getcwd () in
  let dir = bin_dir ^/ Filename.parent_dir_name ^/ "models" in
  Utils.filename_to_absolute ~root dir (* Normalize the path *)

let relative_cpp_extra_include_dir = "cpp" ^/ "include"

let cpp_extra_include_dir = models_src_dir ^/ relative_cpp_extra_include_dir

let relative_cpp_models_dir =
  relative_cpp_extra_include_dir ^/ "infer_model"

let linters_def_dir = lib_dir ^/ "linter_rules"

let linters_def_default_file = linters_def_dir ^/ "linters.al"

let wrappers_dir =
  lib_dir ^/ "wrappers"

let ncpu =
  try
    Utils.with_process_in
      "getconf _NPROCESSORS_ONLN 2>/dev/null"
      (fun chan -> Scanf.bscanf (Scanf.Scanning.from_channel chan) "%d" (fun n -> n))
    |> fst
  with _ ->
    1

let os_type = match Sys.os_type with
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> Unix


(** Resolve relative paths passed as command line options, i.e., with respect to the working
    directory of the initial invocation of infer. *)
let resolve = Utils.filename_to_absolute ~root:CLOpt.init_work_dir

let infer_inside_maven_env_var = "INFER_INSIDE_MAVEN"

let maven = CLOpt.is_env_var_set infer_inside_maven_env_var

let env_inside_maven = `Extend [infer_inside_maven_env_var, "1"]

let infer_is_javac = maven

let startup_action =
  let open CLOpt in
  if infer_is_javac then Javac
  else if !Sys.interactive then NoParse
  else match initial_command with
    | Some Clang ->
        NoParse
    | None | Some (Analyze | Capture | Compile | Report | ReportDiff | Run) ->
        InferCommand

let exe_usage =
  let exe_command_name = match initial_command with
    | Some CLOpt.Clang -> None (* users cannot see this *)
    | Some command -> Some (CommandDoc.name_of_command command)
    | None -> None in
  Printf.sprintf "%s\nUsage: infer %s [options]\nSee `infer%s --help` for more information."
    version_string (Option.value ~default:"command" exe_command_name)
    (Option.value_map ~default:"" ~f:((^) " ") exe_command_name)

(** Command Line options *)

(* HOWTO define a new command line and config file option.

   1. Add an entry in the following let...and...and... binding.  See the documentation in
      [CommandLineOption.mli], and use the existing options as a guide.  Preferably the identifer
      and long option name are the same modulo underscores versus hyphens.

      E.g. [and new_option = CLOpt.mk_bool ~long:"new-option"]

   2. Add a line to the [Freeze initialized configuration values] section below.

      E.g. [and new_option = !new_option]

   3. Add a line to [config.mli].

      E.g. [val new_option : bool]

   These are all in alphabetical order as much as possible.

   The references representing the command line options are defined in a single
   simultaneous let...and...and... binding in order to allow the type-checker to catch
   uses of one reference in code for another. This avoids being sensitive to
   initialization-order and unintended dependence on the order in which options appear on
   the command line. For cases where order-dependence is desired, the interacting options
   can be defined together sharing a reference. See debug and specs_library below for two
   different examples. *)

let anon_args = CLOpt.mk_anon ()

let () =
  let on_unknown_arg_from_command (cmd: CLOpt.command) = match cmd with
    | Clang -> assert false (* filtered out *)
    | Report -> `Add
    | Analyze | Capture | Compile | ReportDiff | Run -> `Reject in
  (* make sure we generate doc for all the commands we know about *)
  List.filter ~f:(Fn.non (CLOpt.(equal_command Clang))) CLOpt.all_commands
  |> List.iter ~f:(fun cmd ->
      let { CommandDoc.name; command_doc } = CommandDoc.data_of_command cmd in
      let on_unknown_arg = on_unknown_arg_from_command cmd in
      let deprecated_long = if CLOpt.(equal_command ReportDiff) cmd then Some "diff" else None in
      CLOpt.mk_subcommand cmd ~name ?deprecated_long ~on_unknown_arg (Some command_doc))

let () =
  CLOpt.mk_subcommand CLOpt.Clang ~name:"clang" ~on_unknown_arg:`Skip None

let abs_struct =
  CLOpt.mk_int ~deprecated:["absstruct"] ~long:"abs-struct" ~default:1
    ~meta:"int"
{|Specify abstraction level for fields of structs:
- 0 = no
- 1 = forget some fields during matching (and so lseg abstraction)
|}

and abs_val =
  CLOpt.mk_int ~deprecated:["absval"] ~long:"abs-val" ~default:2
    ~meta:"int"
{|Specify abstraction level for expressions:
- 0 = no abstraction
- 1 = evaluate all expressions abstractly
- 2 = 1 + abstract constant integer values during join
|}


and allow_leak =
  CLOpt.mk_bool ~deprecated:["leak"] ~long:"allow-leak"
    "Forget leaked memory during abstraction"

and allow_specs_cleanup =
  CLOpt.mk_bool ~deprecated:["allow_specs_cleanup"] ~long:"allow-specs-cleanup" ~default:true
    "Allow to remove existing specs before running analysis when it's not incremental"

and (
  analysis_blacklist_files_containing_options,
  analysis_path_regex_blacklist_options,
  analysis_path_regex_whitelist_options,
  analysis_suppress_errors_options) =
  let mk_filtering_options ~suffix ?(deprecated_suffix=[]) ~help ~meta =
    let mk_option analyzer_name =
      let long = Printf.sprintf "%s-%s" analyzer_name suffix in
      let deprecated =
        List.map ~f:(Printf.sprintf "%s_%s" analyzer_name) deprecated_suffix in
      (* empty doc to hide the options from --help since there are many redundant ones *)
      CLOpt.mk_string_list ~deprecated ~long ~meta "" in
    ignore (
      let long = "<analyzer>-" ^ suffix in
      CLOpt.mk_string_list ~long ~meta ~f:(fun _ -> raise (Arg.Bad "invalid option"))
        ~in_help:CLOpt.[Report, manual_generic; Run, manual_generic]
        help
    );
    List.map ~f:(fun (name, analyzer) -> (analyzer, mk_option name)) string_to_analyzer in
  (
    mk_filtering_options
      ~suffix:"blacklist-files-containing"
      ~deprecated_suffix:["blacklist_files_containing"]
      ~help:"blacklist files containing the specified string for the given analyzer (see \
             $(b,--analyzer) for valid values)"
      ~meta:"string",
    mk_filtering_options
      ~suffix:"blacklist-path-regex"
      ~deprecated_suffix:["blacklist"]
      ~help:"blacklist the analysis of files whose relative path matches the specified OCaml-style \
             regex (to whitelist: $(b,--<analyzer>-whitelist-path-regex))"
      ~meta:"path regex",
    mk_filtering_options
      ~suffix:"whitelist-path-regex"
      ~deprecated_suffix:["whitelist"]
      ~help:""
      ~meta:"path regex",
    mk_filtering_options
      ~suffix:"suppress-errors"
      ~deprecated_suffix:["suppress_errors"]
      ~help:"do not report a type of errors"
      ~meta:"error name")

and analysis_stops =
  CLOpt.mk_bool ~deprecated:["analysis_stops"] ~long:"analysis-stops"
    "Issue a warning when the analysis stops"

and analyzer =
  let () = match BiAbduction with
    (* NOTE: if compilation fails here, it means you have added a new analyzer without updating the
       documentation of this option *)
    | BiAbduction | CaptureOnly | CompileOnly | Eradicate | Checkers | Crashcontext
    | Linters -> () in
  CLOpt.mk_symbol_opt ~deprecated:["analyzer"] ~long:"analyzer" ~short:'a'
    ~in_help:CLOpt.[Analyze, manual_generic; Run, manual_generic]
{|Specify which analyzer to run (only one at a time is supported):
- $(b,infer): run the bi-abduction based checker, in particular to check for memory errors (activated by default)
- $(b,checkers), $(b,eradicate): run the specified analysis
- $(b,capture): similar to specifying the $(b,capture) subcommand (DEPRECATED)
- $(b,compile): similar to specifying the $(b,compile) subcommand (DEPRECATED)
- $(b,crashcontext): experimental (see $(b,--crashcontext))
- $(b,linters): run linters based on the ast only (Objective-C and Objective-C++ only, activated by default)|}
    ~f:(function
        | CaptureOnly | CompileOnly as x ->
            let analyzer_str = List.find_map_exn string_to_analyzer
                ~f:(fun (s, y) -> if equal_analyzer x y then Some s else None) in
            CLOpt.warnf
              "WARNING: The analyzer '%s' is deprecated, use the '%s' subcommand instead:@\n\
               @\n  infer %s ..." analyzer_str analyzer_str analyzer_str;
            x
        | _ as x -> x)
    ~symbols:string_to_analyzer

and android_harness =
  CLOpt.mk_bool ~deprecated:["harness"] ~long:"android-harness"
    "(Experimental) Create harness to detect issues involving the Android lifecycle"

and angelic_execution =
  CLOpt.mk_bool ~deprecated:["angelic_execution"] ~long:"angelic-execution" ~default:true
    "Angelic execution, where the analysis ignores errors caused by unknown procedure calls"

and (annotation_reachability,
     biabduction,
     bufferoverrun,
     crashcontext,
     default_checkers,
     eradicate,
     fragment_retains_view,
     immutable_cast,
     printf_args,
     quandary,
     repeated_calls,
     siof,
     threadsafety,
     suggest_nullable) =
  let annotation_reachability =
    CLOpt.mk_bool ~long:"annotation-reachability" ~in_help:CLOpt.[Analyze, manual_generic]
      ~default:true
      "the annotation reachability checker. Given a pair of source and sink annotation, e.g. \
       @PerformanceCritical and @Expensive, this checker will warn whenever some method annotated \
       with @PerformanceCritical calls, directly or indirectly, another method annotated with \
       @Expensive"

  and biabduction =
    CLOpt.mk_bool ~long:"biabduction" ~in_help:CLOpt.[Analyze, manual_generic]
      "the separation logic based bi-abduction analysis using the checkers framework"

  and bufferoverrun =
    CLOpt.mk_bool ~long:"bufferoverrun" ~in_help:CLOpt.[Analyze, manual_generic]
      "the buffer overrun analysis"

  and crashcontext =
    CLOpt.mk_bool ~long:"crashcontext" ~in_help:CLOpt.[Analyze, manual_generic]
      "the crashcontext checker for Java stack trace context reconstruction"

  and eradicate =
    CLOpt.mk_bool ~long:"eradicate" ~in_help:CLOpt.[Analyze, manual_generic]
      "the eradicate @Nullable checker for Java annotations"

  and fragment_retains_view =
    CLOpt.mk_bool ~long:"fragment-retains-view" ~in_help:CLOpt.[Analyze, manual_generic]
      ~default:true
      "detects when Android fragments are not explicitly nullified before becoming unreabable"

  and immutable_cast =
    CLOpt.mk_bool ~long:"immutable-cast" ~in_help:CLOpt.[Analyze, manual_generic]
      ~default:true
      "the detection of object cast from immutable type to mutable type. \
       For instance, it will detect cast from ImmutableList to List, ImmutableMap to Map, \
       and ImmutableSet to Set."

  and printf_args =
    CLOpt.mk_bool ~long:"printf-args" ~in_help:CLOpt.[Analyze, manual_generic]
      ~default:true
      "the detection of mismatch between the Java printf format strings and the argument types \
       For, example, this checker will warn about the type error in \
       `printf(\"Hello %d\", \"world\")`"

  and repeated_calls =
    CLOpt.mk_bool ~long:"repeated-calls" ~in_help:CLOpt.[Analyze, manual_generic]
      "check for repeated calls"

  and quandary =
    CLOpt.mk_bool ~long:"quandary" ~in_help:CLOpt.[Analyze, manual_generic] ~default:true
      "the quandary taint analysis"

  and siof =
    CLOpt.mk_bool ~long:"siof" ~in_help:CLOpt.[Analyze, manual_generic] ~default:true
      "the Static Initialization Order Fiasco analysis (C++ only)"

  and threadsafety =
    CLOpt.mk_bool ~long:"threadsafety" ~in_help:CLOpt.[Analyze, manual_generic] ~default:true
      "the thread safety analysis"

  and suggest_nullable =
    CLOpt.mk_bool ~long:"suggest-nullable" ~default:false
      "Nullable annotation sugesstions analysis (experimental)" in

  (* IMPORTANT: keep in sync with the checkers that have ~default:true above *)
  let default_checkers =
    CLOpt.mk_bool_group ~long:"default-checkers" ~in_help:CLOpt.[Analyze, manual_generic]
      ~default:true
      "Default checkers: $(b,--annotation-reachability), $(b,--fragments-retains-view), \
       $(b,--immutable-cast), $(b,--printf-args), $(b,--quandary), $(b,--siof), $(b,--threadsafety)"
      [annotation_reachability; fragment_retains_view; immutable_cast; printf_args; quandary;
       siof; threadsafety]
      [] in

  (annotation_reachability,
   biabduction,
   bufferoverrun,
   crashcontext,
   default_checkers,
   eradicate,
   fragment_retains_view,
   immutable_cast,
   printf_args,
   quandary,
   repeated_calls,
   siof,
   threadsafety,
   suggest_nullable)

and annotation_reachability_custom_pairs =
  CLOpt.mk_json ~long:"annotation-reachability-custom-pairs"
    ~in_help:CLOpt.[Analyze, manual_java]
{|Specify custom sources/sink for the annotation reachability checker
Example format: for custom annotations com.my.annotation.{Source1,Source2,Sink1}
{ "sources" : ["Source1", "Source2"], "sink" : "Sink1" }|}

and array_level =
  CLOpt.mk_int ~deprecated:["arraylevel"] ~long:"array-level" ~default:0
    ~meta:"int"
{|Level of treating the array indexing and pointer arithmetic:
- 0 = treats both features soundly
- 1 = assumes that the size of every array is infinite
- 2 = assumes that all heap dereferences via array indexing and pointer arithmetic are correct
|}
and ast_file =
  CLOpt.mk_path_opt ~deprecated:["ast"] ~long:"ast-file"
    ~meta:"file" "AST file for the translation"

and blacklist =
  CLOpt.mk_string_opt ~deprecated:["-blacklist-regex";"-blacklist"] ~long:"buck-blacklist"
    ~in_help:CLOpt.[Run, manual_buck_flavors; Capture, manual_buck_flavors]
    ~meta:"regex" "Skip analysis of files matched by the specified regular expression"

and bootclasspath =
  CLOpt.mk_string_opt ~long:"bootclasspath"
    ~in_help:CLOpt.[Capture, manual_java]
    "Specify the Java bootclasspath"

(** Automatically set when running from within Buck *)
and buck =
  CLOpt.mk_bool ~long:"buck"
    ""

and buck_build_args =
  CLOpt.mk_string_list ~long:"Xbuck"
    ~in_help:CLOpt.[Capture, manual_buck_flavors]
    "Pass values as command-line arguments to invocations of $(i,`buck build`)"

and buck_compilation_database =
  CLOpt.mk_symbol_opt ~long:"buck-compilation-database" ~deprecated:["-use-compilation-database"]
    ~in_help:CLOpt.[Capture, manual_buck_compilation_db]
    "Buck integration using the compilation database, with or without dependencies."
    ~symbols:[("deps", `Deps); ("no-deps", `NoDeps)]

and buck_out =
  CLOpt.mk_path_opt ~long:"buck-out"
    ~in_help:CLOpt.[Capture, manual_buck_java] ~meta:"dir" "Specify the root directory of buck-out"

and bugs_csv =
  CLOpt.mk_path_opt ~deprecated:["bugs"] ~long:"issues-csv"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file" "Write a list of issues in CSV format to a file"

and bugs_json =
  CLOpt.mk_path_opt ~deprecated:["bugs_json"] ~long:"issues-json"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file" "Write a list of issues in JSON format to a file"

and bugs_tests =
  CLOpt.mk_path_opt ~long:"issues-tests"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file"
    "Write a list of issues in a format suitable for tests to a file"

and bugs_txt =
  CLOpt.mk_path_opt ~deprecated:["bugs_txt"] ~long:"issues-txt"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file"
    "Write a list of issues in TXT format to a file"

and calls_csv =
  CLOpt.mk_path_opt ~deprecated:["calls"] ~long:"calls-csv"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file"
    "Write individual calls in CSV format to a file"

and changed_files_index =
  CLOpt.mk_path_opt ~long:"changed-files-index" ~in_help:CLOpt.[Analyze, manual_generic]
    ~meta:"file"
    "Specify the file containing the list of source files from which reactive analysis should \
     start. Source files should be specified relative to project root or be absolute"

and clang_biniou_file =
  CLOpt.mk_path_opt ~long:"clang-biniou-file"
    ~in_help:CLOpt.[Capture, manual_clang] ~meta:"file"
    "Specify a file containing the AST of the program, in biniou format"

and clang_compilation_dbs = ref []

and clang_frontend_action =
  CLOpt.mk_symbol_opt ~long:"clang-frontend-action"
    ~in_help:CLOpt.[Capture, manual_clang; Run, manual_clang]
    "Specify whether the clang frontend should capture or lint or both."
    ~symbols:clang_frontend_action_symbols

and clang_include_to_override_regex =
  CLOpt.mk_string_opt ~long:"clang-include-to-override-regex"
    ~deprecated:["-clang-include-to-override"]
    ~meta:"dir OCaml regex"
    "Use this option in the uncommon case where the normal compilation process overrides the \
     location of internal compiler headers. This option should specify regular expression with \
     the path to those headers so that infer can use its own clang internal headers instead."

and clang_ignore_regex =
  CLOpt.mk_string_opt ~long:"clang-ignore-regex"
    ~meta:"dir OCaml regex"
    "The files in this regex will be ignored in the compilation process and \
     an empty file will be passed to clang instead. This is to be used with the buck flavour \
     infer-capture-all to work around missing generated files."

and classpath =
  CLOpt.mk_string_opt ~long:"classpath"
    "Specify the Java classpath"

and cluster =
  CLOpt.mk_path_opt ~deprecated:["cluster"] ~long:"cluster"
    ~meta:"file" "Specify a .cluster file to be analyzed"

and compilation_database =
  CLOpt.mk_path_list ~long:"compilation-database"
    ~deprecated:["-clang-compilation-db-files"]
    ~in_help:CLOpt.[Capture, manual_clang]
    "File that contain compilation commands (can be specified multiple times)"

and compilation_database_escaped =
  CLOpt.mk_path_list ~long:"compilation-database-escaped"
    ~deprecated:["-clang-compilation-db-files-escaped"]
    ~in_help:CLOpt.[Capture, manual_clang]
    "File that contain compilation commands where all entries are escaped for the shell, eg coming \
     from Xcode (can be specified multiple times)"

and compute_analytics =
  CLOpt.mk_bool ~long:"compute-analytics"
    ~default:false
    ~in_help:CLOpt.[Capture, manual_clang; Run, manual_clang]
    "Emit analytics as info-level issues, like component kit line count and \
     component kit file cyclomatic complexity"

(** Continue the capture for reactive mode:
    If a procedure was changed beforehand, keep the changed marking. *)
and continue =
  CLOpt.mk_bool ~deprecated:["continue"] ~long:"continue"
    ~in_help:CLOpt.[Analyze, manual_generic]
    "Continue the capture for the reactive analysis, increasing the changed files/procedures. (If \
     a procedure was changed beforehand, keep the changed marking.)"

and copy_propagation =
  CLOpt.mk_bool ~deprecated:["copy-propagation"] ~long:"copy-propagation"
    "Perform copy-propagation on the IR"

and cxx, cxx_infer_headers =
  let cxx_infer_headers =
    CLOpt.mk_bool ~long:"cxx-infer-headers"
      ~default:true
      ~in_help:CLOpt.[Capture, manual_clang]
      "Include C++ header models during compilation, set by $(b,--cxx). Infer swaps some C++ \
       headers for its own in order to get a better model of, eg, the standard library. This \
       can sometimes cause compilation failures." in
  let cxx = CLOpt.mk_bool_group ~long:"cxx"
      ~default:true
      ~in_help:CLOpt.[Capture, manual_clang]
      "Analyze C++ methods"
      [cxx_infer_headers] [] in
  cxx, cxx_infer_headers


and (
  bo_debug,
  developer_mode,
  debug,
  debug_exceptions,
  debug_level_analysis,
  debug_level_capture,
  debug_level_linters,
  default_linters,
  failures_allowed,
  filtering,
  frontend_tests,
  linters_developer_mode,
  only_cheap_debug,
  print_buckets,
  print_logs,
  print_types,
  reports_include_ml_loc,
  stats,
  trace_error,
  write_html,
  write_html_whitelist_regex,
  write_dotty
) =
  let all_generic_manuals =
    List.filter_map  CLOpt.all_commands
      ~f:(fun cmd ->
          if CLOpt.(equal_command cmd Clang) then None
          else Some (cmd, manual_generic)) in

  let bo_debug =
    CLOpt.mk_int ~default:0 ~long:"bo-debug"
      ~in_help:CLOpt.[Analyze, manual_buffer_overrun] "Debug level for buffer-overrun checker (0-4)"

  and debug_level_analysis =
    CLOpt.mk_int ~long:"debug-level-analysis" ~default:0
      ~in_help:all_generic_manuals
      "Debug level for the analysis. See $(b,--debug-level) for accepted values."

  and debug_level_capture =
    CLOpt.mk_int ~long:"debug-level-capture" ~default:0
      ~in_help:all_generic_manuals
      "Debug level for the capture. See $(b,--debug-level) for accepted values."

  and debug_level_linters =
    CLOpt.mk_int ~long:"debug-level-linters" ~default:0
      ~in_help:(CLOpt.(Capture, manual_clang_linters)::all_generic_manuals)
      "Debug level for the linters. See $(b,--debug-level) for accepted values."

  and developer_mode =
    CLOpt.mk_bool ~long:"developer-mode"
      ~default:(Option.value_map ~default:false ~f:(CLOpt.(equal_command Report)) initial_command)
      "Show internal exceptions"

  and failures_allowed =
    CLOpt.mk_bool ~deprecated_no:["-no_failures_allowed"] ~long:"failures-allowed" ~default:true
      "Fail if at least one of the translations fails (clang only)"

  and filtering =
    CLOpt.mk_bool ~deprecated_no:["nf"] ~long:"filtering" ~short:'f' ~default:true
      ~in_help:CLOpt.[Report, manual_generic]
      "Do not show the results from experimental and blacklisted checks"

  and only_cheap_debug =
    CLOpt.mk_bool ~long:"only-cheap-debug"
      ~default:true
      "Disable expensive debugging output"

  and print_buckets =
    CLOpt.mk_bool ~long:"print-buckets"
      "Show the internal bucket of Infer reports in their textual description"

  and print_types =
    CLOpt.mk_bool ~long:"print-types" ~default:false
      "Print types in symbolic heaps"

  and reports_include_ml_loc =
    CLOpt.mk_bool ~deprecated:["with_infer_src_loc"] ~long:"reports-include-ml-loc"
      "Include the location in the Infer source code from where reports are generated"

  and trace_error =
    CLOpt.mk_bool ~long:"trace-error"
      "Detailed tracing information during error explanation"

  and write_html =
    CLOpt.mk_bool ~long:"write-html"
      "Produce hmtl debug output in the results directory"

  and write_html_whitelist_regex =
    CLOpt.mk_string_list ~long:"write-html-whitelist-regex"
      "whitelist files that will have its html debug output printed"

  and write_dotty =
    CLOpt.mk_bool ~long:"write-dotty"
      "Produce dotty files for specs in the results directory"
  in

  let set_debug_level level =
    bo_debug := level;
    debug_level_analysis := level;
    debug_level_capture := level;
    debug_level_linters := level in

  let debug =
    CLOpt.mk_bool_group ~deprecated:["debug"] ~long:"debug" ~short:'g'
      ~in_help:all_generic_manuals
      "Debug mode (also sets $(b,--debug-level 2), $(b,--developer-mode), $(b,--no-filtering), \
       $(b,--print-buckets), $(b,--print-types), $(b,--reports-include-ml-loc), \
       $(b,--no-only-cheap-debug), $(b,--trace-error), $(b,--write-dotty), $(b,--write-html))"
      ~f:(fun debug -> if debug then set_debug_level 2 else set_debug_level 0; debug)
      [developer_mode; print_buckets; print_types; reports_include_ml_loc; trace_error; write_html;
       write_dotty]
      [filtering; only_cheap_debug]

  and _ : int option ref =
    CLOpt.mk_int_opt ~long:"debug-level"
      ~in_help:all_generic_manuals ~meta:"level"
      ~f:(fun level -> set_debug_level level; level)
{|Debug level (sets $(b,--bo-debug) $(i,level), $(b,--debug-level-analysis) $(i,level), $(b,--debug-level-capture) $(i,level), $(b,--debug-level-linters) $(i,level)):
  - 0: only basic debugging enabled
  - 1: verbose debugging enabled
  - 2: very verbose debugging enabled|}

  and debug_exceptions =
    CLOpt.mk_bool_group ~long:"debug-exceptions"
      "Generate lightweight debugging information: just print the internal exceptions during \
       analysis (also sets $(b,--developer-mode), $(b,--no-filtering), $(b,--print-buckets), \
       $(b,--reports-include-ml-loc))"
      [developer_mode; print_buckets; reports_include_ml_loc]
      [filtering]

  and default_linters =
    CLOpt.mk_bool ~long:"default-linters" ~in_help:CLOpt.[Capture, manual_clang_linters]
      ~default:true
      "Use the default linters for the analysis."

  and frontend_tests =
    CLOpt.mk_bool_group ~long:"frontend-tests"
      ~in_help:CLOpt.[Capture, manual_clang]
      "Save filename.ext.test.dot with the cfg in dotty format for frontend tests (also sets \
       $(b,--print-types))"
      [print_types] []

  and print_logs =
    CLOpt.mk_bool ~long:"print-logs"
      ~in_help:CLOpt.[Analyze, manual_generic; Capture, manual_generic; Run, manual_generic;
                      Report, manual_generic]
      "Also log messages to stdout and stderr"

  and stats =
    CLOpt.mk_bool ~deprecated:["stats"] ~long:"stats" "Stats mode (debugging)"
      ~f:(fun stats -> if stats then set_debug_level 1 else set_debug_level 0; stats)

  in
  let linters_developer_mode =
    CLOpt.mk_bool_group ~long:"linters-developer-mode"
      ~in_help:CLOpt.[Capture, manual_clang_linters]
      "Debug mode for developing new linters. (Sets the analyzer to $(b,linters); also sets \
       $(b,--debug), $(b,--debug-level-linters 2), $(b,--developer-mode), and \
       unsets $(b,--allowed-failures) and $(b,--default-linters)."
      ~f:(fun debug -> debug_level_linters := if debug then 2 else 0; debug)
      [debug; developer_mode] [failures_allowed; default_linters]

  in (
    bo_debug,
    developer_mode,
    debug,
    debug_exceptions,
    debug_level_analysis,
    debug_level_capture,
    debug_level_linters,
    default_linters,
    failures_allowed,
    filtering,
    frontend_tests,
    linters_developer_mode,
    only_cheap_debug,
    print_buckets,
    print_logs,
    print_types,
    reports_include_ml_loc,
    stats,
    trace_error,
    write_html,
    write_html_whitelist_regex,
    write_dotty
  )


and dependencies =
  CLOpt.mk_bool ~deprecated:["dependencies"] ~long:"dependencies"
    ~in_help:CLOpt.[Capture, manual_java]
    "Translate all the dependencies during the capture. The classes in the given jar file will be \
     translated. No sources needed."

and differential_filter_files =
  CLOpt.mk_string_opt
    ~long:"differential-filter-files" ~in_help:CLOpt.[Report, manual_generic]
    "Specify the file containing the list of source files for which a differential report \
     is desired. Source files should be specified relative to project root or be absolute"

and differential_filter_set =
  CLOpt.mk_symbol_seq ~long:"differential-filter-set" ~eq:PVariant.(=)
    "Specify which set of the differential results is filtered with the modified files provided \
     through the $(b,--differential-modified-files) argument. By default it is applied to all sets \
     ($(b,introduced), $(b,fixed), and $(b,preexisting))"
    ~symbols:[("introduced", `Introduced); ("fixed", `Fixed); ("preexisting", `Preexisting)]
    ~default:[`Introduced; `Fixed; `Preexisting]

and disable_checks =
  CLOpt.mk_string_list ~deprecated:["disable_checks"] ~long:"disable-checks" ~meta:"error name"
    ~in_help:CLOpt.[Report, manual_generic]
    ~default: [
      "ANALYSIS_STOPS";
      "ARRAY_OUT_OF_BOUNDS_L1";
      "ARRAY_OUT_OF_BOUNDS_L2";
      "ARRAY_OUT_OF_BOUNDS_L3";
      "CLASS_CAST_EXCEPTION";
      "CONDITION_ALWAYS_FALSE";
      "CONDITION_ALWAYS_TRUE";
      "DANGLING_POINTER_DEREFERENCE";
      "DIVIDE_BY_ZERO";
      "NULL_TEST_AFTER_DEREFERENCE";
      "RETAIN_CYCLE";
      "RETURN_VALUE_IGNORED";
      "STACK_VARIABLE_ADDRESS_ESCAPE";
      "UNARY_MINUS_APPLIED_TO_UNSIGNED_EXPRESSION";
      "UNINITIALIZED_VALUE";
    ]
    "Do not show reports coming from this type of errors. This option has lower precedence than \
     $(b,--no-filtering) and $(b,--enable-checks)"

and dotty_cfg_libs =
  CLOpt.mk_bool ~deprecated:["dotty_no_cfg_libs"] ~long:"dotty-cfg-libs" ~default:true
    "Print the cfg of the code coming from the libraries"

and dump_duplicate_symbols =
  CLOpt.mk_bool ~long:"dump-duplicate-symbols" ~in_help:CLOpt.[Capture, manual_clang]
    "Dump all symbols with the same name that are defined in more than one file."

and dynamic_dispatch =
  CLOpt.mk_symbol_opt ~long:"dynamic-dispatch"
    "Specify treatment of dynamic dispatch in Java code: 'none' treats dynamic dispatch as a call \
     to unknown code, 'lazy' follows the JVM semantics and creates procedure descriptions during \
     symbolic execution using the type information found in the abstract state; 'sound' is \
     significantly more computationally expensive"
    ~symbols:[("none", `None); ("interface", `Interface); ("sound", `Sound); ("lazy", `Lazy)]

and enable_checks =
  CLOpt.mk_string_list ~deprecated:["enable_checks"] ~long:"enable-checks" ~meta:"error name"
    "Show reports coming from this type of errors. This option has higher precedence than \
     $(b,--disable-checks)"

and eradicate_condition_redundant =
  CLOpt.mk_bool ~long:"eradicate-condition-redundant"
    "Condition redundant warnings"

and eradicate_field_not_mutable =
  CLOpt.mk_bool ~long:"eradicate-field-not-mutable"
    "Field not mutable warnings"

and eradicate_field_over_annotated =
  CLOpt.mk_bool ~long:"eradicate-field-over-annotated"
    "Field over-annotated warnings"

and eradicate_optional_present =
  CLOpt.mk_bool ~long:"eradicate-optional-present"
    "Check for @Present annotations"

and eradicate_propagate_return_nullable =
  CLOpt.mk_bool ~long:"eradicate-propagate-return-nullable"
    "Propagation of nullable to the return value"

and eradicate_return_over_annotated =
  CLOpt.mk_bool ~long:"eradicate-return-over-annotated"
    "Return over-annotated warning"

and eradicate_debug =
  CLOpt.mk_bool ~long:"eradicate-debug"
    "Print debug info when errors are found"

and eradicate_verbose =
  CLOpt.mk_bool ~long:"eradicate-verbose"
    "Print initial and final typestates"

and fail_on_bug =
  CLOpt.mk_bool ~deprecated:["-fail-on-bug"] ~long:"fail-on-issue" ~default:false
    ~in_help:CLOpt.[Run, manual_generic]
    (Printf.sprintf "Exit with error code %d if Infer found something to report"
       fail_on_issue_exit_code)

and fcp_apple_clang =
  CLOpt.mk_path_opt ~long:"fcp-apple-clang"
    ~meta:"path" "Specify the path to Apple Clang"

and fcp_syntax_only =
  CLOpt.mk_bool ~long:"fcp-syntax-only"
    "Skip creation of object files"

and file_renamings =
  CLOpt.mk_path_opt
    ~long:"file-renamings" ~in_help:CLOpt.[ReportDiff, manual_generic]
    "JSON with a list of file renamings to use while computing differential reports"

and filter_paths =
  CLOpt.mk_bool ~long:"filter-paths" ~default:true
    "Filters specified in .inferconfig"

and flavors =
  CLOpt.mk_bool ~deprecated:["-use-flavors"] ~long:"flavors"
    ~in_help:CLOpt.[Capture, manual_buck_flavors]
    "Buck integration using Buck flavors (clang only), eg $(i,`infer --flavors -- buck build \
     //foo:bar#infer`)"

and from_json_report =
  CLOpt.mk_path_opt ~long:"from-json-report"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"report.json"
    "Load analysis results from a report file (default is to load the results from the specs \
     files generated by the analysis)."

and frontend_debug =
  CLOpt.mk_bool ~deprecated:["fd"] ~deprecated_no:["nfd"] ~long:"frontend-debug"
    ~in_help:CLOpt.[Capture, manual_clang]
    "Emit debug info to *.o.astlog and a script *.o.sh that replays the command used to run clang \
     with the plugin attached, piped to the infer frontend"

and frontend_stats =
  CLOpt.mk_bool ~deprecated:["fs"] ~deprecated_no:["nfs"] ~long:"frontend-stats"
    "Output statistics about the capture phase to *.o.astlog (clang only)"

and generated_classes =
  CLOpt.mk_path_opt ~long:"generated-classes"
    ~in_help:CLOpt.[Capture, manual_java]
    "Specify where to load the generated class files"

and headers =
  CLOpt.mk_bool ~deprecated:["headers"; "hd"] ~deprecated_no:["no_headers"; "nhd"] ~long:"headers"
    ~in_help:CLOpt.[Capture, manual_clang]
    "Analyze code in header files"

and help =
  let var = ref `None in
  CLOpt.mk_set var `Help ~long:"help"
    ~in_help:(List.map CLOpt.all_commands ~f:(fun command -> command, manual_generic))
    "Show this manual" ;
  CLOpt.mk_set var `HelpFull ~long:"help-full"
    ~in_help:(List.map CLOpt.all_commands ~f:(fun command -> command, manual_generic))
    (Printf.sprintf "Show this manual with all internal options in the %s section" manual_internal);
  var

and help_format =
  CLOpt.mk_symbol ~long:"help-format"
    ~symbols:[("auto", `Auto); ("groff", `Groff); ("pager", `Pager); ("plain", `Plain)]
    ~eq:PVariant.(=) ~default:`Auto
    ~in_help:(List.map CLOpt.all_commands ~f:(fun command -> command, manual_generic))
    "Show this help in the specified format. $(b,auto) sets the format to $(b,plain) if the \
     environment variable $(b,TERM) is \"dumb\" or undefined, and to $(b,pager) otherwise."

and icfg_dotty_outfile =
  CLOpt.mk_path_opt ~long:"icfg-dotty-outfile" ~meta:"path"
    "If set, specifies path where .dot file should be written, it overrides the path for all \
     other options that would generate icfg file otherwise"

and ignore_trivial_traces = CLOpt.mk_bool
    ~long:"ignore-trivial-traces" ~default:true "Ignore traces whose length is at most 1"

and infer_cache =
  CLOpt.mk_path_opt ~deprecated:["infer_cache"; "-infer_cache"] ~long:"infer-cache"
    ~meta:"dir" "Select a directory to contain the infer cache (Buck and Java only)"

and iphoneos_target_sdk_version =
  CLOpt.mk_string_opt ~long:"iphoneos-target-sdk-version"
    ~in_help:CLOpt.[Capture, manual_clang_linters]
    "Specify the target SDK version to use for iphoneos"

and iphoneos_target_sdk_version_skip_path =
  CLOpt.mk_string_list ~long:"iphoneos-target-sdk-version-skip-path"
    ~in_help:CLOpt.[Capture, manual_clang_linters]
    ~meta:"path prefix OCaml regex"
    "To be used together with iphoneos-target-sdk-version, \
     to disable that flag in a particular path (can be specified multiple times)"

and issues_fields =
  CLOpt.mk_symbol_seq ~long:"issues-fields"
    ~in_help:CLOpt.[Report, manual_generic]
    ~default:[
      `Issue_field_file;
      `Issue_field_procedure;
      `Issue_field_line_offset;
      `Issue_field_bug_type;
      `Issue_field_bug_trace;
    ]
    ~symbols:issues_fields_symbols ~eq:PVariant.(=)
    "Fields to emit with $(b,--issues-tests)"

and iterations =
  CLOpt.mk_int ~deprecated:["iterations"] ~long:"iterations" ~default:1
    ~meta:"int"
    "Specify the maximum number of operations for each function, expressed as a multiple of \
     symbolic operations and a multiple of seconds of elapsed time"

and java_jar_compiler =
  CLOpt.mk_path_opt
    ~long:"java-jar-compiler"
    ~in_help:CLOpt.[Capture, manual_java]
    ~meta:"path" "Specify the Java compiler jar used to generate the bytecode"

and jobs =
  CLOpt.mk_int ~deprecated:["-multicore"] ~long:"jobs" ~short:'j' ~default:ncpu
    ~in_help:CLOpt.[Analyze, manual_generic]
    ~meta:"int" "Run the specified number of analysis jobs simultaneously"

and join_cond =
  CLOpt.mk_int ~deprecated:["join_cond"] ~long:"join-cond" ~default:1
    ~meta:"int"
{|Set the strength of the final information-loss check used by the join:
- 0 = use the most aggressive join for preconditions
- 1 = use the least aggressive join for preconditions
|}

and latex =
  CLOpt.mk_path_opt ~deprecated:["latex"] ~long:"latex"
    ~meta:"file"
    "Write a latex report of the analysis results to a file"

and log_file =
  CLOpt.mk_string ~deprecated:["out_file"; "-out-file"] ~long:"log-file"
    ~meta:"file" ~default:"logs" "Specify the file to use for logging"

and linter =
  CLOpt.mk_string_opt ~long:"linter" ~in_help:CLOpt.[Capture, manual_clang_linters]
    "From the linters available, only run this one linter. \
     (Useful together with $(b,--linters-developer-mode))"

and linters_def_file =
  CLOpt.mk_path_list ~default:[]
    ~long:"linters-def-file" ~in_help:CLOpt.[Capture, manual_clang_linters]
    ~meta:"file" "Specify the file containing linters definition (e.g. 'linters.al')"

and linters_def_folder =
  CLOpt.mk_path_list ~default:[] ~long:"linters-def-folder"
    ~in_help:CLOpt.[Capture, manual_clang_linters]
    ~meta:"dir" "Specify the folder containing linters files with extension .al"

and linters_ignore_clang_failures =
  CLOpt.mk_bool ~long:"linters-ignore-clang-failures"
    ~in_help:CLOpt.[Capture, manual_clang_linters]
    ~default:false
    "Continue linting files even if some compilation fails."

and load_average =
  CLOpt.mk_float_opt ~long:"load-average" ~short:'l'
    ~in_help:CLOpt.[Capture, manual_generic] ~meta:"float"
    "Do not start new parallel jobs if the load average is greater than that specified (Buck and \
     make only)"

and load_results =
  CLOpt.mk_path_opt ~deprecated:["load_results"] ~long:"load-results"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file.iar" "Load analysis results from Infer Analysis Results file file.iar"

(** name of the makefile to create with clusters and dependencies *)
and makefile =
  CLOpt.mk_path ~deprecated:["makefile"] ~long:"makefile" ~default:""
    ~meta:"file" ""

and margin =
  CLOpt.mk_int ~deprecated:["set_pp_margin"] ~long:"margin" ~default:100
    ~meta:"int" "Set right margin for the pretty printing functions"

and merge =
  CLOpt.mk_bool ~deprecated:["merge"] ~long:"merge"
    ~in_help:CLOpt.[Analyze, manual_buck_flavors]
    "Merge the captured results directories specified in the dependency file"

and ml_buckets =
  CLOpt.mk_symbol_seq ~deprecated:["ml_buckets"; "-ml_buckets"] ~long:"ml-buckets"
    ~default:[`MLeak_cf]
    ~in_help:CLOpt.[Analyze, manual_clang]
{|Specify the memory leak buckets to be checked in Objective-C/C++:
- $(b,cf) checks leaks from Core Foundation (activated by default),
- $(b,arc) from code compiled in ARC mode,
- $(b,narc) from code not compiled in ARC mode,
- $(b,cpp) from C++ code
|}
    ~symbols:ml_bucket_symbols ~eq:PVariant.(=)

and models_mode =
  CLOpt.mk_bool ~deprecated:["models_mode"; "-models_mode"] ~long:"models-mode"
    "Mode for analyzing the models"

and modified_targets =
  CLOpt.mk_path_opt ~deprecated:["modified_targets"] ~long:"modified-targets"
    ~meta:"file" "Read the file of Buck targets modified since the last analysis"

and monitor_prop_size =
  CLOpt.mk_bool ~deprecated:["monitor_prop_size"] ~long:"monitor-prop-size"
    "Monitor size of props, and print every time the current max is exceeded"

and nelseg =
  CLOpt.mk_bool ~deprecated:["nelseg"] ~long:"nelseg"
    "Use only nonempty lsegs"

(* TODO: document *)
and objc_memory_model =
  CLOpt.mk_bool ~deprecated:["objcm"] ~long:"objc-memory-model"
    "Use ObjC memory model"

and only_footprint =
  CLOpt.mk_bool ~deprecated:["only_footprint"] ~long:"only-footprint"
    "Skip the re-execution phase"

and passthroughs =
  CLOpt.mk_bool ~long:"passthroughs" ~default:false
    "In error traces, show intermediate steps that propagate data. When false, error traces are \
     shorter and show only direct flow via souces/sinks"

and patterns_modeled_expensive =
  let long = "modeled-expensive" in
  (long,
   CLOpt.mk_json ~deprecated:["modeled_expensive"] ~long
     "Matcher or list of matchers for methods that should be considered expensive by the \
      performance critical checker.")

and patterns_never_returning_null =
  let long = "never-returning-null" in
  (long,
   CLOpt.mk_json ~deprecated:["never_returning_null"] ~long
     "Matcher or list of matchers for functions that never return $(i,null).")

and patterns_skip_translation =
  let long = "skip-translation" in
  (long,
   CLOpt.mk_json ~deprecated:["skip_translation"] ~long
     "Matcher or list of matchers for names of files that should not be analyzed at all.")

and per_procedure_parallelism =
  CLOpt.mk_bool ~long:"per-procedure-parallelism" ~default:true
    "Perform analysis with per-procedure parallelism. Java is not supported."

and pmd_xml =
  CLOpt.mk_bool ~long:"pmd-xml"
    ~in_help:CLOpt.[Run, manual_generic]
    "Output issues in (PMD) XML format"

and precondition_stats =
  CLOpt.mk_bool ~deprecated:["precondition_stats"] ~long:"precondition-stats"
    "Print stats about preconditions to standard output"

and print_active_checkers =
  CLOpt.mk_bool ~long:"print-active-checkers" ~in_help:CLOpt.[Analyze, manual_generic]
    "Print the active checkers before starting the analysis"

and print_builtins =
  CLOpt.mk_bool ~deprecated:["print_builtins"] ~long:"print-builtins"
    "Print the builtin functions and exit"

and print_using_diff =
  CLOpt.mk_bool ~deprecated_no:["noprintdiff"] ~long:"print-using-diff" ~default:true
    "Highlight the difference w.r.t. the previous prop when printing symbolic execution debug info"

and procedures_per_process =
  CLOpt.mk_int ~long:"procedures-per-process" ~default:1000 ~meta:"int"
    "Specify the number of procedures to analyze per process when using \
     $(b,--per-procedure-parallelism).  If 0 is specified, each file is divided into $(b,--jobs) \
     groups of procedures."

and procs_csv =
  CLOpt.mk_path_opt ~deprecated:["procs"] ~long:"procs-csv"
    ~meta:"file" "Write statistics for each procedure in CSV format to a file"

and procs_xml =
  CLOpt.mk_path_opt ~deprecated:["procs_xml"] ~long:"procs-xml"
    ~meta:"file"
    "Write statistics for each procedure in XML format to a file (as a path relative to \
     $(b,--results-dir))"

and progress_bar =
  CLOpt.mk_bool ~deprecated:["pb"] ~deprecated_no:["no_progress_bar"; "npb"] ~short:'p'
    ~long:"progress-bar" ~default:true
    ~in_help:CLOpt.[Run, manual_generic]
    "Show a progress bar"

and project_root =
  CLOpt.mk_path ~deprecated:["project_root"; "-project_root"; "pr"] ~long:"project-root" ~short:'C'
    ~default:CLOpt.init_work_dir
    ~in_help:CLOpt.[Analyze, manual_generic; Capture, manual_generic; Run, manual_generic;
                    Report, manual_generic]
    ~meta:"dir" "Specify the root directory of the project"

and quandary_endpoints =
  CLOpt.mk_json ~long:"quandary-endpoints"
    ~in_help:CLOpt.[Analyze, manual_quandary]
    "Specify endpoint classes for Quandary"

and quandary_sanitizers =
  CLOpt.mk_json ~long:"quandary-sanitizers"
    ~in_help:CLOpt.[Analyze, manual_quandary]
    "Specify custom sanitizers for Quandary"

and quandary_sources =
  CLOpt.mk_json ~long:"quandary-sources"
    ~in_help:CLOpt.[Analyze, manual_quandary]
    "Specify custom sources for Quandary"

and quandary_sinks =
  CLOpt.mk_json ~long:"quandary-sinks"
    ~in_help:CLOpt.[Analyze, manual_quandary]
    "Specify custom sinks for Quandary"

and quiet =
  CLOpt.mk_bool ~long:"quiet" ~short:'q' ~default:false
    ~in_help:CLOpt.[Analyze, manual_generic; Report, manual_generic]
    "Do not print specs on standard output (default: only print for the $(b,report) command)"

and reactive =
  CLOpt.mk_bool ~deprecated:["reactive"] ~long:"reactive" ~short:'r'
    ~in_help:CLOpt.[Analyze, manual_generic]
    "Reactive mode: the analysis starts from the files captured since the $(i,infer) command \
     started"

and reactive_capture =
  CLOpt.mk_bool ~long:"reactive-capture"
    "Compile source files only when required by analyzer (clang only)"

and report =
  CLOpt.mk_bool ~long:"report" ~default:true
    ~in_help:CLOpt.[Analyze, manual_generic; Run, manual_generic]
    "Run the reporting phase once the analysis has completed"

and report_current =
  CLOpt.mk_path_opt ~long:"report-current" ~in_help:CLOpt.[ReportDiff, manual_generic]
    "report of the latest revision"

and report_custom_error =
  CLOpt.mk_bool ~long:"report-custom-error"
    ""

and report_formatter =
  CLOpt.mk_symbol ~long:"report-formatter"
    ~in_help:CLOpt.[Report, manual_generic]
    ~default:`Phabricator_formatter
    ~symbols:[
      ("none", `No_formatter);
      ("phabricator", `Phabricator_formatter);
    ] ~eq:PVariant.(=)
    "Which formatter to use when emitting the report"

and report_hook =
  CLOpt.mk_string_opt ~long:"report-hook"
    ~default:(lib_dir ^/ "python" ^/ "report.py")
    ~meta:"script"
    "Specify a script to be executed after the analysis results are written.  This script will be \
     passed $(b,--issues-csv), $(b,--issues-json), $(b,--issues-txt), $(b,--issues-xml), \
     $(b,--project-root), and $(b,--results-dir)."

and report_previous =
  CLOpt.mk_path_opt ~long:"report-previous" ~in_help:CLOpt.[ReportDiff, manual_generic]
    "Report of the base revision to use for comparison"

and resource_leak =
  CLOpt.mk_bool ~long:"resource-leak" ~default:false
    "the resource leak analysis (experimental)"

and resolve_infer_eradicate_conflict =
  CLOpt.mk_bool ~long:"resolve-infer-eradicate-conflict"
    ~default:false ~in_help:CLOpt.[ReportDiff, manual_generic]
    "Filter out Null Dereferences reported by Infer if Eradicate is enabled"

and rest =
  CLOpt.mk_rest_actions
    ~in_help:CLOpt.[Capture, manual_generic; Run, manual_generic]
    "Stop argument processing, use remaining arguments as a build command"
    ~usage:exe_usage
    (fun build_exe ->
       match Filename.basename build_exe with
       | "java" | "javac" -> CLOpt.Javac
       | _ -> CLOpt.NoParse
    )

and results_dir =
  CLOpt.mk_path ~deprecated:["results_dir"; "-out"] ~long:"results-dir" ~short:'o'
    ~default:(CLOpt.init_work_dir ^/ "infer-out")
    ~in_help:CLOpt.[Analyze, manual_generic; Capture, manual_generic; Run, manual_generic;
                    Report, manual_generic]
    ~meta:"dir" "Write results and internal files in the specified directory"

and save_results =
  CLOpt.mk_path_opt ~deprecated:["save_results"] ~long:"save-results"
    ~in_help:CLOpt.[Report, manual_generic]
    ~meta:"file.iar" "Save analysis results to Infer Analysis Results file file.iar"

and seconds_per_iteration =
  CLOpt.mk_float_opt ~deprecated:["seconds_per_iteration"] ~long:"seconds-per-iteration"
    ~meta:"float" "Set the number of seconds per iteration (see $(b,--iterations))"

and siof_safe_methods =
  CLOpt.mk_string_list ~long:"siof-safe-methods"
    ~in_help:CLOpt.[Analyze, manual_siof]
    "Methods that are SIOF-safe; \"foo::bar\" will match \"foo::bar()\", \"foo<int>::bar()\", \
     etc. (can be specified multiple times)"

and skip_analysis_in_path =
  CLOpt.mk_string_list ~deprecated:["-skip-clang-analysis-in-path"] ~long:"skip-analysis-in-path"
    ~in_help:CLOpt.[Capture, manual_generic; Run, manual_generic]
    ~meta:"path prefix OCaml regex"
    "Ignore files whose path matches the given prefix (can be specified multiple times)"

and skip_analysis_in_path_skips_compilation =
  CLOpt.mk_bool ~long:"skip-analysis-in-path-skips-compilation"
    ~in_help:CLOpt.[Report, manual_generic]
    ~default:false
    "Whether paths in --skip-analysis-in-path should be compiled or not"

and skip_duplicated_types =
  CLOpt.mk_bool ~long:"skip-duplicated-types" ~default:true
    ~in_help:CLOpt.[ReportDiff, manual_generic]
    "Skip fixed-then-introduced duplicated types while computing differential reports"

and skip_translation_headers =
  CLOpt.mk_string_list ~deprecated:["skip_translation_headers"] ~long:"skip-translation-headers"
    ~in_help:CLOpt.[Capture, manual_clang]
    ~meta:"path prefix" "Ignore headers whose path matches the given prefix"

and sources =
  CLOpt.mk_string_list ~long:"sources"
    "Specify the list of source files"

and sourcepath =
  CLOpt.mk_string_opt ~long:"sourcepath"
    "Specify the sourcepath"

and spec_abs_level =
  CLOpt.mk_int ~deprecated:["spec_abs_level"] ~long:"spec-abs-level" ~default:1
    ~meta:"int"
{|Set the level of abstracting the postconditions of discovered specs:
- 0 = nothing special
- 1 = filter out redundant posts implied by other posts
|}

and specs_library =
  let specs_library =
    CLOpt.mk_path_list ~deprecated:["lib"] ~long:"specs-library" ~short:'L'
      ~meta:"dir|jar" "Search for .spec files in given directory or jar file" in
  let _ =
    (* Given a filename with a list of paths, convert it into a list of string iff they are
       absolute *)
    let read_specs_dir_list_file fname =
      let validate_path path =
        if Filename.is_relative path then
          failwith ("Failing because path " ^ path ^ " is not absolute") in
      match Utils.read_file (resolve fname) with
      | Ok pathlist ->
          List.iter ~f:validate_path pathlist;
          pathlist
      | Error error ->
          failwithf "cannot read file '%s' from cwd '%s': %s" fname (Sys.getcwd ()) error
    in
    (* Add the newline-separated directories listed in <file> to the list of directories to be
       searched for .spec files *)
    CLOpt.mk_string ~deprecated:["specs-dir-list-file"; "-specs-dir-list-file"]
      ~long:"specs-library-index"
      ~default:""
      ~f:(fun file -> specs_library := (read_specs_dir_list_file file) @ !specs_library; "")
      ~in_help:CLOpt.[Analyze, manual_generic] ~meta:"file"
      "" in
  specs_library

and stacktrace =
  CLOpt.mk_path_opt ~deprecated:["st"] ~long:"stacktrace"
    ~in_help:CLOpt.[Analyze, manual_crashcontext]
    ~meta:"file" "File path containing a json-encoded Java crash stacktrace. Used to guide the \
                  analysis (only with '-a crashcontext').  See \
                  tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and stacktraces_dir =
  CLOpt.mk_path_opt ~long:"stacktraces-dir"
    ~in_help:CLOpt.[Analyze, manual_crashcontext]
    ~meta:"dir" "Directory path containing multiple json-encoded Java crash stacktraces. \
                 Used to guide the  analysis (only with '-a crashcontext').  See \
                 tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and stats_report =
  CLOpt.mk_path_opt ~long:"stats-report"
    ~meta:"file" "Write a report of the analysis results to a file"

and subtype_multirange =
  CLOpt.mk_bool ~deprecated:["subtype_multirange"] ~long:"subtype-multirange" ~default:true
    "Use the multirange subtyping domain"

and svg =
  CLOpt.mk_bool ~deprecated:["svg"] ~long:"svg"
    "Generate .dot and .svg files from specs"

and symops_per_iteration =
  CLOpt.mk_int_opt ~deprecated:["symops_per_iteration"] ~long:"symops-per-iteration"
    ~meta:"int" "Set the number of symbolic operations per iteration (see $(b,--iterations))"

and test_filtering =
  CLOpt.mk_bool ~deprecated:["test_filtering"] ~long:"test-filtering"
    "List all the files Infer can report on (should be called from the root of the project)"

and testing_mode =
  CLOpt.mk_bool ~deprecated:["testing_mode"; "-testing_mode"; "tm"] ~deprecated_no:["ntm"]
    ~long:"testing-mode"
    "Mode for testing, where no headers are translated, and dot files are created (clang only)"

and threadsafe_aliases =
  CLOpt.mk_json ~long:"threadsafe-aliases"
    ~in_help:CLOpt.[Analyze, manual_threadsafety]
    "Specify custom annotations that should be considered aliases of @ThreadSafe"

and trace_join =
  CLOpt.mk_bool ~deprecated:["trace_join"] ~long:"trace-join"
    "Detailed tracing information during prop join operations"

and trace_ondemand =
  CLOpt.mk_bool ~long:"trace-ondemand" ""

and trace_rearrange =
  CLOpt.mk_bool ~deprecated:["trace_rearrange"] ~long:"trace-rearrange"
    "Detailed tracing information during prop re-arrangement operations"

and tracing =
  CLOpt.mk_bool ~deprecated:["tracing"] ~long:"tracing"
    "Report error traces for runtime exceptions (Java only): generate preconditions for runtime\
     exceptions in Java and report errors for public methods which throw runtime exceptions"

and tv_limit =
  CLOpt.mk_int ~long:"tv-limit" ~default:100
    ~meta:"int" "The maximum number of traces to submit to Traceview"

and type_size =
  CLOpt.mk_bool ~deprecated:["type_size"] ~long:"type-size"
    "Consider the size of types during analysis, e.g. cannot use an int pointer to write to a char"

and unsafe_malloc =
  CLOpt.mk_bool ~long:"unsafe-malloc"
    ~in_help:CLOpt.[Analyze, manual_clang]
    "Assume that malloc(3) never returns null."

(** Set the path to the javac verbose output *)
and verbose_out =
  CLOpt.mk_path ~deprecated:["verbose_out"] ~long:"verbose-out" ~default:""
    ~meta:"file" ""

and version =
  let var = ref `None in
  CLOpt.mk_set var `Full ~deprecated:["version"] ~long:"version"
    ~in_help:CLOpt.[Run, manual_generic]
    "Print version information and exit" ;
  CLOpt.mk_set var `Json ~deprecated:["version_json"] ~long:"version-json"
    ~in_help:CLOpt.[Run, manual_generic]
    "Print version information in json format and exit" ;
  CLOpt.mk_set var `Vcs ~long:"version-vcs"
    "Print version control system commit and exit" ;
  var

and whole_seconds =
  CLOpt.mk_bool ~deprecated:["whole_seconds"] ~long:"whole-seconds"
    "Print whole seconds only"

(** visit mode for the worklist:
    0 depth - fist visit
    1 bias towards exit node
    2 least visited first *)
and worklist_mode =
  let var = ref 0 in
  CLOpt.mk_set var 2 ~long:"coverage"
    "analysis mode to maximize coverage (can take longer)" ;
  CLOpt.mk_set var 1 ~long:"exit-node-bias" ~deprecated:["exit_node_bias"]
    "nodes nearest the exit node are analyzed first" ;
  CLOpt.mk_set var 2 ~long:"visits-bias" ~deprecated:["visits_bias"]
    "nodes visited fewer times are analyzed first" ;
  var

and xcode_developer_dir =
  CLOpt.mk_path_opt ~long:"xcode-developer-dir"
    ~in_help:CLOpt.[Capture, manual_buck_flavors]
    ~meta:"XCODE_DEVELOPER_DIR" "Specify the path to Xcode developer directory"

and xcpretty =
  CLOpt.mk_bool ~long:"xcpretty"
    ~default:true
    ~in_help:CLOpt.[Capture, manual_clang]
    "Infer will use xcpretty together with xcodebuild to analyze an iOS app. xcpretty just needs \
     to be in the path, infer command is still just $(i,`infer -- <xcodebuild command>`). \
     (Recommended)"

and xml_specs =
  CLOpt.mk_bool ~deprecated:["xml"] ~long:"xml-specs"
    "Export specs into XML files file1.xml ... filen.xml"

(* The "rest" args must appear after "--" on the command line, and hence after other args, so they
   are allowed to refer to the other arg variables. *)

let javac_classes_out =
  CLOpt.mk_string_opt ~parse_mode:CLOpt.Javac
    ~deprecated:["classes_out"] ~long:"" ~short:'d'
    ~f:(fun classes_out ->
        if !buck then (
          let classes_out_infer = resolve classes_out ^/ buck_results_dir_name in
          (* extend env var args to pass args to children that do not receive the rest args *)
          CLOpt.extend_env_args ["--results-dir"; classes_out_infer] ;
          results_dir := classes_out_infer;
        );
        classes_out)
    ""

and _ =
  CLOpt.mk_string_opt ~parse_mode:CLOpt.Javac
    ~deprecated:["classpath";"cp"] ~long:""
    ~f:(fun classpath ->
        if !buck then (
          let paths = String.split classpath ~on:':' in
          let files = List.filter paths ~f:(fun path -> Sys.is_file path = `Yes) in
          CLOpt.extend_env_args (List.concat_map files ~f:(fun file -> ["--specs-library"; file])) ;
          specs_library := List.rev_append files !specs_library
        );
        classpath)
    ""


and () =
  CLOpt.mk_set ~parse_mode:CLOpt.Javac version
    ~deprecated:["version"] ~long:"" `Javac
    ""

(** Parse Command Line Args *)

let post_parsing_initialization command_opt =
  (match !version with
   | `Full ->
       (* TODO(11791235) change back to stdout once buck integration is fixed *)
       prerr_endline version_string
   | `Javac when !buck ->
       (* print buck key *)
       let javac_version =
         let javac_args =
           if infer_is_javac then
             match Array.to_list Sys.argv with
             | [] -> []
             | _::args -> "javac"::args
           else
             List.rev !rest in
         (* stderr contents of build command *)
         let chans = Unix.open_process_full (String.concat ~sep:" " javac_args) ~env:[||] in
         let err = String.strip (In_channel.input_all chans.stderr) in
         Unix.close_process_full chans |> ignore;
         err in
       let analyzer_name =
         List.Assoc.find_exn ~equal:equal_analyzer
           (List.map ~f:(fun (n,a) -> (a,n)) string_to_analyzer)
           (match !analyzer with Some a -> a | None -> BiAbduction) in
       let infer_version = Version.commit in
       F.eprintf "%s/%s/%s@." javac_version analyzer_name infer_version
   | `Javac ->
       prerr_endline version_string
   | `Json ->
       print_endline Version.versionJson
   | `Vcs ->
       print_endline Version.commit
   | `None -> ()
  );
  (match !help with
   | `Help ->
       CLOpt.show_manual !help_format CommandDoc.infer command_opt
   | `HelpFull ->
       CLOpt.show_manual ~internal_section:manual_internal !help_format CommandDoc.infer command_opt
   | `None ->
       ()
  );
  if !version <> `None || !help <> `None then exit 0;

  (* Core sets a verbose exception handler by default, with backtrace. This is good for developers
     but in user-mode we want something lighter weight. *)
  if not !developer_mode then
    Caml.Printexc.set_uncaught_exception_handler
      (fun exn _ ->
         let exn_msg = match exn with
           | Failure msg -> msg
           | _ -> Caml.Printexc.to_string exn in
         Format.eprintf "ERROR: %s@." exn_msg
      );

  F.set_margin !margin ;

  let set_minor_heap_size nMb = (* increase the minor heap size to speed up gc *)
    let ctrl = Gc.get () in
    let words_of_Mb nMb = nMb * 1024 * 1024 * 8 / Sys.word_size in
    let new_size = max ctrl.minor_heap_size (words_of_Mb nMb) in
    Gc.set { ctrl with minor_heap_size = new_size }
  in
  set_minor_heap_size 8 ;

  let symops_timeout, seconds_timeout =
    let default_symops_timeout = 1100 in
    let default_seconds_timeout = 10.0 in
    if !models_mode then
      (* disable timeouts when analyzing models *)
      (None, None)
    else
      (Some default_symops_timeout, Some default_seconds_timeout)
  in
  if is_none !symops_per_iteration then symops_per_iteration := symops_timeout ;
  if is_none !seconds_per_iteration then seconds_per_iteration := seconds_timeout ;

  clang_compilation_dbs :=
    List.rev_map ~f:(fun x -> `Raw x) !compilation_database
    |> List.rev_map_append ~f:(fun x -> `Escaped x) !compilation_database_escaped;

  (* set analyzer mode to linters in linters developer mode *)
  if !linters_developer_mode then (
    analyzer := Some Linters;
  );
  if !default_linters then
    linters_def_file := linters_def_default_file :: !linters_def_file;

  (match !analyzer with
   | Some BiAbduction -> biabduction := true
   | Some Crashcontext -> crashcontext := true
   | Some Eradicate -> eradicate := true
   | Some (CaptureOnly | CompileOnly | Checkers | Linters) -> ()
   | None ->
       let open CLOpt in
       match command_opt with
       | Some Compile -> analyzer := Some CompileOnly
       | Some Capture -> analyzer := Some CaptureOnly
       | _ -> biabduction := true (* the default option is to run the biabduction analysis *)
  );
  Option.value ~default:CLOpt.Run command_opt

let inferconfig_path () =
  let rec find dir =
    match Sys.file_exists ~follow_symlinks:false (dir ^/ CommandDoc.inferconfig_file) with
    | `Yes ->
        Some dir
    | `No | `Unknown ->
        let parent = Filename.dirname dir in
        let is_root = String.equal dir parent in
        if is_root then None
        else find parent in
  match Sys.getenv CommandDoc.inferconfig_env_var with
  | Some env_path ->
      (* make sure the path makes sense in children infer processes *)
      Some (
        if Filename.is_relative env_path then
          Utils.filename_to_absolute ~root:CLOpt.init_work_dir env_path
        else
          env_path
      )
  | None ->
      find (Sys.getcwd ())
      |> Option.map ~f:(fun dir -> dir ^/ CommandDoc.inferconfig_file)

let command, parse_args_and_return_usage_exit =
  let config_file = inferconfig_path () in
  let command_opt, usage_exit =
    CLOpt.parse ?config_file ~usage:exe_usage startup_action initial_command in
  let command = post_parsing_initialization command_opt in
  command, usage_exit

let print_usage_exit () =
  parse_args_and_return_usage_exit 1


(** Freeze initialized configuration values *)

let anon_args = !anon_args
and rest = !rest
and abs_struct = !abs_struct
and abs_val_orig = !abs_val
and allow_specs_cleanup = !allow_specs_cleanup
and analysis_path_regex_whitelist_options =
  List.map ~f:(fun (a, b) -> (a, !b)) analysis_path_regex_whitelist_options
and analysis_path_regex_blacklist_options =
  List.map ~f:(fun (a, b) -> (a, !b)) analysis_path_regex_blacklist_options
and analysis_blacklist_files_containing_options =
  List.map ~f:(fun (a, b) -> (a, !b)) analysis_blacklist_files_containing_options
and analysis_suppress_errors_options =
  List.map ~f:(fun (a, b) -> (a, !b)) analysis_suppress_errors_options
and analysis_stops = !analysis_stops
and angelic_execution = !angelic_execution
and annotation_reachability = !annotation_reachability
and annotation_reachability_custom_pairs = !annotation_reachability_custom_pairs
and array_level = !array_level
and ast_file = !ast_file
and biabduction = !biabduction
and blacklist = !blacklist
and bootclasspath = !bootclasspath
and bo_debug = !bo_debug
and buck = !buck
and buck_build_args = !buck_build_args
and buck_cache_mode = !buck && not !debug
and buck_compilation_database = !buck_compilation_database
and buck_out = !buck_out
and bufferoverrun = !bufferoverrun
and bugs_csv = !bugs_csv
and bugs_json = !bugs_json
and frontend_tests = !frontend_tests
and generated_classes = !generated_classes
and bugs_tests = !bugs_tests
and bugs_txt = !bugs_txt
and changed_files_index = !changed_files_index
and calls_csv = !calls_csv
and dump_duplicate_symbols = !dump_duplicate_symbols
and clang_biniou_file = !clang_biniou_file
and clang_ignore_regex = !clang_ignore_regex
and clang_include_to_override_regex = !clang_include_to_override_regex
and classpath = !classpath
and cluster_cmdline = !cluster
and compute_analytics = !compute_analytics
and continue_capture = !continue
and linter = !linter
and default_linters = !default_linters
and linters_ignore_clang_failures = !linters_ignore_clang_failures
and copy_propagation = !copy_propagation
and crashcontext = !crashcontext
and create_harness = !android_harness
and cxx = !cxx
and cxx_infer_headers = !cxx_infer_headers
and debug_level_analysis = !debug_level_analysis
and debug_level_capture = !debug_level_capture
and debug_level_linters = !debug_level_linters
and debug_exceptions = !debug_exceptions
and debug_mode = !debug
and dependency_mode = !dependencies
and developer_mode = !developer_mode
and differential_filter_files = !differential_filter_files
and differential_filter_set = !differential_filter_set
and disable_checks = !disable_checks
and dotty_cfg_libs = !dotty_cfg_libs
and enable_checks = !enable_checks
and eradicate = !eradicate
and eradicate_condition_redundant = !eradicate_condition_redundant
and eradicate_field_not_mutable = !eradicate_field_not_mutable
and eradicate_field_over_annotated = !eradicate_field_over_annotated
and eradicate_optional_present = !eradicate_optional_present
and eradicate_propagate_return_nullable = !eradicate_propagate_return_nullable
and eradicate_return_over_annotated = !eradicate_return_over_annotated
and eradicate_debug = !eradicate_debug
and eradicate_verbose = !eradicate_verbose
and fail_on_bug = !fail_on_bug
and failures_allowed = !failures_allowed
and fcp_apple_clang = !fcp_apple_clang
and fcp_syntax_only = !fcp_syntax_only
and file_renamings = !file_renamings
and filter_paths = !filter_paths
and filtering = !filtering
and flavors = !flavors
and fragment_retains_view = !fragment_retains_view
and from_json_report = !from_json_report
and frontend_debug = !frontend_debug
and frontend_stats = !frontend_stats
and headers = !headers
and icfg_dotty_outfile = !icfg_dotty_outfile
and ignore_trivial_traces = !ignore_trivial_traces
and immutable_cast = !immutable_cast
and infer_cache = !infer_cache
and iphoneos_target_sdk_version = !iphoneos_target_sdk_version
and iphoneos_target_sdk_version_skip_path = !iphoneos_target_sdk_version_skip_path
and issues_fields = !issues_fields
and iterations = !iterations
and java_jar_compiler = !java_jar_compiler
and javac_classes_out = !javac_classes_out
and javac_verbose_out = !verbose_out
and jobs = !jobs
and join_cond = !join_cond
and latex = !latex
and linters_def_file = !linters_def_file
and linters_def_folder = !linters_def_folder
and linters_developer_mode = !linters_developer_mode
and load_average = match !load_average with
  | None when !buck ->
      Some (float_of_int ncpu)
  | _ ->
      !load_average
and load_analysis_results = !load_results
and log_file = !log_file
and makefile_cmdline = !makefile
and merge = !merge
and ml_buckets = !ml_buckets
and models_mode = !models_mode
and modified_targets = !modified_targets
and monitor_prop_size = !monitor_prop_size
and nelseg = !nelseg
and suggest_nullable = !suggest_nullable
and no_translate_libs = not !headers
and objc_memory_model_on = !objc_memory_model
and only_cheap_debug = !only_cheap_debug
and only_footprint = !only_footprint
and passthroughs = !passthroughs
and patterns_never_returning_null = match patterns_never_returning_null with (k,r) -> (k,!r)
and patterns_skip_translation = match patterns_skip_translation with (k,r) -> (k,!r)
and patterns_modeled_expensive = match patterns_modeled_expensive with (k,r) -> (k,!r)
and per_procedure_parallelism = !per_procedure_parallelism
and pmd_xml = !pmd_xml
and precondition_stats = !precondition_stats
and printf_args = !printf_args
and print_active_checkers = !print_active_checkers
and print_builtins = !print_builtins
and print_logs = !print_logs
and print_types = !print_types
and print_using_diff = !print_using_diff
and procedures_per_process = !procedures_per_process
and procs_csv = !procs_csv
and procs_xml = !procs_xml
and project_root = !project_root
and quandary = !quandary
and quandary_endpoints = !quandary_endpoints
and quandary_sanitizers = !quandary_sanitizers
and quandary_sources = !quandary_sources
and quandary_sinks = !quandary_sinks
and quiet = !quiet
and reactive_mode = !reactive
and reactive_capture = !reactive_capture
and repeated_calls = !repeated_calls
and report = !report
and report_current = !report_current
and report_custom_error = !report_custom_error
and report_formatter = !report_formatter
and report_hook = !report_hook
and report_previous = !report_previous
and reports_include_ml_loc = !reports_include_ml_loc
and resolve_infer_eradicate_conflict = !resolve_infer_eradicate_conflict
and resource_leak = !resource_leak
and results_dir = !results_dir
and save_analysis_results = !save_results
and seconds_per_iteration = !seconds_per_iteration
and show_buckets = !print_buckets
and show_progress_bar = !progress_bar
and siof = !siof
and siof_safe_methods = !siof_safe_methods
and skip_analysis_in_path = !skip_analysis_in_path
and skip_analysis_in_path_skips_compilation = !skip_analysis_in_path_skips_compilation
and skip_duplicated_types = !skip_duplicated_types
and skip_translation_headers = !skip_translation_headers
and sources = !sources
and sourcepath = !sourcepath
and spec_abs_level = !spec_abs_level
and stacktrace = !stacktrace
and stacktraces_dir = !stacktraces_dir
and stats_mode = !stats
and stats_report = !stats_report
and subtype_multirange = !subtype_multirange
and svg = !svg
and symops_per_iteration = !symops_per_iteration
and test_filtering = !test_filtering
and testing_mode = !testing_mode
and threadsafety = !threadsafety
and threadsafe_aliases = !threadsafe_aliases
and trace_error = !trace_error
and trace_ondemand = !trace_ondemand
and trace_join = !trace_join
and trace_rearrange = !trace_rearrange
and tracing = !tracing
and tv_limit = !tv_limit
and type_size = !type_size
and unsafe_malloc = !unsafe_malloc
and whole_seconds = !whole_seconds
and worklist_mode = !worklist_mode
and write_dotty = !write_dotty
and write_html = !write_html
and write_html_whitelist_regex = !write_html_whitelist_regex
and xcode_developer_dir = !xcode_developer_dir
and xcpretty = !xcpretty
and xml_specs = !xml_specs


(** Configuration values derived from command-line options *)

let analysis_path_regex_whitelist analyzer =
  List.Assoc.find_exn ~equal:equal_analyzer analysis_path_regex_whitelist_options analyzer
and analysis_path_regex_blacklist analyzer =
  List.Assoc.find_exn ~equal:equal_analyzer analysis_path_regex_blacklist_options analyzer
and analysis_blacklist_files_containing analyzer =
  List.Assoc.find_exn ~equal:equal_analyzer analysis_blacklist_files_containing_options analyzer
and analysis_suppress_errors analyzer =
  List.Assoc.find_exn ~equal:equal_analyzer analysis_suppress_errors_options analyzer

let captured_dir = results_dir ^/ captured_dir_name

let clang_frontend_do_capture, clang_frontend_do_lint =
  match !clang_frontend_action with
  | Some `Lint -> false, true (* no capture, lint *)
  | Some `Capture -> true, false (* capture, no lint *)
  | Some `Lint_and_capture -> true, true (* capture, lint *)
  | None ->
      match !analyzer with
      | Some Linters -> false, true (* no capture, lint *)
      | Some BiAbduction -> true, false (* capture, no lint *)
      | _ -> true, true (* capture, lint *)

let analyzer = match !analyzer with Some a -> a | None -> BiAbduction

let clang_frontend_action_string =
  String.concat ~sep:" and "
    ((if clang_frontend_do_capture then ["translating"] else [])
     @ (if clang_frontend_do_lint then ["linting"] else []))

let dynamic_dispatch =
  let default_mode =
    match analyzer with
    | BiAbduction -> `Lazy
    | Checkers when quandary -> `Sound
    | _ -> `None in
  Option.value ~default:default_mode !dynamic_dispatch

let specs_library =
  match infer_cache with
  | Some cache_dir when use_jar_cache ->
      let add_spec_lib specs_library filename =
        let basename = Filename.basename filename in
        let key = basename ^ Utils.string_crc_hex32 filename in
        let key_dir = cache_dir ^/ key in
        let extract_specs dest_dir filename =
          if Filename.check_suffix filename ".jar" then
            match (Unix.mkdir dest_dir ~perm:0o700) with
            | exception Unix.Unix_error _ ->
                ()
            | () ->
                let zip_channel = Zip.open_in filename in
                let entries = Zip.entries zip_channel in
                let extract_entry (entry : Zip.entry) =
                  let dest_file = dest_dir ^/ (Filename.basename entry.filename) in
                  if Filename.check_suffix entry.filename specs_files_suffix
                  then Zip.copy_entry_to_file zip_channel entry dest_file in
                List.iter ~f:extract_entry entries;
                Zip.close_in zip_channel in
        extract_specs key_dir filename;
        key_dir :: specs_library in
      List.fold ~f:add_spec_lib ~init:[] !specs_library
  | _ ->
      !specs_library


(** Global variables *)

let set_reference_and_call_function reference value f x =
  let saved = !reference in
  let restore () =
    reference := saved in
  try
    reference := value;
    let res = f x in
    restore ();
    res
  with
  | exn ->
      restore ();
      raise exn

(** Current Objective-C Automatic Reference Counting (ARC) mode *)
let arc_mode = ref false

(** Current language *)
let curr_language = ref Clang

let curr_language_is lang =
  equal_language !curr_language lang

(** Flag for footprint discovery mode *)
let footprint = ref true

let run_in_footprint_mode f x =
  set_reference_and_call_function footprint true f x

let run_in_re_execution_mode f x =
  set_reference_and_call_function footprint false f x

(** Set in the middle of forcing delayed prints *)
let forcing_delayed_prints = ref false

(** if true, user simple pretty printing *)
let pp_simple = ref true

let reset_abs_val () =
  abs_val := abs_val_orig

let run_with_abs_val_equal_zero f x =
  set_reference_and_call_function abs_val 0 f x
