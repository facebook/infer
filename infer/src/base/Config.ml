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

(** Configuration values: either constant, determined at compile time, or set at startup
    time by system calls, environment variables, or command line options *)

module CLOpt = CommandLineOption
module F = Format


type analyzer = Capture | Compile | Infer | Eradicate | Checkers | Tracing
              | Crashcontext | Linters | Quandary | Threadsafety

let string_to_analyzer =
  [("capture", Capture); ("compile", Compile);
   ("infer", Infer); ("eradicate", Eradicate); ("checkers", Checkers);
   ("tracing", Tracing); ("crashcontext", Crashcontext); ("linters", Linters);
   ("quandary", Quandary); ("threadsafety", Threadsafety)]

let clang_frontend_action_symbols = [
  ("lint", `Lint);
  ("capture", `Capture);
  ("lint_and_capture", `Lint_and_capture);
]

type language = Clang | Java [@@deriving compare]

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

(** exit code to use for the --fail-on-issue option *)
let fail_on_issue_exit_code = 2

(** If true, filter out errors in low likelyhood buckets, and only show then in developer
    mode *)
let filter_buckets = false

let frontend_stats_dir_name = "frontend_stats"

let global_tenv_filename = "global.tenv"

(** If true, treat calls to no-arg getters as idempotent w.r.t non-nullness *)
let idempotent_getters = true

(** If true, changes to code are checked at the procedure level; if false, at the file
    level *)
let incremental_procs = true

(** Our Python script does its own argument parsing and will fail with this error on failure *)
let infer_py_argparse_error_exit_code = 22

(** Name of the infer configuration file *)
let inferconfig_file = ".inferconfig"

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

(** If true, save the execution time in summaries.  This makes the analysis
    nondeterministic. *)
let save_time_in_summaries = false

(** If true enables printing proposition compatible for the SMT project *)
let smt_output = false

let source_file_extentions = [".java"; ".m"; ".mm"; ".c"; ".cc"; ".cpp"; ".h"]

let specs_dir_name = "specs"

let specs_files_suffix = ".specs"

let start_filename = ".start"

let suppress_warnings_annotations_long = "suppress-warnings-annotations"

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
  ["std"; "move"];
  ["std"; "forward"];
  ["std"; "min"];
  ["std"; "max"];
  ["std"; "__less"];
  ["google"; "CheckNotNull"];
]


(** Compile time configuration values *)

let version_string =
  "Infer version "
  ^ Version.versionString
  ^ "\nCopyright 2009 - present Facebook. All Rights Reserved."


(** System call configuration values *)

(** Initial time of the analysis, i.e. when this module is loaded, gotten from
    Unix.time *)
let initial_analysis_time = Unix.time ()

(* Resolve symlinks to get to the real executable. The real executable is located in [bin_dir]
   below, which allows us to find [lib_dir], [models_dir], etc., relative to it. *)
let real_exe_name =
  Utils.realpath Sys.executable_name

let current_exe =
  if !Sys.interactive then CLOpt.Interactive
  else try IList.assoc String.equal (Filename.basename real_exe_name) CLOpt.exes
    with Not_found -> CLOpt.Toplevel

let bin_dir =
  Filename.dirname real_exe_name

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
  let dir = bin_dir ^/ Filename.parent_dir_name ^/ "models" in
  Utils.filename_to_absolute dir (* Normalize the path *)

let relative_cpp_extra_include_dir = "cpp" ^/ "include"

let cpp_extra_include_dir = models_src_dir ^/ relative_cpp_extra_include_dir

let relative_cpp_models_dir =
  relative_cpp_extra_include_dir ^/ "infer_model"

let wrappers_dir =
  lib_dir ^/ "wrappers"

let ncpu =
  try
    Utils.with_process_in
      "getconf _NPROCESSORS_ONLN 2>/dev/null"
      (fun chan -> Scanf.fscanf chan "%d" (fun n -> n))
    |> fst
  with _ ->
    1

let os_type = match Sys.os_type with
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> Unix


(** The working directory of the initial invocation of infer, to which paths passed as command line
    options are relative. *)
let init_work_dir, is_originator =
  match Sys.getenv "INFER_CWD" with
  | Some dir ->
      (dir, false)
  | None ->
      let cwd =
        (* Use PWD if it denotes the same inode as ., to try to avoid paths with symlinks resolved *)
        (* Approach is borrowed from llvm implementation of *)
        (* llvm::sys::fs::current_path (implemented in Path.inc file) *)
        match Sys.getenv "PWD" with
        | Some pwd ->
            let pwd_stat = Unix.stat pwd in
            let dot_stat = Unix.stat "." in
            if pwd_stat.st_dev = dot_stat.st_dev && pwd_stat.st_ino = dot_stat.st_ino then
              pwd
            else
              Sys.getcwd ()
        | None ->
            Sys.getcwd () in
      let real_cwd = Utils.realpath cwd in
      Unix.putenv ~key:"INFER_CWD" ~data:real_cwd;
      (real_cwd, true)

(** Resolve relative paths passed as command line options, i.e., with respect to the working
    directory of the initial invocation of infer. *)
let resolve = Utils.filename_to_absolute


(** Command Line options *)

(* Declare the phase 1 options *)

let inferconfig_home =
  let all_exes = IList.map snd CLOpt.exes in
  CLOpt.mk_path_opt ~long:"inferconfig-home"
    ~exes:all_exes ~meta:"dir" "Path to the .inferconfig file"

and project_root =
  CLOpt.mk_path ~deprecated:["project_root"; "-project_root"] ~long:"project-root" ~short:"pr"
    ~default:init_work_dir
    ~exes:CLOpt.[Analyze;Clang;Java;Print;Toplevel]
    ~meta:"dir" "Specify the root directory of the project"

(* Parse the phase 1 options, ignoring the rest *)

let _ = CLOpt.parse ~incomplete:true current_exe (fun _ -> "")

(* Define the values that depend on phase 1 options *)

let inferconfig_home = !inferconfig_home
and project_root = !project_root

let inferconfig_path =
  match inferconfig_home with
  | Some dir -> dir ^/ inferconfig_file
  | None -> project_root ^/ inferconfig_file

(* Proceed to declare and parse the remaining options *)

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

let anon_args =
  CLOpt.mk_anon ()

and abs_struct =
  CLOpt.mk_int ~deprecated:["absstruct"] ~long:"abs-struct" ~default:1
    ~meta:"int" "Specify abstraction level for fields of structs:\n\
                 - 0 = no\n\
                 - 1 = forget some fields during matching (and so lseg abstraction)"

and abs_val =
  CLOpt.mk_int ~deprecated:["absval"] ~long:"abs-val" ~default:2
    ~meta:"int" "Specify abstraction level for expressions:\n\
                 - 0 = no abstraction\n\
                 - 1 = evaluate all expressions abstractly\n\
                 - 2 = 1 + abstract constant integer values during join"


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
        IList.map (Printf.sprintf "%s_%s" analyzer_name) deprecated_suffix in
      (* empty doc to hide the options from --help since there are many redundant ones *)
      CLOpt.mk_string_list ~deprecated ~long ~meta "" in
    ignore (
      let long = "<analyzer>-" ^ suffix in
      CLOpt.mk_string_list ~long ~meta ~f:(fun _ -> raise (Arg.Bad "invalid option"))
        ~exes:CLOpt.[Toplevel;Print]
        help
    );
    IList.map (fun (name, analyzer) -> (analyzer, mk_option name)) string_to_analyzer in
  (
    mk_filtering_options
      ~suffix:"blacklist-files-containing"
      ~deprecated_suffix:["blacklist_files_containing"]
      ~help:"blacklist files containing the specified string for the given analyzer (see \
             --analyzer for valid values)"
      ~meta:"string",
    mk_filtering_options
      ~suffix:"blacklist-path-regex"
      ~deprecated_suffix:["blacklist"]
      ~help:"blacklist the analysis of files whose relative path matches the specified OCaml-style \
             regex\n\
             (to whitelist: --<analyzer>-whitelist-path-regex)"
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
  let () = match Infer with
    (* NOTE: if compilation fails here, it means you have added a new analyzer without updating the
       documentation of this option *)
    | Capture | Compile | Infer | Eradicate | Checkers | Tracing | Crashcontext | Linters
    | Quandary | Threadsafety -> () in
  CLOpt.mk_symbol_opt ~deprecated:["analyzer"] ~long:"analyzer" ~short:"a"
    ~exes:CLOpt.[Toplevel]
    "Specify which analyzer to run (only one at a time is supported):\n\
     - infer, eradicate, checkers, quandary, threadsafety: run the specified analysis\n\
     - capture: run capture phase only (no analysis)\n\
     - compile: run compilation command without interfering (not supported by all frontends)\n\
     - crashcontext, tracing: experimental (see --crashcontext and --tracing)\n\
     - linters: run linters based on the ast only (Objective-C and Objective-C++ only)"
    ~symbols:string_to_analyzer

and android_harness =
  CLOpt.mk_bool ~deprecated:["harness"] ~long:"android-harness"
    ~exes:CLOpt.[Java]
    "(Experimental) Create harness to detect issues involving the Android lifecycle"

and angelic_execution =
  CLOpt.mk_bool ~deprecated:["angelic_execution"] ~long:"angelic-execution" ~default:true
    "Angelic execution, where the analysis ignores errors caused by unknown procedure calls"

and array_level =
  CLOpt.mk_int ~deprecated:["arraylevel"] ~long:"array-level" ~default:0
    ~meta:"int" "Level of treating the array indexing and pointer arithmetic:\n\
                 - 0 = treats both features soundly\n\
                 - 1 = assumes that the size of every array is infinite\n\
                 - 2 = assumes that all heap dereferences via array indexing and pointer \
                 arithmetic are correct"
and ast_file =
  CLOpt.mk_path_opt ~long:"ast-file" ~short:"ast"
    ~meta:"file" "AST file for the translation"

and blacklist =
  CLOpt.mk_string_opt ~deprecated:["-blacklist-regex";"-blacklist"] ~long:"buck-blacklist"
    ~exes:CLOpt.[Toplevel]
    ~meta:"regex" "Skip analysis of files matched by the specified regular expression (Buck \
                   flavors only)"

and bootclasspath =
  CLOpt.mk_string_opt ~long:"bootclasspath"
    ~exes:CLOpt.[Toplevel; Java]
    "Specify the Java bootclasspath"

(** Automatically set when running from within Buck *)
and buck =
  CLOpt.mk_bool ~long:"buck"
    ""

and buck_build_args =
  CLOpt.mk_string_list ~long:"Xbuck"
    ~exes:CLOpt.[Toplevel]
    "Pass values as command-line arguments to invocations of `buck build` (Buck flavors only)"

and buck_out =
  CLOpt.mk_path_opt ~long:"buck-out"
    ~exes:CLOpt.[Toplevel] ~meta:"dir" "Specify the root directory of buck-out"

and bugs_csv =
  CLOpt.mk_path_opt ~deprecated:["bugs"] ~long:"issues-csv"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file" "Write a list of issues in CSV format to a file"

and bugs_json =
  CLOpt.mk_path_opt ~deprecated:["bugs_json"] ~long:"issues-json"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file" "Write a list of issues in JSON format to a file"

and bugs_tests =
  CLOpt.mk_path_opt ~long:"issues-tests"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file"
    "Write a list of issues in a format suitable for tests to a file"

and bugs_txt =
  CLOpt.mk_path_opt ~deprecated:["bugs_txt"] ~long:"issues-txt"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file"
    "Write a list of issues in TXT format to a file"

and bugs_xml =
  CLOpt.mk_path_opt ~deprecated:["bugs_xml"] ~long:"issues-xml"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file"
    "Write a list of issues in XML format to a file"

and calls_csv =
  CLOpt.mk_path_opt ~deprecated:["calls"] ~long:"calls-csv"
    ~exes:CLOpt.[Toplevel;Print]
    ~meta:"file"
    "Write individual calls in CSV format to a file"

and changed_files_index =
  CLOpt.mk_path_opt ~long:"changed-files-index" ~exes:CLOpt.[Toplevel] ~meta:"file"
    "Specify the file containing the list of source files from which reactive analysis should \
     start. Source files should be specified relative to project root or be absolute"

and check_duplicate_symbols =
  CLOpt.mk_bool ~long:"check-duplicate-symbols"
    ~exes:CLOpt.[Analyze]
    "Check if a symbol with the same name is defined in more than one file."

and checkers, crashcontext, eradicate, quandary, threadsafety =
  let checkers =
    CLOpt.mk_bool ~deprecated:["checkers"] ~long:"checkers"
      "Activate the checkers instead of the full analysis"
  in
  let crashcontext =
    CLOpt.mk_bool_group ~deprecated:["crashcontext"] ~long:"crashcontext"
      "Activate the crashcontext checker for java stack trace context reconstruction"
      [checkers] []
  in
  let eradicate =
    CLOpt.mk_bool_group ~deprecated:["eradicate"] ~long:"eradicate"
      "Activate the eradicate checker for java annotations"
      [checkers] []
  in
  let quandary =
    CLOpt.mk_bool_group ~deprecated:["quandary"] ~long:"quandary"
      "Activate the quandary taint analysis"
      [checkers] []
  in
  let threadsafety =
    CLOpt.mk_bool_group ~deprecated:["threadsafety"] ~long:"threadsafety"
      "Activate the thread safety analysis"
      [checkers] []
  in
  (checkers, crashcontext, eradicate, quandary, threadsafety)

and checkers_repeated_calls =
  CLOpt.mk_bool ~long:"checkers-repeated-calls"
    "Check for repeated calls"

and clang_biniou_file =
  CLOpt.mk_path_opt ~long:"clang-biniou-file" ~exes:CLOpt.[Clang] ~meta:"file"
    "Specify a file containing the AST of the program, in biniou format"

and clang_compilation_db_files =
  CLOpt.mk_path_list ~long:"clang-compilation-db-files"
    "Files that contain compilation commands"

and clang_frontend_action =
  CLOpt.mk_symbol_opt ~long:"clang-frontend-action"
    ~exes:CLOpt.[Clang]
    "Specify whether the clang frontend should capture or lint or both."
    ~symbols:clang_frontend_action_symbols

and clang_include_to_override =
  CLOpt.mk_string_opt ~long:"clang-include-to-override" ~meta:"dir"
    "Use this option in the uncommon case where the normal compilation process overrides the \
     location of internal compiler headers. This option should specify the path to those headers \
     so that infer can use its own clang internal headers instead."

and classpath =
  CLOpt.mk_string_opt ~long:"classpath"
    ~exes:CLOpt.[Java]
    "Specify the Java classpath"

and cluster =
  CLOpt.mk_path_opt ~deprecated:["cluster"] ~long:"cluster"
    ~meta:"file" "Specify a .cluster file to be analyzed"

and compute_analytics =
  CLOpt.mk_bool ~long:"compute-analytics"
    ~default:false
    ~exes:CLOpt.[Toplevel;Clang]
    "Emit analytics as info-level issues, like component kit line count and \
     component kit file cyclomatic complexity"

(** Continue the capture for reactive mode:
    If a procedure was changed beforehand, keep the changed marking. *)
and continue =
  CLOpt.mk_bool ~deprecated:["continue"] ~long:"continue"
    ~exes:CLOpt.[Toplevel]
    "Continue the capture for the reactive analysis, increasing the changed files/procedures. (If \
     a procedure was changed beforehand, keep the changed marking.)"

and copy_propagation =
  CLOpt.mk_bool ~deprecated:["copy-propagation"] ~long:"copy-propagation"
    "Perform copy-propagation on the IR"

and cxx =
  CLOpt.mk_bool ~deprecated:["cxx-experimental"] ~long:"cxx"
    ~default:true
    ~exes:CLOpt.[Clang]
    "Analyze C++ methods"

and (
  developer_mode,
  debug,
  debug_exceptions,
  filtering,
  print_buckets,
  print_types,
  reports_include_ml_loc,
  test,
  trace_error,
  write_html,
  write_dotty
) =
  let developer_mode =
    CLOpt.mk_bool ~long:"developer-mode"
      ~default:CLOpt.(current_exe = Print)
      "Show internal exceptions"

  and filtering =
    CLOpt.mk_bool ~long:"filtering" ~short:"f" ~default:true
      ~exes:CLOpt.[Toplevel]
      "Do not show the results from experimental checks (note: some of them may contain many false \
       alarms)"

  and print_buckets =
    CLOpt.mk_bool ~long:"print-buckets"
      "Show the internal bucket of Infer reports in their textual description"

  and print_types =
    CLOpt.mk_bool ~long:"print-types"
      ~default:(current_exe = CLOpt.Clang)
      "Print types in symbolic heaps"

  and reports_include_ml_loc =
    CLOpt.mk_bool ~deprecated:["with_infer_src_loc"] ~long:"reports-include-ml-loc"
      "Include the location in the Infer source code from where reports are generated"

  and test =
    CLOpt.mk_bool ~long:"only-cheap-debug"
      ~default:true
      "Disable expensive debugging output"

  and trace_error =
    CLOpt.mk_bool ~long:"trace-error"
      "Detailed tracing information during error explanation"

  and write_html =
    CLOpt.mk_bool ~long:"write-html"
      "Produce hmtl debug output in the results directory"

  and write_dotty =
    CLOpt.mk_bool ~long:"write-dotty"
      "Produce dotty files for specs in the results directory"
  in
  let debug =
    CLOpt.mk_bool_group ~deprecated:["debug"] ~long:"debug" ~short:"g"
      ~exes:CLOpt.[Analyze]
      "Debug mode (also sets --developer-mode, --no-filtering, --print-buckets, --print-types, \
       --reports-include-ml-loc, --no-test, --trace-error, --write-dotty, --write-html)"
      [developer_mode; print_buckets; print_types; reports_include_ml_loc; trace_error; write_html;
       write_dotty]
      [filtering; test]

  and debug_exceptions =
    CLOpt.mk_bool_group ~long:"debug-exceptions"
      "Generate lightweight debugging information: just print the internal exceptions during \
       analysis (also sets --developer-mode, --no-filtering, --print-buckets, \
       --reports-include-ml-loc)"
      [developer_mode; print_buckets; reports_include_ml_loc]
      [filtering]
  in (
    developer_mode,
    debug,
    debug_exceptions,
    filtering,
    print_buckets,
    print_types,
    reports_include_ml_loc,
    test,
    trace_error,
    write_html,
    write_dotty
  )
and dependencies =
  CLOpt.mk_bool ~deprecated:["dependencies"] ~long:"dependencies"
    ~exes:CLOpt.[Java]
    "Translate all the dependencies during the capture. The classes in the given jar file will be \
     translated. No sources needed."

and disable_checks =
  CLOpt.mk_string_list ~deprecated:["disable_checks"] ~long:"disable-checks" ~meta:"error name"
    ~exes:CLOpt.[Toplevel;Print]
    "Do not show reports coming from this type of errors"

and dotty_cfg_libs =
  CLOpt.mk_bool ~deprecated:["dotty_no_cfg_libs"] ~long:"dotty-cfg-libs" ~default:true
    "Print the cfg of the code coming from the libraries"

and dynamic_dispatch =
  CLOpt.mk_symbol ~long:"dynamic-dispatch" ~default:`None
    "Specify treatment of dynamic dispatch in Java code: 'none' treats dynamic dispatch as a call \
     to unknown code, 'lazy' follows the JVM semantics and creates procedure descriptions during \
     symbolic execution using the type information found in the abstract state; 'sound' is \
     significantly more computationally expensive"
    ~symbols:[("none", `None); ("lazy", `Lazy); ("sound", `Sound)]

and enable_checks =
  CLOpt.mk_string_list ~deprecated:["enable_checks"] ~long:"enable-checks" ~meta:"error name"
    "Show reports coming from this type of errors"

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

and eradicate_trace =
  CLOpt.mk_bool ~long:"eradicate-trace"
    "Print step-by-step tracing information"

and eradicate_verbose =
  CLOpt.mk_bool ~long:"eradicate-verbose"
    "Print initial and final typestates"

(* Use file for the err channel *)
and err_file =
  CLOpt.mk_string ~deprecated:["err_file"] ~long:"err-file" ~default:""
    ~meta:"file" ""

and fail_on_bug =
  CLOpt.mk_bool ~deprecated:["-fail-on-bug"] ~long:"fail-on-issue" ~default:false
    ~exes:CLOpt.[Toplevel]
    (Printf.sprintf "Exit with error code %d if Infer found something to report"
       fail_on_issue_exit_code)

and failures_allowed =
  CLOpt.mk_bool ~deprecated_no:["-no_failures_allowed"] ~long:"failures-allowed" ~default:true
    "Fail if at least one of the translations fails (clang only)"

and fcp_apple_clang =
  CLOpt.mk_path_opt ~long:"fcp-apple-clang"
    ~meta:"path" "Specify the path to Apple Clang"

and fcp_syntax_only =
  CLOpt.mk_bool ~long:"fcp-syntax-only"
    "Skip creation of object files"

and filter_paths =
  CLOpt.mk_bool ~long:"filter-paths" ~default:true
    "Filters specified in .inferconfig"

and flavors =
  CLOpt.mk_bool ~deprecated:["-use-flavors"] ~long:"flavors"
    ~exes:CLOpt.[Toplevel]
    "Buck integration using Buck flavors (clang only), eg `infer --flavors -- buck build \
     //foo:bar#infer`"

and from_json_report =
  CLOpt.mk_path_opt ~long:"from-json-report"
    ~exes:CLOpt.[Print]
    ~meta:"report.json"
    "Load analysis results from a report file (default is to load the results from the specs \
     files generated by the analysis)."

and frontend_debug =
  CLOpt.mk_bool ~long:"frontend-debug" ~short:"fd"
    "Emit debug info to *.o.astlog and a script *.o.sh that replays the command used to run clang \
     with the plugin attached, piped to the InferClang frontend command (clang only)"

and frontend_stats =
  CLOpt.mk_bool ~long:"frontend-stats" ~short:"fs"
    "Output statistics about the capture phase to *.o.astlog (clang only)"

and frontend_tests =
  CLOpt.mk_bool ~long:"frontend-tests"
    ~exes:CLOpt.frontend_exes
    "Save filename.ext.test.dot with the cfg in dotty format for frontend tests"

and generated_classes =
  CLOpt.mk_path_opt ~long:"generated-classes"
    ~exes:CLOpt.[Toplevel; Java]
    "Specify where to load the generated class files"

and headers =
  CLOpt.mk_bool ~deprecated:["headers"] ~deprecated_no:["no_headers"] ~long:"headers" ~short:"hd"
    ~exes:CLOpt.[Clang]
    "Analyze code in header files"

and icfg_dotty_outfile =
  CLOpt.mk_path_opt ~long:"icfg-dotty-outfile" ~meta:"path"
    "If set, specifies path where .dot file should be written, it overrides the path for all \
     other options that would generate icfg file otherwise"

and infer_cache =
  CLOpt.mk_path_opt ~deprecated:["infer_cache"; "-infer_cache"] ~long:"infer-cache"
    ~meta:"dir" "Select a directory to contain the infer cache (Buck and Java only)"

and iterations =
  CLOpt.mk_int ~deprecated:["iterations"] ~long:"iterations" ~default:1
    ~meta:"int"
    "Specify the maximum number of operations for each function, expressed as a multiple of \
     symbolic operations and a multiple of seconds of elapsed time"

and java_jar_compiler =
  CLOpt.mk_path_opt
    ~long:"java-jar-compiler"
    ~exes:CLOpt.[Java]
    ~meta:"path" "Specifify the Java compiler jar used to generate the bytecode"

and jobs =
  CLOpt.mk_int ~deprecated:["-multicore"] ~long:"jobs" ~short:"j" ~default:ncpu
    ~exes:CLOpt.[Toplevel] ~meta:"int" "Run the specified number of analysis jobs simultaneously"

and join_cond =
  CLOpt.mk_int ~deprecated:["join_cond"] ~long:"join-cond" ~default:1
    ~meta:"int" "Set the strength of the final information-loss check used by the join:\n\
                 - 0 = use the most aggressive join for preconditions\n\
                 - 1 = use the least aggressive join for preconditions"

and latex =
  CLOpt.mk_path_opt ~deprecated:["latex"] ~long:"latex"
    ~meta:"file"
    "Write a latex report of the analysis results to a file"

and linters_def_file =
  CLOpt.mk_path_opt ~long:"linters-def-file" ~exes:CLOpt.[Clang]
    ~meta:"file" "Specify the file containing linters definition"

and load_average =
  CLOpt.mk_float_opt ~long:"load-average" ~short:"l"
    ~exes:CLOpt.[Toplevel] ~meta:"float"
    "Do not start new parallel jobs if the load average is greater than that specified (Buck and \
     make only)"

and load_results =
  CLOpt.mk_path_opt ~deprecated:["load_results"] ~long:"load-results"
    ~exes:CLOpt.[Print]
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
    ~exes:CLOpt.[Toplevel]
    "Merge the captured results directories specified in the dependency file (Buck flavors only)"

and ml_buckets =
  CLOpt.mk_symbol_seq ~deprecated:["ml_buckets"; "-ml_buckets"] ~long:"ml-buckets"
    ~default:[`MLeak_cf]
    ~exes:CLOpt.[Clang]
    "Specify the memory leak buckets to be checked in Objective-C/C++:\n\
     - 'cf' checks leaks from Core Foundation,\n\
     - 'arc' from code compiled in ARC mode,\n\
     - 'narc' from code not compiled in ARC mode,\n\
     - 'cpp' from C++ code"
    ~symbols:ml_bucket_symbols

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

and optimistic_cast =
  CLOpt.mk_bool ~deprecated:["optimistic_cast"] ~long:"optimistic-cast"
    "Allow cast of undefined values"

and out_file =
  CLOpt.mk_path ~deprecated:["out_file"] ~long:"out-file" ~default:""
    ~meta:"file" "Specify the file for the non-error logs of the analyzer"

and patterns_modeled_expensive =
  let long = "modeled-expensive" in
  (long,
   CLOpt.mk_json ~deprecated:["modeled_expensive"] ~long
     ~exes:CLOpt.[Java]
     "Matcher or list of matchers for methods that should be considered expensive by the \
      performance critical checker.")

and patterns_never_returning_null =
  let long = "never-returning-null" in
  (long,
   CLOpt.mk_json ~deprecated:["never_returning_null"] ~long
     ~exes:CLOpt.[Java]
     "Matcher or list of matchers for functions that never return `null`.")

and patterns_skip_translation =
  let long = "skip-translation" in
  (long,
   CLOpt.mk_json ~deprecated:["skip_translation"] ~long
     ~exes:CLOpt.[Java]
     "Matcher or list of matchers for names of files that should not be analyzed at all.")

and pmd_xml =
  CLOpt.mk_bool ~long:"pmd-xml"
    ~exes:CLOpt.[Toplevel]
    "Output issues in (PMD) XML format"

and precondition_stats =
  CLOpt.mk_bool ~deprecated:["precondition_stats"] ~long:"precondition-stats"
    "Print stats about preconditions to standard output"

and print_builtins =
  CLOpt.mk_bool ~deprecated:["print_builtins"] ~long:"print-builtins"
    "Print the builtin functions and exit"

and print_traces_in_tests =
  CLOpt.mk_bool ~long:"print-traces-in-tests" ~default:true
    ~exes:CLOpt.[Print]
    "Include symbolic traces summaries in the output of --issues-tests"

and print_using_diff =
  CLOpt.mk_bool ~deprecated_no:["noprintdiff"] ~long:"print-using-diff" ~default:true
    "Highlight the difference w.r.t. the previous prop when printing symbolic execution debug info"

and procs_csv =
  CLOpt.mk_path_opt ~deprecated:["procs"] ~long:"procs-csv"
    ~meta:"file" "Write statistics for each procedure in CSV format to a file"

and procs_xml =
  CLOpt.mk_path_opt ~deprecated:["procs_xml"] ~long:"procs-xml"
    ~meta:"file"
    "Write statistics for each procedure in XML format to a file (as a path relative to \
     --results-dir)"

and progress_bar =
  CLOpt.mk_bool ~deprecated_no:["no_progress_bar"] ~long:"progress-bar" ~short:"pb" ~default:true
    ~exes:CLOpt.[Toplevel]
    "Show a progress bar"

and quiet =
  CLOpt.mk_bool ~long:"quiet" ~short:"q" ~default:(current_exe <> CLOpt.Print)
    ~exes:CLOpt.[Print]
    "Do not print specs on standard output"

and reactive =
  CLOpt.mk_bool ~deprecated:["reactive"] ~long:"reactive"
    "Reactive mode: the analysis starts from the files captured since the `infer` command started"

and reactive_capture =
  CLOpt.mk_bool ~long:"reactive-capture"
    "Compile source files only when required by analyzer (clang only)"

and report =
  CLOpt.mk_path_opt ~deprecated:["report"] ~long:"report"
    ~meta:"file" "Write a report of the analysis results to a file"

and report_custom_error =
  CLOpt.mk_bool ~long:"report-custom-error"
    ""

and report_hook =
  CLOpt.mk_string_opt ~long:"report-hook"
    ~default:(lib_dir ^/ "python" ^/ "report.py")
    ~meta:"script"
    "Specify a script to be executed after the analysis results are written.  This script will be \
     passed --issues-csv, --issues-json, --issues-txt, --issues-xml, --project-root, and \
     --results-dir."

and results_dir =
  CLOpt.mk_path ~deprecated:["results_dir"; "-out"] ~long:"results-dir" ~short:"o"
    ~default:(init_work_dir ^/ "infer-out")
    ~exes:CLOpt.[Toplevel;Analyze;Clang;Java;Print]
    ~meta:"dir" "Write results and internal files in the specified directory"

and save_results =
  CLOpt.mk_path_opt ~deprecated:["save_results"] ~long:"save-results"
    ~meta:"file.iar" "Save analysis results to Infer Analysis Results file file.iar"

and seconds_per_iteration =
  CLOpt.mk_float ~deprecated:["seconds_per_iteration"] ~long:"seconds-per-iteration" ~default:0.
    ~meta:"float" "Set the number of seconds per iteration (see --iterations)"

and skip_analysis_in_path =
  CLOpt.mk_string_list ~long:"skip-analysis-in-path"
    ~exes:CLOpt.[Clang;Java]
    ~meta:"path prefix" "Ignore files whose path matches the given prefix"

and skip_clang_analysis_in_path =
  CLOpt.mk_string_list ~long:"skip-clang-analysis-in-path"
    ~exes:CLOpt.[Clang]
    ~meta:"path prefix" "Ignore files whose path matches the given prefix"

and skip_translation_headers =
  CLOpt.mk_string_list ~deprecated:["skip_translation_headers"] ~long:"skip-translation-headers"
    ~exes:CLOpt.[Clang]
    ~meta:"path prefix" "Ignore headers whose path matches the given prefix"

and sources =
  CLOpt.mk_string_list ~long:"sources"
    ~exes:CLOpt.[Java]
    "Specify the list of source files"

and sourcepath =
  CLOpt.mk_string_opt ~long:"sourcepath"
    ~exes:CLOpt.[Java]
    "Specify the sourcepath"

and spec_abs_level =
  CLOpt.mk_int ~deprecated:["spec_abs_level"] ~long:"spec-abs-level" ~default:1
    ~meta:"int" "Set the level of abstracting the postconditions of discovered specs:\n\
                 - 0 = nothing special\n\
                 - 1 = filter out redundant posts implied by other posts"

and specs_library =
  let specs_library =
    CLOpt.mk_path_list ~long:"specs-library" ~short:"lib"
      ~meta:"dir|jar" "Search for .spec files in given directory or jar file" in
  let _ =
    (* Given a filename with a list of paths, convert it into a list of string iff they are
       absolute *)
    let read_specs_dir_list_file fname =
      let validate_path path =
        if Filename.is_relative path then
          failwith ("Failing because path " ^ path ^ " is not absolute") in
      match Utils.read_file (resolve fname) with
      | Some pathlist ->
          IList.iter validate_path pathlist;
          pathlist
      | None -> failwith ("cannot read file " ^ fname ^ " from cwd " ^ (Sys.getcwd ()))
    in
    (* Add the newline-separated directories listed in <file> to the list of directories to be
       searched for .spec files *)
    CLOpt.mk_string ~deprecated:["specs-dir-list-file"; "-specs-dir-list-file"]
      ~long:"specs-library-index"
      ~default:""
      ~f:(fun file -> specs_library := (read_specs_dir_list_file file) @ !specs_library; "")
      ~exes:CLOpt.[Analyze] ~meta:"file"
      "" in
  specs_library

and stacktrace =
  CLOpt.mk_path_opt ~long:"stacktrace" ~short:"st" ~exes:CLOpt.[Toplevel]
    ~meta:"file" "File path containing a json-encoded Java crash stacktrace. Used to guide the \
                  analysis (only with '-a crashcontext').  See \
                  tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and stacktraces_dir =
  CLOpt.mk_path_opt ~long:"stacktraces-dir" ~exes:CLOpt.[Toplevel]
    ~meta:"dir" "Directory path containing multiple json-encoded Java crash stacktraces. \
                 Used to guide the  analysis (only with '-a crashcontext').  See \
                 tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and stats =
  CLOpt.mk_bool ~deprecated:["stats"] ~long:"stats" "Stats mode (debugging)"

and subtype_multirange =
  CLOpt.mk_bool ~deprecated:["subtype_multirange"] ~long:"subtype-multirange" ~default:true
    "Use the multirange subtyping domain"

(* Path to list of collected @SuppressWarnings annotations *)
and suppress_warnings_out =
  CLOpt.mk_path_opt ~deprecated:["suppress_warnings_out"] ~long:suppress_warnings_annotations_long
    ~meta:"path" ""

and svg =
  CLOpt.mk_bool ~deprecated:["svg"] ~long:"svg"
    "Generate .dot and .svg files from specs"

and symops_per_iteration =
  CLOpt.mk_int ~deprecated:["symops_per_iteration"] ~long:"symops-per-iteration" ~default:0
    ~meta:"int" "Set the number of symbolic operations per iteration (see --iterations)"

and test_filtering =
  CLOpt.mk_bool ~deprecated:["test_filtering"] ~long:"test-filtering"
    "List all the files Infer can report on (should be called from the root of the project)"

and testing_mode =
  CLOpt.mk_bool ~deprecated:["testing_mode"; "-testing_mode"] ~long:"testing-mode" ~short:"tm"
    "Mode for testing, where no headers are translated, and dot files are created (clang only)"

and trace_join =
  CLOpt.mk_bool ~deprecated:["trace_join"] ~long:"trace-join"
    "Detailed tracing information during prop join operations"

and trace_ondemand =
  CLOpt.mk_bool ~long:"trace-ondemand" ""

and trace_rearrange =
  CLOpt.mk_bool ~deprecated:["trace_rearrange"] ~long:"trace-rearrange"
    "Detailed tracing information during prop re-arrangement operations"

(** Report error traces for runtime exceptions (Java only): generate preconditions for runtime
    exceptions in Java and report errors for public methods which throw runtime exceptions *)
and tracing =
  CLOpt.mk_bool ~deprecated:["tracing"] ~long:"tracing"
    ""

and type_size =
  CLOpt.mk_bool ~deprecated:["type_size"] ~long:"type-size"
    "Consider the size of types during analysis, e.g. cannot use an int pointer to write to a char"

and unsafe_malloc =
  CLOpt.mk_bool ~long:"unsafe-malloc"
    ~exes:CLOpt.[Analyze]
    "Assume that malloc(3) never returns null."

and use_compilation_database =
  CLOpt.mk_symbol_opt ~long:"use-compilation-database"
    "Buck integration using the compilation database, with or without dependencies."
    ~symbols:[("deps", `Deps); ("no-deps", `NoDeps)]

(** Set the path to the javac verbose output *)
and verbose_out =
  CLOpt.mk_path ~deprecated:["verbose_out"] ~long:"verbose-out" ~default:""
    ~meta:"file" ""

and version =
  CLOpt.mk_bool ~deprecated:["version"] ~long:"version"
    ~exes:CLOpt.[Toplevel;Analyze;Clang;Java;Print] "Print version information and exit"

and version_json =
  CLOpt.mk_bool ~deprecated:["version_json"] ~long:"version-json"
    ~exes:CLOpt.[Analyze;Clang;Java;Print]
    "Print version json formatted"

and version_vcs =
  CLOpt.mk_bool ~long:"version-vcs"
    ~exes:CLOpt.[Analyze;Clang;Java;Print] "Print version control system commit and exit"

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
    ~exes:CLOpt.[Toplevel]
    ~meta:"XCODE_DEVELOPER_DIR" "Specify the path to Xcode developer directory (Buck flavors only)"

and xcpretty =
  CLOpt.mk_bool ~long:"xcpretty"
    ~default:true
    ~exes:CLOpt.[Toplevel]
    "Infer will use xcpretty together with xcodebuild to analyze an iOS app. xcpretty just needs \
     to be in the path, infer command is still just infer -- <xcodebuild command>. (Recommended)"

and xml_specs =
  CLOpt.mk_bool ~deprecated:["xml"] ~long:"xml-specs"
    "Export specs into XML files file1.xml ... filen.xml"

(* The "rest" args must appear after "--" on the command line, and hence after other args, so they
   are allowed to refer to the other arg variables. *)
let rest =
  CLOpt.mk_subcommand
    ~exes:CLOpt.[Toplevel]
    "Stop argument processing, use remaining arguments as a build command"
    (fun build_exe ->
       match Filename.basename build_exe with
       | _ -> []
    )


(** Parse Command Line Args *)

let exe_usage (exe : CLOpt.exe) =
  match exe with
  | Analyze ->
      version_string ^ "\n" ^
      "Usage: InferAnalyze [options]\n\
       Analyze the files captured in the project results directory, which can be specified with \
       the --results-dir option."
  | Clang ->
      "Usage: internal script to capture compilation commands from clang and clang++. \n\
       You shouldn't need to call this directly."
  | Interactive ->
      "Usage: interactive ocaml toplevel. To pass infer config options use env variable"
  | Java ->
      "Usage: InferJava [options]\n\
       Translate the given files using javac into infer internal representation for later analysis."
  | Print ->
      "Usage: InferPrint [options] name1.specs ... namen.specs\n\
       Read, convert, and print .specs files. \
       To process all the .specs in the current directory, pass . as only parameter \
       To process all the .specs in the results directory, use option --results-dir \
       Each spec is printed to standard output unless option -q is used."
  | Toplevel ->
      version_string

let post_parsing_initialization () =
  F.set_margin !margin ;

  if !version then (
    (* TODO(11791235) change back to stdout once buck integration is fixed *)
    F.fprintf F.err_formatter "%s@." version_string ;
    exit 0
  );
  if !version_json then (
    F.fprintf F.std_formatter "%s@." Version.versionJson ;
    exit 0
  );
  if !version_vcs then (
    F.fprintf F.std_formatter "%s@." Version.commit ;
    exit 0
  );

  let set_minor_heap_size nMb = (* increase the minor heap size to speed up gc *)
    let ctrl = Gc.get () in
    let words_of_Mb nMb = nMb * 1024 * 1024 * 8 / Sys.word_size in
    let new_size = max ctrl.minor_heap_size (words_of_Mb nMb) in
    Gc.set { ctrl with minor_heap_size = new_size }
  in
  set_minor_heap_size 8 ;

  let symops_timeout, seconds_timeout =
    let default_symops_timeout = 333 in
    let default_seconds_timeout = 10.0 in
    let long_symops_timeout = 1000 in
    let long_seconds_timeout = 30.0 in
    if !models_mode then
      (* use longer timeouts when analyzing models *)
      long_symops_timeout, long_seconds_timeout
    else
      default_symops_timeout, default_seconds_timeout
  in
  if !seconds_per_iteration = 0. then seconds_per_iteration := seconds_timeout ;
  if !symops_per_iteration = 0 then symops_per_iteration := symops_timeout ;

  match !analyzer with
  | Some Checkers -> checkers := true
  | Some Crashcontext -> checkers := true; crashcontext := true
  | Some Eradicate -> checkers := true; eradicate := true
  | Some Quandary -> checkers := true; quandary := true
  | Some Threadsafety -> checkers := true; threadsafety := true
  | Some Tracing -> tracing := true
  | Some (Capture | Compile | Infer | Linters) | None -> ()


let parse_args_and_return_usage_exit =
  let usage_exit =
    CLOpt.parse ~accept_unknown:true ~config_file:inferconfig_path current_exe exe_usage in
  post_parsing_initialization () ;
  usage_exit

let print_usage_exit () =
  parse_args_and_return_usage_exit 1


(** Freeze initialized configuration values *)

let anon_args = !anon_args
and rest = !rest
and abs_struct = !abs_struct
and abs_val_orig = !abs_val
and allow_specs_cleanup = !allow_specs_cleanup
and analysis_path_regex_whitelist_options =
  IList.map (fun (a, b) -> (a, !b)) analysis_path_regex_whitelist_options
and analysis_path_regex_blacklist_options =
  IList.map (fun (a, b) -> (a, !b)) analysis_path_regex_blacklist_options
and analysis_blacklist_files_containing_options =
  IList.map (fun (a, b) -> (a, !b)) analysis_blacklist_files_containing_options
and analysis_suppress_errors_options =
  IList.map (fun (a, b) -> (a, !b)) analysis_suppress_errors_options
and analysis_stops = !analysis_stops
and angelic_execution = !angelic_execution
and array_level = !array_level
and ast_file = !ast_file
and blacklist = !blacklist
and bootclasspath = !bootclasspath
and buck = !buck
and buck_build_args = !buck_build_args
and buck_out = !buck_out
and bugs_csv = !bugs_csv
and bugs_json = !bugs_json
and frontend_tests = !frontend_tests
and generated_classes = !generated_classes
and bugs_tests = !bugs_tests
and bugs_txt = !bugs_txt
and bugs_xml = !bugs_xml
and changed_files_index = !changed_files_index
and calls_csv = !calls_csv
and check_duplicate_symbols = !check_duplicate_symbols
and checkers = !checkers
and checkers_repeated_calls = !checkers_repeated_calls
and clang_biniou_file = !clang_biniou_file
and clang_include_to_override = !clang_include_to_override
and classpath = !classpath
and cluster_cmdline = !cluster
and compute_analytics = !compute_analytics
and continue_capture = !continue
and copy_propagation = !copy_propagation
and crashcontext = !crashcontext
and create_harness = !android_harness
and cxx = !cxx
and debug_mode = !debug
and debug_exceptions = !debug_exceptions
and dependency_mode = !dependencies
and developer_mode = !developer_mode
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
and eradicate_trace = !eradicate_trace
and eradicate_verbose = !eradicate_verbose
and err_file_cmdline = !err_file
and fail_on_bug = !fail_on_bug
and failures_allowed = !failures_allowed
and fcp_apple_clang = !fcp_apple_clang
and fcp_syntax_only = !fcp_syntax_only
and filter_paths = !filter_paths
and filtering = !filtering
and flavors = !flavors
and from_json_report = !from_json_report
and frontend_debug = !frontend_debug
and frontend_stats = !frontend_stats
and headers = !headers
and icfg_dotty_outfile = !icfg_dotty_outfile
and infer_cache = !infer_cache
and iterations = !iterations
and java_jar_compiler = !java_jar_compiler
and javac_verbose_out = !verbose_out
and jobs = !jobs
and join_cond = !join_cond
and latex = !latex
and linters_def_file = !linters_def_file
and load_average = match !load_average with
  | None when !buck ->
      Some (float_of_int ncpu)
  | _ ->
      !load_average
and load_analysis_results = !load_results
and makefile_cmdline = !makefile
and merge = !merge
and ml_buckets = !ml_buckets
and models_mode = !models_mode
and modified_targets = !modified_targets
and monitor_prop_size = !monitor_prop_size
and nelseg = !nelseg
and no_translate_libs = not !headers
and objc_memory_model_on = !objc_memory_model
and only_footprint = !only_footprint
and optimistic_cast = !optimistic_cast
and out_file_cmdline = !out_file
and patterns_never_returning_null = match patterns_never_returning_null with (k,r) -> (k,!r)
and patterns_skip_translation = match patterns_skip_translation with (k,r) -> (k,!r)
and patterns_modeled_expensive = match patterns_modeled_expensive with (k,r) -> (k,!r)
and pmd_xml = !pmd_xml
and precondition_stats = !precondition_stats
and print_builtins = !print_builtins
and print_traces_in_tests = !print_traces_in_tests
and print_types = !print_types
and print_using_diff = !print_using_diff
and procs_csv = !procs_csv
and procs_xml = !procs_xml
and quandary = !quandary
and quiet = !quiet
and reactive_mode = !reactive
and reactive_capture = !reactive_capture
and report = !report
and report_custom_error = !report_custom_error
and report_hook = !report_hook
and report_runtime_exceptions = !tracing
and reports_include_ml_loc = !reports_include_ml_loc
and results_dir = !results_dir
and save_analysis_results = !save_results
and seconds_per_iteration = !seconds_per_iteration
and show_buckets = !print_buckets
and show_progress_bar = !progress_bar
and skip_analysis_in_path = !skip_analysis_in_path
and skip_clang_analysis_in_path = !skip_clang_analysis_in_path
and skip_translation_headers = !skip_translation_headers
and sources = !sources
and sourcepath = !sourcepath
and spec_abs_level = !spec_abs_level
and stacktrace = !stacktrace
and stacktraces_dir = !stacktraces_dir
and stats_mode = !stats
and subtype_multirange = !subtype_multirange
and suppress_warnings_out = !suppress_warnings_out
and svg = !svg
and symops_per_iteration = !symops_per_iteration
and test = !test
and test_filtering = !test_filtering
and testing_mode = !testing_mode
and threadsafety = !threadsafety
and trace_error = !trace_error
and trace_ondemand = !trace_ondemand
and trace_join = !trace_join
and trace_rearrange = !trace_rearrange
and type_size = !type_size
and unsafe_malloc = !unsafe_malloc
and use_compilation_database = !use_compilation_database
and whole_seconds = !whole_seconds
and worklist_mode = !worklist_mode
and write_dotty = !write_dotty
and write_html = !write_html
and xcode_developer_dir = !xcode_developer_dir
and xcpretty = !xcpretty
and xml_specs = !xml_specs


(** Configuration values derived from command-line options *)

let analysis_path_regex_whitelist analyzer =
  IList.assoc (=) analyzer analysis_path_regex_whitelist_options
and analysis_path_regex_blacklist analyzer =
  IList.assoc (=) analyzer analysis_path_regex_blacklist_options
and analysis_blacklist_files_containing analyzer =
  IList.assoc (=) analyzer analysis_blacklist_files_containing_options
and analysis_suppress_errors analyzer =
  IList.assoc (=) analyzer analysis_suppress_errors_options

let checkers_enabled = not (eradicate || crashcontext || quandary || threadsafety)

let clang_frontend_do_capture, clang_frontend_do_lint =
  match !clang_frontend_action with
  | Some `Lint -> false, true (* no capture, lint *)
  | Some `Capture -> true, false (* capture, no lint *)
  | Some `Lint_and_capture -> true, true (* capture, lint *)
  | None ->
      match !analyzer with
      | Some Linters -> false, true (* no capture, lint *)
      | Some Infer -> true, false (* capture, no lint *)
      | _ -> true, true (* capture, lint *)

let analyzer = match !analyzer with Some a -> a | None -> Infer

let clang_frontend_action_string =
  String.concat ~sep:" and "
    ((if clang_frontend_do_capture then ["translating"] else [])
     @ (if clang_frontend_do_lint then ["linting"] else []))

let dynamic_dispatch = if analyzer = Tracing then `Lazy else !dynamic_dispatch

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
                IList.iter extract_entry entries;
                Zip.close_in zip_channel in
        extract_specs key_dir filename;
        key_dir :: specs_library in
      IList.fold_left add_spec_lib [] !specs_library
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
