(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Configuration values: either constant, determined at compile time, or set at startup
    time by system calls, environment variables, or command line options *)

module CLOpt = CommandLineOption
module F = Format


type analyzer = Capture | Compile | Infer | Eradicate | Checkers | Tracing
              | Crashcontext | Linters | Quandary

let string_to_analyzer =
  [("capture", Capture); ("compile", Compile);
   ("infer", Infer); ("eradicate", Eradicate); ("checkers", Checkers);
   ("tracing", Tracing); ("crashcontext", Crashcontext); ("linters", Linters);
   ("quandary", Quandary);]

let clang_frontend_action_symbols = [
  ("lint", `Lint);
  ("capture", `Capture);
  ("lint_and_capture", `Lint_and_capture);
]

type clang_lang = C | CPP | OBJC | OBJCPP

type language = Clang | Java

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


type method_pattern = {
  class_name : string;
  method_name : string option;
  parameters : (string list) option;
}

type pattern =
  | Method_pattern of language * method_pattern
  | Source_contains of language * string


type zip_library = {
  zip_filename: string;
  zip_channel: Zip.in_file Lazy.t;
  models: bool;
}


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
  "UNSAFE_GUARDED_BY_ACCESS";
]

let clang_build_output_dir_name = "build_output"

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

let nsnotification_center_checker_backend = false

let perf_stats_prefix = "perf_stats"

let proc_stats_filename = "proc_stats.json"

let property_attributes = "property_attributes"

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

let sources_dir_name = "sources"

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

let bin_dir =
  Filename.dirname Sys.executable_name

let lib_dir =
  bin_dir // Filename.parent_dir_name // "lib"

(** Path to lib/specs to retrieve the default models *)
let models_dir =
  lib_dir // specs_dir_name

let cpp_models_dir =
  bin_dir // Filename.parent_dir_name // "models" // "cpp" // "include"

let wrappers_dir =
  lib_dir // "wrappers"

let ncpu =
  try
    with_process_in
      "getconf _NPROCESSORS_ONLN 2>/dev/null"
      (fun chan -> Scanf.fscanf chan "%d" (fun n -> n))
  with _ ->
    1

let os_type = match Sys.os_type with
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> Unix

let patterns_of_json_with_key json_key json =
  let default_method_pattern = {
    class_name = "";
    method_name = None;
    parameters = None
  } in

  let default_source_contains = "" in

  let language_of_string = function
    | "Java" ->
        Ok Java
    | l ->
        Error ("Inferconfig JSON key " ^ json_key ^ " not supported for language " ^ l) in

  let rec detect_language = function
    | [] ->
        Error ("No language found for " ^ json_key ^ " in " ^ inferconfig_file)
    | ("language", `String s) :: _ ->
        language_of_string s
    | _:: tl ->
        detect_language tl in

  (* Detect the kind of pattern, method pattern or pattern based on the content of the source file.
     Detecting the kind of patterns in a first step makes it easier to parse the parts of the
     pattern in a second step *)
  let detect_pattern assoc =
    match detect_language assoc with
    | Ok language ->
        let is_method_pattern key = IList.exists (string_equal key) ["class"; "method"]
        and is_source_contains key = IList.exists (string_equal key) ["source_contains"] in
        let rec loop = function
          | [] ->
              Error ("Unknown pattern for " ^ json_key ^ " in " ^ inferconfig_file)
          | (key, _) :: _ when is_method_pattern key ->
              Ok (Method_pattern (language, default_method_pattern))
          | (key, _) :: _ when is_source_contains key ->
              Ok (Source_contains (language, default_source_contains))
          | _:: tl -> loop tl in
        loop assoc
    | Error _ as error ->
        error in

  (* Translate a JSON entry into a matching pattern *)
  let create_pattern (assoc : (string * Yojson.Basic.json) list) =
    let collect_params l =
      let collect accu = function
        | `String s -> s:: accu
        | _ -> failwith ("Unrecognised parameters in " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.rev (IList.fold_left collect [] l) in
    let create_method_pattern assoc =
      let loop mp = function
        | (key, `String s) when key = "class" ->
            { mp with class_name = s }
        | (key, `String s) when key = "method" ->
            { mp with method_name = Some s }
        | (key, `List l) when key = "parameters" ->
            { mp with parameters = Some (collect_params l) }
        | (key, _) when key = "language" -> mp
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.fold_left loop default_method_pattern assoc
    and create_string_contains assoc =
      let loop sc = function
        | (key, `String pattern) when key = "source_contains" -> pattern
        | (key, _) when key = "language" -> sc
        | _ -> failwith ("Fails to parse " ^ Yojson.Basic.to_string (`Assoc assoc)) in
      IList.fold_left loop default_source_contains assoc in
    match detect_pattern assoc with
    | Ok (Method_pattern (language, _)) ->
        Ok (Method_pattern (language, create_method_pattern assoc))
    | Ok (Source_contains (language, _)) ->
        Ok (Source_contains (language, create_string_contains assoc))
    | Error _ as error ->
        error in

  let warn_user msg =
    F.eprintf "WARNING: in file %s: error parsing option %s@\n%s" inferconfig_file json_key msg in

  (* Translate all the JSON entries into matching patterns *)
  let rec translate accu = function
    | `Assoc l -> (
        match create_pattern l with
        | Ok pattern ->
            pattern :: accu
        | Error msg ->
            warn_user msg;
            accu)
    | `List l ->
        IList.fold_left translate accu l
    | json ->
        warn_user (Printf.sprintf "expected list or assoc json type, but got value %s"
                     (Yojson.Basic.to_string json));
        accu in

  translate [] json


(** Command Line options *)

(** The working directory of the initial invocation of infer, to which paths passed as command line
    options are relative. *)
let init_work_dir, is_originator =
  try
    (Sys.getenv "INFER_CWD", false)
  with Not_found ->
    let cwd =
      (* Use PWD if it denotes the same inode as ., to try to avoid paths with symlinks resolved *)
      (* Approach is borrowed from llvm implementation of *)
      (* llvm::sys::fs::current_path (implemented in Path.inc file) *)
      try
        let pwd = Unix.getenv "PWD" in
        let pwd_stat = Unix.stat pwd in
        let dot_stat = Unix.stat "." in
        if pwd_stat.st_dev = dot_stat.st_dev && pwd_stat.st_ino = dot_stat.st_ino then
          pwd
        else
          Sys.getcwd ()
      with _ ->
        Sys.getcwd ()
    in
    Unix.putenv "INFER_CWD" cwd ;
    (cwd, true)

(** Resolve relative paths passed as command line options, i.e., with respect to the working
    directory of the initial invocation of infer. *)
let resolve path =
  if Filename.is_relative path then
    init_work_dir // path
  else
    path

(* Declare the phase 1 options *)

let inferconfig_home =
  CLOpt.mk_string_opt ~deprecated:["inferconfig_home"] ~long:"inferconfig-home"
    ~exes:CLOpt.all_exes ~meta:"dir" "Path to the .inferconfig file"

and project_root =
  CLOpt.mk_string_opt ~deprecated:["project_root"; "-project_root"] ~long:"project-root" ~short:"pr"
    ?default:CLOpt.(match current_exe with Print | Toplevel | StatsAggregator ->
        Some (Sys.getcwd ()) | _ -> None)
    ~f:resolve
    ~exes:CLOpt.[Analyze;Clang;Java;Llvm;Print;Toplevel]
    ~meta:"dir" "Specify the root directory of the project"

(* Parse the phase 1 options, ignoring the rest *)

let _ = CLOpt.parse ~incomplete:true "INFER_ARGS" (fun _ -> "")

(* Define the values that depend on phase 1 options *)

let inferconfig_home = !inferconfig_home
and project_root = !project_root

let inferconfig_path =
  match inferconfig_home, project_root with
  | Some dir, _ | _, Some dir -> dir // inferconfig_file
  | None, None -> inferconfig_file

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

and rest =
  CLOpt.mk_rest
    ~exes:CLOpt.[Toplevel] "Stop argument processing, use remaining arguments as a build command"

and absolute_paths =
  CLOpt.mk_bool ~long:"absolute-paths"
    ~exes:CLOpt.[Java] "Report errors using absolute paths"

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
  CLOpt.mk_bool ~deprecated:["allow_specs_cleanup"] ~long:"allow-specs-cleanup"
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
    | Quandary -> () in
  CLOpt.mk_symbol_opt ~deprecated:["analyzer"] ~long:"analyzer" ~short:"a"
    ~exes:CLOpt.[Toplevel]
    "Specify which analyzer to run (only one at a time is supported):\n\
     - infer, eradicate, checkers, quandary: run the specified analysis\n\
     - capture: run capture phase only (no analysis)\n\
     - compile: run compilation command without interfering (Java only)\n\
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
  CLOpt.mk_string_opt ~long:"ast-file" ~short:"ast"
    ~meta:"file" "AST file for the translation"

and blacklist =
  CLOpt.mk_string_opt ~deprecated:["-blacklist-regex";"-blacklist"] ~long:"buck-blacklist"
    ~exes:CLOpt.[Toplevel]
    ~meta:"regex" "Skip analysis of files matched by the specified regular expression (Buck \
                   flavors only)"

(** Automatically set when running from within Buck *)
and buck =
  CLOpt.mk_bool ~long:"buck"
    ""

and buck_build_args =
  CLOpt.mk_string_list ~long:"Xbuck"
    ~exes:CLOpt.[Toplevel;BuckCompilationDatabase]
    "Pass values as command-line arguments to invocations of `buck build` (Buck flavors only)"

and buck_out =
  CLOpt.mk_string_opt ~long:"buck-out"
    ~exes:CLOpt.[StatsAggregator] ~meta:"dir" "Specify the root directory of buck-out"

and bugs_csv =
  CLOpt.mk_option ~deprecated:["bugs"] ~long:"issues-csv" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Create a file containing a list of issues in CSV format"

and bugs_json =
  CLOpt.mk_option ~deprecated:["bugs_json"] ~long:"issues-json" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Create a file containing a list of issues in JSON format"

and frontend_tests =
  CLOpt.mk_bool ~long:"frontend-tests"
    ~exes:CLOpt.frontend_exes
    "Save filename.ext.test.dot with the cfg in dotty format for frontend tests"

and bugs_tests =
  CLOpt.mk_option ~long:"issues-tests" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Create a file containing issues in a format suitable for tests"

and bugs_txt =
  CLOpt.mk_option ~deprecated:["bugs_txt"] ~long:"issues-txt" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Create a file containing a list of issues in TXT format"

and bugs_xml =
  CLOpt.mk_option ~deprecated:["bugs_xml"] ~long:"issues-xml" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Create a file containing a list of issues in XML format"

and calls_csv =
  CLOpt.mk_option ~deprecated:["calls"] ~long:"calls-csv" ~f:create_outfile
    ~exes:CLOpt.[Print]
    ~meta:"file" "Write individual calls in csv format to a file"

and changed_files_index =
  CLOpt.mk_string_opt ~long:"changed-files-index" ~exes:CLOpt.[Toplevel] ~meta:"file"
    "Specify the file containing the list of files from which reactive analysis should start"

and check_duplicate_symbols =
  CLOpt.mk_bool ~long:"check-duplicate-symbols"
    ~exes:CLOpt.[Analyze]
    "Check if a symbol with the same name is defined in more than one file."

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

(* Default is objc, since it's the default for clang (at least in Mac OS) *)
and clang_lang =
  CLOpt.mk_symbol ~long:"clang-lang" ~short:"x" ~default:OBJC
    "Specify language for clang frontend"
    ~symbols:[("c", C); ("objective-c", OBJC); ("c++", CPP); ("objective-c++", OBJCPP)]

and _ =
  CLOpt.mk_string_opt ~deprecated:["classpath"] ~long:"classpath"
    ~meta:"path" "Specify where to find user class files and annotation processors"

and cluster =
  CLOpt.mk_string_opt ~deprecated:["cluster"] ~long:"cluster"
    ~meta:"file" "Specify a .cluster file to be analyzed"

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

(** Set language to Java *)
and curr_language =
  let var = ref Clang in
  CLOpt.mk_set var Java ~deprecated:["java"] ~long:"java" "";
  var

and cxx_experimental =
  CLOpt.mk_bool ~deprecated:["cxx-experimental"] ~long:"cxx"
    ~exes:CLOpt.[Clang]
    "Analyze C++ methods, still experimental"

and debug, print_types, write_dotty =
  let print_types =
    CLOpt.mk_bool ~deprecated:["print_types"] ~long:"print-types"
      ~default:(CLOpt.current_exe = CLOpt.Clang)
      "Print types in symbolic heaps"
  and write_dotty =
    CLOpt.mk_bool ~deprecated:["dotty"] ~long:"write-dotty"
      "Produce dotty files for specs in the results directory"
  in
  let debug =
    CLOpt.mk_bool_group ~deprecated:["debug"] ~long:"debug" ~short:"g"
      ~exes:CLOpt.[Analyze]
      "Debug mode (also sets --print-types and --write-dotty)"
      [print_types; write_dotty]
  in
  (debug, print_types, write_dotty)

and debug_exceptions =
  CLOpt.mk_bool ~long:"debug-exceptions"
    ~exes:CLOpt.[Analyze]
    "Generate lightweight debugging information: just print the internal exceptions during analysis"

and dependencies =
  CLOpt.mk_bool ~deprecated:["dependencies"] ~long:"dependencies"
    "Translate all the dependencies during the capture. The classes in the given jar file will be \
     translated. No sources needed."

and developer_mode =
  CLOpt.mk_bool ~deprecated:["developer_mode"] ~long:"developer-mode"
    ~default:(CLOpt.current_exe = CLOpt.Print)
    "Show internal exceptions"

and disable_checks =
  CLOpt.mk_string_list ~deprecated:["disable_checks"] ~long:"disable-checks" ~meta:"error name"
    ~exes:CLOpt.[Toplevel;Print]
    "Do not show reports coming from this type of errors"

and dotty_cfg_libs =
  CLOpt.mk_bool ~deprecated:["dotty_no_cfg_libs"] ~long:"dotty-cfg-libs" ~default:true
    "Print the cfg of the code coming from the libraries"

and enable_checks =
  CLOpt.mk_string_list ~deprecated:["enable_checks"] ~long:"enable-checks" ~meta:"error name"
    "Show reports coming from this type of errors"

and checkers, eradicate, crashcontext, quandary =
  (* Run only the checkers instead of the full analysis *)
  let checkers =
    CLOpt.mk_bool ~deprecated:["checkers"] ~long:"checkers"
      ""
  in
  (* Activate the eradicate checker for java annotations (also sets --checkers) *)
  let eradicate =
    CLOpt.mk_bool_group ~deprecated:["eradicate"] ~long:"eradicate"
      ""
      [checkers]
  in
  (* Activate the crashcontext checker for java stack trace context reconstruction *)
  let crashcontext =
    CLOpt.mk_bool_group ~deprecated:["crashcontext"] ~long:"crashcontext"
      ""
      [checkers]
  in
  (* Activate the quandary taint analysis *)
  let quandary =
    CLOpt.mk_bool_group ~deprecated:["quandary"] ~long:"quandary"
      ""
      [checkers]
  in
  (checkers, eradicate, crashcontext, quandary)

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

and filtering =
  CLOpt.mk_bool ~long:"filtering" ~short:"f" ~default:true
    ~exes:CLOpt.[Toplevel]
    "Do not show the results from experimental checks (note: some of them may contain many false \
     alarms)"

and flavors =
  CLOpt.mk_bool ~deprecated:["-use-flavors"] ~long:"flavors"
    ~exes:CLOpt.[Toplevel]
    "Buck integration using Buck flavors (clang only), eg `infer --flavors -- buck build \
     //foo:bar#infer`"

and frontend_debug =
  CLOpt.mk_bool ~long:"frontend-debug" ~short:"fd"
    "Emit debug info to *.o.astlog and a script *.o.sh that replays the command used to run clang \
     with the plugin attached, piped to the InferClang frontend command (clang only)"

and frontend_stats =
  CLOpt.mk_bool ~long:"frontend-stats" ~short:"fs"
    "Output statistics about the capture phase to *.o.astlog (clang only)"

and headers =
  CLOpt.mk_bool ~deprecated:["headers"] ~deprecated_no:["no_headers"] ~long:"headers" ~short:"hd"
    ~exes:CLOpt.[Clang]
    "Analyze code in header files"

and infer_cache =
  CLOpt.mk_string_opt ~deprecated:["infer_cache"; "-infer_cache"] ~long:"infer-cache" ~f:resolve
    ~meta:"dir" "Select a directory to contain the infer cache (Buck and Java only)"

and iterations =
  CLOpt.mk_int ~deprecated:["iterations"] ~long:"iterations" ~default:1
    ~meta:"int"
    "Specify the maximum number of operations for each function, expressed as a multiple of \
     symbolic operations and a multiple of seconds of elapsed time"

and jobs =
  CLOpt.mk_int ~deprecated:["-multicore"] ~long:"jobs" ~short:"j" ~default:ncpu
    ~exes:CLOpt.[Toplevel] ~meta:"int" "Run the specified number of analysis jobs simultaneously"

and join_cond =
  CLOpt.mk_int ~deprecated:["join_cond"] ~long:"join-cond" ~default:1
    ~meta:"int" "Set the strength of the final information-loss check used by the join:\n\
                 - 0 = use the most aggressive join for preconditions\n\
                 - 1 = use the least aggressive join for preconditions"

and latex =
  CLOpt.mk_option ~deprecated:["latex"] ~long:"latex" ~f:create_outfile
    ~meta:"file" "Print latex report to a file"

and load_average =
  CLOpt.mk_option ~long:"load-average" ~short:"l" ~f:(fun s -> Some (float_of_string s))
    ~meta:"float"
    ~exes:CLOpt.[Toplevel]
    "Do not start new parallel jobs if the load average is greater than that specified (Buck and \
     make only)"

and load_results =
  CLOpt.mk_string_opt ~deprecated:["load_results"] ~long:"load-results"
    ~exes:CLOpt.[Print]
    ~meta:"file.iar" "Load analysis results from Infer Analysis Results file file.iar"

and llvm =
  CLOpt.mk_bool ~long:"llvm" "Analyze C or C++ using the experimental LLVM frontend"

(** name of the makefile to create with clusters and dependencies *)
and makefile =
  CLOpt.mk_string ~deprecated:["makefile"] ~long:"makefile" ~default:""
    ~meta:"file" ""

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

(* Add a zip file containing the Java models *)
and models_file =
  CLOpt.mk_string_opt ~deprecated:["models"] ~long:"models"
    ~meta:"zip file" ""

and models_mode =
  CLOpt.mk_bool ~deprecated:["models_mode"; "-models_mode"] ~long:"models-mode"
    "Mode for analyzing the models"

and modified_targets =
  CLOpt.mk_string_opt ~deprecated:["modified_targets"] ~long:"modified-targets"
    ~meta:"file" "Read the file of Buck targets modified since the last analysis"

and monitor_prop_size =
  CLOpt.mk_bool ~deprecated:["monitor_prop_size"] ~long:"monitor-prop-size"
    "Monitor size of props, and print every time the current max is exceeded"

and nelseg =
  CLOpt.mk_bool ~deprecated:["nelseg"] ~long:"nelseg"
    "Use only nonempty lsegs"

(* Translate with Objective-C Automatic Reference Counting (ARC) *)
and objc_arc =
  CLOpt.mk_bool ~deprecated:["fobjc-arc"] ~long:"objc-arc"
    ""

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
  CLOpt.mk_string ~deprecated:["out_file"] ~long:"out-file" ~default:""
    ~meta:"file" "Specify the file for the non-error logs of the analyzer"

and margin =
  CLOpt.mk_int ~deprecated:["set_pp_margin"] ~long:"margin" ~default:100
    ~meta:"int" "Set right margin for the pretty printing functions"

and (
  patterns_modeled_expensive,
  patterns_never_returning_null,
  patterns_skip_translation) =
  let mk_option ~deprecated ~long doc =
    CLOpt.mk_set_from_json ~deprecated ~long ~default:[] ~default_to_string:(fun _ -> "[]")
      ~exes:CLOpt.[Java]
      ~f:(patterns_of_json_with_key long) doc in
  ( mk_option ~deprecated:["modeled_expensive"] ~long:"modeled-expensive"
      "Matcher or list of matchers for methods that should be considered expensive by the \
       performance critical checker.",
    mk_option ~deprecated:["never_returning_null"] ~long:"never-returning-null"
      "Matcher or list of matchers for functions that never return `null`.",
    mk_option ~deprecated:["skip_translation"] ~long:"skip-translation"
      "Matcher or list of matchers for names of files that should be analyzed at all.")

and pmd_xml =
  CLOpt.mk_bool ~long:"pmd-xml"
    ~exes:CLOpt.[Toplevel]
    "Output issues in (PMD) XML format"

and precondition_stats =
  CLOpt.mk_bool ~deprecated:["precondition_stats"] ~long:"precondition-stats"
    "Print stats about preconditions to standard output"

and print_buckets =
  CLOpt.mk_bool ~deprecated:["print_buckets"] ~long:"print-buckets"
    "Show the internal bucket of Infer reports in their textual description"

and print_builtins =
  CLOpt.mk_bool ~deprecated:["print_builtins"] ~long:"print-builtins"
    "Print the builtin functions and exit"

and print_using_diff =
  CLOpt.mk_bool ~deprecated_no:["noprintdiff"] ~long:"print-using-diff" ~default:true
    "Highlight the difference w.r.t. the previous prop when printing symbolic execution debug info"

and procs_csv =
  CLOpt.mk_option ~deprecated:["procs"] ~long:"procs-csv" ~f:create_outfile
    ~meta:"file" "Create a file containing statistics for each procedure in CSV format"

and procs_xml =
  CLOpt.mk_option ~deprecated:["procs_xml"] ~long:"procs-xml" ~f:create_outfile
    ~meta:"file" "Create a file containing statistics for each procedure in XML format"

and progress_bar =
  CLOpt.mk_bool ~deprecated_no:["no_progress_bar"] ~long:"progress-bar" ~short:"pb" ~default:true
    ~exes:CLOpt.[Toplevel]
    "Show a progress bar"

and quiet =
  CLOpt.mk_bool ~long:"quiet" ~short:"q"
    ~exes:CLOpt.[Print]
    "Do not print specs on standard output"

and reactive =
  CLOpt.mk_bool ~deprecated:["reactive"] ~long:"reactive"
    "Reactive mode: the analysis starts from the files captured since the `infer` command started"

and report =
  CLOpt.mk_option ~deprecated:["report"] ~long:"report" ~f:create_outfile
    ~meta:"file" "Create a file containing a report of the analysis results"

and reports_include_ml_loc =
  CLOpt.mk_bool ~deprecated:["with_infer_src_loc"] ~long:"reports-include-ml-loc"
    "Include the location in the Infer source code from where reports are generated"

and results_dir =
  CLOpt.mk_string ~deprecated:["results_dir"; "-out"] ~long:"results-dir" ~short:"o"
    ~default:(init_work_dir // "infer-out")
    ~exes:CLOpt.[Analyze;Clang;Java;Llvm;Print;StatsAggregator]
    ~meta:"dir" "Write results and internal files in the specified directory"

and save_results =
  CLOpt.mk_string_opt ~deprecated:["save_results"] ~long:"save-results"
    ~meta:"file.iar" "Save analysis results to Infer Analysis Results file file.iar"

and seconds_per_iteration =
  CLOpt.mk_float ~deprecated:["seconds_per_iteration"] ~long:"seconds-per-iteration" ~default:0.
    ~meta:"float" "Set the number of seconds per iteration (see --iterations)"

and skip_clang_analysis_in_path =
  CLOpt.mk_string_list ~long:"skip-clang-analysis-in-path"
    ~exes:CLOpt.[Clang]
    ~meta:"path prefix" "Ignore files whose path matches the given prefix"

and skip_translation_headers =
  CLOpt.mk_string_list ~deprecated:["skip_translation_headers"] ~long:"skip-translation-headers"
    ~exes:CLOpt.[Clang]
    ~meta:"path prefix" "Ignore headers whose path matches the given prefix"

(** File to translate *)
and source_file =
  (* clang-plugin normalizes filenames *)
  CLOpt.mk_string_opt ~long:"source-file" ~short:"c" ~f:filename_to_absolute
    ~meta:"file" ""

and source_file_copy =
  CLOpt.mk_string_opt ~deprecated:["source_file_copy"] ~long:"source-file-copy"
    ~meta:"source_file" "Print the path of the copy of source_file in the results directory"

and spec_abs_level =
  CLOpt.mk_int ~deprecated:["spec_abs_level"] ~long:"spec-abs-level" ~default:1
    ~meta:"int" "Set the level of abstracting the postconditions of discovered specs:\n\
                 - 0 = nothing special\n\
                 - 1 = filter out redundant posts implied by other posts"

and specs_library =
  (* Add dir to the list of directories to be searched for .spec files *)
  let specs_library =
    CLOpt.mk_string_list ~long:"specs-library" ~short:"lib" ~f:resolve
      ~meta:"dir"
      "" in
  let _ =
    (* Given a filename with a list of paths, convert it into a list of string iff they are
       absolute *)
    let read_specs_dir_list_file fname =
      let validate_path path =
        if Filename.is_relative path then
          failwith ("Failing because path " ^ path ^ " is not absolute") in
      match read_file (resolve fname) with
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
  CLOpt.mk_string_opt ~long:"stacktrace" ~short:"st" ~f:resolve ~exes:CLOpt.[Toplevel]
    ~meta:"file" "File path containing a json-encoded Java crash stacktrace. Used to guide the \
                  analysis (only with '-a crashcontext').  See \
                  tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and stacktraces_dir =
  CLOpt.mk_string_opt ~long:"stacktraces-dir" ~f:resolve ~exes:CLOpt.[Toplevel]
    ~meta:"dir" "Directory path containing multiple json-encoded Java crash stacktraces. \
                 Used to guide the  analysis (only with '-a crashcontext').  See \
                 tests/codetoanalyze/java/crashcontext/*.json for examples of the expected format."

and static_final =
  CLOpt.mk_bool ~deprecated_no:["no-static_final"] ~long:"static-final" ~default:true
    "Special treatment for static final fields"

and stats =
  CLOpt.mk_bool ~deprecated:["stats"] ~long:"stats" "Stats mode (debugging)"

and no_stop =
  CLOpt.mk_bool ~deprecated:["nonstop"] ~long:"no-stop"
    "Nonstop mode: the analysis continues after finding errors. With this option the analysis can \
     become less precise."

and subtype_multirange =
  CLOpt.mk_bool ~deprecated:["subtype_multirange"] ~long:"subtype-multirange" ~default:true
    "Use the multirange subtyping domain"

(* Path to list of collected @SuppressWarnings annotations *)
and suppress_warnings_out =
  CLOpt.mk_string_opt ~deprecated:["suppress_warnings_out"] ~long:suppress_warnings_annotations_long
    ~meta:"path" ""

and svg =
  CLOpt.mk_bool ~deprecated:["svg"] ~long:"svg"
    "Generate .dot and .svg files from specs"

and symops_per_iteration =
  CLOpt.mk_int ~deprecated:["symops_per_iteration"] ~long:"symops-per-iteration" ~default:0
    ~meta:"int" "Set the number of symbolic operations per iteration (see --iterations)"

and test =
  CLOpt.mk_bool ~deprecated_no:["notest"] ~deprecated:["-test"] ~long:"only-cheap-debug"
    ~default:true
    "Disable expensive debugging output"

and test_filtering =
  CLOpt.mk_bool ~deprecated:["test_filtering"] ~long:"test-filtering"
    "List all the files Infer can report on (should be called from the root of the project)"

and testing_mode =
  CLOpt.mk_bool ~deprecated:["testing_mode"; "-testing_mode"] ~long:"testing-mode" ~short:"tm"
    "Mode for testing, where no headers are translated, and dot files are created (clang only)"

and trace_error =
  CLOpt.mk_bool ~deprecated:["trace_error"] ~long:"trace-error"
    "Detailed tracing information during error explanation"

and trace_join =
  CLOpt.mk_bool ~deprecated:["trace_join"] ~long:"trace-join"
    "Detailed tracing information during prop join operations"

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
  CLOpt.mk_string ~deprecated:["verbose_out"] ~long:"verbose-out" ~default:""
    ~meta:"file" ""

and version =
  CLOpt.mk_bool ~deprecated:["version"] ~long:"version"
    ~exes:CLOpt.[Analyze;Clang;Java;Llvm;Print] "Print version information and exit"

and version_json =
  CLOpt.mk_bool ~deprecated:["version_json"] ~long:"version-json"
    ~exes:CLOpt.[Analyze;Clang;Java;Llvm;Print]
    "Print version json formatted"

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

and write_html =
  CLOpt.mk_bool ~deprecated:["html"] ~long:"write-html"
    "Produce hmtl debug output in the results directory"

and xcode_developer_dir =
  CLOpt.mk_string_opt ~long:"xcode-developer-dir"
    ~exes:CLOpt.[Toplevel]
    ~meta:"XCODE_DEVELOPER_DIR" "Specify the path to Xcode developer directory (Buck flavors only)"

and xml_specs =
  CLOpt.mk_bool ~deprecated:["xml"] ~long:"xml-specs"
    "Export specs into XML files file1.xml ... filen.xml"

(** list of the zip files to search for specs files *)
and zip_libraries : zip_library list ref = ref []

and zip_specs_library =
  CLOpt.mk_string_list ~long:"zip-specs-library" ~short:"ziplib" ~f:resolve
    ~meta:"zip file" "Search for .spec files in a zip file"


(** Configuration values specified by environment variables *)

let from_env_variable var_name =
  try
    let _ = Sys.getenv var_name in true
  with Not_found -> false

let get_env_variable var_name =
  try
    let v = Sys.getenv var_name in
    if v = "" then None else Some v
  with Not_found -> None

let analyze_models = from_env_variable "INFER_ANALYZE_MODELS"

(** experimental: handle dynamic dispatch by following the JVM semantics and creating
    during the symbolic excution procedure descriptions using the types information
    found in the abstract state *)
let lazy_dynamic_dispatch = from_env_variable "INFER_LAZY_DYNAMIC_DISPATCH"

let report_custom_error = from_env_variable "INFER_REPORT_CUSTOM_ERROR"

(** experimental: dynamic dispatch for interface calls only in Java. off by default because of the
    cost *)
let sound_dynamic_dispatch = from_env_variable "INFER_SOUND_DYNAMIC_DISPATCH"

let use_jar_cache = true

(** Parse Command Line Args *)

let exe_usage (exe : CLOpt.exe) =
  match exe with
  | Analyze ->
      version_string ^ "\n\
                        Usage: InferAnalyze [options]\n\
                        Analyze the files captured in the project results directory, \
                        which can be specified with the --results-dir option."
  | Clang ->
      "Usage: InferClang -c <c files> -ast <ast files> --results-dir <output-dir> [options] \n\
       Translate the given files using clang into infer internal representation for later analysis."
  | Java ->
      "Usage: InferJava [options]\n\
       Translate the given files using javac into infer internal representation for later analysis."
  | Llvm ->
      "Usage: InferLLVM -c <c file> [options]\n\
       Translate the given files using llvm into infer internal representation for later analysis."
  | Print ->
      "Usage: InferPrint [options] name1.specs ... namen.specs\n\
       Read, convert, and print .specs files. \
       To process all the .specs in the current directory, pass . as only parameter \
       To process all the .specs in the results directory, use option --results-dir \
       Each spec is printed to standard output unless option -q is used."
  | StatsAggregator ->
      "Usage: InferStatsAggregator --results-dir <dir> --buck-out <dir>\n\
       Aggregates all the perf stats generated by Buck on each target"
  | Toplevel ->
      version_string
  | Interactive ->
      "Usage: interactive ocaml toplevel. To pass infer config options use env variable"
  | BuckCompilationDatabase ->
      "Usage: BuckCompilationDatabase --Xbuck //target \n\
       Runs buck with the flavor compilation-database or uber-compilation-database. It then \n\
       reads the compilation database emited in json and runs the capture in parallel for \n\
       those commands"

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

  let set_minor_heap_size nMb = (* increase the minor heap size to speed up gc *)
    let ctrl = Gc.get () in
    let words_of_Mb nMb = nMb * 1024 * 1024 * 8 / Sys.word_size in
    let new_size = max ctrl.Gc.minor_heap_size (words_of_Mb nMb) in
    Gc.set { ctrl with Gc.minor_heap_size = new_size }
  in
  set_minor_heap_size 8 ;

  let symops_timeout, seconds_timeout =
    let default_symops_timeout = 333 in
    let default_seconds_timeout = 10.0 in
    let long_symops_timeout = 1000 in
    let long_seconds_timeout = 30.0 in
    if analyze_models then
      (* use longer timeouts when analyzing models *)
      long_symops_timeout, long_seconds_timeout
    else
      default_symops_timeout, default_seconds_timeout
  in
  if !seconds_per_iteration = 0. then seconds_per_iteration := seconds_timeout ;
  if !symops_per_iteration = 0 then symops_per_iteration := symops_timeout ;

  let add_zip_library zip_filename =
    match !infer_cache with
    | Some cache_dir when use_jar_cache ->
        let mkdir s =
          try
            Unix.mkdir s 0o700;
            true
          with Unix.Unix_error _ -> false
        in
        let extract_specs dest_dir zip_filename =
          let zip_channel = Zip.open_in zip_filename in
          let entries = Zip.entries zip_channel in
          let extract_entry entry =
            let dest_file = dest_dir // (Filename.basename entry.Zip.filename) in
            if Filename.check_suffix entry.Zip.filename specs_files_suffix
            then Zip.copy_entry_to_file zip_channel entry dest_file in
          IList.iter extract_entry entries;
          Zip.close_in zip_channel
        in
        let basename = Filename.basename zip_filename in
        let key = basename ^ string_crc_hex32 zip_filename in
        let key_dir = cache_dir // key in
        if (mkdir key_dir)
        then extract_specs key_dir zip_filename;
        specs_library := !specs_library @ [key_dir]
    | _ ->
        (* The order matters, the jar files should be added following the order specs files should
           be searched in them *)
        let zip_library = {
          zip_filename = zip_filename;
          zip_channel = lazy (Zip.open_in zip_filename);
          models = false
        } in
        zip_libraries := zip_library :: !zip_libraries
  in
  IList.iter add_zip_library (IList.rev !zip_specs_library) ;

  let zip_models = ref [] in
  let add_models zip_filename =
    let zip_library = {
      zip_filename = zip_filename;
      zip_channel = lazy (Zip.open_in zip_filename);
      models = true
    } in
    zip_models := zip_library :: !zip_models
  in
  (match !models_file with
   | Some file -> add_models (resolve file)
   | None -> ());

  zip_libraries := IList.rev_append !zip_models (IList.rev !zip_libraries)


let parse_args_and_return_usage_exit =
  let usage_exit =
    CLOpt.parse ~accept_unknown:true ~config_file:inferconfig_path "INFER_ARGS" exe_usage in
  if !debug || (!developer_mode && not (CLOpt.current_exe = CLOpt.Print)) then
    prerr_endline
      ((Filename.basename Sys.executable_name) ^ " got args "
       ^ (try Unix.getenv "INFER_ARGS" with Not_found -> "")) ;
  post_parsing_initialization () ;
  usage_exit

let print_usage_exit () =
  parse_args_and_return_usage_exit 1


(** Freeze initialized configuration values *)

let anon_args = IList.rev !anon_args
and rest = !rest
and abs_struct = !abs_struct
and abs_val_orig = !abs_val
and absolute_paths = !absolute_paths
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
and analyzer = !analyzer
and angelic_execution = !angelic_execution
and arc_mode = objc_arc
and array_level = !array_level
and ast_file = !ast_file
and blacklist = !blacklist
and buck = !buck
and buck_build_args = !buck_build_args
and buck_out = !buck_out
and bugs_csv = !bugs_csv
and bugs_json = !bugs_json
and frontend_tests = !frontend_tests
and bugs_tests = !bugs_tests
and bugs_txt = !bugs_txt
and bugs_xml = !bugs_xml
and changed_files_index = !changed_files_index
and calls_csv = !calls_csv
and check_duplicate_symbols = !check_duplicate_symbols
and checkers = !checkers

(** should the checkers be run? *)
and checkers_enabled = not (!eradicate || !crashcontext || !quandary)
(* TODO (t12740727): Remove this variable once the transition to linters mode is finished *)
and clang_frontend_action =
  match !clang_frontend_action with
  | Some clang_frontend_action ->
      clang_frontend_action
  | None ->
      match !analyzer with
      | Some Linters -> `Lint
      | Some Infer -> `Capture
      | _ -> `Lint_and_capture
and clang_include_to_override = !clang_include_to_override
and clang_lang = !clang_lang
and cluster_cmdline = !cluster
and continue_capture = !continue
and copy_propagation = !copy_propagation
and crashcontext = !crashcontext
and create_harness = !android_harness
and cxx_experimental = !cxx_experimental
and debug_mode = !debug
and debug_exceptions = !debug_exceptions
and dependency_mode = !dependencies
and developer_mode = !developer_mode
and disable_checks = !disable_checks
and dotty_cfg_libs = !dotty_cfg_libs
and enable_checks = !enable_checks
and eradicate = !eradicate
and err_file_cmdline = !err_file
and fail_on_bug = !fail_on_bug
and failures_allowed = !failures_allowed
and filtering = !filtering
and flavors = !flavors
and frontend_debug = !frontend_debug
and frontend_stats = !frontend_stats
and headers = !headers
and infer_cache = !infer_cache
and iterations = !iterations
and javac_verbose_out = !verbose_out
and jobs = !jobs
and join_cond = !join_cond
and latex = !latex
and load_average = !load_average
and load_analysis_results = !load_results
and llvm = !llvm
and makefile_cmdline = !makefile
and merge = !merge
and ml_buckets = !ml_buckets
and models_file = !models_file
and models_mode = !models_mode
and modified_targets = !modified_targets
and monitor_prop_size = !monitor_prop_size
and nelseg = !nelseg
and no_static_final = not !static_final
and no_translate_libs = not !headers
and nonstop = !no_stop
and objc_memory_model_on = !objc_memory_model
and only_footprint = !only_footprint
and optimistic_cast = !optimistic_cast
and out_file_cmdline = !out_file
and patterns_never_returning_null = !patterns_never_returning_null
and patterns_skip_translation = !patterns_skip_translation
and patterns_modeled_expensive = !patterns_modeled_expensive
and pmd_xml = !pmd_xml
and precondition_stats = !precondition_stats
and print_builtins = !print_builtins
and print_types = !print_types
and print_using_diff = !print_using_diff
and procs_csv = !procs_csv
and procs_xml = !procs_xml
and quandary = !quandary
and quiet = !quiet
and reactive_mode = !reactive
and report = !report
and report_runtime_exceptions = !tracing
and reports_include_ml_loc = !reports_include_ml_loc
and results_dir = !results_dir
and save_analysis_results = !save_results
and seconds_per_iteration = !seconds_per_iteration
and show_buckets = !print_buckets
and show_progress_bar = !progress_bar
and skip_clang_analysis_in_path = !skip_clang_analysis_in_path
and skip_translation_headers = !skip_translation_headers
and source_file = !source_file
and source_file_copy = !source_file_copy
and spec_abs_level = !spec_abs_level
and specs_library = !specs_library
and stacktrace = !stacktrace
and stacktraces_dir = !stacktraces_dir
and stats_mode = !stats
and subtype_multirange = !subtype_multirange
and svg = !svg
and symops_per_iteration = !symops_per_iteration
and test = !test
and test_filtering = !test_filtering
and testing_mode = !testing_mode
and trace_error = !trace_error
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
and xml_specs = !xml_specs
and zip_libraries = !zip_libraries

let clang_frontend_do_capture, clang_frontend_do_lint =
  match clang_frontend_action with
  | `Lint -> false, true
  | `Capture -> true, false
  | `Lint_and_capture -> true, true

let clang_frontend_action_string =
  String.concat " and "
    ((if clang_frontend_do_capture then ["translating"] else [])
     @ (if clang_frontend_do_lint then ["linting"] else []))


let analysis_path_regex_whitelist analyzer =
  IList.assoc (=) analyzer analysis_path_regex_whitelist_options
and analysis_path_regex_blacklist analyzer =
  IList.assoc (=) analyzer analysis_path_regex_blacklist_options
and analysis_blacklist_files_containing analyzer =
  IList.assoc (=) analyzer analysis_blacklist_files_containing_options
and analysis_suppress_errors analyzer =
  IList.assoc (=) analyzer analysis_suppress_errors_options

let patterns_suppress_warnings =
  let error msg =
    F.eprintf "There was an issue reading the option %s.@\n"
      suppress_warnings_annotations_long ;
    F.eprintf "If you did not call %s directly, this is likely a bug in Infer.@\n"
      (Filename.basename Sys.executable_name) ;
    F.eprintf "%s@." msg ;
    [] in
  match !suppress_warnings_out with
  | Some path -> (
      match read_optional_json_file path with
      | Ok json -> (
          let json_key = "suppress_warnings" in
          match Yojson.Basic.Util.member json_key json with
          | `Null -> []
          | json -> patterns_of_json_with_key json_key json)
      | Error msg -> error ("Could not read or parse the supplied " ^ path ^ ":\n" ^ msg))
  | None ->
      if CLOpt.(current_exe <> Java) then []
      else error ("Error: The option " ^ suppress_warnings_annotations_long ^ " was not provided")

(** Name of files for logging the output in the specific executable *)
let log_files_of_current_exe =
  let prefix =
    match CLOpt.current_exe with
    | Analyze -> "analyze"
    | BuckCompilationDatabase -> "buck_compilation_database"
    | Clang -> "clang"
    | Interactive -> "interactive"
    | Java -> "java"
    | Llvm -> "llvm"
    | Print -> "print"
    | StatsAggregator -> "stats_agregator"
    | Toplevel -> "top_level" in
  prefix ^ "_out", prefix ^ "_err"

(** should_log_exe exe = true means that files for logging in the log folder will be created
    and uses of Logging.out or Logging.err will log in those files *)
let should_log_current_exe =
  match CLOpt.current_exe with
  | Analyze -> debug_mode || stats_mode
  | BuckCompilationDatabase -> true
  | _ -> false

let tmp_log_files_of_current_exe () =
  let out_name, err_name = log_files_of_current_exe in
  let log_dir = results_dir // log_dir_name in
  let out_file =
    if out_file_cmdline = "" then
      Filename.temp_file ~temp_dir:log_dir out_name ""
    else out_file_cmdline in
  let err_file =
    if err_file_cmdline = "" then
      Filename.temp_file ~temp_dir:log_dir err_name ""
    else err_file_cmdline in
  out_file, err_file

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

(** Flag for footprint discovery mode *)
let footprint = ref true

let run_in_footprint_mode f x =
  set_reference_and_call_function footprint true f x

let run_in_re_execution_mode f x =
  set_reference_and_call_function footprint false f x

(** Set in the middle of forcing delayed prints *)
let forcing_delayed_prints = ref false

(** Number of lines of code in original file *)
let nLOC = ref 0

(** if true, user simple pretty printing *)
let pp_simple = ref true

let reset_abs_val () =
  abs_val := abs_val_orig

let run_with_abs_val_equal_zero f x =
  set_reference_and_call_function abs_val 0 f x
