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


type language = Clang | Java

let string_of_language = function
  | Java -> "Java"
  | Clang -> "C_CPP"

type clang_lang = C | CPP | OBJC | OBJCPP

type os_type = Unix | Win32 | Cygwin

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

let captured_dir_name = "captured"

let checks_disabled_by_default = [
  "GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL";
]

let default_failure_name = "ASSERTION_FAILURE"

let default_in_zip_results_dir = "infer"

(** Dotty output filename **)
let dotty_output = "icfg.dot"

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

(** Name of the infer configuration file *)
let inferconfig_file = ".inferconfig"

let ivar_attributes = "ivar_attributes"

(** Maximum level of recursion during the analysis, after which a timeout is generated *)
let max_recursion = 5

(** Flag to tune the level of applying the meet operator for
    preconditions during the footprint analysis:
    0 = do not use the meet
    1 = use the meet to generate new preconditions *)
let meet_level = 1

let nsnotification_center_checker_backend = false

(** If true, it deals with messages (method calls) in objective-c using the objective-c
    typical semantics. That is: if the receiver is nil then the method is nop and it
    returns 0.  When the flag is false we deal with messages as standard method / function
    calls *)
let objc_method_call_semantics = true

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

(** letters used in the analysis output *)
let log_analysis_file = "F"
let log_analysis_procedure = "."
let log_analysis_wallclock_timeout = "T"
let log_analysis_symops_timeout = "S"
let log_analysis_recursion_timeout = "R"
let log_analysis_crash = "C"


(** Compile time configuration values *)

let version_string =
  "Infer version "
  ^ Version.versionString
  ^ "\nCopyright 2009 - present Facebook. All Rights Reserved.\n"


(** System call configuration values *)

(** Initial time of the analysis, i.e. when this module is loaded, gotten from
    Unix.time *)
let initial_analysis_time = Unix.time ()

(** Path to lib/specs to retrieve the default models *)
let models_dir =
  let bin_dir = Filename.dirname Sys.executable_name in
  let lib_dir = Filename.concat (Filename.concat bin_dir Filename.parent_dir_name) "lib" in
  let lib_specs_dir = Filename.concat lib_dir specs_dir_name in
  lib_specs_dir

let os_type = match Sys.os_type with
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> Unix


(** Command Line options *)

(** The references representing the command line options are defined in a single
    simultaneous let...and...and... binding in order to allow the type-checker to catch
    uses of one reference in code for another. This avoids being sensitive to
    initialization-order and unintended dependence on the order in which options appear on
    the command line. For cases where order-dependence is desired, the interacting options
    can be defined together sharing a reference. See debug and specs_library below for two
    different examples. *)

let anon_args =
  CLOpt.mk_anon ()

(** Flag for abstracting fields of structs
    0 = no
    1 = forget some fields during matching (and so lseg abstraction) *)
and abs_struct =
  CLOpt.mk_int ~deprecated:["absstruct"] ~long:"abs-struct" ~default:1
    ~meta:"int" "Specify abstraction level for fields of structs"

(** Flag for abstracting numerical values
    0 = no abstraction.
    1 = evaluate all expressions abstractly.
    2 = 1 + abstract constant integer values during join.
*)
and abs_val =
  CLOpt.mk_int ~deprecated:["absval"] ~long:"abs-val" ~default:2
    ~meta:"int" "Specify abstraction level for expressions"

(** Flag for forgetting memory leak
    false = no
    true = forget leaked memory cells during abstraction
*)
and allow_leak =
  CLOpt.mk_bool ~deprecated:["leak"] ~long:"allow-leak"
    "Forget leaks during abstraction"

(** Whether specs can be cleaned up before starting analysis *)
and allow_specs_cleanup =
  CLOpt.mk_bool ~deprecated:["allow_specs_cleanup"] ~long:"allow-specs-cleanup"
    "Allow to remove existing specs before running analysis when it's not incremental"

(** Check whether to report Analysis_stops message in user mode *)
and analysis_stops =
  CLOpt.mk_bool ~deprecated:["analysis_stops"] ~long:"analysis-stops"
    "Issue a warning when the analysis stops"

(** Setup the analyzer in order to filter out errors for this analyzer only *)
and analyzer =
  CLOpt.mk_symbol_opt ~deprecated:["analyzer"] ~long:"analyzer"
    "Specify the analyzer for the path filtering"
    ~symbols:Utils.string_to_analyzer

(** if true, completely ignore the possibility that errors can be caused by unknown procedures
    during the symbolic execution phase *)
and angelic_execution =
  CLOpt.mk_bool ~deprecated:["angelic_execution"] ~long:"angelic-execution" ~default:true
    "Angelic execution, where the analysis ignores errors caused by unknown procedure calls"

(** Flag for ignoring arrays and pointer arithmetic.
    0 = treats both features soundly.
    1 = assumes that the size of every array is infinite.
    2 = assumes that all heap dereferences via array indexing and pointer arithmetic are correct.
*)
and array_level =
  CLOpt.mk_int ~deprecated:["arraylevel"] ~long:"array-level" ~default:0
    ~meta:"int" "Level of treating the array indexing and pointer arithmetic"

and ast_file =
  CLOpt.mk_string_opt ~long:"ast-file" ~short:"ast"
    ~meta:"file" "AST file for the translation"

(** Outfile to save bugs stats in csv format *)
and bugs_csv =
  CLOpt.mk_option ~deprecated:["bugs"] ~long:"bugs-csv" ~f:create_outfile
    ~meta:"bugs.csv" "Create file bugs.csv containing a list of bugs in CSV format"

(** Outfile to save bugs stats in json format *)
and bugs_json =
  CLOpt.mk_option ~deprecated:["bugs_json"] ~long:"bugs-json" ~f:create_outfile
    ~meta:"bugs.json" "Create file bugs.json containing a list of bugs in JSON format"

(** Outfile to save bugs stats in txt format *)
and bugs_txt =
  CLOpt.mk_option ~deprecated:["bugs_txt"] ~long:"bugs-txt" ~f:create_outfile
    ~meta:"bugs.txt" "Create file bugs.txt containing a list of bugs in TXT format"

(** Outfile to save bugs stats in xml format *)
and bugs_xml =
  CLOpt.mk_option ~deprecated:["bugs_xml"] ~long:"bugs-xml" ~f:create_outfile
    ~meta:"bugs.xml" "Create file bugs.xml containing a list of bugs in XML format"

(** Outfile to save call stats in csv format *)
and calls_csv =
  CLOpt.mk_option ~deprecated:["calls"] ~long:"calls-csv" ~f:create_outfile
    ~meta:"calls.csv" "Write individual calls in csv format to calls.csv"

(* Default is objc, since it's the default for clang (at least in Mac OS) *)
and clang_lang =
  CLOpt.mk_symbol ~long:"clang-lang" ~short:"x" ~default:OBJC
    "Specify language for clang frontend"
    ~symbols:[("c", C); ("objective-c", OBJC); ("c++", CPP); ("objective-c++", OBJCPP)]

and _ =
  CLOpt.mk_string_opt ~deprecated:["classpath"] ~long:"classpath"
    ~meta:"path" "Specify where to find user class files and annotation processors"

and _ =
  CLOpt.mk_string_opt ~deprecated:["class_source_map"] ~long:"class-source-map"
    ~meta:"file" "path to class -> source map in JSON format"

(** optional command-line name of the .cluster file *)
and cluster =
  CLOpt.mk_string_opt ~deprecated:["cluster"] ~long:"cluster"
    ~meta:"file" "Specify a .cluster file to be analyzed"

and code_query =
  CLOpt.mk_string_opt ~deprecated:["codequery"] ~long:"code-query"
    ~meta:"query" "Execute the code query"

(** Continue the capture for reactive mode:
    If a procedure was changed beforehand, keep the changed marking. *)
and continue =
  CLOpt.mk_bool ~deprecated:["continue"] ~long:"continue"
    "Continue the capture for the reactive analysis,\
     increasing the changed files/procedures."

and curr_language =
  let var = ref Clang in
  CLOpt.mk_set var Java ~deprecated:["java"] ~long:"java" "Set language to Java" ;
  var

and cxx_experimental =
  CLOpt.mk_bool ~deprecated:["cxx-experimental"] ~long:"cxx-experimental"
    "Analyze C++ methods, still experimental"

and debug, print_types, write_dotty =
  (** flag: if true print full type info *)
  let print_types =
    CLOpt.mk_bool ~deprecated:["print_types"] ~long:"print-types"
      ~default:(CLOpt.current_exe = CLOpt.C)
      "Print types in symbolic heaps"
  (** flag: if true write dot files in db dir*)
  and write_dotty =
    CLOpt.mk_bool ~deprecated:["dotty"] ~long:"write-dotty"
      "Produce dotty files in the results directory"
  in
  let debug =
    CLOpt.mk_bool_group ~deprecated:["debug"] ~long:"debug" ~short:"g"
      "Debug mode (also sets --print-types and --write-dotty)"
      [print_types; write_dotty]
  in
  (debug, print_types, write_dotty)

(* The classes in the given jar file will be translated. No sources needed *)
and dependencies =
  CLOpt.mk_bool ~deprecated:["dependencies"] ~long:"dependencies"
    "Translate all the dependencies during the capture"

(** If true shows internal exceptions*)
and developer_mode =
  CLOpt.mk_bool ~deprecated:["developer_mode"] ~long:"developer-mode"
    ~default:(CLOpt.current_exe = CLOpt.P)
    "Reserved"

(** if true, print cfg nodes in the dot file that are not defined in that file *)
and dotty_cfg_libs =
  CLOpt.mk_bool ~deprecated:["dotty_no_cfg_libs"] ~long:"dotty-cfg-libs" ~default:true
    "Prints the cfg of the code coming from the libraries"

(** command line option to activate the eradicate checker. *)
and eradicate, checkers =
  (** command line option: if true, run the analysis in checker mode *)
  let checkers =
    CLOpt.mk_bool ~deprecated:["checkers"] ~long:"checkers"
      "Run only the checkers instead of the full analysis"
  in
  let eradicate =
    CLOpt.mk_bool_group ~deprecated:["eradicate"] ~long:"eradicate"
      "Activate the eradicate checker for java annotations (also sets --checkers)"
      [checkers]
  in
  (eradicate, checkers)

and err_file =
  CLOpt.mk_string ~deprecated:["err_file"] ~long:"err-file" ~default:""
    ~exes:CLOpt.[A] ~meta:"file" "use file for the err channel"

(* Generate harness for Android code *)
and harness =
  CLOpt.mk_bool ~deprecated:["harness"] ~long:"harness"
    "Create Android lifecycle harness"

and headers =
  CLOpt.mk_bool ~deprecated:["headers"] ~deprecated_no:["no_headers"] ~long:"headers"
    "Translate code in header files"

and infer_cache =
  CLOpt.mk_string_opt ~deprecated:["infer_cache"] ~long:"infer-cache"
    ~f:filename_to_absolute
    ~meta:"dir" "Select a directory to contain the infer cache"

and inferconfig_home =
  CLOpt.mk_string_opt ~deprecated:["inferconfig_home"] ~long:"inferconfig-home"
    ~exes:CLOpt.[A] ~meta:"dir" "Path to the .inferconfig file"

(** Set the timeout values in seconds and symops, computed as a multiple of the integer
    parameter *)
and iterations =
  CLOpt.mk_int ~deprecated:["iterations"] ~long:"iterations" ~default:1
    ~meta:"int"
    "Specify the maximum number of operations for each function, expressed as a multiple \
     of symbolic operations"

(** Flag to tune the final information-loss check used by the join
    0 = use the most aggressive join for preconditions
    1 = use the least aggressive join for preconditions
*)
and join_cond =
  CLOpt.mk_int ~deprecated:["join_cond"] ~long:"join-cond" ~default:1
    ~meta:"int" "Set the strength of the final information-loss check used by the join"

(** Outfile to save the latex report *)
and latex =
  CLOpt.mk_option ~deprecated:["latex"] ~long:"latex" ~f:create_outfile
    ~meta:"file.tex" "Print latex report to file.tex"

(** name of the file to load analysis results from *)
and load_results =
  CLOpt.mk_string_opt ~deprecated:["load_results"] ~long:"load-results"
    ~meta:"file.iar" "Load analysis results from Infer Analysis Results file file.iar"

(** name of the makefile to create with clusters and dependencies *)
and makefile =
  CLOpt.mk_string ~deprecated:["makefile"] ~long:"makefile" ~default:""
    ~exes:CLOpt.[A] ~meta:"file" "create a makefile to perform the analysis"

(** Merge the captured results directories specified in the dependency file *)
and merge =
  CLOpt.mk_bool ~deprecated:["merge"] ~long:"merge"
    "Merge the captured results directories specified in the dependency file"

(** List of obj memory leak buckets to be checked in Objective-C/C++ *)
and ml_buckets =
  CLOpt.mk_symbol_seq ~deprecated:["ml_buckets"] ~long:"ml-buckets" ~default:[`MLeak_cf]
    "Specify the memory leak buckets to be checked: \
     'cf' checks leaks from Core Foundation, \
     'arc' from code compiled in ARC mode, \
     'narc' from code not compiled in ARC mode, \
     'cpp' from C++ code"
    ~symbols:[
      ("all", `MLeak_all);
      ("cf", `MLeak_cf);
      ("arc", `MLeak_arc);
      ("narc", `MLeak_no_arc);
      ("cpp", `MLeak_cpp);
      ("unknown_origin", `MLeak_unknown)]

and models_file =
  CLOpt.mk_string_opt ~deprecated:["models"] ~long:"models"
    ~exes:CLOpt.[A;J] ~meta:"zip file" "add a zip file containing the models"

and models_mode =
  CLOpt.mk_bool ~deprecated:["models_mode"] ~long:"models-mode"
    "Mode for computing the models"

and modified_targets =
  CLOpt.mk_string_opt ~deprecated:["modified_targets"] ~long:"modified-targets"
    ~meta:"file" "Read the file of buck targets modified since the last analysis"

(** Monitor the size of the props, and print every time the current max is exceeded *)
and monitor_prop_size =
  CLOpt.mk_bool ~deprecated:["monitor_prop_size"] ~long:"monitor-prop-size"
    "Monitor size of props"

(** Flag for using the nonempty lseg only **)
and nelseg =
  CLOpt.mk_bool ~deprecated:["nelseg"] ~long:"nelseg"
    "Use only nonempty lsegs"

(** true if the current objective-c source file is compiled with automatic reference counting
    (ARC) *)
and objc_arc =
  CLOpt.mk_bool ~deprecated:["fobjc-arc"] ~long:"objc-arc"
    "Translate with Objective-C Automatic Reference Counting (ARC)"

and objc_memory_model =
  CLOpt.mk_bool ~deprecated:["objcm"] ~long:"objc-memory-model"
    "Use ObjC memory model"

(** if true, skip the re-execution phase *)
and only_footprint =
  CLOpt.mk_bool ~deprecated:["only_footprint"] ~long:"only-footprint"
    "Skip the re-execution phase"

and optimistic_cast =
  CLOpt.mk_bool ~deprecated:["optimistic_cast"] ~long:"optimistic-cast"
    "Allow cast of undefined values"

and out_file =
  CLOpt.mk_string ~deprecated:["out_file"] ~long:"out-file" ~default:""
    ~exes:CLOpt.[A] ~meta:"file" "use file for the out channel"

and margin =
  CLOpt.mk_int ~deprecated:["set_pp_margin"] ~long:"margin" ~default:100
    ~meta:"int" "Set right margin for the pretty printing functions"

(** command line flag: if true, print stats about preconditions to standard output *)
and precondition_stats =
  CLOpt.mk_bool ~deprecated:["precondition_stats"] ~long:"precondition-stats"
    "Print stats about preconditions to standard output"

(** if true, show buckets in textual description of errors *)
and print_buckets =
  CLOpt.mk_bool ~deprecated:["print_buckets"] ~long:"print-buckets"
    "Add buckets to issue descriptions, useful when developing infer"

and print_builtins =
  CLOpt.mk_bool ~deprecated:["print_builtins"] ~long:"print-builtins"
    "Print the builtin functions and exit"

(** if true, acrtivate color printing by diff'ing w.r.t. previous prop *)
and print_using_diff =
  CLOpt.mk_bool ~deprecated_no:["noprintdiff"] ~long:"print-using-diff" ~default:true
    "Highlighting diff w.r.t. previous prop in printing"

(** Outfile to save procedures stats in csv format *)
and procs_csv =
  CLOpt.mk_option ~deprecated:["procs"] ~long:"procs-csv" ~f:create_outfile
    ~meta:"procs.csv" "Create file procs.csv containing statistics for each procedure in CSV format"

(** Outfile to save procedures stats in xml format *)
and procs_xml =
  CLOpt.mk_option ~deprecated:["procs_xml"] ~long:"procs-xml" ~f:create_outfile
    ~meta:"procs.xml" "Create file procs.xml containing statistics for each procedure in XML format"

and progress_bar =
  CLOpt.mk_bool ~deprecated_no:["no_progress_bar"] ~long:"progress-bar" ~short:"pb" ~default:true
    "Show a progress bar"

and project_root =
  CLOpt.mk_string_opt ~deprecated:["project_root"] ~long:"project-root" ~short:"pr"
    ?default:(if CLOpt.(current_exe = P) then Some (Sys.getcwd ()) else None)
    ~f:filename_to_absolute
    ~exes:CLOpt.[A;C;J;L;P] ~meta:"dir" "Specify the root directory of the project"

(** command line flag: if true, do not print the spec to standard output *)
and quiet =
  CLOpt.mk_bool ~long:"quiet" ~short:"q"
    "Do not print specs on standard output"

(** flag for reactive mode:
    the analysis starts from the files captured since the "infer" command started *)
and reactive =
  CLOpt.mk_bool ~deprecated:["reactive"] ~long:"reactive"
    "Reactive propagation mode starting analysis from changed files"

(** Outfile to save the analysis report *)
and report =
  CLOpt.mk_option ~deprecated:["report"] ~long:"report" ~f:create_outfile
    ~meta:"report_file" "Create file report_file containing a report of the analysis results"

(** If true then include Infer source code locations in json reports *)
and reports_include_ml_loc =
  CLOpt.mk_bool ~deprecated:["with_infer_src_loc"] ~long:"reports-include-ml-loc"
    "Include the location (in the Infer source code) from where reports are generated"

and results_dir =
  CLOpt.mk_string ~deprecated:["results_dir"] ~long:"results-dir"
    ~default:(Filename.concat (Sys.getcwd ()) "infer-out")
    ~exes:CLOpt.[A;C;J;L;P] ~meta:"dir" "Specify the project results directory"

(** name of the file to load save results to *)
and save_results =
  CLOpt.mk_string_opt ~deprecated:["save_results"] ~long:"save-results"
    ~meta:"file.iar" "Save analysis results to Infer Analysis Results file file.iar"

(** number of seconds to multiply by the number of iterations, after which there is a timeout *)
and seconds_per_iteration =
  CLOpt.mk_float ~deprecated:["seconds_per_iteration"] ~long:"seconds-per-iteration" ~default:0.
    ~meta:"float" "Set the number of seconds per iteration"

and source_file =
  CLOpt.mk_string_opt ~long:"source-file" ~short:"c" ~meta:"file" "File to translate"

(** command-line option to print the location of the copy of a source file *)
and source_file_copy =
  CLOpt.mk_string_opt ~deprecated:["source_file_copy"] ~long:"source-file-copy"
    ~meta:"source_file" "Print the path of the copy of source_file in the results directory"

(** Flag to tune the level of abstracting the postconditions of specs discovered
    by the footprint analysis.
    0 = nothing special.
    1 = filter out redundant posts implied by other posts. *)
and spec_abs_level =
  CLOpt.mk_int ~deprecated:["spec_abs_level"] ~long:"spec-abs-level" ~default:1
    ~meta:"int" "Set the level of abstracting the postconditions of discovered specs"

(** List of paths to the directories containing specs for library functions. *)
and specs_library =
  let specs_library =
    CLOpt.mk_string_list ~long:"specs-library" ~short:"lib" ~f:filename_to_absolute
      ~exes:CLOpt.[A] ~meta:"dir"
      "add dir to the list of directories to be searched for spec files" in
  let _ =
    (* Given a filename with a list of paths, convert it into a list of string iff they are
       absolute *)
    let read_specs_dir_list_file fname =
      let validate_path path =
        if Filename.is_relative path then
          failwith ("Failing because path " ^ path ^ " is not absolute") in
      match read_file fname with
      | Some pathlist ->
          IList.iter validate_path pathlist;
          pathlist
      | None -> failwith ("cannot read file " ^ fname)
    in
    CLOpt.mk_string ~deprecated:["specs-dir-list-file"] ~long:"specs-library-index"
      ~default:""
      ~f:(fun file -> specs_library := (read_specs_dir_list_file file) @ !specs_library; "")
      ~exes:CLOpt.[A] ~meta:"file"
      "add the newline-separated directories listed in <file> to the list of directories to be \
       searched for spec files" in
  specs_library

(** If active, enable special treatment of static final fields. *)
and static_final =
  CLOpt.mk_bool ~deprecated_no:["no-static_final"] ~long:"static-final" ~default:true
    "Special treatment for static final fields"

and stats =
  CLOpt.mk_bool ~deprecated:["stats"] ~long:"stats" "Enables stats mode"

(** Flag to activate nonstop mode: the analysis continues after in encounters errors *)
and no_stop =
  CLOpt.mk_bool ~deprecated:["nonstop"] ~long:"no-stop"
    "Nonstop mode: the analysis continues after finding errors. \
     With this option the analysis can become less precise."

and subtype_multirange =
  CLOpt.mk_bool ~deprecated:["subtype_multirange"] ~long:"subtype-multirange" ~default:true
    "Use the multirange subtyping domain"

and suppress_warnings_out =
  CLOpt.mk_string_opt ~deprecated:["suppress_warnings_out"] ~long:suppress_warnings_annotations_long
    ~exes:CLOpt.[J] ~meta:"path" "Path to list of collected @SuppressWarnings annotations"

(** command line flag: if true, produce a svg file *)
and svg =
  CLOpt.mk_bool ~deprecated:["svg"] ~long:"svg"
    "Generate .dot and .svg"

(** number of symops to multiply by the number of iterations, after which there is a timeout *)
and symops_per_iteration =
  CLOpt.mk_int ~deprecated:["symops_per_iteration"] ~long:"symops-per-iteration" ~default:0
    ~meta:"int" "Set the number of symbolic operations per iteration"

(** Flag for test mode *)
and test =
  CLOpt.mk_bool ~deprecated_no:["notest"] ~long:"test" ~default:true
    "Test mode"

(** command line option to test the filtering based on .inferconfig *)
and test_filtering =
  CLOpt.mk_bool ~deprecated:["test_filtering"] ~long:"test-filtering"
    "List all the files Infer can report on (should be call at the root of the project)"

and testing_mode =
  CLOpt.mk_bool ~deprecated:["testing_mode"] ~long:"testing-mode"
    "Mode for testing, where no headers are translated, and dot files are created"

(** Flag set to enable detailed tracing informatin during error explanation *)
and trace_error =
  CLOpt.mk_bool ~deprecated:["trace_error"] ~long:"trace-error"
    "Turn on tracing of error explanation"

(** Flag set to enable detailed tracing information during join *)
and trace_join =
  CLOpt.mk_bool ~deprecated:["trace_join"] ~long:"trace-join"
    "Turn on tracing of join"

(** Flag set to enable detailed tracing information during re-arrangement *)
and trace_rearrange =
  CLOpt.mk_bool ~deprecated:["trace_rearrange"] ~long:"trace-rearrange"
    "Turn on tracing of rearrangement"

(** if true, generate preconditions for runtime exceptions in Java and report errors for the public
    methods having preconditions to throw runtime exceptions *)
and tracing =
  CLOpt.mk_bool ~deprecated:["tracing"] ~long:"tracing"
    "Report error traces for runtime exceptions (Only for Java)"

(** Consider the size of types during analysis, e.g. cannot use an int pointer to write to a char *)
and type_size =
  CLOpt.mk_bool ~deprecated:["type_size"] ~long:"type-size"
    "Consider the size of types during analysis"

(** command line flag: if true, produce unit test for each spec *)
and unit_test =
  CLOpt.mk_bool ~deprecated:["unit_test"] ~long:"unit-test"
    "Print unit test code"

and verbose_out =
  CLOpt.mk_string ~deprecated:["verbose_out"] ~long:"verbose-out" ~default:""
    ~exes:CLOpt.[J] ~meta:"file" "Set the path to the javac verbose output"

and version =
  CLOpt.mk_bool ~deprecated:["version"] ~long:"version"
    ~exes:CLOpt.[A;C;J;L;P] "Print version information and exit"

and version_json =
  CLOpt.mk_bool ~deprecated:["version_json"] ~long:"version-json"
    "Print version json formatted"

(** command line flag: if true, print whole seconds only *)
and whole_seconds =
  CLOpt.mk_bool ~deprecated:["whole_seconds"] ~long:"whole-seconds"
    "Print whole seconds only"

(** visit mode for the worklist:
    0 depth - fist visit
    1 bias towards exit node
    2 least visited first *)
and worklist_mode =
  let var = ref 0 in
  CLOpt.mk_set var 2 ~long:"coverage" ~exes:CLOpt.[A]
    "analysis mode to maximize coverage (can take longer)" ;
  CLOpt.mk_set var 1 ~long:"exit-node-bias" ~deprecated:["exit_node_bias"]
    "nodes nearest the exit node are analyzed first" ;
  CLOpt.mk_set var 2 ~long:"visits-bias" ~deprecated:["visits_bias"]
    "nodes visited fewer times are analyzed first" ;
  var

(** flag: if true write html files in db dir *)
and write_html =
  CLOpt.mk_bool ~deprecated:["html"] ~long:"write-html"
    "Produce hmtl output in the results directory"

(** command line flag: if true, export specs to xml files *)
and xml_specs =
  CLOpt.mk_bool ~deprecated:["xml"] ~long:"xml-specs"
    "Export specs into XML files file1.xml ... filen.xml"

(** list of the zip files to search for specs files *)
and zip_libraries : zip_library list ref = ref []

and zip_specs_library =
  CLOpt.mk_string_list ~long:"zip-specs-library" ~short:"ziplib" ~f:filename_to_absolute
    ~exes:CLOpt.[A] ~meta:"zip file" "add a zip file containing library spec files"


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

let reset_abs_val =
  let abs_val_default = !abs_val in
  fun () ->
    abs_val := abs_val_default

let run_with_abs_val_equal_zero f x =
  set_reference_and_call_function abs_val 0 f x


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

let use_jar_cache = from_env_variable "INFER_USE_JAR_CACHE"


(** Parse Command Line Args *)

let exe_usage (exe : CLOpt.exe) =
  match exe with
  | A ->
      version_string ^ "\
      Usage: InferAnalyze [options]\n\
      Analyze the files captured in the project results directory, \
      which can be specified with the --results-dir option."
  | C ->
      "\nUsage: InferClang -c C Files -ast AST Files --results-dir <output-dir> [options] \n"
  | J ->
      "Usage: InferJava -d compilation_dir -sources filename\n"
  | L ->
      "Usage: InferLLVM -c <cfile> [options]\n"
  | P ->
      "Usage: InferPrint [options] name1.specs ... namen.specs\n\
       Read, convert, and print .specs files. \
       To process all the .specs in the current directory, pass . as only parameter \
       To process all the .specs in the results directory, use option --results-dir \
       Each spec is printed to standard output unless option -q is used."
  | T ->
      version_string

let post_parsing_initialization () =
  F.set_margin !margin ;

  if !version then (
    F.fprintf F.std_formatter "%s@." version_string ;
    exit 0
  );
  if !version_json then (
    F.fprintf F.std_formatter "%s@." Version.versionJson ;
    exit 0
  );

  let set_minor_heap_size nMb = (* increase the minor heap size to speed up gc *)
    let ctrl = Gc.get () in
    let oneMb = 1048576 in
    let new_size = max ctrl.Gc.minor_heap_size (nMb * oneMb)
    in Gc.set { ctrl with Gc.minor_heap_size = new_size }
  in
  set_minor_heap_size 1 ;

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
            let dest_file = Filename.concat dest_dir (Filename.basename entry.Zip.filename) in
            if Filename.check_suffix entry.Zip.filename specs_files_suffix
            then Zip.copy_entry_to_file zip_channel entry dest_file in
          IList.iter extract_entry entries;
          Zip.close_in zip_channel
        in
        let basename = Filename.basename zip_filename in
        let key = basename ^ string_crc_hex32 zip_filename in
        let key_dir = Filename.concat cache_dir key in
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
   | Some file -> add_models (filename_to_absolute file)
   | None -> ());

  zip_libraries := IList.rev_append !zip_models (IList.rev !zip_libraries)


let parse_args_and_return_usage_exit =
  let usage_exit = CLOpt.parse "INFER_ARGS" exe_usage in
  if !debug || (!developer_mode && not (CLOpt.current_exe = CLOpt.P)) then
    prerr_endline
      ((Filename.basename Sys.executable_name) ^ " got args "
       ^ (try Unix.getenv "INFER_ARGS" with Not_found -> "")) ;
  post_parsing_initialization () ;
  usage_exit

let print_usage_exit () =
  parse_args_and_return_usage_exit 1


(** Freeze initialized configuration values *)

let anon_args = !anon_args
and abs_struct = !abs_struct
and allow_specs_cleanup = !allow_specs_cleanup
and analysis_stops = !analysis_stops
and analyzer = !analyzer
and angelic_execution = !angelic_execution
and arc_mode = objc_arc
and array_level = !array_level
and ast_file = !ast_file
and bugs_csv = !bugs_csv
and bugs_json = !bugs_json
and bugs_txt = !bugs_txt
and bugs_xml = !bugs_xml
and calls_csv = !calls_csv
and checkers = !checkers
(** should the checkers be run? *)
and checkers_enabled = not !eradicate
and clang_lang = !clang_lang
and cluster_cmdline = !cluster
and code_query = !code_query
and continue_capture = !continue
and create_harness = !harness
and cxx_experimental = !cxx_experimental
and debug_mode = !debug
and dependency_mode = !dependencies
and developer_mode = !developer_mode
and dotty_cfg_libs = !dotty_cfg_libs
and eradicate = !eradicate
and err_file_cmdline = !err_file
and infer_cache = !infer_cache
and iterations = !iterations
and javac_verbose_out = !verbose_out
and join_cond = !join_cond
and latex = !latex
and load_analysis_results = !load_results
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
and precondition_stats = !precondition_stats
and print_builtins = !print_builtins
and print_types = !print_types
and print_using_diff = !print_using_diff
and procs_csv = !procs_csv
and procs_xml = !procs_xml
and project_root = !project_root
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
and source_file = !source_file
and source_file_copy = !source_file_copy
and spec_abs_level = !spec_abs_level
and specs_library = !specs_library
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
and unit_test = !unit_test
and whole_seconds = !whole_seconds
and worklist_mode = !worklist_mode
and write_dotty = !write_dotty
and write_html = !write_html
and xml_specs = !xml_specs
and zip_libraries = !zip_libraries

let inferconfig_json =
  let inferconfig_path =
    match !inferconfig_home, project_root with
    | Some dir, _ | _, Some dir -> Filename.concat dir inferconfig_file
    | None, None -> inferconfig_file in
  lazy (
    match read_optional_json_file inferconfig_path with
    | Ok json -> json
    | Error msg ->
        F.fprintf F.err_formatter "Could not read or parse Infer config in %s:@\n%s@."
          inferconfig_path msg;
        exit 1)

and suppress_warnings_json = lazy (
  let error msg =
    F.fprintf F.err_formatter "There was an issue reading the option %s.@\n"
      suppress_warnings_annotations_long ;
    F.fprintf F.err_formatter "If you did not call %s directly, this is likely a bug in Infer.@\n"
      (Filename.basename Sys.executable_name) ;
    F.fprintf F.err_formatter "%s@." msg ;
    exit 1 in
  match !suppress_warnings_out with
  | Some path -> (
      match read_optional_json_file path with
      | Ok json -> json
      | Error msg -> error ("Could not read or parse the supplied " ^ path ^ ":\n" ^ msg))
  | None ->
      error ("Error: The option " ^ suppress_warnings_annotations_long ^ " was not provided"))
