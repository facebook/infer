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


(** Various kind of analyzers *)
type analyzer = Capture | Compile | Infer | Eradicate | Checkers | Tracing
              | Crashcontext | Linters | Quandary

(** Association list of analyzers and their names *)
val string_to_analyzer : (string * analyzer) list


type clang_lang = C | CPP | OBJC | OBJCPP

type language = Clang | Java

val string_of_language : language -> string


val ml_bucket_symbols :
  (string * [ `MLeak_all | `MLeak_arc | `MLeak_cf | `MLeak_cpp | `MLeak_no_arc | `MLeak_unknown ])
    list


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
  zip_filename : string;
  zip_channel : Zip.in_file Lazy.t;
  models : bool;
}


(** Constant configuration values *)

val allow_missing_index_in_proc_call : bool
val anonymous_block_num_sep : string
val anonymous_block_prefix : string
val assign : string
val attributes_dir_name : string
val backend_stats_dir_name : string
val bound_error_allowed_in_procedure_call : bool
val buck_generated_folder : string
val buck_infer_deps_file_name : string
val captured_dir_name : string
val checks_disabled_by_default : string list
val clang_build_output_dir_name : string
val cpp_models_dir : string
val csl_analysis : bool
val default_failure_name : string
val default_in_zip_results_dir : string
val dotty_output : string
val fail_on_issue_exit_code : int
val filter_buckets : bool
val frontend_stats_dir_name : string
val global_tenv_filename : string
val idempotent_getters : bool
val incremental_procs : bool
val infer_py_argparse_error_exit_code : int
val initial_analysis_time : float
val ivar_attributes : string
val lint_issues_dir_name : string
val load_average : float option
val log_analysis_crash : string
val log_analysis_file : string
val log_analysis_procedure : string
val log_analysis_recursion_timeout : string
val log_analysis_symops_timeout : string
val log_analysis_wallclock_timeout : string
val log_dir_name : string
val max_recursion : int
val meet_level : int
val models_dir : string
val ncpu : int
val nsnotification_center_checker_backend : bool
val os_type : os_type
val patterns_modeled_expensive : pattern list
val patterns_never_returning_null : pattern list
val patterns_skip_translation : pattern list
val patterns_suppress_warnings : pattern list
val perf_stats_prefix : string
val proc_stats_filename : string
val property_attributes : string
val report_nullable_inconsistency : bool
val reporting_stats_dir_name : string
val save_compact_summaries : bool
val save_time_in_summaries : bool
val smt_output : bool
val source_file_extentions : string list
val sources_dir_name : string
val specs_dir_name : string
val specs_files_suffix : string
val start_filename : string
val taint_analysis : bool
val trace_absarray : bool
val undo_join : bool
val unsafe_unret : string
val weak : string
val whitelisted_cpp_methods : string list list
val wrappers_dir : string


(** Configuration values specified by environment variables *)

val from_env_variable : string -> bool
val get_env_variable : string -> string option

val analyze_models : bool
val lazy_dynamic_dispatch : bool
val report_custom_error : bool
val sound_dynamic_dispatch : bool

val is_originator : bool


(** Configuration values specified by command-line options *)

val anon_args : string list
val rest : string list
val abs_struct : int
val absolute_paths : bool
val allow_specs_cleanup : bool
val analysis_path_regex_whitelist : analyzer -> string list
val analysis_path_regex_blacklist : analyzer -> string list
val analysis_blacklist_files_containing : analyzer -> string list
val analysis_stops : bool
val analysis_suppress_errors : analyzer -> string list
val analyzer : analyzer option
val angelic_execution : bool
val array_level : int
val ast_file : string option
val blacklist : string option
val buck : bool
val buck_build_args : string list
val buck_out : string option
val bugs_csv : outfile option
val bugs_json : outfile option
val bugs_tests : outfile option
val bugs_txt : outfile option
val bugs_xml : outfile option
val changed_files_index : string option
val calls_csv : outfile option
val check_duplicate_symbols : bool
val checkers : bool
val checkers_enabled : bool
val clang_frontend_action : [`Lint | `Capture | `Lint_and_capture ]
val clang_frontend_action_string : string
val clang_frontend_do_capture : bool
val clang_frontend_do_lint : bool
val clang_include_to_override : string option
val clang_lang : clang_lang
val cluster_cmdline : string option
val continue_capture : bool
val copy_propagation : bool
val crashcontext : bool
val create_harness : bool
val cxx_experimental : bool
val debug_mode : bool
val debug_exceptions : bool
val dependency_mode : bool
val developer_mode : bool
val disable_checks : string list
val dotty_cfg_libs : bool
val enable_checks : string list
val eradicate : bool
val err_file_cmdline : string
val fail_on_bug : bool
val failures_allowed : bool
val filtering : bool
val flavors : bool
val frontend_debug : bool
val frontend_tests : bool
val frontend_stats : bool
val headers : bool
val infer_cache : string option
val iterations : int
val javac_verbose_out : string
val jobs : int
val join_cond : int
val latex : outfile option
val load_analysis_results : string option
val llvm : bool
val makefile_cmdline : string
val merge : bool
val ml_buckets :
  [ `MLeak_all | `MLeak_arc | `MLeak_cf | `MLeak_cpp | `MLeak_no_arc | `MLeak_unknown ] list
val models_file : string option
val models_mode : bool
val modified_targets : string option
val monitor_prop_size : bool
val nelseg : bool
val no_static_final : bool
val no_translate_libs : bool
val nonstop : bool
val objc_memory_model_on : bool
val only_footprint : bool
val optimistic_cast : bool
val out_file_cmdline : string
val pmd_xml : bool
val precondition_stats : bool
val print_builtins : bool
val print_types : bool
val print_using_diff : bool
val procs_csv : outfile option
val procs_xml : outfile option
val project_root : string option
val quandary : bool
val quiet : bool
val reactive_mode : bool
val report : outfile option
val report_runtime_exceptions : bool
val reports_include_ml_loc : bool
val results_dir : string
val save_analysis_results : string option
val seconds_per_iteration : float
val show_buckets : bool
val show_progress_bar : bool
val skip_clang_analysis_in_path : string list
val skip_translation_headers : string list
val source_file : string option
val source_file_copy : string option
val spec_abs_level : int
val specs_library : string list
val stacktrace : string option
val stacktraces_dir : string option
val stats_mode : bool
val subtype_multirange : bool
val svg : bool
val symops_per_iteration : int
val test : bool
val test_filtering : bool
val testing_mode : bool
val trace_error : bool
val trace_join : bool
val trace_rearrange : bool
val type_size : bool
val unsafe_malloc : bool
val use_compilation_database : [ `Deps | `NoDeps ] option
val whole_seconds : bool
val worklist_mode : int
val write_dotty : bool
val write_html : bool
val xcode_developer_dir : string option
val xml_specs : bool
val zip_libraries : zip_library list


(** Global variables *)

(** [set_reference_and_call_function ref val f x] calls f x with ref set to val.
    Restore the initial value also in case of exception. *)
val set_reference_and_call_function : 'a ref -> 'a -> ('b -> 'c) -> 'b -> 'c

val footprint : bool ref

(** Call f x with footprint set to true.
    Restore the initial value of footprint also in case of exception. *)
val run_in_footprint_mode : ('a -> 'b) -> 'a -> 'b

(** Call f x with footprint set to false.
    Restore the initial value of footprint also in case of exception. *)
val run_in_re_execution_mode : ('a -> 'b) -> 'a -> 'b

val forcing_delayed_prints : bool ref
val nLOC : int ref
val pp_simple : bool ref



(** Global variables with initial values specified by command-line options *)

val abs_val : int ref

val reset_abs_val : unit -> unit

(** Call f x with abs_val set to zero.
    Restore the initial value also in case of exception. *)
val run_with_abs_val_equal_zero : ('a -> 'b) -> 'a -> 'b

val allow_leak : bool ref
val arc_mode : bool ref
val curr_language : language ref


(** Command Line Interface Documentation *)

val print_usage_exit : unit -> 'a

(** Name of files for logging the output in the current executable *)
val log_files_of_current_exe : string * string

(** Name of current temporary files for logging the output in the current executable *)
val tmp_log_files_of_current_exe : unit -> string * string

(** should_log_exe = true means that files for logging in the log folder will be created
    and uses of Logging.out or Logging.err will log in those files *)
val should_log_current_exe : bool
