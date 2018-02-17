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
module CLOpt = CommandLineOption

(** Configuration values: either constant, determined at compile time, or set at startup
    time by system calls, environment variables, or command line options *)

type analyzer =
  | CaptureOnly
  | CompileOnly
  | Checkers
  | Crashcontext
  | Linters
  [@@deriving compare]

val equal_analyzer : analyzer -> analyzer -> bool

val string_to_analyzer : (string * analyzer) list
(** Association list of analyzers and their names *)

val string_of_analyzer : analyzer -> string

type os_type = Unix | Win32 | Cygwin

type compilation_database_dependencies =
  | Deps of int option
      (** get the compilation database of the dependencies up to depth n
     by [Deps (Some n)], or all by [Deps None]  *)
  | NoDeps
  [@@deriving compare]

type build_system =
  | BAnalyze
  | BAnt
  | BBuck
  | BClang
  | BGradle
  | BJava
  | BJavac
  | BMake
  | BMvn
  | BNdk
  | BPython
  | BXcode
  [@@deriving compare]

val equal_build_system : build_system -> build_system -> bool

val build_system_of_exe_name : string -> build_system

val string_of_build_system : build_system -> string

val env_inside_maven : Unix.env

(** Constant configuration values *)

val anonymous_block_num_sep : string

val anonymous_block_prefix : string

val append_buck_flavors : string list

val assign : string

val backend_stats_dir_name : string

val bin_dir : string

val bound_error_allowed_in_procedure_call : bool

val buck_infer_deps_file_name : string

val captured_dir_name : string

val clang_initializer_prefix : string

val clang_inner_destructor_prefix : string

val classnames_dir_name : string

val classpath : string option

val cpp_extra_include_dir : string

val duplicates_filename : string

val relative_cpp_models_dir : string

val csl_analysis : bool

val default_failure_name : string

val default_in_zip_results_dir : string

val dotty_output : string

val events_dir_name : string

val etc_dir : string

val fail_on_issue_exit_code : int

val frontend_stats_dir_name : string

val global_tenv_filename : string

val idempotent_getters : bool

val incremental_procs : bool

val infer_py_argparse_error_exit_code : int

val infer_top_results_dir_env_var : string

val initial_analysis_time : float

val ivar_attributes : string

val lib_dir : string

val lint_dotty_dir_name : string

val lint_issues_dir_name : string

val linters_failed_sentinel_filename : string

val load_average : float option

val log_analysis_crash : string

val log_analysis_file : string

val log_analysis_procedure : string

val log_analysis_recursion_timeout : string

val log_analysis_symops_timeout : string

val log_analysis_wallclock_timeout : string

val max_meet : int

val max_widens : int

val meet_level : int

val models_dir : string

val models_jar : string

val models_src_dir : string

val multicore_dir_name : string

val nsnotification_center_checker_backend : bool

val os_type : os_type

val passthroughs : bool

val patterns_modeled_expensive : string * Yojson.Basic.json

val patterns_never_returning_null : string * Yojson.Basic.json

val patterns_skip_implementation : string * Yojson.Basic.json

val patterns_skip_translation : string * Yojson.Basic.json

val per_procedure_parallelism : bool

val perf_stats_prefix : string

val pp_version : Format.formatter -> unit -> unit

val proc_stats_filename : string

val property_attributes : string

val report : bool

val report_condition_always_true_in_clang : bool

val report_custom_error : bool

val report_json : string

val report_nullable_inconsistency : bool

val reporting_stats_dir_name : string

val retain_cycle_dotty_dir : string

val save_compact_summaries : bool

val smt_output : bool

val source_file_extentions : string list

val sources : string list

val sourcepath : string option

val specs_dir_name : string

val specs_files_suffix : string

val trace_absarray : bool

val undo_join : bool

val unsafe_unret : string

val use_jar_cache : bool

val weak : string

val whitelisted_cpp_methods : string list

val whitelisted_cpp_classes : string list

val wrappers_dir : string

(** Configuration values specified by command-line options *)

val anon_args : string list

val rest : string list

val abs_struct : int

val analysis_path_regex_whitelist : analyzer -> string list

val analysis_path_regex_blacklist : analyzer -> string list

val analysis_blacklist_files_containing : analyzer -> string list

val analysis_stops : bool

val analysis_suppress_errors : analyzer -> string list

val analyzer : analyzer

val annotation_reachability : bool

val annotation_reachability_custom_pairs : Yojson.Basic.json

val array_level : int

val biabduction : bool

val blacklist : string option

val bootclasspath : string option

val bo_debug : int

val buck : bool

val buck_build_args : string list

val buck_cache_mode : bool

val buck_compilation_database : compilation_database_dependencies option

val buck_out : string option

val bufferoverrun : bool

val calls_csv : string option

val capture : bool

val captured_dir : string
(** directory where the results of the capture phase are stored *)

val changed_files_index : string option

val check_nullable : bool

val check_version : string option

val clang_biniou_file : string option

val clang_frontend_action_string : string

val clang_ignore_regex : string option

val clang_include_to_override_regex : string option

val cluster_cmdline : string option

val command : InferCommand.t

val compute_analytics : bool

val continue_capture : bool

val crashcontext : bool

val current_to_previous_script : string option

val cxx : bool

val cxx_infer_headers : bool

val cxx_scope_guards : Yojson.Basic.json

val debug_level_analysis : int

val debug_level_capture : int

val debug_level_linters : int

val debug_exceptions : bool

val debug_mode : bool

val default_linters : bool

val dependency_mode : bool

val developer_mode : bool

val differential_filter_files : string option

val differential_filter_set : [`Introduced | `Fixed | `Preexisting] list

val dotty_cfg_libs : bool

val dump_duplicate_symbols : bool

val dynamic_dispatch : bool

val eradicate : bool

val eradicate_condition_redundant : bool

val eradicate_field_not_mutable : bool

val eradicate_field_over_annotated : bool

val eradicate_optional_present : bool

val eradicate_return_over_annotated : bool

val eradicate_debug : bool

val eradicate_verbose : bool

val fail_on_bug : bool

val fcp_apple_clang : string option

val fcp_syntax_only : bool

val file_renamings : string option

val filter_paths : bool

val filter_report : ((bool * Str.regexp) * (bool * Str.regexp) * string) list

val filtering : bool

val flavors : bool

val force_delete_results_dir : bool

val fragment_retains_view : bool

val force_integration : build_system option

val from_json_report : string option

val frontend_tests : bool

val frontend_stats : bool

val gen_previous_build_command_script : string option

val generated_classes : string option

val html : bool

val icfg_dotty_outfile : string option

val ignore_trivial_traces : bool

val immutable_cast : bool

val infer_cache : string option

val infer_is_clang : bool

val infer_is_javac : bool

val inferconfig_file : string option

val iphoneos_target_sdk_version : string option

type iphoneos_target_sdk_version_path_regex = {path: Str.regexp; version: string}

val iphoneos_target_sdk_version_path_regex : iphoneos_target_sdk_version_path_regex list

val issues_fields :
  [ `Issue_field_bug_class
  | `Issue_field_kind
  | `Issue_field_bug_type
  | `Issue_field_qualifier
  | `Issue_field_severity
  | `Issue_field_visibility
  | `Issue_field_line
  | `Issue_field_column
  | `Issue_field_procedure
  | `Issue_field_procedure_id
  | `Issue_field_procedure_start_line
  | `Issue_field_file
  | `Issue_field_bug_trace
  | `Issue_field_key
  | `Issue_field_hash
  | `Issue_field_line_offset
  | `Issue_field_procedure_id_without_crc
  | `Issue_field_qualifier_contains_potential_exception_note ]
  list

val issues_tests : string option

val issues_txt : string option

val iterations : int

val java_jar_compiler : string option

val javac_classes_out : string

val jobs : int

val join_cond : int

val keep_going : bool

val linter : string option

val linters : bool

val linters_def_file : string list

val linters_def_folder : string list

type linter_doc_url = {linter: string; doc_url: string}

val linters_doc_url : linter_doc_url list

val linters_developer_mode : bool

val linters_ignore_clang_failures : bool

val linters_validate_syntax_only : bool

val litho : bool

val liveness : bool

val log_events : bool

val log_file : string

val max_nesting : int option

val merge : bool

val ml_buckets :
  [`MLeak_all | `MLeak_arc | `MLeak_cf | `MLeak_cpp | `MLeak_no_arc | `MLeak_unknown] list

val models_mode : bool

val modified_targets : string option

val monitor_prop_size : bool

val nelseg : bool

val no_translate_libs : bool

val nullable_annotation : string option

val only_cheap_debug : bool

val only_footprint : bool

val only_show : bool

val ownership : bool

val pmd_xml : bool

val precondition_stats : bool

val previous_to_current_script : string option

val print_active_checkers : bool

val print_builtins : bool

val print_log_identifier : bool

val print_logs : bool

val print_types : bool

val print_using_diff : bool

val printf_args : bool

val procedures_per_process : int

val procs_csv : string option

val project_root : string

val quandary : bool

val quandary_endpoints : Yojson.Basic.json

val quandary_sanitizers : Yojson.Basic.json

val quandary_sources : Yojson.Basic.json

val quandary_sinks : Yojson.Basic.json

val quiet : bool

val reactive_mode : bool

val reactive_capture : bool

val report_current : string option

val report_formatter : [`No_formatter | `Phabricator_formatter]

val report_hook : string option

val report_previous : string option

val tracing : bool

val reports_include_ml_loc : bool

val resource_leak : bool

val results_dir : string

val seconds_per_iteration : float option

val select : int option

val show_buckets : bool

val show_progress_bar : bool

val siof : bool

val siof_safe_methods : string list

val skip_analysis_in_path : string list

val skip_analysis_in_path_skips_compilation : bool

val skip_duplicated_types : bool

val skip_translation_headers : string list

val source_preview : bool

val spec_abs_level : int

val specs_library : string list

val sqlite_vfs : string option

val stacktrace : string option

val stacktraces_dir : string option

val stats_report : string option

val subtype_multirange : bool

val suggest_nullable : bool

val symops_per_iteration : int option

val test_filtering : bool

val testing_mode : bool

val racerd : bool

val threadsafe_aliases : Yojson.Basic.json

val trace_error : bool

val trace_ondemand : bool

val trace_join : bool

val trace_rearrange : bool

val tv_commit : string option

val tv_limit : int

val tv_limit_filtered : int

val type_size : bool

val uninit : bool

val uninit_interproc : bool

val unsafe_malloc : bool

val worklist_mode : int

val write_dotty : bool

val write_html : bool

val write_html_whitelist_regex : string list

val xcode_developer_dir : string option

val xcpretty : bool

(** Global variables *)

val arc_mode : bool ref

val footprint : bool ref

val run_in_footprint_mode : ('a -> 'b) -> 'a -> 'b
(** Call f x with footprint set to true.
    Restore the initial value of footprint also in case of exception. *)

val run_in_re_execution_mode : ('a -> 'b) -> 'a -> 'b
(** Call f x with footprint set to false.
    Restore the initial value of footprint also in case of exception. *)

val forcing_delayed_prints : bool ref

val pp_simple : bool ref

(** Global variables with initial values specified by command-line options *)

val abs_val : int ref

val reset_abs_val : unit -> unit

val run_with_abs_val_equal_zero : ('a -> 'b) -> 'a -> 'b
(** Call f x with abs_val set to zero.
    Restore the initial value also in case of exception. *)

val allow_leak : bool ref

val clang_compilation_dbs : [`Escaped of string | `Raw of string] list ref

(** Command Line Interface Documentation *)

val print_usage_exit : unit -> 'a

(** Miscellanous *)

val register_late_epilogue : (unit -> unit) -> unit
