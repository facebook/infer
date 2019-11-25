(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Configuration values: either constant, determined at compile time, or set at startup
    time by system calls, environment variables, or command line options *)

type os_type = Unix | Win32 | Cygwin

type compilation_database_dependencies =
  | Deps of int option
      (** get the compilation database of the dependencies up to depth n
     by [Deps (Some n)], or all by [Deps None]  *)
  | NoDeps
[@@deriving compare]

type build_system =
  | BAnt
  | BBuck
  | BClang
  | BGradle
  | BJava
  | BJavac
  | BMake
  | BMvn
  | BNdk
  | BXcode
[@@deriving compare]

val equal_build_system : build_system -> build_system -> bool

val build_system_of_exe_name : string -> build_system

val string_of_build_system : build_system -> string

val env_inside_maven : Unix.env

(** {2 Constant configuration values} *)

val anonymous_block_num_sep : string

val anonymous_block_prefix : string

val append_buck_flavors : string list

val assign : string

val backend_stats_dir_name : string

val biabduction_models_dir : string

val biabduction_models_jar : string

val biabduction_models_src_dir : string

val bin_dir : string

val bound_error_allowed_in_procedure_call : bool

val buck_infer_deps_file_name : string

val captured_dir_name : string

val clang_initializer_prefix : string

val clang_inner_destructor_prefix : string

val classnames_dir_name : string

val classpath : string option

val costs_report_json : string

val csl_analysis : bool

val default_failure_name : string

val default_in_zip_results_dir : string

val dotty_frontend_output : string

val driver_stats_dir_name : string

val duplicates_filename : string

val etc_dir : string

val events_dir_name : string

val fail_on_issue_exit_code : int

val frontend_stats_dir_name : string

val global_tenv_filename : string

val idempotent_getters : bool

val infer_py_argparse_error_exit_code : int

val initial_analysis_time : float

val ivar_attributes : string

val lib_dir : string

val lint_dotty_dir_name : string

val lint_issues_dir_name : string

val load_average : float option

val max_narrows : int

val max_widens : int

val meet_level : int

val nsnotification_center_checker_backend : bool

val os_type : os_type

val passthroughs : bool

val patterns_modeled_expensive : string * Yojson.Basic.t

val patterns_never_returning_null : string * Yojson.Basic.t

val patterns_skip_implementation : string * Yojson.Basic.t

val patterns_skip_translation : string * Yojson.Basic.t

val perf_stats_prefix : string

val pp_version : Format.formatter -> unit -> unit

val proc_stats_filename : string

val property_attributes : string

val racerd_issues_dir_name : string

val relative_path_backtrack : int

val report : bool

val report_condition_always_true_in_clang : bool

val report_custom_error : bool

val report_force_relative_path : bool

val report_json : string

val report_nullable_inconsistency : bool

val reporting_stats_dir_name : string

val retain_cycle_dotty_dir : string

val save_compact_summaries : bool

val smt_output : bool

val source_file_extentions : string list

val sourcepath : string option

val sources : string list

val specs_dir_name : string

val specs_files_suffix : string

val starvation_issues_dir_name : string

val test_determinator_results : string

val trace_absarray : bool

val trace_events_file : string

val unsafe_unret : string

val use_cost_threshold : bool

val incremental_analysis : bool

val weak : string

val whitelisted_cpp_classes : string list

val whitelisted_cpp_methods : string list

val wrappers_dir : string

(** {2 Configuration values specified by command-line options} *)

type iphoneos_target_sdk_version_path_regex = {path: Str.regexp; version: string}

val abs_struct : int

val abs_val : int

val allow_leak : bool

val analysis_stops : bool

val annotation_reachability : bool

val annotation_reachability_cxx : Yojson.Basic.t

val annotation_reachability_cxx_sources : Yojson.Basic.t

val annotation_reachability_custom_pairs : Yojson.Basic.t

val anon_args : string list

val array_level : int

val biabduction : bool

val biabduction_model_alloc_pattern : Str.regexp option

val biabduction_model_free_pattern : Str.regexp option

val biabduction_models_mode : bool

val bo_debug : int

val bo_relational_domain : [`Bo_relational_domain_oct | `Bo_relational_domain_poly] option

val bootclasspath : string option

val buck : bool

val buck_blacklist : string list

val buck_build_args : string list

val buck_build_args_no_inline : string list

val buck_cache_mode : bool

val buck_compilation_database : compilation_database_dependencies option

val buck_merge_all_deps : bool

val buck_out : string option

val buck_targets_blacklist : string list

val bufferoverrun : bool

val call_graph_schedule : bool

val capture : bool

val capture_blacklist : string option

val captured_dir : string
(** directory where the results of the capture phase are stored *)

val censor_report : ((bool * Str.regexp) * (bool * Str.regexp) * string) list

val changed_files_index : string option

val check_version : string option

val clang_biniou_file : string option

val clang_extra_flags : string list

val clang_blacklisted_flags : string list

val clang_blacklisted_flags_with_arg : string list

val clang_ignore_regex : string option

val clang_isystem_to_override_regex : Str.regexp option

val clang_idirafter_to_override_regex : Str.regexp option

val clang_libcxx_include_to_override_regex : string option

val class_loads : bool

val class_loads_roots : String.Set.t

val command : InferCommand.t

val compute_analytics : bool

val continue_capture : bool

val cost : bool

val costs_current : string option

val costs_previous : string option

val cxx : bool

val cxx_scope_guards : Yojson.Basic.t

val deduplicate : bool

val debug_exceptions : bool

val debug_level_analysis : int

val debug_level_capture : int

val debug_level_linters : int

val debug_level_test_determinator : int

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

val eradicate_field_over_annotated : bool

val eradicate_return_over_annotated : bool

val eradicate_verbose : bool

val fail_on_bug : bool

val fcp_apple_clang : string option

val fcp_syntax_only : bool

val file_renamings : string option

val filter_paths : bool

val filtering : bool

val flavors : bool

val force_delete_results_dir : bool

val force_integration : build_system option

val fragment_retains_view : bool

val from_json_report : string option

val frontend_stats : bool

val frontend_tests : bool

val function_pointer_specialization : bool

val generated_classes : string option

val genrule_master_mode : bool

val genrule_mode : bool

val get_linter_doc_url : linter_id:string -> string option

val hoisting_report_only_expensive : bool

val html : bool

val icfg_dotty_outfile : string option

val immutable_cast : bool

val impurity : bool

val inefficient_keyset_iterator : bool

val infer_is_clang : bool

val infer_is_javac : bool

val implicit_sdk_root : string option

val inferconfig_file : string option

val inferconfig_dir : string option

val iphoneos_target_sdk_version : string option

val iphoneos_target_sdk_version_path_regex : iphoneos_target_sdk_version_path_regex list

val issues_fields :
  [ `Issue_field_bug_type
  | `Issue_field_qualifier
  | `Issue_field_severity
  | `Issue_field_bucket
  | `Issue_field_line
  | `Issue_field_column
  | `Issue_field_procedure
  | `Issue_field_procedure_start_line
  | `Issue_field_file
  | `Issue_field_bug_trace
  | `Issue_field_key
  | `Issue_field_hash
  | `Issue_field_line_offset
  | `Issue_field_qualifier_contains_potential_exception_note ]
  list

val issues_tests : string option

val issues_txt : string option

val iterations : int

val java_jar_compiler : string option

val java_version : int option

val javac_classes_out : string

val job_id : string option

val jobs : int

val join_cond : int

val keep_going : bool

val linter : string option

val linters : bool

val linters_def_file : string list

val linters_def_folder : string list

val linters_developer_mode : bool

val linters_ignore_clang_failures : bool

val linters_validate_syntax_only : bool

val litho_graphql_field_access : bool

val litho_required_props : bool

val liveness : bool

val liveness_dangerous_classes : Yojson.Basic.t

val log_events : bool

val log_file : string

val log_skipped : bool

val loop_hoisting : bool

val max_nesting : int option

val merge : bool

val method_decls_info : string option

val ml_buckets :
  [`MLeak_all | `MLeak_arc | `MLeak_cf | `MLeak_cpp | `MLeak_no_arc | `MLeak_unknown] list

val modified_lines : string option

val monitor_prop_size : bool

val nelseg : bool

val no_translate_libs : bool

val nullable_annotation : string option

val nullsafe : bool

val nullsafe_optimistic_third_party_params_in_non_strict : bool

val nullsafe_third_party_signatures : string option

val nullsafe_third_party_location_for_messaging_only : string option

val nullsafe_strict_containers : bool

val oom_threshold : int option

val only_cheap_debug : bool

val only_footprint : bool

val only_show : bool

val self_in_block : bool

val perf_profiler_data_file : string option

val pmd_xml : bool

val precondition_stats : bool

val print_active_checkers : bool

val print_builtins : bool

val print_log_identifier : bool

val print_logs : bool

val print_types : bool

val print_using_diff : bool

val printf_args : bool

val procedures : bool

val procedures_attributes : bool

val procedures_definedness : bool

val procedures_filter : string option

val procedures_name : bool

val procedures_source_file : bool

val process_clang_ast : bool

val clang_frontend_action_string : string

val procs_csv : string option

val profiler_samples : string option

val progress_bar : [`MultiLine | `Plain | `Quiet]

val project_root : string

val pulse : bool

val pulse_max_disjuncts : int

val pulse_widen_threshold : int

val purity : bool

val pure_by_default : bool

val quandary : bool

val quandary_endpoints : Yojson.Basic.t

val quandary_sanitizers : Yojson.Basic.t

val quandary_sinks : Yojson.Basic.t

val quandary_sources : Yojson.Basic.t

val quandaryBO : bool

val quiet : bool

val racerd : bool

val racerd_guardedby : bool

val reactive_capture : bool

val reactive_mode : bool

val reanalyze : bool

val report_blacklist_files_containing : string list

val report_current : string option

val report_formatter : [`No_formatter | `Phabricator_formatter]

val report_hook : string option

val report_path_regex_blacklist : string list

val report_path_regex_whitelist : string list

val report_previous : string option

val report_suppress_errors : string list

val reports_include_ml_loc : bool

val resource_leak : bool

val rest : string list

val results_dir : string

val scuba_logging : bool

val scuba_normals : string String.Map.t

val seconds_per_iteration : float option

val select : int option

val show_buckets : bool

val siof : bool

val siof_check_iostreams : bool

val siof_safe_methods : string list

val skip_analysis_in_path : string list

val skip_analysis_in_path_skips_compilation : bool

val skip_duplicated_types : bool

val skip_translation_headers : string list

val source_files : bool

val source_files_cfg : bool

val source_files_filter : string option

val source_files_freshly_captured : bool

val source_files_procedure_names : bool

val source_files_type_environment : bool

val source_preview : bool

val spec_abs_level : int

val specs_library : string list

val sqlite_cache_size : int

val sqlite_page_size : int

val sqlite_lock_timeout : int

val sqlite_vfs : string option

val sqlite_write_daemon : bool

val starvation : bool

val starvation_skip_analysis : Yojson.Basic.t

val starvation_strict_mode : bool

val starvation_whole_program : bool

val stats_report : string option

val subtype_multirange : bool

val summary_stats : bool

val symops_per_iteration : int option

val test_determinator : bool

val test_determinator_output : string

val export_changed_functions : bool

val export_changed_functions_output : string

val test_filtering : bool

val testing_mode : bool

val threadsafe_aliases : Yojson.Basic.t

val topl_properties : string list

val toplevel_results_dir : string
(** In some integrations, eg Buck, infer subprocesses started by the build system (started by the
    toplevel infer process) will have their own results directory; this points to the results
    directory of the toplevel infer process, which can be useful for, eg, storing debug info. In
    other cases this is equal to {!results_dir}. *)

val trace_error : bool

val trace_events : bool

val trace_join : bool

val trace_ondemand : bool

val trace_rearrange : bool

val trace_topl : bool

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
(** {2 Global variables with initial values specified by command-line options} *)

val clang_compilation_dbs : [`Escaped of string | `Raw of string] list ref

(** {2 Command Line Interface Documentation} *)

val print_usage_exit : unit -> 'a

(** {2 Miscellanous} *)

val is_in_custom_symbols : string -> string -> bool
(** Does named symbol match any prefix in the named custom symbol list? *)

val java_package_is_external : string -> bool
(** Check if a Java package is external to the repository *)

val quandaryBO_filtered_issues : IssueType.t list
(** List of issues that are enabled by QuandaryBO but should not be in the final report.json *)

val execution_id : Int64.t
