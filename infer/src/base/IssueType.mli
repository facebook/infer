(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** visibility of the issue type *)
type visibility =
  | User  (** always add to error log *)
  | Developer  (** only add to error log in some debug modes *)
  | Silent  (** never add to error log *)
[@@deriving compare, equal]

val string_of_visibility : visibility -> string

(** severity of the report *)
type severity = Like | Info | Advice | Warning | Error [@@deriving compare, equal, enumerate]

val string_of_severity : severity -> string

type t = private
  { unique_id: string
  ; checker: Checker.t
  ; visibility: visibility
  ; user_documentation: string option
  ; mutable default_severity: severity
        (** used for documentation but can be overriden at report time *)
  ; mutable enabled: bool
  ; mutable hum: string
  ; mutable doc_url: string option
  ; mutable linters_def_file: string option }
[@@deriving compare]

val equal : t -> t -> bool

val all_issues : unit -> t list
(** all the issues declared so far *)

val pp : Format.formatter -> t -> unit
(** pretty print a localised string *)

val find_from_string : id:string -> t option
(** return the issue type if it was previously registered *)

val register_dynamic :
     ?enabled:bool
  -> ?hum:string
  -> ?doc_url:string
  -> linters_def_file:string option
  -> id:string
  -> ?user_documentation:string
  -> severity
  -> Checker.t
  -> t
(** Create a new issue and register it in the list of all issues. NOTE: if the issue with the same
    string id is already registered, overrides `hum`, `doc_url`, and `linters_def_file`, but DOES
    NOT override `enabled`. This trick allows to deal with disabling/enabling dynamic AL issues from
    the config, when we don't know all params yet. Thus, the human-readable description can be
    updated when we encounter the definition of the issue type, eg in AL. *)

val checker_can_report : Checker.t -> t -> bool
(** Whether the issue was registered as coming from the given checker. Important to call this before
    reporting to keep documentation accurate. *)

val set_enabled : t -> bool -> unit

val abduction_case_not_implemented : t

val arbitrary_code_execution_under_lock : t

val array_of_pointsto : t

val array_out_of_bounds_l1 : t

val array_out_of_bounds_l2 : t

val array_out_of_bounds_l3 : t

val assert_failure : t

val bad_footprint : t

val biabduction_analysis_stops : t

val buffer_overrun_l1 : t

val buffer_overrun_l2 : t

val buffer_overrun_l3 : t

val buffer_overrun_l4 : t

val buffer_overrun_l5 : t

val buffer_overrun_s2 : t

val buffer_overrun_u5 : t

val cannot_star : t

val captured_strong_self : t

val checkers_allocates_memory : t
(** Warning name when a performance critical method directly or indirectly calls a method allocating
    memory *)

val checkers_annotation_reachability_error : t

val checkers_calls_expensive_method : t
(** Warning name when a performance critical method directly or indirectly calls a method annotatd
    as expensive *)

val checkers_expensive_overrides_unexpensive : t
(** Warning name for the subtyping rule: method not annotated as expensive cannot be overridden by a
    method annotated as expensive *)

val checkers_fragment_retain_view : t

val checkers_immutable_cast : t

val checkers_printf_args : t

val class_cast_exception : t

val complexity_increase : kind:CostKind.t -> is_on_ui_thread:bool -> t

val component_with_multiple_factory_methods : t

val condition_always_false : t

val condition_always_true : t

val config_checks_between_markers : t

val config_impact_analysis : t

val constant_address_dereference : t

val create_intent_from_uri : t

val cross_site_scripting : t

val dangling_pointer_dereference : t

val dangling_pointer_dereference_maybe : t

val dead_store : t

val deadlock : t

val divide_by_zero : t

val do_not_report : t
(** an issue type that should never be reported *)

val empty_vector_access : t

val eradicate_annotation_graph : t

val eradicate_condition_redundant : t

val eradicate_field_not_initialized : t

val eradicate_field_not_nullable : t

val eradicate_field_over_annotated : t

val eradicate_inconsistent_subclass_parameter_annotation : t

val eradicate_inconsistent_subclass_return_annotation : t

val eradicate_redundant_nested_class_annotation : t

val eradicate_bad_nested_class_annotation : t

val eradicate_nullable_dereference : t

val eradicate_parameter_not_nullable : t

val eradicate_return_not_nullable : t

val eradicate_return_over_annotated : t

val eradicate_unvetted_third_party_in_nullsafe : t

val eradicate_unchecked_usage_in_nullsafe : t

val eradicate_meta_class_can_be_nullsafe : t

val eradicate_meta_class_needs_improvement : t

val eradicate_meta_class_is_nullsafe : t

val exposed_insecure_intent_handling : t

val expensive_cost_call : kind:CostKind.t -> t

val failure_exe : t

val field_not_null_checked : t

val guardedby_violation : t

val guardedby_violation_nullsafe : t

val impure_function : t

val inefficient_keyset_iterator : t

val inferbo_alloc_is_big : t

val inferbo_alloc_is_negative : t

val inferbo_alloc_is_zero : t

val inferbo_alloc_may_be_big : t

val inferbo_alloc_may_be_negative : t

val infinite_cost_call : kind:CostKind.t -> t

val inherently_dangerous_function : t

val insecure_intent_handling : t

val integer_overflow_l1 : t

val integer_overflow_l2 : t

val integer_overflow_l5 : t

val integer_overflow_u5 : t

val interface_not_thread_safe : t

val internal_error : t

val invariant_call : t

val ipc_on_ui_thread : t

val javascript_injection : t

val lab_resource_leak : t

val dotnet_resource_leak : t

val leak_after_array_abstraction : t

val leak_in_footprint : t

val leak_unknown_origin : t

val lockless_violation : t

val lock_consistency_violation : t

val logging_private_data : t

val expensive_loop_invariant_call : t

val memory_leak : t

val missing_fld : t

val missing_required_prop : t

val mixed_self_weakself : t

val modifies_immutable : t

val multiple_weakself : t

val mutable_local_variable_in_component_file : t

val null_dereference : t

val nullptr_dereference : t

val optional_empty_access : t

val parameter_not_null_checked : t

val precondition_not_found : t

val precondition_not_met : t

val premature_nil_termination : t

val pulse_memory_leak : t

val pure_function : t

val quandary_taint_error : t

val resource_leak : t

val retain_cycle : t

val skip_function : t

val shell_injection : t

val shell_injection_risk : t

val sql_injection : t

val sql_injection_risk : t

val stack_variable_address_escape : t

val starvation : t

val static_initialization_order_fiasco : t

val strict_mode_violation : t

val strong_self_not_checked : t

val symexec_memory_error : t

val thread_safety_violation : t

val thread_safety_violation_nullsafe : t

val topl_biabd_error : t

val topl_pulse_error : t

val uninitialized_value : t

val uninitialized_value_pulse : t

val unreachable_code_after : t

val use_after_delete : t

val use_after_free : t

val use_after_lifetime : t

val untrusted_buffer_access : t

val untrusted_deserialization : t

val untrusted_deserialization_risk : t

val untrusted_file : t

val untrusted_file_risk : t

val untrusted_heap_allocation : t

val untrusted_intent_creation : t

val untrusted_url_risk : t

val untrusted_environment_change_risk : t

val untrusted_variable_length_array : t

val user_controlled_sql_risk : t

val vector_invalidation : t

val weak_self_in_noescape_block : t

val wrong_argument_number : t

val unreachable_cost_call : kind:CostKind.t -> t

val is_autoreleasepool_size_issue : t -> bool

module Map : PrettyPrintable.PPMap with type key = t
