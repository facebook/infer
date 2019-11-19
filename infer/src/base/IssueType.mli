(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** type of string used for localisation *)
type t = private
  { unique_id: string
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

val register_from_string :
  ?enabled:bool -> ?hum:string -> ?doc_url:string -> ?linters_def_file:string -> string -> t
(** Create a new issue and register it in the list of all issues.
    NOTE: if the issue with the same string id is already registered,
          overrides `hum`, `doc_url`, and `linters_def_file`, but DOES NOT override
          `enabled`. This trick allows to deal with disabling/enabling dynamic AL issues
          from the config, when we don't know all params yet.
          Thus, the human-readable description can be updated when we encounter the
          definition of the issue type, eg in AL.
  *)

val set_enabled : t -> bool -> unit

val abduction_case_not_implemented : t

val analysis_stops : t

val array_of_pointsto : t

val array_out_of_bounds_l1 : t

val array_out_of_bounds_l2 : t

val array_out_of_bounds_l3 : t

val assert_failure : t

val bad_footprint : t

val biabd_use_after_free : t

val buffer_overrun_l1 : t

val buffer_overrun_l2 : t

val buffer_overrun_l3 : t

val buffer_overrun_l4 : t

val buffer_overrun_l5 : t

val buffer_overrun_r2 : t

val buffer_overrun_s2 : t

val buffer_overrun_u5 : t

val cannot_star : t

val checkers_allocates_memory : t
(** Warning name when a performance critical method directly or indirectly
    calls a method allocating memory *)

val checkers_annotation_reachability_error : t

val checkers_calls_expensive_method : t
(** Warning name when a performance critical method directly or indirectly
    calls a method annotatd as expensive *)

val checkers_expensive_overrides_unexpensive : t
(** Warning name for the subtyping rule: method not annotated as expensive cannot be overridden
    by a method annotated as expensive *)

val checkers_fragment_retain_view : t

val checkers_immutable_cast : t

val checkers_printf_args : t

val class_cast_exception : t

val class_load : t

val codequery : t

val comparing_floats_for_equality : t

val complexity_increase : kind:CostKind.t -> is_on_cold_start:bool -> is_on_ui_thread:bool -> t

val component_factory_function : t

val component_file_cyclomatic_complexity : t

val component_file_line_count : t

val component_initializer_with_side_effects : t

val component_with_multiple_factory_methods : t

val component_with_unconventional_superclass : t

val condition_always_false : t

val condition_always_true : t

val constant_address_dereference : t

val create_intent_from_uri : t

val cross_site_scripting : t

val dangling_pointer_dereference : t

val dead_store : t

val deadlock : t

val deallocate_stack_variable : t

val deallocate_static_memory : t

val deallocation_mismatch : t

val divide_by_zero : t

val do_not_report : t
(** an issue type that should never be reported *)

val empty_vector_access : t

val eradicate_condition_redundant : t

val eradicate_field_not_initialized : t

val eradicate_field_not_nullable : t

val eradicate_field_over_annotated : t

val eradicate_inconsistent_subclass_parameter_annotation : t

val eradicate_inconsistent_subclass_return_annotation : t

val eradicate_nullable_dereference : t

val eradicate_parameter_not_nullable : t

val eradicate_return_not_nullable : t

val eradicate_return_over_annotated : t

val eradicate_unvetted_third_party_in_strict : t

val eradicate_forbidden_non_strict_in_strict : t

val expensive_cost_call : kind:CostKind.t -> is_on_cold_start:bool -> is_on_ui_thread:bool -> t

val exposed_insecure_intent_handling : t

val failure_exe : t

val field_not_null_checked : t

val graphql_field_access : t

val guardedby_violation_racerd : t

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

val integer_overflow_r2 : t

val integer_overflow_u5 : t

val interface_not_thread_safe : t

val internal_error : t

val invariant_call : t

val javascript_injection : t

val leak_after_array_abstraction : t

val leak_in_footprint : t

val lockless_violation : t

val lock_consistency_violation : t

val logging_private_data : t

val expensive_loop_invariant_call : t

val memory_leak : t

val missing_fld : t

val missing_required_prop : t

val mixed_self_weakself : t

val mutable_local_variable_in_component_file : t

val null_dereference : t

val null_test_after_dereference : t

val nullptr_dereference : t

val nullsafe_field_not_nullable : t

val nullsafe_nullable_dereference : t

val parameter_not_null_checked : t

val pointer_size_mismatch : t

val precondition_not_found : t

val precondition_not_met : t

val premature_nil_termination : t

val pure_function : t

val quandary_taint_error : t

val registered_observer_being_deallocated : t

val resource_leak : t

val retain_cycle : t

val return_expression_required : t

val return_statement_missing : t

val return_value_ignored : t

val skip_function : t

val skip_pointer_dereference : t

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

val tainted_buffer_access : t

val tainted_memory_allocation : t

val thread_safety_violation : t

val topl_error : t

val unary_minus_applied_to_unsigned_expression : t

val uninitialized_value : t

val unknown_proc : t

val unreachable_code_after : t

val unsafe_guarded_by_access : t

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

val wrong_argument_number : t

val zero_cost_call : kind:CostKind.t -> t
