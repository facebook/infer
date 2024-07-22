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
type severity = Info | Advice | Warning | Error [@@deriving compare, equal, enumerate]

type category =
  | Concurrency
  | LogicError
  | MemoryError
  | NoCategory
  | NullPointerDereference
  | PerfRegression
  | ResourceLeak
  | RuntimeException
  | SensitiveDataFlow
  | UngatedCode
[@@deriving compare, equal, enumerate]

val string_of_severity : severity -> string

val string_of_category : category -> string

val category_documentation : category -> string

type t = private
  { unique_id: string
  ; checker: Checker.t
  ; category: category
  ; visibility: visibility
  ; user_documentation: string option
  ; mutable default_severity: severity
        (** used for documentation but can be overriden at report time *)
  ; mutable enabled: bool
  ; mutable hum: string }
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
  -> id:string
  -> ?user_documentation:string
  -> severity
  -> Checker.t
  -> t
(** Create a new issue and register it in the list of all issues. NOTE: if the issue with the same
    string id is already registered, overrides `hum` but DOES NOT override `enabled`. This trick
    allows to deal with disabling/enabling dynamic issues from the config, when we don't know all
    params yet. Thus, the human-readable description can be updated when we encounter the definition
    of the issue type. *)

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

val bad_arg : latent:bool -> t

val bad_generator : latent:bool -> t

val bad_key : latent:bool -> t

val bad_map : latent:bool -> t

val bad_record : latent:bool -> t

val bad_return : latent:bool -> t

val biabduction_analysis_stops : t

val biabduction_retain_cycle : t

val block_parameter_not_null_checked : t

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

val class_cast_exception : t

val complexity_increase : kind:CostKind.t -> is_on_ui_thread:bool -> t

val condition_always_false : t

val condition_always_true : t

val config_impact_analysis : t

val config_impact_analysis_strict : t

val pulse_config_usage : t

val pulse_const_refable : t

val constant_address_dereference : latent:bool -> t

val cxx_ref_captured_in_block : t

val dangling_pointer_dereference : t

val dangling_pointer_dereference_maybe : t

val data_flow_to_sink : t

val dead_store : t

val deadlock : t

val divide_by_zero : t

val do_not_report : t
(** an issue type that should never be reported *)

val empty_vector_access : t

val expensive_cost_call : kind:CostKind.t -> t

val failure_exe : t

val guardedby_violation : t

val impure_function : t

val inefficient_keyset_iterator : t

val inferbo_alloc_is_big : t

val inferbo_alloc_is_negative : t

val inferbo_alloc_is_zero : t

val inferbo_alloc_may_be_big : t

val inferbo_alloc_may_be_negative : t

val infinite_cost_call : kind:CostKind.t -> t

val inherently_dangerous_function : t

val integer_overflow_l1 : t

val integer_overflow_l2 : t

val integer_overflow_l5 : t

val integer_overflow_u5 : t

val interface_not_thread_safe : t

val internal_error : t

val invalid_sil : t

val invariant_call : t

val ipc_on_ui_thread : t

val lab_resource_leak : t

val leak_after_array_abstraction : t

val leak_in_footprint : t

val leak_unknown_origin : t

val lockless_violation : t

val lock_consistency_violation : t

val expensive_loop_invariant_call : t

val memory_leak : t

val missing_fld : t

val missing_required_prop : t

val mixed_self_weakself : t

val modifies_immutable : t

val multiple_weakself : t

val mutual_recursion_cycle : t

val nil_block_call : latent:bool -> t

val nil_insertion_into_collection : latent:bool -> t

val nil_messaging_to_non_pod : latent:bool -> t

val no_match_of_rhs : latent:bool -> t

val no_matching_case_clause : latent:bool -> t

val no_matching_else_clause : latent:bool -> t

val no_matching_function_clause : latent:bool -> t

val no_true_branch_in_if : latent:bool -> t

val no_matching_branch_in_try : latent:bool -> t

val null_argument : latent:bool -> t

val null_dereference : t

val nullptr_dereference : latent:bool -> t

val nullptr_dereference_in_nullsafe_class : latent:bool -> t

val optional_empty_access : latent:bool -> t

val precondition_not_found : t

val precondition_not_met : t

val premature_nil_termination : t

val pulse_cannot_instantiate_abstract_class : t

val pulse_dict_missing_key : t

val pulse_dynamic_type_mismatch : t

val pulse_transitive_access : t

val pulse_memory_leak_c : t

val pulse_memory_leak_cpp : t

val pulse_resource_leak : t

val pulse_unawaited_awaitable : t

val pulse_unfinished_builder : t

val pulse_uninitialized_const : t

val pure_function : t

val readonly_shared_ptr_param : t

val regex_op_on_ui_thread : t

val resource_leak : t

val retain_cycle : t

val retain_cycle_no_weak_info : t

val scope_leakage : t

val self_in_block_passed_to_init : t

val sensitive_data_flow : t

val skip_function : t

val stack_variable_address_escape : t

val starvation : t

val static_initialization_order_fiasco : t

val strict_mode_violation : t

val strong_self_not_checked : t

val symexec_memory_error : t

val taint_error : t

val thread_safety_violation : t

val topl_error : latent:bool -> t

val uninitialized_value_pulse : t

val unnecessary_copy_pulse : t

val unnecessary_copy_assignment_pulse : t

val unnecessary_copy_assignment_const_pulse : t

val unnecessary_copy_assignment_movable_pulse : t

val unnecessary_copy_intermediate_pulse : t

val unnecessary_copy_intermediate_const_pulse : t

val unnecessary_copy_movable_pulse : t

val unnecessary_copy_optional_pulse : t

val unnecessary_copy_optional_const_pulse : t

val unnecessary_copy_return_pulse : t

val unreachable_code_after : t

val use_after_delete : latent:bool -> t

val use_after_free : latent:bool -> t

val use_after_lifetime : latent:bool -> t

val vector_invalidation : latent:bool -> t

val pulse_reference_stability : t

val weak_self_in_noescape_block : t

val wrong_argument_number : t

val unreachable_cost_call : kind:CostKind.t -> t

val lineage_flow : t

module Map : PrettyPrintable.PPMap with type key = t
