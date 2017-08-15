(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** type of string used for localisation *)
type t = private
  {unique_id: string; mutable enabled: bool; mutable hum: string}
  [@@deriving compare]

val equal : t -> t -> bool

val all_issues : unit -> t list
(** all the issues declared so far *)

val pp : Format.formatter -> t -> unit
(** pretty print a localised string *)

val from_string : ?enabled:bool -> ?hum:string -> string -> t
(** create from an ordinary string *)

val set_enabled : t -> bool -> unit

val abduction_case_not_implemented : t

val analysis_stops : t

val array_of_pointsto : t

val array_out_of_bounds_l1 : t

val array_out_of_bounds_l2 : t

val array_out_of_bounds_l3 : t

val assert_failure : t

val bad_footprint : t

val buffer_overrun : t

val cannot_star : t

val checkers_access_global : t

val checkers_immutable_cast : t

val checkers_print_c_call : t

val checkers_print_objc_method_calls : t

val checkers_printf_args : t

val checkers_repeated_calls : t

val checkers_trace_calls_sequence : t

val class_cast_exception : t

val cluster_callback : t

val codequery : t

val comparing_floats_for_equality : t

val condition_always_false : t

val condition_always_true : t

val context_leak : t

val dangling_pointer_dereference : t

val dead_store : t

val deallocate_stack_variable : t

val deallocate_static_memory : t

val deallocation_mismatch : t

val divide_by_zero : t

val double_lock : t

val empty_vector_access : t

val eradicate_condition_redundant : t

val eradicate_condition_redundant_nonnull : t

val eradicate_field_not_initialized : t

val eradicate_field_not_mutable : t

val eradicate_field_not_nullable : t

val eradicate_field_over_annotated : t

val eradicate_field_value_absent : t

val eradicate_inconsistent_subclass_parameter_annotation : t

val eradicate_inconsistent_subclass_return_annotation : t

val eradicate_null_field_access : t

val eradicate_null_method_call : t

val eradicate_parameter_not_nullable : t

val eradicate_parameter_value_absent : t

val eradicate_return_not_nullable : t

val eradicate_return_over_annotated : t

val eradicate_return_value_not_present : t

val eradicate_value_not_present : t

val failure_exe : t

val field_should_be_nullable : t

val field_not_null_checked : t

val inherently_dangerous_function : t

val internal_error : t

val leak_after_array_abstraction : t

val leak_in_footprint : t

val memory_leak : t

val missing_fld : t

val null_dereference : t

val null_test_after_dereference : t

val parameter_not_null_checked : t

val pointer_size_mismatch : t

val precondition_not_found : t

val precondition_not_met : t

val premature_nil_termination : t

val proc_callback : t

val quandary_taint_error : t

val registered_observer_being_deallocated : t

val resource_leak : t

val retain_cycle : t

val return_expression_required : t

val return_statement_missing : t

val return_value_ignored : t

val skip_function : t

val skip_pointer_dereference : t

val stack_variable_address_escape : t

val static_initialization_order_fiasco : t

val symexec_memory_error : t

val thread_safety_violation : t

val unary_minus_applied_to_unsigned_expression : t

val uninitialized_value : t

val unknown_proc : t

val unreachable_code_after : t

val unsafe_guarded_by_access : t

val use_after_free : t

val wrong_argument_number : t
