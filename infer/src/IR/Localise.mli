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

(** Support for localisation *)

(** type of string used for localisation *)
type t [@@deriving compare]

val equal : t -> t -> bool

(** pretty print a localised string *)
val pp : Format.formatter -> t -> unit

(** create a localised string from an ordinary string *)
val from_string : string -> t

(** convert a localised string to an ordinary string *)
val to_string : t -> string

val analysis_stops : t
val array_out_of_bounds_l1 : t
val array_out_of_bounds_l2 : t
val array_out_of_bounds_l3 : t
val buffer_overrun : t
val class_cast_exception : t
val comparing_floats_for_equality : t
val condition_is_assignment : t
val condition_always_false : t
val condition_always_true : t
val context_leak : t
val dangling_pointer_dereference : t
val deallocate_stack_variable : t
val deallocate_static_memory : t
val deallocation_mismatch : t
val divide_by_zero : t
val empty_vector_access : t
val field_not_null_checked : t
val inherently_dangerous_function : t
val memory_leak : t
val null_dereference : t
val parameter_not_null_checked : t
val null_test_after_dereference : t
val pointer_size_mismatch : t
val precondition_not_found : t
val precondition_not_met : t
val premature_nil_termination : t
val quandary_taint_error : t
val registered_observer_being_deallocated : t
val retain_cycle : t
val resource_leak : t
val return_value_ignored : t
val return_expression_required : t
val return_statement_missing : t
val stack_variable_address_escape : t
val static_initialization_order_fiasco : t
val unary_minus_applied_to_unsigned_expression : t
val uninitialized_value : t
val unsafe_guarded_by_access : t
val use_after_free : t
val skip_function : t
val skip_pointer_dereference : t
val tainted_value_reaching_sensitive_function : t
val thread_safety_violation : t

(** description field of error messages *)
type error_desc = {
  descriptions : string list;
  advice : string option;
  tags : (string * string) list;
  dotty : string option;
} [@@deriving compare]

(** empty error description *)
val no_desc: error_desc

(** verbatim desc from a string, not to be used for user-visible descs *)
val verbatim_desc : string -> error_desc

(** verbatim desc with custom tags *)
val custom_desc : string -> (string * string) list -> error_desc

(** verbatim desc with advice and custom tags *)
val custom_desc_with_advice : string -> string -> (string * string) list -> error_desc

module BucketLevel : sig
  val b1 : string (* highest likelyhood *)
  val b2 : string
  val b3 : string
  val b4 : string
  val b5 : string (* lowest likelyhood *)
end

(** returns the value of a tag or the empty string *)
val error_desc_extract_tag_value : error_desc -> string -> string

(** returns all the tuples (tag, value) of an error_desc *)
val error_desc_to_tag_value_pairs : error_desc -> (string * string) list

(** returns the content of the value tag of the error_desc *)
val error_desc_get_tag_value : error_desc -> string

(** returns the content of the call_procedure tag of the error_desc *)
val error_desc_get_tag_call_procedure : error_desc -> string

(** get the bucket value of an error_desc, if any *)
val error_desc_get_bucket : error_desc -> string option

(** set the bucket value of an error_desc.
    The boolean indicates where the bucket should be shown in the message *)
val error_desc_set_bucket : error_desc -> string -> bool -> error_desc

(** hash function for error_desc *)
val error_desc_hash : error_desc -> int

(** equality for error_desc *)
val error_desc_equal : error_desc -> error_desc -> bool

(** pretty print an error description *)
val pp_error_desc : Format.formatter -> error_desc -> unit

(** pretty print an error advice *)
val pp_error_advice : Format.formatter -> error_desc -> unit

(** get tags of error description *)
val error_desc_get_tags : error_desc -> (string * string) list

val error_desc_get_dotty : error_desc -> string option

(** Description functions for error messages *)

(** dereference strings used to explain a dereference action in an error message *)
type deref_str

(** dereference strings for null dereference *)
val deref_str_null : Procname.t option -> deref_str

(** dereference strings for null dereference due to Nullable annotation *)
val deref_str_nullable : Procname.t option -> string -> deref_str

(** dereference strings for null dereference due to weak captured variable in block *)
val deref_str_weak_variable_in_block : Procname.t option -> string -> deref_str

(** dereference strings for an undefined value coming from the given procedure *)
val deref_str_undef : Procname.t * Location.t -> deref_str

(** dereference strings for a freed pointer dereference *)
val deref_str_freed : PredSymb.res_action -> deref_str

(** dereference strings for a dangling pointer dereference *)
val deref_str_dangling : PredSymb.dangling_kind option -> deref_str

(** dereference strings for an array out of bound access *)
val deref_str_array_bound : IntLit.t option -> IntLit.t option -> deref_str

(** dereference strings for an uninitialized access whose lhs has the given attribute *)
val deref_str_uninitialized : Sil.atom option -> deref_str

(** dereference strings for nonterminal nil arguments in c/objc variadic methods *)
val deref_str_nil_argument_in_variadic_method : Procname.t -> int -> int -> deref_str

(** dereference strings for a pointer size mismatch *)
val deref_str_pointer_size_mismatch : Typ.t -> Typ.t -> deref_str

(** type of access *)
type access =
  | Last_assigned of int * bool (* line, null_case_flag *)
  | Last_accessed of int * bool (* line, is_nullable flag *)
  | Initialized_automatically
  | Returned_from_call of int

val dereference_string : deref_str -> string -> access option -> Location.t -> error_desc

val parameter_field_not_null_checked_desc : error_desc -> Exp.t -> error_desc

val is_parameter_not_null_checked_desc : error_desc -> bool

val is_field_not_null_checked_desc : error_desc -> bool

val is_parameter_field_not_null_checked_desc : error_desc -> bool

val desc_allocation_mismatch :
  Procname.t * Procname.t * Location.t -> Procname.t * Procname.t * Location.t -> error_desc

val desc_class_cast_exception :
  Procname.t option -> string -> string -> string option -> Location.t -> error_desc

val desc_comparing_floats_for_equality : Location.t -> error_desc

val desc_condition_is_assignment : Location.t -> error_desc

val desc_condition_always_true_false : IntLit.t -> string option -> Location.t -> error_desc

val desc_deallocate_stack_variable : string -> Procname.t -> Location.t -> error_desc

val desc_deallocate_static_memory : string -> Procname.t -> Location.t -> error_desc

val desc_divide_by_zero : string -> Location.t -> error_desc

val desc_empty_vector_access : Procname.t option -> string -> Location.t -> error_desc

val is_empty_vector_access_desc : error_desc -> bool

val desc_frontend_warning : string -> string option -> Location.t -> error_desc

val desc_leak :
  Exp.t option -> string option -> PredSymb.resource option -> PredSymb.res_action option ->
  Location.t -> string option -> error_desc

val desc_buffer_overrun : string -> string -> error_desc

val desc_null_test_after_dereference : string -> int -> Location.t -> error_desc

val java_unchecked_exn_desc : Procname.t -> Typename.t -> string -> error_desc

val desc_context_leak :
  Procname.t -> Typ.t -> Ident.fieldname ->
  (Ident.fieldname option * Typ.t) list -> error_desc

val desc_fragment_retains_view :
  Typ.t -> Ident.fieldname -> Typ.t -> Procname.t -> error_desc

(* Create human-readable error description for assertion failures *)
val desc_custom_error : Location.t -> error_desc

(** kind of precondition not met *)
type pnm_kind =
  | Pnm_bounds
  | Pnm_dangling

val desc_precondition_not_met : pnm_kind option -> Procname.t -> Location.t -> error_desc

val desc_return_expression_required : string -> Location.t -> error_desc

val desc_retain_cycle :
  ((Sil.strexp * Typ.t) * Ident.fieldname * Sil.strexp) list ->
  Location.t -> string option -> error_desc

val registered_observer_being_deallocated_str : string -> string

val desc_registered_observer_being_deallocated : Pvar.t -> Location.t -> error_desc

val desc_return_statement_missing : Location.t -> error_desc

val desc_return_value_ignored : Procname.t -> Location.t -> error_desc

val desc_stack_variable_address_escape : string -> string option -> Location.t -> error_desc

val desc_skip_function : Procname.t -> error_desc

val desc_inherently_dangerous_function : Procname.t -> error_desc

val desc_unary_minus_applied_to_unsigned_expression :
  string option -> string -> Location.t -> error_desc

val desc_unsafe_guarded_by_access :
  Procname.t -> Ident.fieldname -> string -> Location.t -> error_desc

val desc_tainted_value_reaching_sensitive_function :
  PredSymb.taint_kind -> string -> Procname.t -> Procname.t -> Location.t -> error_desc

val desc_uninitialized_dangling_pointer_deref : deref_str -> string -> Location.t -> error_desc
