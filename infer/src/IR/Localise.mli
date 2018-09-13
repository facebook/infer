(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Support for localisation *)

module Tags : sig
  type t
end

(** description field of error messages *)
type error_desc = {descriptions: string list; tags: Tags.t; dotty: string option}
[@@deriving compare]

val no_desc : error_desc
(** empty error description *)

val verbatim_desc : string -> error_desc
(** verbatim desc from a string, not to be used for user-visible descs *)

module BucketLevel : sig
  val b1 : string
  (** highest likelihood *)

  val b2 : string

  val b3 : string

  val b4 : string [@@warning "-32"]

  val b5 : string
  (** lowest likelihood *)
end

val error_desc_get_bucket : error_desc -> string option
(** get the bucket value of an error_desc, if any *)

val error_desc_set_bucket : error_desc -> string -> error_desc
(** set the bucket value of an error_desc *)

val error_desc_is_reportable_bucket : error_desc -> bool
(** check if the report is in a high confidence bucket *)

val error_desc_hash : error_desc -> int
(** hash function for error_desc *)

val error_desc_equal : error_desc -> error_desc -> bool
(** equality for error_desc *)

val pp_error_desc : Format.formatter -> error_desc -> unit
(** pretty print an error description *)

val error_desc_get_dotty : error_desc -> string option

(** Description functions for error messages *)

(** dereference strings used to explain a dereference action in an error message *)
type deref_str

val deref_str_null : Typ.Procname.t option -> deref_str
(** dereference strings for null dereference *)

val deref_str_nullable : Typ.Procname.t option -> string -> deref_str
(** dereference strings for null dereference due to Nullable annotation *)

val deref_str_weak_variable_in_block : Typ.Procname.t option -> string -> deref_str
(** dereference strings for null dereference due to weak captured variable in block *)

val deref_str_undef : Typ.Procname.t * Location.t -> deref_str
(** dereference strings for an undefined value coming from the given procedure *)

val deref_str_freed : PredSymb.res_action -> deref_str
(** dereference strings for a freed pointer dereference *)

val deref_str_dangling : PredSymb.dangling_kind option -> deref_str
(** dereference strings for a dangling pointer dereference *)

val deref_str_array_bound : IntLit.t option -> IntLit.t option -> deref_str
(** dereference strings for an array out of bound access *)

val deref_str_nil_argument_in_variadic_method : Typ.Procname.t -> int -> int -> deref_str
(** dereference strings for nonterminal nil arguments in c/objc variadic methods *)

val deref_str_pointer_size_mismatch : Typ.t -> Typ.t -> deref_str
(** dereference strings for a pointer size mismatch *)

(** type of access *)
type access =
  | Last_assigned of int * bool  (** line, null_case_flag *)
  | Last_accessed of int * bool  (** line, is_nullable flag *)
  | Initialized_automatically
  | Returned_from_call of int

val nullable_annotation_name : Typ.Procname.t -> string
(** Name of the nullable annotation *)

val dereference_string :
  Typ.Procname.t -> deref_str -> string -> access option -> Location.t -> error_desc

val parameter_field_not_null_checked_desc : error_desc -> Exp.t -> error_desc

val is_parameter_not_null_checked_desc : error_desc -> bool

val is_field_not_null_checked_desc : error_desc -> bool

val desc_allocation_mismatch :
     Typ.Procname.t * Typ.Procname.t * Location.t
  -> Typ.Procname.t * Typ.Procname.t * Location.t
  -> error_desc

val desc_class_cast_exception :
  Typ.Procname.t option -> string -> string -> string option -> Location.t -> error_desc

val desc_condition_always_true_false : IntLit.t -> string option -> Location.t -> error_desc

val desc_deallocate_stack_variable : string -> Typ.Procname.t -> Location.t -> error_desc

val desc_deallocate_static_memory : string -> Typ.Procname.t -> Location.t -> error_desc

val desc_divide_by_zero : string -> Location.t -> error_desc

val desc_empty_vector_access : Typ.Procname.t option -> string -> Location.t -> error_desc

val is_empty_vector_access_desc : error_desc -> bool

val desc_frontend_warning : string -> string option -> Location.t -> error_desc

val desc_leak :
     Exp.t option
  -> string option
  -> PredSymb.resource option
  -> PredSymb.res_action option
  -> Location.t
  -> string option
  -> error_desc

val desc_null_test_after_dereference : string -> int -> Location.t -> error_desc

val java_unchecked_exn_desc : Typ.Procname.t -> Typ.Name.t -> string -> error_desc

val desc_custom_error : Location.t -> error_desc
(** Create human-readable error description for assertion failures *)

(** kind of precondition not met *)
type pnm_kind = Pnm_bounds | Pnm_dangling

val desc_precondition_not_met : pnm_kind option -> Typ.Procname.t -> Location.t -> error_desc

val desc_retain_cycle : string -> Location.t -> string option -> error_desc

val desc_registered_observer_being_deallocated : Pvar.t -> Location.t -> error_desc

val desc_stack_variable_address_escape : Pvar.t -> string option -> Location.t -> error_desc

val desc_skip_function : Typ.Procname.t -> error_desc

val desc_inherently_dangerous_function : Typ.Procname.t -> error_desc

val desc_unary_minus_applied_to_unsigned_expression :
  string option -> string -> Location.t -> error_desc

val desc_unsafe_guarded_by_access : Typ.Fieldname.t -> string -> Location.t -> error_desc

val desc_uninitialized_dangling_pointer_deref : deref_str -> string -> Location.t -> error_desc

val access_desc : access option -> string list
