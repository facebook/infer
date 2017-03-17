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

(** Functions for logging and printing exceptions *)

(** visibility of the exception *)
type visibility =
  | Exn_user (** always add to error log *)
  | Exn_developer (** only add to error log in developer mode *)
  | Exn_system (** never add to error log *)
[@@deriving compare]

val equal_visibility : visibility -> visibility -> bool

val string_of_visibility : visibility -> string

(** severity of bugs *)
type severity =
  | High (** high severity bug *)
  | Medium (** medium severity bug *)
  | Low (** low severity bug *)

(** kind of error/warning *)
type err_kind = Kwarning | Kerror | Kinfo | Kadvice [@@deriving compare]

val equal_err_kind : err_kind -> err_kind -> bool

(** class of error *)
type err_class = Checker | Prover | Nocat | Linters

val equal_err_class : err_class -> err_class -> bool

exception Abduction_case_not_implemented of Logging.ml_loc
exception Analysis_stops of Localise.error_desc * Logging.ml_loc option
exception Array_of_pointsto of Logging.ml_loc
exception Array_out_of_bounds_l1 of Localise.error_desc * Logging.ml_loc
exception Array_out_of_bounds_l2 of Localise.error_desc * Logging.ml_loc
exception Array_out_of_bounds_l3 of Localise.error_desc * Logging.ml_loc
exception Bad_footprint of Logging.ml_loc
exception Cannot_star of Logging.ml_loc
exception Class_cast_exception of Localise.error_desc * Logging.ml_loc
exception Codequery of Localise.error_desc
exception Comparing_floats_for_equality of Localise.error_desc * Logging.ml_loc
exception Condition_always_true_false of Localise.error_desc * bool * Logging.ml_loc
exception Condition_is_assignment of Localise.error_desc * Logging.ml_loc
exception Context_leak of Localise.error_desc * Logging.ml_loc
exception Custom_error of string * Localise.error_desc
exception Dangling_pointer_dereference of
    PredSymb.dangling_kind option * Localise.error_desc * Logging.ml_loc
exception Deallocate_stack_variable of Localise.error_desc
exception Deallocate_static_memory of Localise.error_desc
exception Deallocation_mismatch of Localise.error_desc * Logging.ml_loc
exception Divide_by_zero of Localise.error_desc * Logging.ml_loc
exception Field_not_null_checked of Localise.error_desc * Logging.ml_loc
exception Empty_vector_access of Localise.error_desc * Logging.ml_loc
exception Eradicate of string * Localise.error_desc
exception Checkers of string * Localise.error_desc
exception Frontend_warning of string * Localise.error_desc * Logging.ml_loc
exception Inherently_dangerous_function of Localise.error_desc
exception Internal_error of Localise.error_desc
exception Java_runtime_exception of Typ.Name.t * string * Localise.error_desc
exception Leak of
    bool * Sil.hpred * (visibility * Localise.error_desc)
    * bool * PredSymb.resource * Logging.ml_loc
exception Missing_fld of Ident.fieldname * Logging.ml_loc
exception Premature_nil_termination of Localise.error_desc * Logging.ml_loc
exception Null_dereference of Localise.error_desc * Logging.ml_loc
exception Null_test_after_dereference of Localise.error_desc * Logging.ml_loc
exception Parameter_not_null_checked of Localise.error_desc * Logging.ml_loc
exception Pointer_size_mismatch of Localise.error_desc * Logging.ml_loc
exception Precondition_not_found of Localise.error_desc * Logging.ml_loc
exception Precondition_not_met of Localise.error_desc * Logging.ml_loc
exception Retain_cycle of Sil.hpred * Localise.error_desc * Logging.ml_loc
exception Registered_observer_being_deallocated of Localise.error_desc * Logging.ml_loc
exception Return_expression_required of Localise.error_desc * Logging.ml_loc
exception Return_statement_missing of Localise.error_desc * Logging.ml_loc
exception Return_value_ignored of Localise.error_desc * Logging.ml_loc
exception Skip_function of Localise.error_desc
exception Skip_pointer_dereference of Localise.error_desc * Logging.ml_loc
exception Stack_variable_address_escape of Localise.error_desc * Logging.ml_loc
exception Symexec_memory_error of Logging.ml_loc
exception Tainted_value_reaching_sensitive_function of Localise.error_desc * Logging.ml_loc
exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * Logging.ml_loc
exception Uninitialized_value of Localise.error_desc * Logging.ml_loc
exception Unknown_proc
exception Unsafe_guarded_by_access of Localise.error_desc * Logging.ml_loc
exception Use_after_free of Localise.error_desc * Logging.ml_loc
exception Wrong_argument_number of Logging.ml_loc

(** string describing an error class *)
val err_class_string : err_class -> string

(** string describing an error kind *)
val err_kind_string : err_kind -> string

(** Return true if the exception is not serious and should be handled in timeout mode *)
val handle_exception : exn -> bool

(** print a description of the exception to the html output *)
val print_exception_html : string -> exn -> unit

(** pretty print an error given its (id,key), location, kind, name, description,
    and optional ml location *)
val pp_err : int * int -> Location.t -> err_kind -> Localise.t -> Localise.error_desc ->
  Logging.ml_loc option -> Format.formatter -> unit -> unit

(** Turn an exception into an error name, error description,
    location in ml source, and category *)
val recognize_exception : exn ->
  (Localise.t * Localise.error_desc * (Logging.ml_loc option) * visibility *
   severity * err_kind option * err_class)
