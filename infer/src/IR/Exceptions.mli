(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for logging and printing exceptions *)

(** visibility of the exception *)
type visibility =
  | Exn_user  (** always add to error log *)
  | Exn_developer  (** only add to error log in developer mode *)
  | Exn_system  (** never add to error log *)
[@@deriving compare]

val equal_visibility : visibility -> visibility -> bool

val string_of_visibility : visibility -> string

(** severity of the report *)
type severity = Like | Info | Advice | Warning | Error [@@deriving compare]

val equal_severity : severity -> severity -> bool

(** class of error *)
type err_class = Checker | Prover | Nocat | Linters

val equal_err_class : err_class -> err_class -> bool

exception Abduction_case_not_implemented of Logging.ocaml_pos

exception Analysis_stops of Localise.error_desc * Logging.ocaml_pos option

exception Array_of_pointsto of Logging.ocaml_pos

exception Array_out_of_bounds_l1 of Localise.error_desc * Logging.ocaml_pos

exception Array_out_of_bounds_l2 of Localise.error_desc * Logging.ocaml_pos

exception Array_out_of_bounds_l3 of Localise.error_desc * Logging.ocaml_pos

exception Bad_footprint of Logging.ocaml_pos

exception Cannot_star of Logging.ocaml_pos

exception Class_cast_exception of Localise.error_desc * Logging.ocaml_pos

exception Codequery of Localise.error_desc

exception Comparing_floats_for_equality of Localise.error_desc * Logging.ocaml_pos

exception Condition_always_true_false of Localise.error_desc * bool * Logging.ocaml_pos

exception Custom_error of string * Localise.error_desc

exception Dummy_exception of Localise.error_desc

exception
  Dangling_pointer_dereference of
    PredSymb.dangling_kind option * Localise.error_desc * Logging.ocaml_pos

exception Deallocate_stack_variable of Localise.error_desc

exception Deallocate_static_memory of Localise.error_desc

exception Deallocation_mismatch of Localise.error_desc * Logging.ocaml_pos

exception Divide_by_zero of Localise.error_desc * Logging.ocaml_pos

exception Field_not_null_checked of Localise.error_desc * Logging.ocaml_pos

exception Empty_vector_access of Localise.error_desc * Logging.ocaml_pos

exception Eradicate of IssueType.t * Localise.error_desc

exception Checkers of IssueType.t * Localise.error_desc

exception Frontend_warning of IssueType.t * Localise.error_desc * Logging.ocaml_pos

exception Inherently_dangerous_function of Localise.error_desc

exception Internal_error of Localise.error_desc

exception Java_runtime_exception of Typ.Name.t * string * Localise.error_desc

exception
  Leak of
    bool
    * Sil.hpred
    * (visibility * Localise.error_desc)
    * bool
    * PredSymb.resource
    * Logging.ocaml_pos

exception Missing_fld of Typ.Fieldname.t * Logging.ocaml_pos

exception Premature_nil_termination of Localise.error_desc * Logging.ocaml_pos

exception Null_dereference of Localise.error_desc * Logging.ocaml_pos

exception Null_test_after_dereference of Localise.error_desc * Logging.ocaml_pos

exception Parameter_not_null_checked of Localise.error_desc * Logging.ocaml_pos

exception Pointer_size_mismatch of Localise.error_desc * Logging.ocaml_pos

exception Precondition_not_found of Localise.error_desc * Logging.ocaml_pos

exception Precondition_not_met of Localise.error_desc * Logging.ocaml_pos

exception Retain_cycle of Localise.error_desc * Logging.ocaml_pos

exception Registered_observer_being_deallocated of Localise.error_desc * Logging.ocaml_pos

exception Return_expression_required of Localise.error_desc * Logging.ocaml_pos

exception Return_statement_missing of Localise.error_desc * Logging.ocaml_pos

exception Return_value_ignored of Localise.error_desc * Logging.ocaml_pos

exception Skip_function of Localise.error_desc

exception Skip_pointer_dereference of Localise.error_desc * Logging.ocaml_pos

exception Stack_variable_address_escape of Localise.error_desc * Logging.ocaml_pos

exception Symexec_memory_error of Logging.ocaml_pos

exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * Logging.ocaml_pos

exception Unknown_proc

exception Unsafe_guarded_by_access of Localise.error_desc * Logging.ocaml_pos

exception Use_after_free of Localise.error_desc * Logging.ocaml_pos

exception Wrong_argument_number of Logging.ocaml_pos

val severity_string : severity -> string
(** string describing an error kind *)

val handle_exception : exn -> bool
(** Return true if the exception is not serious and should be handled in timeout mode *)

val print_exception_html : string -> exn -> unit
(** print a description of the exception to the html output *)

val pp_err :
     Location.t
  -> severity
  -> IssueType.t
  -> Localise.error_desc
  -> Logging.ocaml_pos option
  -> Format.formatter
  -> unit
  -> unit
(** pretty print an error *)

type t =
  { name: IssueType.t
  ; description: Localise.error_desc
  ; ocaml_pos: Logging.ocaml_pos option  (** location in the infer source code *)
  ; visibility: visibility
  ; severity: severity option
  ; category: err_class }

val recognize_exception : exn -> t
