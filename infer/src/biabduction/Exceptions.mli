(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {1 Biabduction uses exceptions to store issues in summaries} *)

exception Abduction_case_not_implemented of Logging.ocaml_pos

exception Analysis_stops of Localise.error_desc * Logging.ocaml_pos option

exception Array_of_pointsto of Logging.ocaml_pos

exception Array_out_of_bounds_l1 of Localise.error_desc * Logging.ocaml_pos

exception Array_out_of_bounds_l2 of Localise.error_desc * Logging.ocaml_pos

exception Array_out_of_bounds_l3 of Localise.error_desc * Logging.ocaml_pos

exception Bad_footprint of Logging.ocaml_pos

exception Cannot_star of Logging.ocaml_pos

exception Class_cast_exception of Localise.error_desc * Logging.ocaml_pos

exception Custom_error of string * IssueType.severity * Localise.error_desc

exception
  Dangling_pointer_dereference of
    bool (* is it user visible? *) * Localise.error_desc * Logging.ocaml_pos

exception Divide_by_zero of Localise.error_desc * Logging.ocaml_pos

exception Empty_vector_access of Localise.error_desc * Logging.ocaml_pos

exception Inherently_dangerous_function of Localise.error_desc

exception Internal_error of Localise.error_desc

exception
  Leak of
    bool
    * (bool (* is it user visible? *) * Localise.error_desc)
    * bool
    * PredSymb.resource
    * Logging.ocaml_pos

exception Missing_fld of Fieldname.t * Logging.ocaml_pos

exception Premature_nil_termination of Localise.error_desc * Logging.ocaml_pos

exception Null_dereference of Localise.error_desc * Logging.ocaml_pos

exception Precondition_not_found of Localise.error_desc * Logging.ocaml_pos

exception Precondition_not_met of Localise.error_desc * Logging.ocaml_pos

exception Retain_cycle of Localise.error_desc * Logging.ocaml_pos

exception Registered_observer_being_deallocated of Localise.error_desc * Logging.ocaml_pos

exception Skip_function of Localise.error_desc

exception Symexec_memory_error of Logging.ocaml_pos

exception Wrong_argument_number of Logging.ocaml_pos

val handle_exception : exn -> bool
(** Return true if the exception is not serious and should be handled in timeout mode *)

val print_exception_html : string -> exn -> unit
(** print a description of the exception to the html output *)

val recognize_exception : exn -> IssueToReport.t
