(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging
module F = Format
open Utils

type exception_visibility = (** visibility of the exception *)
  | Exn_user (** always add to error log *)
  | Exn_developer (** only add to error log in developer mode *)
  | Exn_system (** never add to error log *)

type exception_severity = (** severity of bugs *)
  | High (* high severity bug *)
  | Medium (* medium severity bug *)
  | Low (* low severity bug *)

(** class of error *)
type err_class = Checker | Prover | Nocat

(** kind of error/warning *)
type err_kind =
    Kwarning | Kerror | Kinfo

exception Abduction_case_not_implemented of ml_location
exception Analysis_stops of Localise.error_desc * ml_location option
exception Array_out_of_bounds_l1 of Localise.error_desc * ml_location
exception Array_out_of_bounds_l2 of Localise.error_desc * ml_location
exception Array_out_of_bounds_l3 of Localise.error_desc * ml_location
exception Array_of_pointsto of ml_location
exception Bad_footprint of ml_location
exception Bad_pointer_comparison of Localise.error_desc * ml_location
exception Class_cast_exception of Localise.error_desc * ml_location
exception Codequery of Localise.error_desc
exception Comparing_floats_for_equality of Localise.error_desc * ml_location
exception Condition_is_assignment of Localise.error_desc * ml_location
exception Condition_always_true_false of Localise.error_desc * bool * ml_location
exception Context_leak of Localise.error_desc * ml_location
exception Custom_error of string * Localise.error_desc
exception Dangling_pointer_dereference of Sil.dangling_kind option * Localise.error_desc * ml_location
exception Deallocate_stack_variable of Localise.error_desc
exception Deallocate_static_memory of Localise.error_desc
exception Deallocation_mismatch of Localise.error_desc * ml_location
exception Divide_by_zero of Localise.error_desc * ml_location
exception Eradicate of string * Localise.error_desc
exception Field_not_null_checked of Localise.error_desc * ml_location
exception Frontend_warning of string * Localise.error_desc * ml_location
exception Checkers of string * Localise.error_desc
exception Inherently_dangerous_function of Localise.error_desc
exception Internal_error of Localise.error_desc
exception Java_runtime_exception of Typename.t * string * Localise.error_desc
exception Leak of bool * Prop.normal Prop.t * Sil.hpred * (exception_visibility * Localise.error_desc) * bool * Sil.resource * ml_location
exception Missing_fld of Ident.fieldname * ml_location
exception Premature_nil_termination of Localise.error_desc * ml_location
exception Null_dereference of Localise.error_desc * ml_location
exception Null_test_after_dereference of Localise.error_desc * ml_location
exception Parameter_not_null_checked of Localise.error_desc * ml_location
exception Pointer_size_mismatch of Localise.error_desc * ml_location
exception Precondition_not_found of Localise.error_desc * ml_location
exception Precondition_not_met of Localise.error_desc * ml_location
exception Retain_cycle of Prop.normal Prop.t * Sil.hpred * Localise.error_desc * ml_location
exception Return_expression_required of Localise.error_desc * ml_location
exception Return_statement_missing of Localise.error_desc * ml_location
exception Return_value_ignored of Localise.error_desc * ml_location
exception Skip_function of Localise.error_desc
exception Skip_pointer_dereference of Localise.error_desc * ml_location
exception Stack_variable_address_escape of Localise.error_desc * ml_location
exception Symexec_memory_error of ml_location
exception Tainted_value_reaching_sensitive_function of Localise.error_desc * ml_location
exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * ml_location
exception Uninitialized_value of Localise.error_desc * ml_location
exception Unknown_proc
exception Use_after_free of Localise.error_desc * ml_location
exception Wrong_argument_number of ml_location

(** Turn an exception into a descriptive string, error description, location in ml source, and category *)
let recognize_exception exn =
  let filter_out_bucket desc =
    !Config.filter_buckets &&
    match Localise.error_desc_get_bucket desc with
    | None -> false
    | Some bucket -> bucket <> Localise.BucketLevel.b1 in
  let err_name, desc, mloco, visibility, severity, force_kind, eclass = match exn with (* all the names of Exn_user errors must be defined in Localise *)
    | Abduction_case_not_implemented mloc ->
        (Localise.from_string "Abduction_case_not_implemented", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Context_leak (desc, _) ->
        (Localise.context_leak, desc, None, Exn_user, High, None, Nocat)
    | Analysis_stops (desc, mloco) ->
        let visibility = if !Config.analysis_stops then Exn_user else Exn_developer in
        (Localise.analysis_stops, desc, mloco, visibility, Medium, None, Nocat)
    | Array_of_pointsto mloc ->
        (Localise.from_string "Array_of_pointsto", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Array_out_of_bounds_l1 (desc, mloc) ->
        (Localise.array_out_of_bounds_l1, desc, Some mloc, Exn_user, High, Some Kerror, Checker)
    | Array_out_of_bounds_l2 (desc, mloc) ->
        (Localise.array_out_of_bounds_l2, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Array_out_of_bounds_l3 (desc, mloc) ->
        (Localise.array_out_of_bounds_l3, desc, Some mloc, Exn_developer, Medium, None, Nocat)
    | Assert_failure mloc ->
        (Localise.from_string "Assert_failure", Localise.no_desc, Some mloc, Exn_developer, High, None, Nocat)
    | Bad_pointer_comparison (desc, mloc) ->
        (Localise.bad_pointer_comparison, desc, Some mloc, Exn_user, High, Some Kerror, Prover)
    | Bad_footprint mloc ->
        (Localise.from_string "Bad_footprint", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Prop.Cannot_star mloc ->
        (Localise.from_string "Cannot_star", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Class_cast_exception (desc, mloc) ->
        (Localise.class_cast_exception, desc, Some mloc, Exn_user, High, None, Prover)
    | Codequery desc ->
        (Localise.from_string "Codequery", desc, None, Exn_user, High, None, Prover)
    | Comparing_floats_for_equality(desc, mloc) ->
        (Localise.comparing_floats_for_equality, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Condition_always_true_false (desc, b, mloc) ->
        let name = if b then Localise.condition_always_true else Localise.condition_always_false in
        (name, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Custom_error (error_msg, desc) ->
        (Localise.from_string error_msg, desc, None, Exn_user, High, None, Checker)
    | Condition_is_assignment(desc, mloc) ->
        (Localise.condition_is_assignment, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Dangling_pointer_dereference (dko, desc, mloc) ->
        let visibility = match dko with
          | Some dk -> Exn_user (* only show to the user if the category was identified *)
          | None -> Exn_developer in
        (Localise.dangling_pointer_dereference, desc, Some mloc, visibility, High, None, Prover)
    | Deallocate_stack_variable desc ->
        (Localise.deallocate_stack_variable, desc, None, Exn_user, High, None, Prover)
    | Deallocate_static_memory desc ->
        (Localise.deallocate_static_memory, desc, None, Exn_user, High, None, Prover)
    | Deallocation_mismatch (desc, mloc) ->
        (Localise.deallocation_mismatch, desc, Some mloc, Exn_user, High, None, Prover)
    | Divide_by_zero (desc, mloc) ->
        (Localise.divide_by_zero, desc, Some mloc, Exn_user, High, Some Kerror, Checker)
    | Eradicate (kind_s, desc) ->
        (Localise.from_string kind_s, desc, None, Exn_user, High, None, Prover)
    | Field_not_null_checked (desc, mloc) ->
        (Localise.field_not_null_checked, desc, Some mloc, Exn_user, Medium, Some Kwarning, Nocat)
    | Frontend_warning (name, desc, mloc) ->
        (Localise.from_string name, desc, Some mloc, Exn_user, Medium, Some Kwarning, Nocat)
    | Checkers (kind_s, desc) ->
        (Localise.from_string kind_s, desc, None, Exn_user, High, None, Prover)
    | Null_dereference (desc, mloc) ->
        (Localise.null_dereference, desc, Some mloc, Exn_user, High, None, Prover)
    | Null_test_after_dereference (desc, mloc) ->
        (Localise.null_test_after_dereference, desc, Some mloc, Exn_user, High, None, Nocat)
    | Pointer_size_mismatch (desc, mloc) ->
        (Localise.pointer_size_mismatch, desc, Some mloc, Exn_user, High, Some Kerror, Checker)
    | Inherently_dangerous_function desc ->
        (Localise.inherently_dangerous_function, desc, None, Exn_developer, Medium, None, Nocat)
    | Internal_error desc ->
        (Localise.from_string "Internal_error", desc, None, Exn_developer, High, None, Nocat)
    | Invalid_argument s ->
        let desc = Localise.verbatim_desc s in
        (Localise.from_string "Invalid_argument", desc, None, Exn_system, Low, None, Nocat)
    | Java_runtime_exception (exn_name, pre_str, desc) ->
        let exn_str = Typename.name exn_name in
        (Localise.from_string exn_str, desc, None, Exn_user, High, None, Prover)
    | Leak (fp_part, _, _, (exn_vis, error_desc), done_array_abstraction, resource, mloc) ->
        if done_array_abstraction
        then (Localise.from_string "Leak_after_array_abstraction", error_desc, Some mloc, Exn_developer, High, None, Prover)
        else if fp_part
        then (Localise.from_string "Leak_in_footprint", error_desc, Some mloc, Exn_developer, High, None, Prover)
        else
          let loc_str = match resource with
            | Sil.Rmemory _ -> Localise.memory_leak
            | Sil.Rfile -> Localise.resource_leak
            | Sil.Rlock -> Localise.resource_leak
            | Sil.Rignore -> Localise.memory_leak in
          (loc_str, error_desc, Some mloc, exn_vis, High, None, Prover)
    | Match_failure mloc ->
        (Localise.from_string "Match failure", Localise.no_desc, Some mloc, Exn_developer, High, None, Nocat)
    | Missing_fld (fld, mloc) ->
        let desc = Localise.verbatim_desc (Ident.fieldname_to_string fld) in
        (Localise.from_string "Missing_fld", desc, Some mloc, Exn_developer, Medium, None, Nocat)
    | Premature_nil_termination (desc, mloc) ->
        (Localise.premature_nil_termination, desc, Some mloc, Exn_user, High, None, Prover)
    | Not_found ->
        (Localise.from_string "Not_found", Localise.no_desc, None, Exn_system, Low, None, Nocat)
    | Parameter_not_null_checked (desc, mloc) ->
        (Localise.parameter_not_null_checked, desc, Some mloc, Exn_user, Medium, Some Kwarning, Nocat)
    | Precondition_not_found (desc, mloc) ->
        (Localise.precondition_not_found, desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Precondition_not_met (desc, mloc) ->
        (Localise.precondition_not_met, desc, Some mloc, Exn_user, Medium, Some Kwarning, Nocat) (** always a warning *)
    | Retain_cycle (prop, hpred, desc, mloc) ->
        (Localise.retain_cycle, desc, Some mloc, Exn_user, High, None, Prover)
    | Return_expression_required (desc, mloc) ->
        (Localise.return_expression_required, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Stack_variable_address_escape (desc, mloc) ->
        (Localise.stack_variable_address_escape, desc, Some mloc, Exn_user, High, Some Kerror, Nocat)
    | Return_statement_missing (desc, mloc) ->
        (Localise.return_statement_missing, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Return_value_ignored (desc, mloc) ->
        (Localise.return_value_ignored, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Analysis_failure_exe _ ->
        (Localise.from_string "Failure_exe", Localise.no_desc, None, Exn_system, Low, None, Nocat)
    | Skip_function desc ->
        (Localise.skip_function, desc, None, Exn_developer, Low, None, Nocat)
    | Skip_pointer_dereference (desc, mloc) ->
        (Localise.skip_pointer_dereference, desc, Some mloc, Exn_user, Medium, Some Kinfo, Nocat) (** always an info *)
    | Symexec_memory_error mloc ->
        (Localise.from_string "Symexec_memory_error", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Sys_error s ->
        let desc = Localise.verbatim_desc s in
        (Localise.from_string "Sys_error", desc, None, Exn_system, Low, None, Nocat)
    | Tainted_value_reaching_sensitive_function (desc, mloc) ->
        (Localise.tainted_value_reaching_sensitive_function, desc, Some mloc,
         Exn_user, Medium, Some Kerror, Nocat)
    | Unix.Unix_error (_, s1, s2) ->
        let desc = Localise.verbatim_desc (s1 ^ s2) in
        (Localise.from_string "Unix_error", desc, None, Exn_system, Low, None, Nocat)
    | Uninitialized_value (desc, mloc) ->
        (Localise.uninitialized_value, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Unary_minus_applied_to_unsigned_expression(desc, mloc) ->
        (Localise.unary_minus_applied_to_unsigned_expression, desc, Some mloc, Exn_user, Medium, None, Nocat)
    | Unknown_proc ->
        (Localise.from_string "Unknown_proc", Localise.no_desc, None, Exn_developer, Low, None, Nocat)
    | Use_after_free (desc, mloc) ->
        (Localise.use_after_free, desc, Some mloc, Exn_user, High, None, Prover)
    | Wrong_argument_number mloc ->
        (Localise.from_string "Wrong_argument_number", Localise.no_desc, Some mloc, Exn_developer, Low, None, Nocat)
    | Failure _ as f ->
        raise f
    | exn ->
        let exn_name = Printexc.to_string exn in
        (Localise.from_string exn_name, Localise.no_desc, None, Exn_developer, Low, None, Nocat) in
  let visibility' =
    if visibility = Exn_user && filter_out_bucket desc then Exn_developer else visibility in
  (err_name, desc, mloco, visibility', severity, force_kind, eclass)

(** print a description of the exception to the html output *)
let print_exception_html s exn =
  let err_name, desc, mloco, _, _, _, _ = recognize_exception exn in
  let mloc_string = match mloco with
    | None -> ""
    | Some mloc -> " " ^ ml_location_string mloc in
  let desc_str = pp_to_string Localise.pp_error_desc desc in
  (L.d_strln_color Red) (s ^ (Localise.to_string err_name) ^ " " ^ desc_str ^ mloc_string)

(** string describing an error kind *)
let err_kind_string = function
  | Kwarning -> "WARNING"
  | Kerror -> "ERROR"
  | Kinfo -> "INFO"

(** string describing an error class *)
let err_class_string = function
  | Checker -> "CHECKER"
  | Prover -> "PROVER"
  | Nocat -> ""

(** wether to print the bug key together with the error message *)
let print_key = false

(** pretty print an error given its (id,key), location, kind, name, description, and optional ml location *)
let pp_err (node_id, node_key) loc ekind ex_name desc mloco fmt () =
  let kind = err_kind_string (if ekind = Kinfo then Kwarning else ekind) (* eclipse does not know about infos: treat as warning *) in
  let pp_key fmt k = if print_key then F.fprintf fmt " key: %d " k else () in
  F.fprintf fmt "%s:%d: %s: %a %a%a%a@\n"
    (DB.source_file_to_string loc.Location.file)
    loc.Location.line
    kind
    Localise.pp ex_name
    Localise.pp_error_desc desc
    pp_key node_key
    pp_ml_location_opt mloco

(** Return true if the exception is not serious and should be handled in timeout mode *)
let handle_exception exn =
  let _, _, _, visibility, _, _, _ = recognize_exception exn in
  visibility == Exn_user || visibility == Exn_developer

