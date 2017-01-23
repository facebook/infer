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

module L = Logging
module F = Format

(** visibility of the exception *)
type visibility =
  | Exn_user (** always add to error log *)
  | Exn_developer (** only add to error log in developer mode *)
  | Exn_system (** never add to error log *)
[@@deriving compare]

let equal_visibility = [%compare.equal : visibility]

let string_of_visibility vis =
  match vis with
  | Exn_user -> "user"
  | Exn_developer -> "developer"
  | Exn_system  -> "system"

(** severity of bugs *)
type severity =
  | High (* high severity bug *)
  | Medium (* medium severity bug *)
  | Low (* low severity bug *)

(** class of error/warning *)
type err_class = Checker | Prover | Nocat | Linters [@@deriving compare]

let equal_err_class = [%compare.equal : err_class]

(** kind of error/warning *)
type err_kind = Kwarning | Kerror | Kinfo | Kadvice [@@deriving compare]

let equal_err_kind = [%compare.equal : err_kind]

exception Abduction_case_not_implemented of L.ml_loc
exception Analysis_stops of Localise.error_desc * L.ml_loc option
exception Array_out_of_bounds_l1 of Localise.error_desc * L.ml_loc
exception Array_out_of_bounds_l2 of Localise.error_desc * L.ml_loc
exception Array_out_of_bounds_l3 of Localise.error_desc * L.ml_loc
exception Array_of_pointsto of L.ml_loc
exception Bad_footprint of L.ml_loc
exception Cannot_star of L.ml_loc
exception Class_cast_exception of Localise.error_desc * L.ml_loc
exception Codequery of Localise.error_desc
exception Comparing_floats_for_equality of Localise.error_desc * L.ml_loc
exception Condition_is_assignment of Localise.error_desc * L.ml_loc
exception Condition_always_true_false of Localise.error_desc * bool * L.ml_loc
exception Context_leak of Localise.error_desc * L.ml_loc
exception Custom_error of string * Localise.error_desc
exception Dangling_pointer_dereference of
    PredSymb.dangling_kind option * Localise.error_desc * L.ml_loc
exception Deallocate_stack_variable of Localise.error_desc
exception Deallocate_static_memory of Localise.error_desc
exception Deallocation_mismatch of Localise.error_desc * L.ml_loc
exception Divide_by_zero of Localise.error_desc * L.ml_loc
exception Empty_vector_access of Localise.error_desc * L.ml_loc
exception Eradicate of string * Localise.error_desc
exception Field_not_null_checked of Localise.error_desc * L.ml_loc
exception Frontend_warning of string * Localise.error_desc * L.ml_loc
exception Checkers of string * Localise.error_desc
exception Inherently_dangerous_function of Localise.error_desc
exception Internal_error of Localise.error_desc
exception Java_runtime_exception of Typename.t * string * Localise.error_desc
exception Leak of
    bool * Sil.hpred * (visibility * Localise.error_desc)
    * bool * PredSymb.resource * L.ml_loc
exception Missing_fld of Ident.fieldname * L.ml_loc
exception Premature_nil_termination of Localise.error_desc * L.ml_loc
exception Null_dereference of Localise.error_desc * L.ml_loc
exception Null_test_after_dereference of Localise.error_desc * L.ml_loc
exception Parameter_not_null_checked of Localise.error_desc * L.ml_loc
exception Pointer_size_mismatch of Localise.error_desc * L.ml_loc
exception Precondition_not_found of Localise.error_desc * L.ml_loc
exception Precondition_not_met of Localise.error_desc * L.ml_loc
exception Retain_cycle of Sil.hpred * Localise.error_desc * L.ml_loc
exception Registered_observer_being_deallocated of Localise.error_desc * L.ml_loc
exception Return_expression_required of Localise.error_desc * L.ml_loc
exception Return_statement_missing of Localise.error_desc * L.ml_loc
exception Return_value_ignored of Localise.error_desc * L.ml_loc
exception Skip_function of Localise.error_desc
exception Skip_pointer_dereference of Localise.error_desc * L.ml_loc
exception Stack_variable_address_escape of Localise.error_desc * L.ml_loc
exception Symexec_memory_error of L.ml_loc
exception Tainted_value_reaching_sensitive_function of Localise.error_desc * L.ml_loc
exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * L.ml_loc
exception Uninitialized_value of Localise.error_desc * L.ml_loc
exception Unknown_proc
exception Unsafe_guarded_by_access of Localise.error_desc * L.ml_loc
exception Use_after_free of Localise.error_desc * L.ml_loc
exception Wrong_argument_number of L.ml_loc


(** Turn an exception into a descriptive string, error description, location in ml source, and category *)
let recognize_exception exn =
  let err_name, desc, (ml_loc_opt : L.ml_loc option), visibility, severity, force_kind, eclass =
    match exn with (* all the names of Exn_user errors must be defined in Localise *)
    | Abduction_case_not_implemented ml_loc ->
        (Localise.from_string "Abduction_case_not_implemented",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Context_leak (desc, _) ->
        (Localise.context_leak,
         desc, None, Exn_user, High, None, Nocat)
    | Analysis_stops (desc, ml_loc_opt) ->
        let visibility = if Config.analysis_stops then Exn_user else Exn_developer in
        (Localise.analysis_stops, desc, ml_loc_opt, visibility, Medium, None, Nocat)
    | Array_of_pointsto ml_loc ->
        (Localise.from_string "Array_of_pointsto",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Array_out_of_bounds_l1 (desc, ml_loc) ->
        (Localise.array_out_of_bounds_l1,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Checker)
    | Array_out_of_bounds_l2 (desc, ml_loc) ->
        (Localise.array_out_of_bounds_l2,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Array_out_of_bounds_l3 (desc, ml_loc) ->
        (Localise.array_out_of_bounds_l3,
         desc, Some ml_loc, Exn_developer, Medium, None, Nocat)
    | Assert_failure (f, l, c) ->
        let ml_loc = (f, l, c, c) in
        (Localise.from_string "Assert_failure",
         Localise.no_desc, Some ml_loc, Exn_developer, High, None, Nocat)
    | Bad_footprint ml_loc ->
        (Localise.from_string "Bad_footprint",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Cannot_star ml_loc ->
        (Localise.from_string "Cannot_star",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Class_cast_exception (desc, ml_loc) ->
        (Localise.class_cast_exception,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Codequery desc ->
        (Localise.from_string "Codequery",
         desc, None, Exn_user, High, None, Prover)
    | Comparing_floats_for_equality(desc, ml_loc) ->
        (Localise.comparing_floats_for_equality,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Condition_always_true_false (desc, b, ml_loc) ->
        let name =
          if b then Localise.condition_always_true
          else Localise.condition_always_false in
        (name, desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Custom_error (error_msg, desc) ->
        (Localise.from_string error_msg,
         desc, None, Exn_user, High, None, Checker)
    | Condition_is_assignment(desc, ml_loc) ->
        (Localise.condition_is_assignment,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Dangling_pointer_dereference (dko, desc, ml_loc) ->
        let visibility = match dko with
          | Some _ -> Exn_user (* only show to the user if the category was identified *)
          | None -> Exn_developer in
        (Localise.dangling_pointer_dereference,
         desc, Some ml_loc, visibility, High, None, Prover)
    | Deallocate_stack_variable desc ->
        (Localise.deallocate_stack_variable,
         desc, None, Exn_user, High, None, Prover)
    | Deallocate_static_memory desc ->
        (Localise.deallocate_static_memory,
         desc, None, Exn_user, High, None, Prover)
    | Deallocation_mismatch (desc, ml_loc) ->
        (Localise.deallocation_mismatch,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Divide_by_zero (desc, ml_loc) ->
        (Localise.divide_by_zero,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Checker)
    | Eradicate (kind_s, desc) ->
        (Localise.from_string kind_s, desc, None, Exn_user, High, None, Prover)
    | Empty_vector_access (desc, ml_loc) ->
        (Localise.empty_vector_access,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Prover)
    | Field_not_null_checked (desc, ml_loc) ->
        (Localise.field_not_null_checked,
         desc, Some ml_loc, Exn_user, Medium, Some Kwarning, Nocat)
    | Frontend_warning (name, desc, ml_loc) ->
        (Localise.from_string name,
         desc, Some ml_loc, Exn_user, Medium, None, Linters)
    | Checkers (kind_s, desc) ->
        (Localise.from_string kind_s,
         desc, None, Exn_user, High, None, Prover)
    | Null_dereference (desc, ml_loc) ->
        (Localise.null_dereference,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Null_test_after_dereference (desc, ml_loc) ->
        (Localise.null_test_after_dereference,
         desc, Some ml_loc, Exn_user, High, None, Nocat)
    | Pointer_size_mismatch (desc, ml_loc) ->
        (Localise.pointer_size_mismatch,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Checker)
    | Inherently_dangerous_function desc ->
        (Localise.inherently_dangerous_function,
         desc, None, Exn_developer, Medium, None, Nocat)
    | Internal_error desc ->
        (Localise.from_string "Internal_error",
         desc, None, Exn_developer, High, None, Nocat)
    | Invalid_argument s ->
        let desc = Localise.verbatim_desc s in
        (Localise.from_string "Invalid_argument", desc, None, Exn_system, Low, None, Nocat)
    | Java_runtime_exception (exn_name, _, desc) ->
        let exn_str = Typename.name exn_name in
        (Localise.from_string exn_str, desc, None, Exn_user, High, None, Prover)
    | Leak (fp_part, _, (exn_vis, error_desc), done_array_abstraction, resource, ml_loc) ->
        if done_array_abstraction
        then (Localise.from_string "Leak_after_array_abstraction",
              error_desc, Some ml_loc, Exn_developer, High, None, Prover)
        else if fp_part
        then (Localise.from_string "Leak_in_footprint",
              error_desc, Some ml_loc, Exn_developer, High, None, Prover)
        else
          let loc_str = match resource with
            | PredSymb.Rmemory _ -> Localise.memory_leak
            | PredSymb.Rfile -> Localise.resource_leak
            | PredSymb.Rlock -> Localise.resource_leak
            | PredSymb.Rignore -> Localise.memory_leak in
          (loc_str, error_desc, Some ml_loc, exn_vis, High, None, Prover)
    | Match_failure (f, l, c) ->
        let ml_loc = (f, l, c, c) in
        (Localise.from_string "Match failure",
         Localise.no_desc, Some ml_loc, Exn_developer, High, None, Nocat)
    | Missing_fld (fld, ml_loc) ->
        let desc = Localise.verbatim_desc (Ident.fieldname_to_string fld) in
        (Localise.from_string "Missing_fld", desc, Some ml_loc, Exn_developer, Medium, None, Nocat)
    | Premature_nil_termination (desc, ml_loc) ->
        (Localise.premature_nil_termination,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Not_found ->
        (Localise.from_string "Not_found",
         Localise.no_desc, None, Exn_system, Low, None, Nocat)
    | Parameter_not_null_checked (desc, ml_loc) ->
        (Localise.parameter_not_null_checked,
         desc, Some ml_loc, Exn_user, Medium, Some Kwarning, Nocat)
    | Precondition_not_found (desc, ml_loc) ->
        (Localise.precondition_not_found,
         desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Precondition_not_met (desc, ml_loc) ->
        (Localise.precondition_not_met,
         desc, Some ml_loc, Exn_developer, Medium, Some Kwarning, Nocat) (* always a warning *)
    | Retain_cycle (_, desc, ml_loc) ->
        (Localise.retain_cycle,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Registered_observer_being_deallocated (desc, ml_loc) ->
        (Localise.registered_observer_being_deallocated,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Nocat)
    | Return_expression_required (desc, ml_loc) ->
        (Localise.return_expression_required,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Stack_variable_address_escape (desc, ml_loc) ->
        (Localise.stack_variable_address_escape,
         desc, Some ml_loc, Exn_user, High, Some Kerror, Nocat)
    | Return_statement_missing (desc, ml_loc) ->
        (Localise.return_statement_missing,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Return_value_ignored (desc, ml_loc) ->
        (Localise.return_value_ignored,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | SymOp.Analysis_failure_exe _ ->
        (Localise.from_string "Failure_exe",
         Localise.no_desc, None, Exn_system, Low, None, Nocat)
    | Skip_function desc ->
        (Localise.skip_function, desc, None, Exn_developer, Low, None, Nocat)
    | Skip_pointer_dereference (desc, ml_loc) ->
        (Localise.skip_pointer_dereference,
         desc, Some ml_loc, Exn_user, Medium, Some Kinfo, Nocat) (* always an info *)
    | Symexec_memory_error ml_loc ->
        (Localise.from_string "Symexec_memory_error",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Sys_error s ->
        let desc = Localise.verbatim_desc s in
        (Localise.from_string "Sys_error",
         desc, None, Exn_system, Low, None, Nocat)
    | Tainted_value_reaching_sensitive_function (desc, ml_loc) ->
        (Localise.tainted_value_reaching_sensitive_function,
         desc, Some ml_loc, Exn_user, Medium, Some Kerror, Nocat)
    | Unix.Unix_error (_, s1, s2) ->
        let desc = Localise.verbatim_desc (s1 ^ s2) in
        (Localise.from_string "Unix_error",
         desc, None, Exn_system, Low, None, Nocat)
    | Uninitialized_value (desc, ml_loc) ->
        (Localise.uninitialized_value,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Unary_minus_applied_to_unsigned_expression(desc, ml_loc) ->
        (Localise.unary_minus_applied_to_unsigned_expression,
         desc, Some ml_loc, Exn_user, Medium, None, Nocat)
    | Unknown_proc ->
        (Localise.from_string "Unknown_proc",
         Localise.no_desc, None, Exn_developer, Low, None, Nocat)
    | Unsafe_guarded_by_access (desc, ml_loc) ->
        (Localise.unsafe_guarded_by_access,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Use_after_free (desc, ml_loc) ->
        (Localise.use_after_free,
         desc, Some ml_loc, Exn_user, High, None, Prover)
    | Wrong_argument_number ml_loc ->
        (Localise.from_string "Wrong_argument_number",
         Localise.no_desc, Some ml_loc, Exn_developer, Low, None, Nocat)
    | Failure _ as f ->
        raise f
    | exn ->
        let exn_name = Exn.to_string exn in
        (Localise.from_string exn_name,
         Localise.no_desc, None, Exn_developer, Low, None, Nocat) in
  (err_name, desc, ml_loc_opt, visibility, severity, force_kind, eclass)

(** print a description of the exception to the html output *)
let print_exception_html s exn =
  let err_name, desc, ml_loc_opt, _, _, _, _ = recognize_exception exn in
  let ml_loc_string = match ml_loc_opt with
    | None -> ""
    | Some ml_loc -> " " ^ L.ml_loc_to_string ml_loc in
  let desc_str = F.asprintf "%a" Localise.pp_error_desc desc in
  (L.d_strln_color Red) (s ^ (Localise.to_string err_name) ^ " " ^ desc_str ^ ml_loc_string)

(** string describing an error kind *)
let err_kind_string = function
  | Kwarning -> "WARNING"
  | Kerror -> "ERROR"
  | Kinfo -> "INFO"
  | Kadvice -> "ADVICE"

(** string describing an error class *)
let err_class_string = function
  | Checker -> "CHECKER"
  | Prover -> "PROVER"
  | Nocat -> ""
  | Linters -> "Linters"

(** wether to print the bug key together with the error message *)
let print_key = false

(** pretty print an error given its (id,key), location, kind, name, description, and optional ml location *)
let pp_err (_, node_key) loc ekind ex_name desc ml_loc_opt fmt () =
  let kind = err_kind_string (if equal_err_kind ekind Kinfo then Kwarning else ekind) in
  let pp_key fmt k = if print_key then F.fprintf fmt " key: %d " k else () in
  F.fprintf fmt "%a:%d: %s: %a %a%a%a@\n"
    SourceFile.pp loc.Location.file
    loc.Location.line
    kind
    Localise.pp ex_name
    Localise.pp_error_desc desc
    pp_key node_key
    L.pp_ml_loc_opt ml_loc_opt

(** Return true if the exception is not serious and should be handled in timeout mode *)
let handle_exception exn =
  let _, _, _, visibility, _, _, _ = recognize_exception exn in
  equal_visibility visibility Exn_user ||
  equal_visibility visibility Exn_developer
