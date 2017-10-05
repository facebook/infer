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
  | Exn_user  (** always add to error log *)
  | Exn_developer  (** only add to error log in developer mode *)
  | Exn_system  (** never add to error log *)
  [@@deriving compare]

let equal_visibility = [%compare.equal : visibility]

let string_of_visibility vis =
  match vis with Exn_user -> "user" | Exn_developer -> "developer" | Exn_system -> "system"

(** severity of bugs *)
type severity =
  | High  (** high severity bug *)
  | Medium  (** medium severity bug *)
  | Low  (** low severity bug *)

(** class of error/warning *)
type err_class = Checker | Prover | Nocat | Linters [@@deriving compare]

let equal_err_class = [%compare.equal : err_class]

(** kind of error/warning *)
type err_kind = Kwarning | Kerror | Kinfo | Kadvice | Klike [@@deriving compare]

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

exception Condition_always_true_false of Localise.error_desc * bool * L.ml_loc

exception Context_leak of Localise.error_desc * L.ml_loc

exception Custom_error of string * Localise.error_desc

exception Dangling_pointer_dereference of
  PredSymb.dangling_kind option * Localise.error_desc * L.ml_loc

exception Deallocate_stack_variable of Localise.error_desc

exception Deallocate_static_memory of Localise.error_desc

exception Deallocation_mismatch of Localise.error_desc * L.ml_loc

exception Divide_by_zero of Localise.error_desc * L.ml_loc

exception Double_lock of Localise.error_desc * L.ml_loc

exception Empty_vector_access of Localise.error_desc * L.ml_loc

exception Eradicate of string * Localise.error_desc

exception Field_not_null_checked of Localise.error_desc * L.ml_loc

exception Frontend_warning of (string * string option) * Localise.error_desc * L.ml_loc

exception Checkers of string * Localise.error_desc

exception Inherently_dangerous_function of Localise.error_desc

exception Internal_error of Localise.error_desc

exception Java_runtime_exception of Typ.Name.t * string * Localise.error_desc

exception Leak of
  bool * Sil.hpred * (visibility * Localise.error_desc) * bool * PredSymb.resource * L.ml_loc

exception Missing_fld of Typ.Fieldname.t * L.ml_loc

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

exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * L.ml_loc

exception Uninitialized_value of Localise.error_desc * L.ml_loc

exception Unknown_proc

exception Unreachable_code_after of Localise.error_desc * L.ml_loc

exception Unsafe_guarded_by_access of Localise.error_desc * L.ml_loc

exception Use_after_free of Localise.error_desc * L.ml_loc

exception Wrong_argument_number of L.ml_loc

type t =
  { name: IssueType.t
  ; description: Localise.error_desc
  ; ml_loc: Logging.ml_loc option  (** location in the infer source code *)
  ; visibility: visibility
  ; severity: severity
  ; kind: err_kind option
  ; category: err_class }

let recognize_exception exn =
  match exn with
  (* all the static names of errors must be defined in Config.IssueType *)
  | Abduction_case_not_implemented ml_loc
   -> { name= IssueType.abduction_case_not_implemented
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Context_leak (desc, _)
   -> { name= IssueType.context_leak
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Nocat }
  | Analysis_stops (desc, ml_loc_opt)
   -> let visibility = if Config.analysis_stops then Exn_user else Exn_developer in
      { name= IssueType.analysis_stops
      ; description= desc
      ; ml_loc= ml_loc_opt
      ; visibility
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Array_of_pointsto ml_loc
   -> { name= IssueType.array_of_pointsto
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Array_out_of_bounds_l1 (desc, ml_loc)
   -> { name= IssueType.array_out_of_bounds_l1
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Checker }
  | Array_out_of_bounds_l2 (desc, ml_loc)
   -> { name= IssueType.array_out_of_bounds_l2
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Array_out_of_bounds_l3 (desc, ml_loc)
   -> { name= IssueType.array_out_of_bounds_l3
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Assert_failure (f, l, c)
   -> let ml_loc = (f, l, c, c) in
      { name= IssueType.assert_failure
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= High
      ; kind= None
      ; category= Nocat }
  | Bad_footprint ml_loc
   -> { name= IssueType.bad_footprint
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Cannot_star ml_loc
   -> { name= IssueType.cannot_star
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Class_cast_exception (desc, ml_loc)
   -> { name= IssueType.class_cast_exception
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Codequery desc
   -> { name= IssueType.codequery
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Comparing_floats_for_equality (desc, ml_loc)
   -> { name= IssueType.comparing_floats_for_equality
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Condition_always_true_false (desc, b, ml_loc)
   -> let name = if b then IssueType.condition_always_true else IssueType.condition_always_false in
      { name
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Custom_error (error_msg, desc)
   -> { name= IssueType.from_string error_msg
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Checker }
  | Dangling_pointer_dereference (dko, desc, ml_loc)
   -> let visibility =
        match dko with
        | Some _
         -> Exn_user (* only show to the user if the category was identified *)
        | None
         -> Exn_developer
      in
      { name= IssueType.dangling_pointer_dereference
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Deallocate_stack_variable desc
   -> { name= IssueType.deallocate_stack_variable
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Deallocate_static_memory desc
   -> { name= IssueType.deallocate_static_memory
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Deallocation_mismatch (desc, ml_loc)
   -> { name= IssueType.deallocation_mismatch
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Divide_by_zero (desc, ml_loc)
   -> { name= IssueType.divide_by_zero
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Checker }
  | Double_lock (desc, ml_loc)
   -> { name= IssueType.double_lock
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Prover }
  | Eradicate (kind_s, desc)
   -> { name= IssueType.from_string kind_s
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Empty_vector_access (desc, ml_loc)
   -> { name= IssueType.empty_vector_access
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Prover }
  | Field_not_null_checked (desc, ml_loc)
   -> { name= IssueType.field_not_null_checked
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= Some Kwarning
      ; category= Nocat }
  | Frontend_warning ((name, hum), desc, ml_loc)
   -> { name= IssueType.from_string name ?hum
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Linters }
  | Checkers (kind_s, desc)
   -> { name= IssueType.from_string kind_s
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Null_dereference (desc, ml_loc)
   -> { name= IssueType.null_dereference
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Null_test_after_dereference (desc, ml_loc)
   -> { name= IssueType.null_test_after_dereference
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Nocat }
  | Pointer_size_mismatch (desc, ml_loc)
   -> { name= IssueType.pointer_size_mismatch
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Checker }
  | Inherently_dangerous_function desc
   -> { name= IssueType.inherently_dangerous_function
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_developer
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Internal_error desc
   -> { name= IssueType.internal_error
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_developer
      ; severity= High
      ; kind= None
      ; category= Nocat }
  | Java_runtime_exception (exn_name, _, desc)
   -> let exn_str = Typ.Name.name exn_name in
      { name= IssueType.from_string exn_str
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Leak (fp_part, _, (exn_vis, error_desc), done_array_abstraction, resource, ml_loc)
   -> if done_array_abstraction then
        { name= IssueType.leak_after_array_abstraction
        ; description= error_desc
        ; ml_loc= Some ml_loc
        ; visibility= Exn_developer
        ; severity= High
        ; kind= None
        ; category= Prover }
      else if fp_part then
        { name= IssueType.leak_in_footprint
        ; description= error_desc
        ; ml_loc= Some ml_loc
        ; visibility= Exn_developer
        ; severity= High
        ; kind= None
        ; category= Prover }
      else
        let name =
          match resource with
          | PredSymb.Rmemory _
           -> IssueType.memory_leak
          | PredSymb.Rfile
           -> IssueType.resource_leak
          | PredSymb.Rlock
           -> IssueType.resource_leak
          | PredSymb.Rignore
           -> IssueType.memory_leak
        in
        { name
        ; description= error_desc
        ; ml_loc= Some ml_loc
        ; visibility= exn_vis
        ; severity= High
        ; kind= None
        ; category= Prover }
  | Missing_fld (fld, ml_loc)
   -> let desc = Localise.verbatim_desc (Typ.Fieldname.to_full_string fld) in
      { name= IssueType.missing_fld
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Premature_nil_termination (desc, ml_loc)
   -> { name= IssueType.premature_nil_termination
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Parameter_not_null_checked (desc, ml_loc)
   -> { name= IssueType.parameter_not_null_checked
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= Some Kwarning
      ; category= Nocat }
  | Precondition_not_found (desc, ml_loc)
   -> { name= IssueType.precondition_not_found
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Precondition_not_met (desc, ml_loc)
   -> { name= IssueType.precondition_not_met
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Medium
      ; kind= Some Kwarning
      ; category= Nocat }
      (* always a warning *)
  | Retain_cycle (_, desc, ml_loc)
   -> { name= IssueType.retain_cycle
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Registered_observer_being_deallocated (desc, ml_loc)
   -> { name= IssueType.registered_observer_being_deallocated
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Nocat }
  | Return_expression_required (desc, ml_loc)
   -> { name= IssueType.return_expression_required
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Stack_variable_address_escape (desc, ml_loc)
   -> { name= IssueType.stack_variable_address_escape
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= Some Kerror
      ; category= Nocat }
  | Return_statement_missing (desc, ml_loc)
   -> { name= IssueType.return_statement_missing
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Return_value_ignored (desc, ml_loc)
   -> { name= IssueType.return_value_ignored
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | SymOp.Analysis_failure_exe _
   -> { name= IssueType.failure_exe
      ; description= Localise.no_desc
      ; ml_loc= None
      ; visibility= Exn_system
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Skip_function desc
   -> { name= IssueType.skip_function
      ; description= desc
      ; ml_loc= None
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Skip_pointer_dereference (desc, ml_loc)
   -> { name= IssueType.skip_pointer_dereference
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= Some Kinfo
      ; category= Nocat }
      (* always an info *)
  | Symexec_memory_error ml_loc
   -> { name= IssueType.symexec_memory_error
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Uninitialized_value (desc, ml_loc)
   -> { name= IssueType.uninitialized_value
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Unary_minus_applied_to_unsigned_expression (desc, ml_loc)
   -> { name= IssueType.unary_minus_applied_to_unsigned_expression
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Unknown_proc
   -> { name= IssueType.unknown_proc
      ; description= Localise.no_desc
      ; ml_loc= None
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | Unreachable_code_after (desc, ml_loc)
   -> { name= IssueType.unreachable_code_after
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= Medium
      ; kind= None
      ; category= Nocat }
  | Unsafe_guarded_by_access (desc, ml_loc)
   -> { name= IssueType.unsafe_guarded_by_access
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Use_after_free (desc, ml_loc)
   -> { name= IssueType.use_after_free
      ; description= desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_user
      ; severity= High
      ; kind= None
      ; category= Prover }
  | Wrong_argument_number ml_loc
   -> { name= IssueType.wrong_argument_number
      ; description= Localise.no_desc
      ; ml_loc= Some ml_loc
      ; visibility= Exn_developer
      ; severity= Low
      ; kind= None
      ; category= Nocat }
  | exn
   -> { name= IssueType.failure_exe
      ; description=
          Localise.verbatim_desc (F.asprintf "%a: %s" Exn.pp exn (Caml.Printexc.get_backtrace ()))
      ; ml_loc= None
      ; visibility= Exn_system
      ; severity= Low
      ; kind= None
      ; category= Nocat }

(** print a description of the exception to the html output *)
let print_exception_html s exn =
  let error = recognize_exception exn in
  let ml_loc_string =
    match error.ml_loc with None -> "" | Some ml_loc -> " " ^ L.ml_loc_to_string ml_loc
  in
  let desc_str = F.asprintf "%a" Localise.pp_error_desc error.description in
  L.d_strln_color Red (s ^ error.name.IssueType.unique_id ^ " " ^ desc_str ^ ml_loc_string)

(** string describing an error kind *)
let err_kind_string = function
  | Kwarning
   -> "WARNING"
  | Kerror
   -> "ERROR"
  | Kinfo
   -> "INFO"
  | Kadvice
   -> "ADVICE"
  | Klike
   -> "LIKE"

(** string describing an error class *)
let err_class_string = function
  | Checker
   -> "CHECKER"
  | Prover
   -> "PROVER"
  | Nocat
   -> ""
  | Linters
   -> "Linters"

(** whether to print the bug key together with the error message *)
let print_key = false

(** pretty print an error  *)
let pp_err ~node_key loc ekind ex_name desc ml_loc_opt fmt () =
  let kind = err_kind_string (if equal_err_kind ekind Kinfo then Kwarning else ekind) in
  let pp_key fmt k = if print_key then F.fprintf fmt " key: %d " k else () in
  F.fprintf fmt "%a:%d: %s: %a %a%a%a@\n" SourceFile.pp loc.Location.file loc.Location.line kind
    IssueType.pp ex_name Localise.pp_error_desc desc pp_key node_key L.pp_ml_loc_opt ml_loc_opt

(** Return true if the exception is not serious and should be handled in timeout mode *)
let handle_exception exn =
  let error = recognize_exception exn in
  equal_visibility error.visibility Exn_user || equal_visibility error.visibility Exn_developer
