(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

let equal_visibility = [%compare.equal: visibility]

let string_of_visibility vis =
  match vis with Exn_user -> "user" | Exn_developer -> "developer" | Exn_system -> "system"


(** class of error/warning *)
type err_class = Checker | Prover | Nocat | Linters [@@deriving compare]

let equal_err_class = [%compare.equal: err_class]

(** severity of the report *)
type severity = Like | Info | Advice | Warning | Error [@@deriving compare]

let equal_severity = [%compare.equal: severity]

exception Abduction_case_not_implemented of L.ocaml_pos

exception Analysis_stops of Localise.error_desc * L.ocaml_pos option

exception Array_out_of_bounds_l1 of Localise.error_desc * L.ocaml_pos

exception Array_out_of_bounds_l2 of Localise.error_desc * L.ocaml_pos

exception Array_out_of_bounds_l3 of Localise.error_desc * L.ocaml_pos

exception Array_of_pointsto of L.ocaml_pos

exception Bad_footprint of L.ocaml_pos

exception Cannot_star of L.ocaml_pos

exception Class_cast_exception of Localise.error_desc * L.ocaml_pos

exception Codequery of Localise.error_desc

exception Comparing_floats_for_equality of Localise.error_desc * L.ocaml_pos

exception Condition_always_true_false of Localise.error_desc * bool * L.ocaml_pos

exception Custom_error of string * Localise.error_desc

exception Dummy_exception of Localise.error_desc

exception
  Dangling_pointer_dereference of PredSymb.dangling_kind option * Localise.error_desc * L.ocaml_pos

exception Deallocate_stack_variable of Localise.error_desc

exception Deallocate_static_memory of Localise.error_desc

exception Deallocation_mismatch of Localise.error_desc * L.ocaml_pos

exception Divide_by_zero of Localise.error_desc * L.ocaml_pos

exception Empty_vector_access of Localise.error_desc * L.ocaml_pos

exception Eradicate of IssueType.t * Localise.error_desc

exception Field_not_null_checked of Localise.error_desc * L.ocaml_pos

exception Frontend_warning of IssueType.t * Localise.error_desc * L.ocaml_pos

exception Checkers of IssueType.t * Localise.error_desc

exception Inherently_dangerous_function of Localise.error_desc

exception Internal_error of Localise.error_desc

exception Java_runtime_exception of Typ.Name.t * string * Localise.error_desc

exception
  Leak of
    bool * Sil.hpred * (visibility * Localise.error_desc) * bool * PredSymb.resource * L.ocaml_pos

exception Missing_fld of Typ.Fieldname.t * L.ocaml_pos

exception Premature_nil_termination of Localise.error_desc * L.ocaml_pos

exception Null_dereference of Localise.error_desc * L.ocaml_pos

exception Null_test_after_dereference of Localise.error_desc * L.ocaml_pos

exception Parameter_not_null_checked of Localise.error_desc * L.ocaml_pos

exception Pointer_size_mismatch of Localise.error_desc * L.ocaml_pos

exception Precondition_not_found of Localise.error_desc * L.ocaml_pos

exception Precondition_not_met of Localise.error_desc * L.ocaml_pos

exception Retain_cycle of Localise.error_desc * L.ocaml_pos

exception Registered_observer_being_deallocated of Localise.error_desc * L.ocaml_pos

exception Return_expression_required of Localise.error_desc * L.ocaml_pos

exception Return_statement_missing of Localise.error_desc * L.ocaml_pos

exception Return_value_ignored of Localise.error_desc * L.ocaml_pos

exception Skip_function of Localise.error_desc

exception Skip_pointer_dereference of Localise.error_desc * L.ocaml_pos

exception Stack_variable_address_escape of Localise.error_desc * L.ocaml_pos

exception Symexec_memory_error of L.ocaml_pos

exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * L.ocaml_pos

exception Unknown_proc

exception Unsafe_guarded_by_access of Localise.error_desc * L.ocaml_pos

exception Use_after_free of Localise.error_desc * L.ocaml_pos

exception Wrong_argument_number of L.ocaml_pos

type t =
  { name: IssueType.t
  ; description: Localise.error_desc
  ; ocaml_pos: L.ocaml_pos option  (** location in the infer source code *)
  ; visibility: visibility
  ; severity: severity option
  ; category: err_class }

let recognize_exception exn =
  match exn with
  (* all the static names of errors must be defined in Config.IssueType *)
  | Abduction_case_not_implemented ocaml_pos ->
      { name= IssueType.abduction_case_not_implemented
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Analysis_stops (desc, ocaml_pos_opt) ->
      let visibility = if Config.analysis_stops then Exn_user else Exn_developer in
      { name= IssueType.analysis_stops
      ; description= desc
      ; ocaml_pos= ocaml_pos_opt
      ; visibility
      ; severity= None
      ; category= Nocat }
  | Array_of_pointsto ocaml_pos ->
      { name= IssueType.array_of_pointsto
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Array_out_of_bounds_l1 (desc, ocaml_pos) ->
      { name= IssueType.array_out_of_bounds_l1
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= Some Error
      ; category= Checker }
  | Array_out_of_bounds_l2 (desc, ocaml_pos) ->
      { name= IssueType.array_out_of_bounds_l2
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Array_out_of_bounds_l3 (desc, ocaml_pos) ->
      { name= IssueType.array_out_of_bounds_l3
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Assert_failure (f, l, c) ->
      let ocaml_pos = (f, l, c, c) in
      { name= IssueType.assert_failure
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Bad_footprint ocaml_pos ->
      { name= IssueType.bad_footprint
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Cannot_star ocaml_pos ->
      { name= IssueType.cannot_star
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Class_cast_exception (desc, ocaml_pos) ->
      { name= IssueType.class_cast_exception
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Prover }
  | Codequery desc ->
      { name= IssueType.codequery
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Comparing_floats_for_equality (desc, ocaml_pos) ->
      { name= IssueType.comparing_floats_for_equality
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Condition_always_true_false (desc, b, ocaml_pos) ->
      let name = if b then IssueType.condition_always_true else IssueType.condition_always_false in
      { name
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Custom_error (error_msg, desc) ->
      { name= IssueType.from_string error_msg
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Checker }
  | Dummy_exception desc ->
      { name= IssueType.analysis_stops
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= Some Info
      ; category= Checker }
  | Dangling_pointer_dereference (dko, desc, ocaml_pos) ->
      let visibility =
        match dko with
        | Some _ ->
            Exn_user (* only show to the user if the category was identified *)
        | None ->
            Exn_developer
      in
      { name= IssueType.dangling_pointer_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility
      ; severity= None
      ; category= Prover }
  | Deallocate_stack_variable desc ->
      { name= IssueType.deallocate_stack_variable
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Deallocate_static_memory desc ->
      { name= IssueType.deallocate_static_memory
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Deallocation_mismatch (desc, ocaml_pos) ->
      { name= IssueType.deallocation_mismatch
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Divide_by_zero (desc, ocaml_pos) ->
      { name= IssueType.divide_by_zero
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error
      ; category= Checker }
  | Eradicate (kind, desc) ->
      { name= kind
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Empty_vector_access (desc, ocaml_pos) ->
      { name= IssueType.empty_vector_access
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error
      ; category= Prover }
  | Field_not_null_checked (desc, ocaml_pos) ->
      { name= IssueType.field_not_null_checked
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Warning
      ; category= Nocat }
  | Frontend_warning (issue_type, desc, ocaml_pos) ->
      { name= issue_type
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Linters }
  | Checkers (kind, desc) ->
      { name= kind
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Null_dereference (desc, ocaml_pos) ->
      { name= IssueType.null_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Null_test_after_dereference (desc, ocaml_pos) ->
      { name= IssueType.null_test_after_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Pointer_size_mismatch (desc, ocaml_pos) ->
      { name= IssueType.pointer_size_mismatch
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error
      ; category= Checker }
  | Inherently_dangerous_function desc ->
      { name= IssueType.inherently_dangerous_function
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Internal_error desc ->
      { name= IssueType.internal_error
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Java_runtime_exception (exn_name, _, desc) ->
      let exn_str = Typ.Name.name exn_name in
      { name= IssueType.from_string exn_str
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Leak (fp_part, _, (exn_vis, error_desc), done_array_abstraction, resource, ocaml_pos) ->
      if done_array_abstraction then
        { name= IssueType.leak_after_array_abstraction
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_developer
        ; severity= None
        ; category= Prover }
      else if fp_part then
        { name= IssueType.leak_in_footprint
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_developer
        ; severity= None
        ; category= Prover }
      else
        let name =
          match resource with
          | PredSymb.Rmemory _ ->
              IssueType.memory_leak
          | PredSymb.Rfile ->
              IssueType.resource_leak
          | PredSymb.Rlock ->
              IssueType.resource_leak
          | PredSymb.Rignore ->
              IssueType.memory_leak
        in
        { name
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= exn_vis
        ; severity= None
        ; category= Prover }
  | Missing_fld (fld, ocaml_pos) ->
      let desc = Localise.verbatim_desc (Typ.Fieldname.to_full_string fld) in
      { name= IssueType.missing_fld
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Premature_nil_termination (desc, ocaml_pos) ->
      { name= IssueType.premature_nil_termination
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Parameter_not_null_checked (desc, ocaml_pos) ->
      { name= IssueType.parameter_not_null_checked
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Warning
      ; category= Nocat }
  | Precondition_not_found (desc, ocaml_pos) ->
      { name= IssueType.precondition_not_found
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Precondition_not_met (desc, ocaml_pos) ->
      { name= IssueType.precondition_not_met
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= Some Warning
      ; category= Nocat }
      (* always a warning *)
  | Retain_cycle (desc, ocaml_pos) ->
      { name= IssueType.retain_cycle
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Registered_observer_being_deallocated (desc, ocaml_pos) ->
      { name= IssueType.registered_observer_being_deallocated
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error
      ; category= Nocat }
  | Return_expression_required (desc, ocaml_pos) ->
      { name= IssueType.return_expression_required
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Stack_variable_address_escape (desc, ocaml_pos) ->
      { name= IssueType.stack_variable_address_escape
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error
      ; category= Nocat }
  | Return_statement_missing (desc, ocaml_pos) ->
      { name= IssueType.return_statement_missing
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Return_value_ignored (desc, ocaml_pos) ->
      { name= IssueType.return_value_ignored
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | SymOp.Analysis_failure_exe _ ->
      { name= IssueType.failure_exe
      ; description= Localise.no_desc
      ; ocaml_pos= None
      ; visibility= Exn_system
      ; severity= None
      ; category= Nocat }
  | Skip_function desc ->
      { name= IssueType.skip_function
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Skip_pointer_dereference (desc, ocaml_pos) ->
      { name= IssueType.skip_pointer_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Info
      ; category= Nocat }
      (* always an info *)
  | Symexec_memory_error ocaml_pos ->
      { name= IssueType.symexec_memory_error
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Unary_minus_applied_to_unsigned_expression (desc, ocaml_pos) ->
      { name= IssueType.unary_minus_applied_to_unsigned_expression
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Nocat }
  | Unknown_proc ->
      { name= IssueType.unknown_proc
      ; description= Localise.no_desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | Unsafe_guarded_by_access (desc, ocaml_pos) ->
      { name= IssueType.unsafe_guarded_by_access
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Use_after_free (desc, ocaml_pos) ->
      { name= IssueType.use_after_free
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None
      ; category= Prover }
  | Wrong_argument_number ocaml_pos ->
      { name= IssueType.wrong_argument_number
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None
      ; category= Nocat }
  | exn ->
      { name= IssueType.failure_exe
      ; description=
          Localise.verbatim_desc (F.asprintf "%a: %s" Exn.pp exn (Caml.Printexc.get_backtrace ()))
      ; ocaml_pos= None
      ; visibility= Exn_system
      ; severity= None
      ; category= Nocat }


(** print a description of the exception to the html output *)
let print_exception_html s exn =
  let error = recognize_exception exn in
  let ocaml_pos_string =
    match error.ocaml_pos with
    | None ->
        ""
    | Some ocaml_pos ->
        " " ^ L.ocaml_pos_to_string ocaml_pos
  in
  L.d_printfln ~color:Red "%s%s %a%s" s error.name.IssueType.unique_id Localise.pp_error_desc
    error.description ocaml_pos_string


(** string describing an error kind *)
let severity_string = function
  | Advice ->
      "ADVICE"
  | Error ->
      "ERROR"
  | Info ->
      "INFO"
  | Like ->
      "LIKE"
  | Warning ->
      "WARNING"


(** pretty print an error  *)
let pp_err loc severity ex_name desc ocaml_pos_opt fmt () =
  let kind = severity_string (if equal_severity severity Info then Warning else severity) in
  F.fprintf fmt "%a:%d: %s: %a %a%a@\n" SourceFile.pp loc.Location.file loc.Location.line kind
    IssueType.pp ex_name Localise.pp_error_desc desc L.pp_ocaml_pos_opt ocaml_pos_opt


(** Return true if the exception is not serious and should be handled in timeout mode *)
let handle_exception exn =
  let error = recognize_exception exn in
  equal_visibility error.visibility Exn_user || equal_visibility error.visibility Exn_developer
