(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
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

exception Biabd_use_after_free of Localise.error_desc * L.ocaml_pos

exception Cannot_star of L.ocaml_pos

exception Class_cast_exception of Localise.error_desc * L.ocaml_pos

exception Condition_always_true_false of Localise.error_desc * bool * L.ocaml_pos

exception Custom_error of string * Localise.error_desc

exception Dummy_exception of Localise.error_desc

exception
  Dangling_pointer_dereference of bool (* is it user visible? *) * Localise.error_desc * L.ocaml_pos

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

exception
  Leak of
    bool
    * (bool (* is it user visible? *) * Localise.error_desc)
    * bool
    * PredSymb.resource
    * L.ocaml_pos

exception Missing_fld of Fieldname.t * L.ocaml_pos

exception Premature_nil_termination of Localise.error_desc * L.ocaml_pos

exception Null_dereference of Localise.error_desc * L.ocaml_pos

exception Null_test_after_dereference of Localise.error_desc * L.ocaml_pos

exception Parameter_not_null_checked of Localise.error_desc * L.ocaml_pos

exception Pointer_size_mismatch of Localise.error_desc * L.ocaml_pos

exception Precondition_not_found of Localise.error_desc * L.ocaml_pos

exception Precondition_not_met of Localise.error_desc * L.ocaml_pos

exception Retain_cycle of Localise.error_desc * L.ocaml_pos

exception Registered_observer_being_deallocated of Localise.error_desc * L.ocaml_pos

exception Skip_function of Localise.error_desc

exception Skip_pointer_dereference of Localise.error_desc * L.ocaml_pos

exception Stack_variable_address_escape of Localise.error_desc * L.ocaml_pos

exception Symexec_memory_error of L.ocaml_pos

exception Unary_minus_applied_to_unsigned_expression of Localise.error_desc * L.ocaml_pos

exception Wrong_argument_number of L.ocaml_pos

type t =
  { issue_type: IssueType.t
  ; description: Localise.error_desc
  ; ocaml_pos: L.ocaml_pos option  (** location in the infer source code *)
  ; visibility: visibility
  ; severity: severity option }

let recognize_exception exn =
  match exn with
  | Abduction_case_not_implemented ocaml_pos ->
      { issue_type= IssueType.abduction_case_not_implemented
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Analysis_stops (desc, ocaml_pos_opt) ->
      let visibility = if Config.analysis_stops then Exn_user else Exn_developer in
      { issue_type= IssueType.analysis_stops
      ; description= desc
      ; ocaml_pos= ocaml_pos_opt
      ; visibility
      ; severity= None }
  | Array_of_pointsto ocaml_pos ->
      { issue_type= IssueType.array_of_pointsto
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Array_out_of_bounds_l1 (desc, ocaml_pos) ->
      { issue_type= IssueType.array_out_of_bounds_l1
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= Some Error }
  | Array_out_of_bounds_l2 (desc, ocaml_pos) ->
      { issue_type= IssueType.array_out_of_bounds_l2
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Array_out_of_bounds_l3 (desc, ocaml_pos) ->
      { issue_type= IssueType.array_out_of_bounds_l3
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Assert_failure (f, l, c) ->
      let ocaml_pos = (f, l, c, c) in
      { issue_type= IssueType.assert_failure
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Bad_footprint ocaml_pos ->
      { issue_type= IssueType.bad_footprint
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Biabd_use_after_free (desc, ocaml_pos) ->
      { issue_type= IssueType.biabd_use_after_free
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Cannot_star ocaml_pos ->
      { issue_type= IssueType.cannot_star
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Class_cast_exception (desc, ocaml_pos) ->
      { issue_type= IssueType.class_cast_exception
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Condition_always_true_false (desc, b, ocaml_pos) ->
      let issue_type =
        if b then IssueType.biabd_condition_always_true else IssueType.biabd_condition_always_false
      in
      { issue_type
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Custom_error (error_msg, desc) ->
      { issue_type= IssueType.register_from_string ~id:error_msg Biabduction
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None }
  | Dummy_exception desc ->
      { issue_type= IssueType.analysis_stops
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= Some Info }
  | Dangling_pointer_dereference (user_visible, desc, ocaml_pos) ->
      let issue_type, visibility =
        if user_visible then (IssueType.dangling_pointer_dereference, Exn_user)
        else (IssueType.dangling_pointer_dereference_maybe, Exn_developer)
      in
      {issue_type; description= desc; ocaml_pos= Some ocaml_pos; visibility; severity= None}
  | Deallocate_stack_variable desc ->
      { issue_type= IssueType.deallocate_stack_variable
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None }
  | Deallocate_static_memory desc ->
      { issue_type= IssueType.deallocate_static_memory
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_user
      ; severity= None }
  | Deallocation_mismatch (desc, ocaml_pos) ->
      { issue_type= IssueType.deallocation_mismatch
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Divide_by_zero (desc, ocaml_pos) ->
      { issue_type= IssueType.divide_by_zero
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error }
  | Eradicate (kind, desc) ->
      {issue_type= kind; description= desc; ocaml_pos= None; visibility= Exn_user; severity= None}
  | Empty_vector_access (desc, ocaml_pos) ->
      { issue_type= IssueType.empty_vector_access
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error }
  | Field_not_null_checked (desc, ocaml_pos) ->
      { issue_type= IssueType.field_not_null_checked
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Warning }
  | Frontend_warning (issue_type, desc, ocaml_pos) ->
      { issue_type
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Checkers (kind, desc) ->
      {issue_type= kind; description= desc; ocaml_pos= None; visibility= Exn_user; severity= None}
  | Null_dereference (desc, ocaml_pos) ->
      { issue_type= IssueType.null_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Null_test_after_dereference (desc, ocaml_pos) ->
      { issue_type= IssueType.null_test_after_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Pointer_size_mismatch (desc, ocaml_pos) ->
      { issue_type= IssueType.pointer_size_mismatch
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error }
  | Inherently_dangerous_function desc ->
      { issue_type= IssueType.inherently_dangerous_function
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None }
  | Internal_error desc ->
      { issue_type= IssueType.internal_error
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None }
  | Leak (fp_part, (user_visible, error_desc), done_array_abstraction, resource, ocaml_pos) ->
      if done_array_abstraction then
        { issue_type= IssueType.leak_after_array_abstraction
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_developer
        ; severity= None }
      else if fp_part then
        { issue_type= IssueType.leak_in_footprint
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_developer
        ; severity= None }
      else if not user_visible then
        { issue_type= IssueType.leak_unknown_origin
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_developer
        ; severity= None }
      else
        let issue_type =
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
        { issue_type
        ; description= error_desc
        ; ocaml_pos= Some ocaml_pos
        ; visibility= Exn_user
        ; severity= None }
  | Missing_fld (fld, ocaml_pos) ->
      let desc = Localise.verbatim_desc (Fieldname.to_full_string fld) in
      { issue_type= IssueType.missing_fld
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Premature_nil_termination (desc, ocaml_pos) ->
      { issue_type= IssueType.premature_nil_termination
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Parameter_not_null_checked (desc, ocaml_pos) ->
      { issue_type= IssueType.parameter_not_null_checked
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Warning }
  | Precondition_not_found (desc, ocaml_pos) ->
      { issue_type= IssueType.precondition_not_found
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Precondition_not_met (desc, ocaml_pos) ->
      { issue_type= IssueType.precondition_not_met
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= Some Warning }
      (* always a warning *)
  | Retain_cycle (desc, ocaml_pos) ->
      { issue_type= IssueType.retain_cycle
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Registered_observer_being_deallocated (desc, ocaml_pos) ->
      { issue_type= IssueType.biabd_registered_observer_being_deallocated
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error }
  | Stack_variable_address_escape (desc, ocaml_pos) ->
      { issue_type= IssueType.biabd_stack_variable_address_escape
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Error }
  | SymOp.Analysis_failure_exe _ ->
      { issue_type= IssueType.failure_exe
      ; description= Localise.no_desc
      ; ocaml_pos= None
      ; visibility= Exn_system
      ; severity= None }
  | Skip_function desc ->
      { issue_type= IssueType.skip_function
      ; description= desc
      ; ocaml_pos= None
      ; visibility= Exn_developer
      ; severity= None }
  | Skip_pointer_dereference (desc, ocaml_pos) ->
      { issue_type= IssueType.skip_pointer_dereference
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= Some Info }
      (* always an info *)
  | Symexec_memory_error ocaml_pos ->
      { issue_type= IssueType.symexec_memory_error
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | Unary_minus_applied_to_unsigned_expression (desc, ocaml_pos) ->
      { issue_type= IssueType.unary_minus_applied_to_unsigned_expression
      ; description= desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_user
      ; severity= None }
  | Wrong_argument_number ocaml_pos ->
      { issue_type= IssueType.wrong_argument_number
      ; description= Localise.no_desc
      ; ocaml_pos= Some ocaml_pos
      ; visibility= Exn_developer
      ; severity= None }
  | exn ->
      { issue_type= IssueType.failure_exe
      ; description=
          Localise.verbatim_desc (F.asprintf "%a: %s" Exn.pp exn (Caml.Printexc.get_backtrace ()))
      ; ocaml_pos= None
      ; visibility= Exn_system
      ; severity= None }


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
  L.d_printfln ~color:Red "%s%s %a%s" s error.issue_type.unique_id Localise.pp_error_desc
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


(** pretty print an error *)
let pp_err loc severity issue_type desc ocaml_pos_opt fmt () =
  let kind = severity_string (if equal_severity severity Info then Warning else severity) in
  F.fprintf fmt "%a:%d: %s: %a %a%a@\n" SourceFile.pp loc.Location.file loc.Location.line kind
    IssueType.pp issue_type Localise.pp_error_desc desc L.pp_ocaml_pos_opt ocaml_pos_opt


(** Return true if the exception is not serious and should be handled in timeout mode *)
let handle_exception exn =
  let error = recognize_exception exn in
  equal_visibility error.visibility Exn_user || equal_visibility error.visibility Exn_developer
