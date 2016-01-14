(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Module for warnings detected at translation time by the frontend
 * To specify a checker define the following:
 * - condition: a boolean condition when the warning should be flagged
 * - description: a string describing the warning
 * - loc: the location where is occurs
*)

open Utils
open CFrontend_utils
open General_utils

(* List of checkers on properties *)
let property_checkers_list = [CFrontend_checkers.checker_strong_delegate_warning]

(* List of checkers on ivar access *)
let ivar_access_checker_list =  [CFrontend_checkers.direct_atomic_property_access]

(* List of checkers for captured vars in objc blocks *)
let captured_vars_checker_list = [CFrontend_checkers.captured_cxx_ref_in_objc_block]

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_warning pdesc warn_desc =
  let loc = warn_desc.loc in
  let errlog = Cfg.Procdesc.get_err_log pdesc in
  let err_desc =
    Errdesc.explain_frontend_warning warn_desc.description warn_desc.suggestion loc in
  let exn = Exceptions.Frontend_warning
      (warn_desc.name, err_desc,
       try assert false with Assert_failure x -> x) in
  Reporting.log_error_from_errlog errlog exn ~loc:(Some loc)

(* Call all checkers on properties of class c *)
let rec check_for_property_errors cfg cg tenv class_name decl_list =
  let open Clang_ast_t in
  let do_one_property decl_info pname_info pdi =
    IList.iter (fun checker ->
        let (condition, warning_desc) = checker decl_info pname_info pdi in
        if condition then
          let proc_desc =
            CMethod_trans.get_method_for_frontend_checks cfg cg tenv class_name decl_info in
          log_frontend_warning proc_desc warning_desc) property_checkers_list in
  match decl_list with
  | [] -> ()
  | ObjCPropertyDecl (decl_info, pname_info, pdi) :: rest ->
      do_one_property decl_info pname_info pdi;
      check_for_property_errors cfg cg tenv class_name rest
  | _ :: rest  ->
      check_for_property_errors cfg cg tenv class_name rest

(* Call checkers on a specific access of an ivar *)
let check_for_ivar_errors context stmt_info obj_c_ivar_ref_expr_info =
  let dr_name = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref.Clang_ast_t.dr_name in
  let pdesc = CContext.get_procdesc context in
  IList.iter (fun checker ->
      match checker context stmt_info dr_name with
      | true, Some warning_desc -> log_frontend_warning pdesc warning_desc
      | _, _ -> ()) ivar_access_checker_list

let check_for_captured_vars context stmt_info captured_vars =
  let pdesc = CContext.get_procdesc context in
  IList.iter (fun checker ->
      match checker stmt_info captured_vars with
      | true, Some warning_desc -> log_frontend_warning pdesc warning_desc
      | _, _ -> ()) captured_vars_checker_list
