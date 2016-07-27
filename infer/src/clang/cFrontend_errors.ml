(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open CFrontend_utils

(* List of checkers on properties *)
let property_checkers_list = [CFrontend_checkers.strong_delegate_warning;
                              CFrontend_checkers.assign_pointer_warning;]

(* Invocation of checker belonging to property_checker_list *)
let checkers_for_property decl_info pname_info pdi checker =
  checker decl_info pname_info pdi

(* List of checkers on ivar access *)
let ivar_access_checker_list =  [CFrontend_checkers.direct_atomic_property_access_warning]

(* Invocation of checker belonging to ivar_access_checker_list *)
let checkers_for_ivar stmt_info method_decl ivar_decl_ref checker =
  checker stmt_info method_decl ivar_decl_ref

(* List of checkers for captured vars in objc blocks *)
let captured_vars_checker_list = [CFrontend_checkers.captured_cxx_ref_in_objc_block_warning]

(* Invocation of checker belonging to captured_vars_checker_list *)
let checkers_for_capture_vars stmt_info captured_vars checker =
  checker stmt_info captured_vars

(* List of checkers on NSNotificationCenter *)
let ns_notification_checker_list = [CFrontend_checkers.checker_NSNotificationCenter]

(* Invocation of checker belonging to ns_notification_center_list *)
let checkers_for_ns decl_info decls checker =
  checker decl_info decls

(* List of checkers on global variables *)
let global_var_checker_list = [CFrontend_checkers.global_var_init_with_calls_warning]

(* Invocation of checker belonging to global_var_checker_list *)
let checker_for_global_var dec checker =
  checker dec

let errLogMap = ref Procname.Map.empty

let get_err_log cfg cg method_decl_opt loc =
  if Config.linters_mode_enabled then
    let procname = match method_decl_opt with
      | Some method_decl -> General_utils.procname_of_decl method_decl
      | None -> General_utils.get_procname_for_frontend_checks loc in
    try Procname.Map.find procname !errLogMap
    with Not_found ->
      let errlog = Errlog.empty () in
      errLogMap := Procname.Map.add procname errlog !errLogMap; errlog
  else
    let pdesc = CMethod_trans.get_method_for_frontend_checks cfg cg loc in
    Cfg.Procdesc.get_err_log pdesc

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue cfg cg method_decl_opt issue_desc =
  let issue = issue_desc.CIssue.issue in
  let loc = issue_desc.CIssue.loc in
  let errlog = get_err_log cfg cg method_decl_opt loc in
  let err_desc = Errdesc.explain_frontend_warning issue_desc.CIssue.description
      issue_desc.CIssue.suggestion loc in
  let name = CIssue.to_string issue in
  let exn = Exceptions.Frontend_warning (name, err_desc, __POS__) in
  let trace = [
    { Errlog.lt_level = 0;
      Errlog.lt_loc = issue_desc.CIssue.loc;
      Errlog.lt_description = "";
      Errlog.lt_node_tags = []}] in
  let err_kind = CIssue.severity_of_issue issue in
  Reporting.log_issue_from_errlog err_kind errlog exn ~loc:(Some loc) ~ltr:(Some trace)

(* General invocation function for checkers
   Takes
   1. f a particular way to apply a checker, it's a partial function
   2. context
   3. the list of checkers to be applied *)
let invoke_set_of_checkers f cfg cg method_decl_opt checkers  =
  IList.iter (fun checker ->
      match f checker with
      | Some issue_desc -> log_frontend_issue cfg cg method_decl_opt issue_desc
      | None -> ()) checkers

let run_frontend_checkers_on_stmt cfg cg method_decl instr =
  let open Clang_ast_t in
  match instr with
  | ObjCIvarRefExpr(stmt_info, _, _, obj_c_ivar_ref_expr_info) ->
      let dr_ref = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref in
      let call_checker_for_ivar = checkers_for_ivar method_decl stmt_info dr_ref in
      let method_decl_opt = Some method_decl in
      invoke_set_of_checkers call_checker_for_ivar cfg cg method_decl_opt ivar_access_checker_list
  | BlockExpr(stmt_info, _ , _, Clang_ast_t.BlockDecl (_, block_decl_info)) ->
      let captured_block_vars = block_decl_info.Clang_ast_t.bdi_captured_variables in
      let call_captured_vars_checker = checkers_for_capture_vars stmt_info captured_block_vars in
      let decl_opt = Some method_decl in
      invoke_set_of_checkers call_captured_vars_checker cfg cg decl_opt captured_vars_checker_list
  | _ -> ()

let run_frontend_checkers_on_decl cfg cg dec =
  let open Clang_ast_t in
  let decl_info = Clang_ast_proj.get_decl_tuple dec in
  if CLocation.should_do_frontend_check decl_info.Clang_ast_t.di_source_range then
    match dec with
    | ObjCImplementationDecl (decl_info, _, decl_list, _, _)
    | ObjCProtocolDecl (decl_info, _, decl_list, _, _) ->
        let call_ns_checker = checkers_for_ns decl_info decl_list in
        invoke_set_of_checkers call_ns_checker cfg cg None ns_notification_checker_list
    | VarDecl _ ->
        let call_global_checker = checker_for_global_var dec in
        invoke_set_of_checkers call_global_checker cfg cg None global_var_checker_list
    | ObjCPropertyDecl (decl_info, pname_info, pdi) ->
        let call_property_checker = checkers_for_property decl_info pname_info pdi in
        invoke_set_of_checkers call_property_checker cfg cg None property_checkers_list
    | _ -> ()
