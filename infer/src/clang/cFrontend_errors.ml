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
let checkers_for_property decl_info pname_info pdi checker context =
  checker context decl_info pname_info pdi

(* List of checkers on ivar access *)
let ivar_access_checker_list =  [CFrontend_checkers.direct_atomic_property_access_warning]

(* Invocation of checker belonging to ivar_access_checker_list *)
let checkers_for_ivar stmt_info ivar_decl_ref checker context =
  checker context stmt_info ivar_decl_ref

(* List of checkers for captured vars in objc blocks *)
let captured_vars_checker_list = [CFrontend_checkers.captured_cxx_ref_in_objc_block_warning]

(* Invocation of checker belonging to captured_vars_checker_list *)
let checkers_for_capture_vars stmt_info captured_vars checker context =
  checker context stmt_info captured_vars

(* List of checkers on NSNotificationCenter *)
let ns_notification_checker_list = [CFrontend_checkers.checker_NSNotificationCenter]

(* Invocation of checker belonging to ns_notification_center_list *)
let checkers_for_ns decl_info impl_decl_info decls checker context =
  checker context decl_info impl_decl_info decls

(* List of checkers on variables *)
let var_checker_list = [CFrontend_checkers.global_var_init_with_calls_warning;
                        ComponentKit.mutable_local_vars_advice]

(* Invocation of checker belonging to var_checker_list *)
let checker_for_var dec checker context =
  checker context dec

(* List of checkers on conditional operator *)
let conditional_op_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to conditional_op_checker_list *)
let checker_for_conditional_op stmt_info first_stmt checker context =
  checker context stmt_info first_stmt

(* List of checkers on if-statement *)
let if_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to if_stmt_checker_list *)
let checker_for_if_stmt stmt_info cond checker context =
  checker context stmt_info cond

(* List of checkers on for statement *)
let for_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to for_stmt_checker_list *)
let checker_for_for_stmt stmt_info cond checker context =
  checker context stmt_info cond

(* List of checkers on while statement *)
let while_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to while_stmt_checker_list *)
let checker_for_while_stmt stmt_info cond checker context =
  checker context stmt_info cond

let get_err_log cfg cg method_decl_opt loc =
  let procname = match method_decl_opt with
    | Some method_decl -> General_utils.procname_of_decl method_decl
    | None -> General_utils.get_procname_for_frontend_checks loc in
  if Config.clang_frontend_action = `Lint then
    LintIssues.get_err_log procname
  else (* TODO (t12740727): Remove this branch once the transition to linters mode is finished *)
    let pdesc = CMethod_trans.get_method_for_frontend_checks cfg cg loc in
    Cfg.Procdesc.get_err_log pdesc

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue cfg cg method_decl_opt key issue_desc =
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
  let method_name = Ast_utils.full_name_of_decl_opt method_decl_opt in
  let key = Hashtbl.hash (key ^ method_name) in
  Reporting.log_issue_from_errlog err_kind errlog exn ~loc ~ltr:trace
    ~node_id:(0, key)

(* General invocation function for checkers
   Takes
   1. f a particular way to apply a checker, it's a partial function
   2. context
   3. the list of checkers to be applied *)
let invoke_set_of_checkers f context cfg cg key checkers  =
  IList.iter (fun checker ->
      match f checker context with
      | Some issue_desc ->
          log_frontend_issue cfg cg context.CLintersContext.current_method key issue_desc
      | None -> ()) checkers

let run_frontend_checkers_on_stmt context cfg cg instr =
  let open Clang_ast_t in
  match instr with
  | ObjCIvarRefExpr (stmt_info, _, _, obj_c_ivar_ref_expr_info) ->
      let dr_ref = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref in
      let call_checker_for_ivar = checkers_for_ivar stmt_info dr_ref in
      let key = Ast_utils.generate_key_stmt instr in
      invoke_set_of_checkers
        call_checker_for_ivar context cfg cg key ivar_access_checker_list;
      context
  | BlockExpr (stmt_info, _ , _, Clang_ast_t.BlockDecl (_, block_decl_info)) ->
      let captured_block_vars = block_decl_info.Clang_ast_t.bdi_captured_variables in
      let call_captured_vars_checker = checkers_for_capture_vars stmt_info captured_block_vars in
      let key = Ast_utils.generate_key_stmt instr in
      invoke_set_of_checkers call_captured_vars_checker context cfg cg key
        captured_vars_checker_list;
      context
  | IfStmt (stmt_info, _ :: _ :: cond :: _) ->
      let call_checker = checker_for_if_stmt stmt_info [cond] in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context cfg cg key if_stmt_checker_list;
      context
  | ConditionalOperator (stmt_info, first_stmt :: _, _) ->
      let call_checker = checker_for_conditional_op stmt_info [first_stmt] in
      let key = Ast_utils.generate_key_stmt first_stmt in
      invoke_set_of_checkers call_checker context cfg cg key conditional_op_checker_list;
      context
  | ForStmt (stmt_info, [_; _; cond; _; _]) ->
      let call_checker = checker_for_for_stmt stmt_info [cond] in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context cfg cg key for_stmt_checker_list;
      context
  | WhileStmt (stmt_info, [_; cond; _]) ->
      let call_checker = checker_for_while_stmt stmt_info [cond] in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context cfg cg key while_stmt_checker_list;
      context
  | ObjCAtSynchronizedStmt _ ->
      { context with CLintersContext.in_synchronized_block = true }
  | _ -> context

let run_frontend_checkers_on_decl context cfg cg dec =
  let open Clang_ast_t in
  match dec with
  | ObjCImplementationDecl (decl_info, _, decl_list, _, _)
  | ObjCProtocolDecl (decl_info, _, decl_list, _, _) ->
      let idi = match dec with
        | ObjCImplementationDecl (_, _, _, _, impl_decl_info) -> Some impl_decl_info
        | _ -> None in
      let call_ns_checker = checkers_for_ns decl_info idi decl_list in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_ns_checker context cfg cg key ns_notification_checker_list;
      context
  | VarDecl _ ->
      let call_var_checker = checker_for_var dec in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_var_checker context cfg cg key var_checker_list;
      context
  | ObjCPropertyDecl (decl_info, pname_info, pdi) ->
      let call_property_checker = checkers_for_property decl_info pname_info pdi in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_property_checker context cfg cg key property_checkers_list;
      context
  | _ -> context
