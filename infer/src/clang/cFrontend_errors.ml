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
let checkers_for_property decl checker context =
  checker context decl

(* List of checkers on ivar access *)
let ivar_access_checker_list =  [CFrontend_checkers.direct_atomic_property_access_warning]

(* Invocation of checker belonging to ivar_access_checker_list *)
let checkers_for_ivar stmt checker context =
  checker context stmt

(* List of checkers for captured vars in objc blocks *)
let captured_vars_checker_list = [CFrontend_checkers.captured_cxx_ref_in_objc_block_warning]

(* Invocation of checker belonging to captured_vars_checker_list *)
let checkers_for_capture_vars stmt checker context =
  checker context stmt

(* List of checkers on ObjCProtocol decls *)
let objc_protocol_checker_list = [CFrontend_checkers.checker_NSNotificationCenter]

(* Invocation of checker belonging to objc_protocol_checker_list *)
let checkers_for_objc_protocol decl checker context =
  checker context decl

(* List of checkers running on ObjCImpl decls *)
let objc_impl_checker_list = [CFrontend_checkers.checker_NSNotificationCenter;
                              ComponentKit.component_with_multiple_factory_methods_advice;
                              ComponentKit.component_with_unconventional_superclass_advice]

(* Invocation of checker belonging to objc_impl_checker_list *)
let checkers_for_objc_impl decl checker context =
  checker context decl

let call_expr_checker_list = [ComponentKit.component_initializer_with_side_effects_advice]

(* Invocation of checker belonging to call_expr_checker_list *)
let checkers_for_call_expr stmt checker context =
  checker context stmt

(* List of checkers on variables *)
let var_checker_list = [CFrontend_checkers.global_var_init_with_calls_warning;
                        ComponentKit.mutable_local_vars_advice]

(* Invocation of checker belonging to var_checker_list *)
let checker_for_var dec checker context =
  checker context dec

(* List of checkers on conditional operator *)
let conditional_op_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to conditional_op_checker_list *)
let checker_for_conditional_op first_stmt checker context =
  checker context first_stmt

(* List of checkers on if-statement *)
let if_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to if_stmt_checker_list *)
let checker_for_if_stmt cond checker context =
  checker context cond

(* List of checkers on for statement *)
let for_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to for_stmt_checker_list *)
let checker_for_for_stmt cond checker context =
  checker context cond

(* List of checkers on while statement *)
let while_stmt_checker_list = [CFrontend_checkers.bad_pointer_comparison_warning]

(* Invocation of checker belonging to while_stmt_checker_list *)
let checker_for_while_stmt cond checker context =
  checker context cond

let function_decl_checker_list = [ComponentKit.component_factory_function_advice]

let checker_for_function_decl decl checker context =
  checker context decl

let get_err_log translation_unit_context method_decl_opt =
  let procname = match method_decl_opt with
    | Some method_decl -> General_utils.procname_of_decl translation_unit_context method_decl
    | None -> Procname.Linters_dummy_method in
  LintIssues.get_err_log procname

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_issue translation_unit_context method_decl_opt key issue_desc =
  let issue = issue_desc.CIssue.issue in
  let loc = issue_desc.CIssue.loc in
  let errlog = get_err_log translation_unit_context method_decl_opt in
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
let invoke_set_of_checkers f context key checkers  =
  IList.iter (fun checker ->
      match f checker context with
      | Some issue_desc ->
          log_frontend_issue context.CLintersContext.translation_unit_context
            context.CLintersContext.current_method key issue_desc
      | None -> ()) checkers

let run_frontend_checkers_on_stmt context instr =
  let open Clang_ast_t in
  match instr with
  | ObjCIvarRefExpr _ ->
      let call_checker_for_ivar = checkers_for_ivar instr in
      let key = Ast_utils.generate_key_stmt instr in
      invoke_set_of_checkers
        call_checker_for_ivar context key ivar_access_checker_list;
      context
  | BlockExpr _ ->
      let call_captured_vars_checker = checkers_for_capture_vars instr in
      let key = Ast_utils.generate_key_stmt instr in
      invoke_set_of_checkers call_captured_vars_checker context key
        captured_vars_checker_list;
      context
  | IfStmt (_, _ :: _ :: cond :: _) ->
      let call_checker = checker_for_if_stmt cond in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context key if_stmt_checker_list;
      context
  | ConditionalOperator (_, first_stmt :: _, _) ->
      let call_checker = checker_for_conditional_op first_stmt in
      let key = Ast_utils.generate_key_stmt first_stmt in
      invoke_set_of_checkers call_checker context key conditional_op_checker_list;
      context
  | ForStmt (_, [_; _; cond; _; _]) ->
      let call_checker = checker_for_for_stmt cond in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context key for_stmt_checker_list;
      context
  | WhileStmt (_, [_; cond; _]) ->
      let call_checker = checker_for_while_stmt cond in
      let key = Ast_utils.generate_key_stmt cond in
      invoke_set_of_checkers call_checker context key while_stmt_checker_list;
      context
  | CallExpr (_, called_func_stmt :: _, _) ->
      let call_checker = checkers_for_call_expr called_func_stmt in
      let key = Ast_utils.generate_key_stmt called_func_stmt in
      invoke_set_of_checkers call_checker context key call_expr_checker_list;
      context
  | ObjCAtSynchronizedStmt _ ->
      { context with CLintersContext.in_synchronized_block = true }
  | _ -> context

let run_frontend_checkers_on_decl (context: CLintersContext.context) dec =
  let open Clang_ast_t in
  match dec with
  | ObjCImplementationDecl _ ->
      let call_objc_impl_checker = checkers_for_objc_impl dec in
      let key = Ast_utils.generate_key_decl dec in
      let context' = {context with current_objc_impl = Some dec} in
      invoke_set_of_checkers call_objc_impl_checker context' key objc_impl_checker_list;
      context'
  | ObjCProtocolDecl _ ->
      let call_objc_protocol_checker = checkers_for_objc_protocol dec in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_objc_protocol_checker context key objc_protocol_checker_list;
      context
  | VarDecl _ ->
      let call_var_checker = checker_for_var dec in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_var_checker context key var_checker_list;
      context
  | FunctionDecl _ ->
      let call_function_decl_checker = checker_for_function_decl dec in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_function_decl_checker context key function_decl_checker_list;
      context
  | ObjCPropertyDecl _ ->
      let call_property_checker = checkers_for_property dec in
      let key = Ast_utils.generate_key_decl dec in
      invoke_set_of_checkers call_property_checker context key property_checkers_list;
      context
  | _ -> context
