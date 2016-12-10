(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

open CFrontend_utils

(* List of checkers on properties *)
let decl_checkers_list = [CFrontend_checkers.ctl_strong_delegate_warning;
                          CFrontend_checkers.ctl_assign_pointer_warning;
                          CFrontend_checkers.ctl_ns_notification_warning;
                          CFrontend_checkers.ctl_global_var_init_with_calls_warning;
                          ComponentKit.component_with_unconventional_superclass_advice;
                          ComponentKit.mutable_local_vars_advice;
                          ComponentKit.component_factory_function_advice;
                          ComponentKit.component_file_cyclomatic_complexity_info;
                          ComponentKit.component_with_multiple_factory_methods_advice;]

(* List of checkers on ivar access *)
let stmt_checkers_list =  [CFrontend_checkers.ctl_direct_atomic_property_access_warning;
                           CFrontend_checkers.ctl_captured_cxx_ref_in_objc_block_warning;
                           CFrontend_checkers.ctl_bad_pointer_comparison_warning;
                           ComponentKit.component_file_cyclomatic_complexity_info;
                           ComponentKit.component_initializer_with_side_effects_advice;]

(* List of checkers on translation unit that potentially output multiple issues *)
let translation_unit_checkers_list = [ComponentKit.component_file_line_count_info;]






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
  let trace = [ Errlog.make_trace_element 0 issue_desc.CIssue.loc "" [] ] in
  let err_kind = CIssue.severity_of_issue issue in
  let method_name = Ast_utils.full_name_of_decl_opt method_decl_opt in
  let key = Hashtbl.hash (key ^ method_name) in
  Reporting.log_issue_from_errlog err_kind errlog exn ~loc ~ltr:trace
    ~node_id:(0, key)

let invoke_set_of_checkers_an an context =
  let checkers, key  = match an with
    | CTL.Decl dec -> decl_checkers_list, Ast_utils.generate_key_decl dec
    | CTL.Stmt st -> stmt_checkers_list, Ast_utils.generate_key_stmt st in
  IList.iter (fun checker ->
      let condition, issue_desc_opt = checker context an in
      match  CTL.eval_formula condition an context, issue_desc_opt with
      | true, Some issue_desc -> log_frontend_issue context.CLintersContext.translation_unit_context
                                   context.CLintersContext.current_method key issue_desc
      | _, _ -> ()) checkers


let run_frontend_checkers_on_an (context: CLintersContext.context) an =
  let open Clang_ast_t in
  let context' = match an with
    | CTL.Decl (ObjCImplementationDecl _ as dec) ->
        {context with current_objc_impl = Some dec}
    | CTL.Stmt (ObjCAtSynchronizedStmt _ )->
        { context with CLintersContext.in_synchronized_block = true }
    | _ -> context in
  invoke_set_of_checkers_an an context';
  context'

let run_translation_unit_checker (context: CLintersContext.context) dec =
  IList.iter (fun checker ->
      let issue_desc_list = checker context dec in
      IList.iter (fun issue_desc ->
          let key = Ast_utils.generate_key_decl dec in
          log_frontend_issue context.CLintersContext.translation_unit_context
            context.CLintersContext.current_method key issue_desc
        ) issue_desc_list) translation_unit_checkers_list
