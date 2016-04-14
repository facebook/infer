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
let checkers_for_ivar context stmt_info dr_name checker =
  checker context stmt_info dr_name

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

(* Add a frontend warning with a description desc at location loc to the errlog of a proc desc *)
let log_frontend_warning pdesc warn_desc =
  let open CFrontend_checkers in
  let loc = warn_desc.loc in
  let errlog = Cfg.Procdesc.get_err_log pdesc in
  let err_desc =
    Errdesc.explain_frontend_warning warn_desc.description warn_desc.suggestion loc in
  let exn = Exceptions.Frontend_warning
      (warn_desc.name, err_desc, __POS__) in
  let trace = [
    { Errlog.lt_level = 0;
      Errlog.lt_loc = warn_desc.loc;
      Errlog.lt_description = "";
      Errlog.lt_node_tags = []}] in
  Reporting.log_error_from_errlog errlog exn ~loc:(Some loc) ~ltr:(Some trace)

(* General invocation function for checkers
   Takes
   1. f a particular way to apply a checker, it's a partial function
   2. context
   3. the list of checkers to be applied *)
let invoke_set_of_checkers f cfg cg pdesc_opt checkers  =
  IList.iter (fun checker ->
      match f checker with
      | Some warning_desc ->
          let pdesc =
            match pdesc_opt with
            | Some pdesc -> pdesc
            | None ->
                let loc = warning_desc.CFrontend_checkers.loc in
                CMethod_trans.get_method_for_frontend_checks cfg cg loc in
          log_frontend_warning pdesc warning_desc
      | None -> ()) checkers

(* Call all checkers on properties of class c *)
let rec check_for_property_errors cfg cg decl_list =
  let open Clang_ast_t in
  let do_one_property decl_info pname_info pdi =
    let call_property_checker = checkers_for_property decl_info pname_info pdi in
    invoke_set_of_checkers call_property_checker cfg cg None property_checkers_list in
  match decl_list with
  | [] -> ()
  | ObjCPropertyDecl (decl_info, pname_info, pdi) :: rest ->
      do_one_property decl_info pname_info pdi;
      check_for_property_errors cfg cg rest
  | _ :: rest  ->
      check_for_property_errors cfg cg rest

let get_categories_decls decl_ref =
  match Ast_utils.get_decl_opt_with_decl_ref decl_ref with
  | Some ObjCCategoryDecl (_, _, decls, _, _)
  | Some ObjCInterfaceDecl (_, _, decls, _, _) -> decls
  | _ -> []

let run_frontend_checkers_on_stmt trans_state instr =
  let open Clang_ast_t in
  let context = trans_state.CTrans_utils.context in
  let pdesc = CContext.get_procdesc context in
  let cg = context.CContext.cg in
  let cfg = context.CContext.cfg in
  match instr with
  | ObjCIvarRefExpr(stmt_info, _, _, obj_c_ivar_ref_expr_info) ->
      let dr_name = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref.Clang_ast_t.dr_name in
      let call_checker_for_ivar = checkers_for_ivar context stmt_info dr_name in
      invoke_set_of_checkers call_checker_for_ivar cfg cg (Some pdesc) ivar_access_checker_list
  | BlockExpr(stmt_info, _ , _, Clang_ast_t.BlockDecl (_, block_decl_info)) ->
      let captured_block_vars = block_decl_info.Clang_ast_t.bdi_captured_variables in
      let captured_vars = CVar_decl.captured_vars_from_block_info context captured_block_vars in
      let call_captured_vars_checker =  checkers_for_capture_vars stmt_info captured_vars in
      let pdesc_opt = Some pdesc in
      invoke_set_of_checkers call_captured_vars_checker cfg cg pdesc_opt captured_vars_checker_list
  | _ -> ()

let rec run_frontend_checkers_on_decl cfg cg dec =
  let open Clang_ast_t in
  match dec with
  | ObjCCategoryImplDecl(_, _, decl_list, _, ocidi) ->
      let decls = (get_categories_decls ocidi.Clang_ast_t.ocidi_category_decl) @ decl_list in
      check_for_property_errors cfg cg decls;
      IList.iter (run_frontend_checkers_on_decl cfg cg) decl_list
  | ObjCImplementationDecl(decl_info, _, decl_list, _, idi) ->
      let decls = (get_categories_decls idi.Clang_ast_t.oidi_class_interface) @ decl_list in
      check_for_property_errors cfg cg decls;
      let call_ns_checker = checkers_for_ns decl_info decl_list in
      invoke_set_of_checkers call_ns_checker cfg cg None ns_notification_checker_list;
      IList.iter (run_frontend_checkers_on_decl cfg cg) decl_list
  | ObjCProtocolDecl (decl_info, _, decl_list, _, _) ->
      if CLocation.should_do_frontend_check decl_info.Clang_ast_t.di_source_range then
        (check_for_property_errors cfg cg decl_list;
         let call_ns_checker = checkers_for_ns decl_info decl_list in
         invoke_set_of_checkers call_ns_checker cfg cg None ns_notification_checker_list;
         IList.iter (run_frontend_checkers_on_decl cfg cg) decl_list)
      else ()
  | _ -> ()
