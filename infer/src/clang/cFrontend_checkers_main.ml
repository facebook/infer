(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils
open CFrontend_utils

let rec do_frontend_checks_stmt (context:CLintersContext.context) cfg cg stmt =
  let open Clang_ast_t in
  let context' = CFrontend_errors.run_frontend_checkers_on_stmt context cfg cg stmt in
  let do_all_checks_on_stmts stmt =
    (match stmt with
     | DeclStmt (_, _, decl_list) ->
         IList.iter (do_frontend_checks_decl context' cfg cg) decl_list
     | BlockExpr (_, _, _, decl) ->
         IList.iter (do_frontend_checks_decl context' cfg cg) [decl]
     | _ -> ());
    do_frontend_checks_stmt context' cfg cg stmt in
  let stmts = Ast_utils.get_stmts_from_stmt stmt in
  IList.iter (do_all_checks_on_stmts) stmts

and do_frontend_checks_decl context cfg cg decl =
  let open Clang_ast_t in
  let info = Clang_ast_proj.get_decl_tuple decl in
  CLocation.update_curr_file info;
  (match decl with
   | FunctionDecl(_, _, _, fdi)
   | CXXMethodDecl (_, _, _, fdi, _)
   | CXXConstructorDecl (_, _, _, fdi, _)
   | CXXConversionDecl (_, _, _, fdi, _)
   | CXXDestructorDecl (_, _, _, fdi, _) ->
       (match fdi.Clang_ast_t.fdi_body with
        | Some stmt ->
            let context = {context with CLintersContext.current_method = Some decl } in
            do_frontend_checks_stmt context cfg cg stmt
        | None -> ())
   | ObjCMethodDecl (_, _, mdi) ->
       (match mdi.Clang_ast_t.omdi_body with
        | Some stmt ->
            let context = {context with CLintersContext.current_method = Some decl } in
            do_frontend_checks_stmt context cfg cg stmt
        | None -> ())
   | BlockDecl (_, block_decl_info) ->
       (match block_decl_info.Clang_ast_t.bdi_body with
        | Some stmt ->
            let context = {context with CLintersContext.current_method = Some decl } in
            do_frontend_checks_stmt context cfg cg stmt
        | None -> ())
   | _ -> ());
  let context' = CFrontend_errors.run_frontend_checkers_on_decl context cfg cg decl in
  match Clang_ast_proj.get_decl_context_tuple decl with
  | Some (decls, _) -> IList.iter (do_frontend_checks_decl context' cfg cg) decls
  | None -> ()

let context_with_ck_set context decl_list =
  let is_ck = context.CLintersContext.is_ck_translation_unit
              || ComponentKit.contains_ck_impl decl_list in
  if is_ck then
    { context with CLintersContext.is_ck_translation_unit = true }
  else
    context

let store_issues source_file =
  let abbrev_source_file = DB.source_file_encoding source_file in
  let lint_issues_dir = Config.results_dir // Config.lint_issues_dir_name in
  DB.create_dir lint_issues_dir;
  let lint_issues_file =
    DB.filename_from_string (Filename.concat lint_issues_dir (abbrev_source_file ^ ".issue")) in
  LintIssues.store_issues lint_issues_file !LintIssues.errLogMap

let do_frontend_checks cfg cg source_file ast =
  Printing.log_stats "Start linting file %s\n" (DB.source_file_to_string source_file);
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      let context = context_with_ck_set CLintersContext.empty decl_list in
      let is_decl_allowed decl =
        let decl_info = Clang_ast_proj.get_decl_tuple decl in
        CLocation.should_do_frontend_check decl_info.Clang_ast_t.di_source_range in
      let allowed_decls = IList.filter is_decl_allowed decl_list in
      IList.iter (do_frontend_checks_decl context cfg cg) allowed_decls;
      (* TODO (t12740727): Remove condition once the transition to linters mode is finished *)
      if Config.clang_frontend_action = `Lint && (LintIssues.exists_issues ()) then
        store_issues source_file;
      Printing.log_stats "End linting file %s\n" (DB.source_file_to_string source_file)
  | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)
