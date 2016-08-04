(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

let rec do_frontend_checks_stmt (context:CLintersContext.context) cfg cg method_decl stmt =
  let context' = CFrontend_errors.run_frontend_checkers_on_stmt context cfg cg method_decl stmt in
  let stmts = CFrontend_utils.Ast_utils.get_stmts_from_stmt stmt in
  let do_all_checks_on_stmts stmt =
    (match stmt with
     | Clang_ast_t.DeclStmt (_, _, decl_list) ->
         IList.iter (do_frontend_checks_decl context' cfg cg) decl_list
     | _ -> ());
    do_frontend_checks_stmt context' cfg cg method_decl stmt in
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
        | Some stmt -> do_frontend_checks_stmt context cfg cg decl stmt
        | None -> ())
   | ObjCMethodDecl (_, _, mdi) ->
       (match mdi.Clang_ast_t.omdi_body with
        | Some stmt -> do_frontend_checks_stmt context cfg cg decl stmt
        | None -> ())
   | _ -> ());
  let context' = CFrontend_errors.run_frontend_checkers_on_decl context cfg cg decl in
  match Clang_ast_proj.get_decl_context_tuple decl with
  | Some (decls, _) -> IList.iter (do_frontend_checks_decl context' cfg cg) decls
  | None -> ()

let do_frontend_checks cfg cg ast =
  match ast with
  | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
      IList.iter (do_frontend_checks_decl CLintersContext.empty cfg cg) decl_list
  | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)
