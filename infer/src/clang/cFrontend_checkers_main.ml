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
open Lexing
open Ctl_lexer

let parse_ctl_file filename =
  let print_position _ lexbuf =
    let pos = lexbuf.lex_curr_p in
    Logging.err "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) in
  let parse_with_error lexbuf =
    try Some (Ctl_parser.checkers_list token lexbuf) with
    | SyntaxError msg ->
        Logging.err "%a: %s\n" print_position lexbuf msg;
        None
    | Ctl_parser.Error ->
        Logging.err "%a: syntax error\n" print_position lexbuf;
        exit (-1) in
  let parse_and_print lexbuf =  match parse_with_error lexbuf with
    | Some l ->
        IList.iter (fun { Ctl_parser_types.name = s; Ctl_parser_types.definitions = defs } ->
            Logging.out "Parsed checker definition: %s\n" s;
            IList.iter (fun d -> match d with
                | Ctl_parser_types.CSet ("report_when", phi) ->
                    Logging.out "    Report when:  \n    %a\n"
                      CTL.Debug.pp_formula phi
                | _ -> ()) defs
          ) l;
    | None -> () in
  match filename with
  | Some fn ->
      let inx = open_in fn in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fn };
      parse_and_print lexbuf;
      close_in inx
  | None ->
      Logging.out "No linters file specified. Nothing to parse.\n"


let rec do_frontend_checks_stmt (context:CLintersContext.context) stmt =
  let open Clang_ast_t in
  let context' = CFrontend_errors.run_frontend_checkers_on_stmt context stmt in
  let do_all_checks_on_stmts stmt =
    (match stmt with
     | DeclStmt (_, _, decl_list) ->
         IList.iter (do_frontend_checks_decl context') decl_list
     | BlockExpr (_, _, _, decl) ->
         IList.iter (do_frontend_checks_decl context') [decl]
     | _ -> ());
    do_frontend_checks_stmt context' stmt in
  let stmts = Ast_utils.get_stmts_from_stmt stmt in
  IList.iter (do_all_checks_on_stmts) stmts

and do_frontend_checks_decl context decl =
  let open Clang_ast_t in
  let info = Clang_ast_proj.get_decl_tuple decl in
  CLocation.update_curr_file context.CLintersContext.translation_unit_context info;
  let context' =
    (match decl with
     | FunctionDecl(_, _, _, fdi)
     | CXXMethodDecl (_, _, _, fdi, _)
     | CXXConstructorDecl (_, _, _, fdi, _)
     | CXXConversionDecl (_, _, _, fdi, _)
     | CXXDestructorDecl (_, _, _, fdi, _) ->
         let context' = {context with CLintersContext.current_method = Some decl } in
         (match fdi.Clang_ast_t.fdi_body with
          | Some stmt ->
              do_frontend_checks_stmt context' stmt
          | None -> ());
         context'
     | ObjCMethodDecl (_, _, mdi) ->
         let context' = {context with CLintersContext.current_method = Some decl } in
         (match mdi.Clang_ast_t.omdi_body with
          | Some stmt ->
              do_frontend_checks_stmt context' stmt
          | None -> ());
         context'
     | BlockDecl (_, block_decl_info) ->
         let context' = {context with CLintersContext.current_method = Some decl } in
         (match block_decl_info.Clang_ast_t.bdi_body with
          | Some stmt ->
              do_frontend_checks_stmt context' stmt
          | None -> ());
         context'
     | _ -> context) in
  let context'' = CFrontend_errors.run_frontend_checkers_on_decl context' decl in
  let context_with_orig_current_method =
    {context'' with CLintersContext.current_method = context.CLintersContext.current_method } in
  match Clang_ast_proj.get_decl_context_tuple decl with
  | Some (decls, _) -> IList.iter (do_frontend_checks_decl context_with_orig_current_method) decls
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
  create_dir lint_issues_dir;
  let lint_issues_file =
    DB.filename_from_string (Filename.concat lint_issues_dir (abbrev_source_file ^ ".issue")) in
  LintIssues.store_issues lint_issues_file !LintIssues.errLogMap

let do_frontend_checks trans_unit_ctx ast =
  try
    parse_ctl_file Config.linters_def_file;
    let source_file = trans_unit_ctx.CFrontend_config.source_file in
    Logging.out "Start linting file %s@\n" (DB.source_file_to_string source_file);
    match ast with
    | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
        let context =
          context_with_ck_set (CLintersContext.empty trans_unit_ctx) decl_list in
        let is_decl_allowed decl =
          let decl_info = Clang_ast_proj.get_decl_tuple decl in
          CLocation.should_do_frontend_check trans_unit_ctx decl_info.Clang_ast_t.di_source_range in
        let allowed_decls = IList.filter is_decl_allowed decl_list in
        IList.iter (do_frontend_checks_decl context) allowed_decls;
        if (LintIssues.exists_issues ()) then
          store_issues source_file;
        Logging.out "End linting file %s@\n" (DB.source_file_to_string source_file);
        CTL.save_dotty_when_in_debug_mode trans_unit_ctx.CFrontend_config.source_file;
    | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)
  with
  | Assert_failure (file, line, column) as exn ->
      Logging.err "Fatal error: exception Assert_failure(%s, %d, %d)@\n%!" file line column;
      raise exn
