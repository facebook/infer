(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Lexing
open Ctl_lexer

(* Parse the file with linters definitions, and it returns a list
   of checkers in the form of pairs (condition, issue_desc) *)
let parse_ctl_file linters_files =
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
        Logging.err "\n#######################################################\
                     \n\n%a: SYNTAX ERROR\n\
                     \n########################################################\n@."
          print_position lexbuf;
        exit (-1) in
  List.iter ~f:(fun fn ->
      Logging.out "Loading linters rules from %s\n" fn;
      let inx = open_in fn in
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fn };
      (match parse_with_error lexbuf with
       | Some parsed_checkers ->
           Logging.out "#### Start Expanding checkers #####\n";
           let exp_checkers = CFrontend_errors.expand_checkers parsed_checkers in
           Logging.out "#### Checkers Expanded #####\n";
           if Config.debug_mode then List.iter ~f:CTL.print_checker exp_checkers;
           CFrontend_errors.make_condition_issue_desc_pair exp_checkers;
       | None -> Logging.out "No linters found.\n");
      In_channel.close inx) linters_files


let rec get_responds_to_selector stmt =
  let open Clang_ast_t in
  match stmt with
  | ObjCMessageExpr (_, [_; ObjCSelectorExpr (_, _, _, method_name)], _, mdi)
  | ObjCMessageExpr (_, [ObjCSelectorExpr (_, _, _, method_name)], _, mdi)
    when  String.equal mdi.Clang_ast_t.omei_selector "respondsToSelector:" ->
      [method_name]
  | BinaryOperator (_, [stmt1;stmt2], _, bo_info)
    when PVariant.(=) bo_info.Clang_ast_t.boi_kind `LAnd ->
      List.append (get_responds_to_selector stmt1) (get_responds_to_selector stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups(_, [stmt], _, _) ->
      get_responds_to_selector stmt
  | _ -> []

let rec is_core_foundation_version_number stmt =
  let open Clang_ast_t in
  match stmt with
  | DeclRefExpr (_, _, _, decl_ref_info) ->
      (match decl_ref_info.drti_decl_ref with
       | Some decl_ref_info ->
           let name_info, _, _ = CAst_utils.get_info_from_decl_ref  decl_ref_info in
           String.equal name_info.ni_name "kCFCoreFoundationVersionNumber"
       | None -> false)
  | ImplicitCastExpr (_, [stmt], _, _) ->
      is_core_foundation_version_number stmt
  | _ -> false

let rec current_os_version_constant stmt =
  let open Clang_ast_t in
  match stmt with
  | FloatingLiteral (_, _, _, number) ->
      CiOSVersionNumbers.version_of number
  | IntegerLiteral (_, _, _, info) ->
      CiOSVersionNumbers.version_of info.ili_value
  | ImplicitCastExpr (_, [stmt], _, _) ->
      current_os_version_constant stmt
  | _ -> None

let rec get_current_os_version stmt =
  let open Clang_ast_t in
  match stmt with
  | BinaryOperator (_, [stmt1;stmt2], _, bo_info) when
      PVariant.(=) bo_info.Clang_ast_t.boi_kind `GE &&
      is_core_foundation_version_number stmt1 ->
      Option.to_list (current_os_version_constant stmt2)
  | BinaryOperator (_, [stmt1;stmt2], _, bo_info) when
      PVariant.(=) bo_info.Clang_ast_t.boi_kind `LE &&
      is_core_foundation_version_number stmt2 ->
      Option.to_list (current_os_version_constant stmt1)
  | BinaryOperator (_, [stmt1;stmt2], _, bo_info) when
      PVariant.(=) bo_info.Clang_ast_t.boi_kind `LAnd ->
      List.append (get_current_os_version stmt1) (get_current_os_version stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups(_, [stmt], _, _) ->
      get_current_os_version stmt
  | _ -> []

let compute_if_context (context:CLintersContext.context) stmt =
  let selector = get_responds_to_selector stmt in
  let os_version = get_current_os_version stmt in
  let within_responds_to_selector_block, ios_version_guard =
    match context.if_context with
    | Some if_context ->
        let within_responds_to_selector_block =
          List.append selector if_context.within_responds_to_selector_block in
        let ios_version_guard =
          List.append os_version if_context.ios_version_guard in
        within_responds_to_selector_block, ios_version_guard
    | None -> selector, os_version in
  Some ({within_responds_to_selector_block; ios_version_guard} : CLintersContext.if_context)

let is_factory_method (context: CLintersContext.context) decl =
  let interface_decl_opt =
    (match context.current_objc_impl with
     | Some ObjCImplementationDecl (_, _, _, _, impl_decl_info) ->
         CAst_utils.get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface
     | _ -> None) in
  (match interface_decl_opt with
   | Some interface_decl -> CAst_utils.is_objc_factory_method interface_decl decl
   | _ -> false)

let rec do_frontend_checks_stmt (context:CLintersContext.context) stmt =
  let open Clang_ast_t in
  let do_all_checks_on_stmts context stmt =
    (match stmt with
     | DeclStmt (_, _, decl_list) ->
         List.iter ~f:(do_frontend_checks_decl context) decl_list
     | BlockExpr (_, _, _, decl) ->
         List.iter ~f:(do_frontend_checks_decl context) [decl]
     | _ -> ());
    do_frontend_checks_stmt context stmt in
  CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Stmt stmt);
  match stmt with
  | ObjCAtSynchronizedStmt (_, stmt_list) ->
      let stmt_context = { context with CLintersContext.in_synchronized_block = true } in
      List.iter ~f:(do_all_checks_on_stmts stmt_context) stmt_list
  | IfStmt (_, [stmt1; stmt2; cond_stmt; inside_if_stmt; inside_else_stmt]) ->
      (* here we analyze the children of the if stmt with the standard context,
         except for inside_if_stmt... *)
      List.iter ~f:(do_all_checks_on_stmts context) [stmt1; stmt2; cond_stmt; inside_else_stmt];
      let inside_if_stmt_context =
        {context with CLintersContext.if_context = compute_if_context context cond_stmt } in
      (* ...and here we analyze the stmt inside the if with the context
         extended with the condition of the if *)
      do_all_checks_on_stmts inside_if_stmt_context inside_if_stmt
  | _ ->
      let stmts = CAst_utils.get_stmts_from_stmt stmt in
      List.iter ~f:(do_all_checks_on_stmts context) stmts

and do_frontend_checks_decl (context: CLintersContext.context) decl =
  let open Clang_ast_t in
  CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Decl decl);
  match decl with
  | FunctionDecl(_, _, _, fdi)
  | CXXMethodDecl (_, _, _, fdi, _)
  | CXXConstructorDecl (_, _, _, fdi, _)
  | CXXConversionDecl (_, _, _, fdi, _)
  | CXXDestructorDecl (_, _, _, fdi, _) ->
      let context' = {context with CLintersContext.current_method = Some decl } in
      (match fdi.Clang_ast_t.fdi_body with
       | Some stmt -> do_frontend_checks_stmt context' stmt
       | None -> ())
  | ObjCMethodDecl (_, _, mdi) ->
      let context' = {context with
                      CLintersContext.current_method = Some decl;
                      CLintersContext.in_objc_static_factory_method =
                        is_factory_method context decl} in
      (match mdi.Clang_ast_t.omdi_body with
       | Some stmt -> do_frontend_checks_stmt context' stmt
       | None -> ())
  | BlockDecl (_, block_decl_info) ->
      let context' = {context with CLintersContext.current_method = Some decl } in
      (match block_decl_info.Clang_ast_t.bdi_body with
       | Some stmt -> do_frontend_checks_stmt context' stmt
       | None -> ())
  | ObjCImplementationDecl (_, _, decls, _, _) ->
      let context' = {context with current_objc_impl = Some decl} in
      List.iter ~f:(do_frontend_checks_decl context') decls
  | _ -> match Clang_ast_proj.get_decl_context_tuple decl with
    | Some (decls, _) ->
        List.iter ~f:(do_frontend_checks_decl context) decls
    | None -> ()

let context_with_ck_set context decl_list =
  let is_ck = context.CLintersContext.is_ck_translation_unit
              || ComponentKit.contains_ck_impl decl_list in
  if is_ck then
    { context with CLintersContext.is_ck_translation_unit = true }
  else
    context

let store_issues source_file =
  let abbrev_source_file = SourceFile.encoding source_file in
  let lint_issues_dir = Config.results_dir ^/ Config.lint_issues_dir_name in
  Utils.create_dir lint_issues_dir;
  let lint_issues_file =
    DB.filename_from_string (Filename.concat lint_issues_dir (abbrev_source_file ^ ".issue")) in
  LintIssues.store_issues lint_issues_file !LintIssues.errLogMap

let do_frontend_checks trans_unit_ctx ast =
  try
    parse_ctl_file Config.linters_def_file;
    let source_file = trans_unit_ctx.CFrontend_config.source_file in
    Logging.out "Start linting file %a@\n" SourceFile.pp source_file;
    match ast with
    | Clang_ast_t.TranslationUnitDecl(_, decl_list, _, _) ->
        let context =
          context_with_ck_set (CLintersContext.empty trans_unit_ctx) decl_list in
        let is_decl_allowed decl =
          let decl_info = Clang_ast_proj.get_decl_tuple decl in
          CLocation.should_do_frontend_check trans_unit_ctx decl_info.Clang_ast_t.di_source_range in
        let allowed_decls = List.filter ~f:is_decl_allowed decl_list in
        (* We analyze the top level and then all the allowed declarations *)
        CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Decl ast);
        List.iter ~f:(do_frontend_checks_decl context) allowed_decls;
        if (LintIssues.exists_issues ()) then
          store_issues source_file;
        Logging.out "End linting file %a@\n" SourceFile.pp source_file;
        CTL.save_dotty_when_in_debug_mode trans_unit_ctx.CFrontend_config.source_file;
    | _ -> assert false (* NOTE: Assumes that an AST alsways starts with a TranslationUnitDecl *)
  with
  | Assert_failure (file, line, column) as exn ->
      Logging.err "Fatal error: exception Assert_failure(%s, %d, %d)@\n%!" file line column;
      raise exn
