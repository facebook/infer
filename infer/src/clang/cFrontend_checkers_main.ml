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
module L = Logging

let parse_al_file fname channel : CTL.al_file option =
  let pos_str lexbuf =
    let pos = lexbuf.lex_curr_p in
    pos.pos_fname ^ ":" ^ string_of_int pos.pos_lnum ^ ":"
    ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)
  in
  let parse_with_error lexbuf =
    try Some (Ctl_parser.al_file token lexbuf) with
    | Ctl_parser_types.ALParsingException s
     -> raise (Ctl_parser_types.ALParsingException (s ^ " at " ^ pos_str lexbuf))
    | SyntaxError _ | Ctl_parser.Error
     -> raise (Ctl_parser_types.ALParsingException ("SYNTAX ERROR at " ^ pos_str lexbuf))
  in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= fname} ;
  parse_with_error lexbuf

let already_imported_files = ref []

let rec parse_import_file import_file channel =
  if List.mem ~equal:String.equal !already_imported_files import_file then
    failwith ("Cyclic imports: file '" ^ import_file ^ "' was already imported.")
  else
    match parse_al_file import_file channel with
    | Some
        { import_files= imports
        ; global_macros= curr_file_macros
        ; global_paths= curr_file_paths
        ; checkers= _ }
     -> already_imported_files := import_file :: !already_imported_files ;
        collect_all_macros_and_paths imports curr_file_macros curr_file_paths
    | None
     -> L.(debug Linters Medium) "No macros or paths found.@\n" ; ([], [])

and collect_all_macros_and_paths imports curr_file_macros curr_file_paths =
  L.(debug Linters Medium) "#### Start parsing import macros #####@\n" ;
  let import_macros, import_paths = parse_imports imports in
  L.(debug Linters Medium) "#### Add global macros to import macros #####@\n" ;
  let macros = List.append import_macros curr_file_macros in
  let paths = List.append import_paths curr_file_paths in
  (macros, paths)

(* Parse import files with macro definitions, and it returns a list of LET clauses *)
and parse_imports imports_files =
  let parse_one_import_file fimport (macros, paths) =
    L.(debug Linters Medium) "  Loading import macros from file %s@\n" fimport ;
    let in_channel = In_channel.create fimport in
    let parsed_macros, parsed_paths = parse_import_file fimport in_channel in
    In_channel.close in_channel ;
    let macros = List.append parsed_macros macros in
    let paths = List.append parsed_paths paths in
    (macros, paths)
  in
  List.fold_right ~f:parse_one_import_file ~init:([], []) imports_files

let parse_ctl_file linters_def_file channel : CFrontend_errors.linter list =
  match parse_al_file linters_def_file channel with
  | Some
      { import_files= imports
      ; global_macros= curr_file_macros
      ; global_paths= curr_file_paths
      ; checkers= parsed_checkers }
   -> already_imported_files := [linters_def_file] ;
      let macros, paths = collect_all_macros_and_paths imports curr_file_macros curr_file_paths in
      let macros_map = CFrontend_errors.build_macros_map macros in
      let paths_map = CFrontend_errors.build_paths_map paths in
      L.(debug Linters Medium) "#### Start Expanding checkers #####@\n" ;
      let exp_checkers = CFrontend_errors.expand_checkers macros_map paths_map parsed_checkers in
      L.(debug Linters Medium) "#### Checkers Expanded #####@\n" ;
      if Config.debug_mode then List.iter ~f:CTL.print_checker exp_checkers ;
      CFrontend_errors.create_parsed_linters linters_def_file exp_checkers
  | None
   -> L.(debug Linters Medium) "No linters found.@\n" ; []

(* Parse the files with linters definitions, and it returns a list of linters *)
let parse_ctl_files linters_def_files : CFrontend_errors.linter list =
  let collect_parsed_linters linters_def_file linters =
    L.(debug Linters Medium) "Loading linters rules from %s@\n" linters_def_file ;
    let in_channel = In_channel.create linters_def_file in
    let parsed_linters = parse_ctl_file linters_def_file in_channel in
    In_channel.close in_channel ; List.append parsed_linters linters
  in
  List.fold_right ~f:collect_parsed_linters ~init:[] linters_def_files

let rec get_responds_to_selector stmt =
  let open Clang_ast_t in
  let responToSelectorMethods = ["respondsToSelector:"; "instancesRespondToSelector:"] in
  match stmt with
  | ObjCMessageExpr (_, [_; (ObjCSelectorExpr (_, _, _, method_name))], _, mdi)
  | ObjCMessageExpr (_, [(ObjCSelectorExpr (_, _, _, method_name))], _, mdi)
    when List.mem ~equal:String.equal responToSelectorMethods mdi.Clang_ast_t.omei_selector
   -> [method_name]
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PVariant.( = ) bo_info.Clang_ast_t.boi_kind `LAnd
   -> List.append (get_responds_to_selector stmt1) (get_responds_to_selector stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups (_, [stmt], _, _)
   -> get_responds_to_selector stmt
  | _
   -> []

let rec is_core_foundation_version_number stmt =
  let open Clang_ast_t in
  match stmt with
  | DeclRefExpr (_, _, _, decl_ref_info) -> (
    match decl_ref_info.drti_decl_ref with
    | Some decl_ref_info
     -> let name_info, _, _ = CAst_utils.get_info_from_decl_ref decl_ref_info in
        String.equal name_info.ni_name "kCFCoreFoundationVersionNumber"
    | None
     -> false )
  | ImplicitCastExpr (_, [stmt], _, _)
   -> is_core_foundation_version_number stmt
  | _
   -> false

let rec current_os_version_constant stmt =
  let open Clang_ast_t in
  match stmt with
  | FloatingLiteral (_, _, _, number)
   -> CiOSVersionNumbers.version_of number
  | IntegerLiteral (_, _, _, info)
   -> CiOSVersionNumbers.version_of info.ili_value
  | ImplicitCastExpr (_, [stmt], _, _)
   -> current_os_version_constant stmt
  | _
   -> None

let rec get_current_os_version stmt =
  let open Clang_ast_t in
  match stmt with
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PVariant.( = ) bo_info.Clang_ast_t.boi_kind `GE && is_core_foundation_version_number stmt1
   -> Option.to_list (current_os_version_constant stmt2)
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PVariant.( = ) bo_info.Clang_ast_t.boi_kind `LE && is_core_foundation_version_number stmt2
   -> Option.to_list (current_os_version_constant stmt1)
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PVariant.( = ) bo_info.Clang_ast_t.boi_kind `LAnd
   -> List.append (get_current_os_version stmt1) (get_current_os_version stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups (_, [stmt], _, _)
   -> get_current_os_version stmt
  | _
   -> []

let compute_if_context (context: CLintersContext.context) stmt =
  let selector = get_responds_to_selector stmt in
  let os_version = get_current_os_version stmt in
  let within_responds_to_selector_block, ios_version_guard =
    match context.if_context with
    | Some if_context
     -> let within_responds_to_selector_block =
          List.append selector if_context.within_responds_to_selector_block
        in
        let ios_version_guard = List.append os_version if_context.ios_version_guard in
        (within_responds_to_selector_block, ios_version_guard)
    | None
     -> (selector, os_version)
  in
  Some ({within_responds_to_selector_block; ios_version_guard} : CLintersContext.if_context)

let is_factory_method (context: CLintersContext.context) decl =
  let interface_decl_opt =
    match context.current_objc_impl with
    | Some ObjCImplementationDecl (_, _, _, _, impl_decl_info)
     -> CAst_utils.get_decl_opt_with_decl_ref impl_decl_info.oidi_class_interface
    | _
     -> None
  in
  match interface_decl_opt with
  | Some interface_decl
   -> CAst_utils.is_objc_factory_method interface_decl decl
  | _
   -> false

let rec do_frontend_checks_stmt (context: CLintersContext.context) stmt =
  let open Clang_ast_t in
  let do_all_checks_on_stmts context stmt =
    ( match stmt with
    | DeclStmt (_, _, decl_list)
     -> List.iter ~f:(do_frontend_checks_decl context) decl_list
    | BlockExpr (_, _, _, decl)
     -> List.iter ~f:(do_frontend_checks_decl context) [decl]
    | _
     -> () ) ;
    do_frontend_checks_stmt context stmt
  in
  CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Stmt stmt) ;
  match stmt with
  | ObjCAtSynchronizedStmt (_, stmt_list)
   -> let stmt_context = {context with CLintersContext.in_synchronized_block= true} in
      List.iter ~f:(do_all_checks_on_stmts stmt_context) stmt_list
  | IfStmt (_, [stmt1; stmt2; cond_stmt; inside_if_stmt; inside_else_stmt])
   -> (* here we analyze the children of the if stmt with the standard context,
         except for inside_if_stmt... *)
      List.iter ~f:(do_all_checks_on_stmts context) [stmt1; stmt2; cond_stmt; inside_else_stmt] ;
      let inside_if_stmt_context =
        {context with CLintersContext.if_context= compute_if_context context cond_stmt}
      in
      (* ...and here we analyze the stmt inside the if with the context
         extended with the condition of the if *)
      do_all_checks_on_stmts inside_if_stmt_context inside_if_stmt
  | OpaqueValueExpr (_, lstmt, _, opaque_value_expr_info)
   -> let stmts =
        match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
        | Some stmt
         -> lstmt @ [stmt]
        | _
         -> lstmt
      in
      List.iter ~f:(do_all_checks_on_stmts context) stmts
  (* given that this has not been translated, looking up for variables *)
  (* inside leads to inconsistencies *)
  | ObjCAtCatchStmt _
   -> ()
  | _
   -> let stmts = snd (Clang_ast_proj.get_stmt_tuple stmt) in
      List.iter ~f:(do_all_checks_on_stmts context) stmts

and do_frontend_checks_decl (context: CLintersContext.context) decl =
  let open Clang_ast_t in
  CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Decl decl) ;
  match decl with
  | FunctionDecl (_, _, _, fdi)
  | CXXMethodDecl (_, _, _, fdi, _)
  | CXXConstructorDecl (_, _, _, fdi, _)
  | CXXConversionDecl (_, _, _, fdi, _)
  | CXXDestructorDecl (_, _, _, fdi, _)
   -> (
      let context' = {context with CLintersContext.current_method= Some decl} in
      match fdi.Clang_ast_t.fdi_body with
      | Some stmt
       -> do_frontend_checks_stmt context' stmt
      | None
       -> () )
  | ObjCMethodDecl (_, _, mdi)
   -> (
      let context' =
        { context with
          CLintersContext.current_method= Some decl
        ; CLintersContext.in_objc_static_factory_method= is_factory_method context decl }
      in
      match mdi.Clang_ast_t.omdi_body with
      | Some stmt
       -> do_frontend_checks_stmt context' stmt
      | None
       -> () )
  | BlockDecl (_, block_decl_info)
   -> (
      let context' = {context with CLintersContext.current_method= Some decl} in
      match block_decl_info.Clang_ast_t.bdi_body with
      | Some stmt
       -> do_frontend_checks_stmt context' stmt
      | None
       -> () )
  | ObjCImplementationDecl (_, _, decls, _, _)
   -> let context' = {context with current_objc_impl= Some decl} in
      List.iter ~f:(do_frontend_checks_decl context') decls
  | _ ->
    match Clang_ast_proj.get_decl_context_tuple decl with
    | Some (decls, _)
     -> List.iter ~f:(do_frontend_checks_decl context) decls
    | None
     -> ()

let context_with_ck_set context decl_list =
  let is_ck =
    context.CLintersContext.is_ck_translation_unit || ComponentKit.contains_ck_impl decl_list
  in
  if is_ck then {context with CLintersContext.is_ck_translation_unit= true} else context

let store_issues source_file =
  let abbrev_source_file = DB.source_file_encoding source_file in
  let lint_issues_dir = Config.results_dir ^/ Config.lint_issues_dir_name in
  Utils.create_dir lint_issues_dir ;
  let lint_issues_file =
    DB.filename_from_string (Filename.concat lint_issues_dir (abbrev_source_file ^ ".issue"))
  in
  LintIssues.store_issues lint_issues_file !LintIssues.errLogMap

let find_linters_files () =
  let rec find_aux init dir_path =
    let aux base_path files rel_path =
      let full_path = Filename.concat base_path rel_path in
      match (Unix.stat full_path).Unix.st_kind with
      | Unix.S_REG when String.is_suffix ~suffix:".al" full_path
       -> full_path :: files
      | Unix.S_DIR
       -> find_aux files full_path
      | _
       -> files
    in
    Sys.fold_dir ~init ~f:(aux dir_path) dir_path
  in
  List.concat (List.map ~f:(fun folder -> find_aux [] folder) Config.linters_def_folder)

let linters_files =
  List.dedup ~compare:String.compare (find_linters_files () @ Config.linters_def_file)

let do_frontend_checks (trans_unit_ctx: CFrontend_config.translation_unit_context) ast =
  L.(debug Capture Quiet)
    "Loading the following linters files: %a@\n" (Pp.comma_seq Format.pp_print_string)
    linters_files ;
  CTL.create_ctl_evaluation_tracker trans_unit_ctx.source_file ;
  try
    let parsed_linters = parse_ctl_files linters_files in
    let filtered_parsed_linters =
      CFrontend_errors.filter_parsed_linters parsed_linters trans_unit_ctx.source_file
    in
    CFrontend_errors.parsed_linters := filtered_parsed_linters ;
    let source_file = trans_unit_ctx.CFrontend_config.source_file in
    L.(debug Linters Medium)
      "Start linting file %a with rules: @\n%a@\n" SourceFile.pp source_file
      CFrontend_errors.pp_linters filtered_parsed_linters ;
    if Config.print_active_checkers then
      L.progress "Linting file %a, active linters: @\n%a@\n" SourceFile.pp source_file
        CFrontend_errors.pp_linters filtered_parsed_linters ;
    match ast with
    | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _)
     -> let context = context_with_ck_set (CLintersContext.empty trans_unit_ctx) decl_list in
        let is_decl_allowed decl =
          let decl_info = Clang_ast_proj.get_decl_tuple decl in
          CLocation.should_do_frontend_check trans_unit_ctx decl_info.Clang_ast_t.di_source_range
        in
        let allowed_decls = List.filter ~f:is_decl_allowed decl_list in
        (* We analyze the top level and then all the allowed declarations *)
        CFrontend_errors.invoke_set_of_checkers_on_node context (Ctl_parser_types.Decl ast) ;
        List.iter ~f:(do_frontend_checks_decl context) allowed_decls ;
        if LintIssues.exists_issues () then store_issues source_file ;
        L.(debug Linters Medium) "End linting file %a@\n" SourceFile.pp source_file ;
        CTL.save_dotty_when_in_debug_mode trans_unit_ctx.CFrontend_config.source_file
    | _
     -> assert false
    (* NOTE: Assumes that an AST always starts with a TranslationUnitDecl *)
  with Assert_failure (file, line, column) as exn ->
    L.internal_error "Fatal error: exception Assert_failure(%s, %d, %d)@\n%!" file line column ;
    raise exn
