(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let already_imported_files = ref []

let rec parse_import_file import_file channel =
  if List.mem ~equal:String.equal !already_imported_files import_file then
    L.(die ExternalError) "Cyclic imports: file '%s' was already imported." import_file
  else
    match CTLParserHelper.parse_al_file import_file channel with
    | Some
        { import_files= imports
        ; global_macros= curr_file_macros
        ; global_paths= curr_file_paths
        ; checkers= _ } ->
        already_imported_files := import_file :: !already_imported_files ;
        collect_all_macros_and_paths ~from_file:import_file imports curr_file_macros curr_file_paths
    | None ->
        L.(debug Linters Medium) "No macros or paths found.@\n" ;
        ([], [])


and collect_all_macros_and_paths ~from_file imports curr_file_macros curr_file_paths =
  L.(debug Linters Medium) "#### Start parsing import macros #####@\n" ;
  let import_macros, import_paths = parse_imports ~from_file imports in
  L.(debug Linters Medium) "#### Add global macros to import macros #####@\n" ;
  let macros = List.append import_macros curr_file_macros in
  let paths = List.append import_paths curr_file_paths in
  (macros, paths)


(** Parse import files with macro definitions, and return a list of LET clauses *)
and parse_imports ~from_file imports_files =
  let source_dir = Filename.dirname from_file in
  let resolve_import fimport =
    if Filename.is_relative fimport then source_dir ^/ fimport else fimport |> Filename.realpath
  in
  let parse_one_import_file fimport0 (macros, paths) =
    let fimport = resolve_import fimport0 in
    L.(debug Linters Medium) "  Loading import macros from file %s@\n" fimport ;
    let in_channel = In_channel.create fimport in
    let parsed_macros, parsed_paths = parse_import_file fimport in_channel in
    In_channel.close in_channel ;
    let macros = List.append parsed_macros macros in
    let paths = List.append parsed_paths paths in
    (macros, paths)
  in
  List.fold_right ~f:parse_one_import_file ~init:([], []) imports_files


let parse_ctl_file linters_def_file channel : ALIssues.linter list =
  match CTLParserHelper.parse_al_file linters_def_file channel with
  | Some
      { import_files= imports
      ; global_macros= curr_file_macros
      ; global_paths= curr_file_paths
      ; checkers= parsed_checkers } ->
      already_imported_files := [linters_def_file] ;
      let macros, paths =
        collect_all_macros_and_paths ~from_file:linters_def_file imports curr_file_macros
          curr_file_paths
      in
      let macros_map = ALIssues.build_macros_map macros in
      let paths_map = ALIssues.build_paths_map paths in
      L.(debug Linters Medium) "#### Start Expanding checkers #####@\n" ;
      let exp_checkers = ALIssues.expand_checkers macros_map paths_map parsed_checkers in
      L.(debug Linters Medium) "#### Checkers Expanded #####@\n" ;
      if Config.debug_mode then List.iter ~f:CTL.print_checker exp_checkers ;
      ALIssues.create_parsed_linters linters_def_file exp_checkers
  | None ->
      L.(debug Linters Medium) "No linters found.@\n" ;
      []


(** Parse the files with linters definitions, and return a list of linters *)
let parse_ctl_files linters_def_files : ALIssues.linter list =
  let collect_parsed_linters linters_def_file linters =
    L.(debug Linters Medium) "Loading linters rules from %s@\n" linters_def_file ;
    let in_channel = In_channel.create linters_def_file in
    let parsed_linters = parse_ctl_file linters_def_file in_channel in
    In_channel.close in_channel ;
    List.append parsed_linters linters
  in
  List.fold_right ~f:collect_parsed_linters ~init:[] linters_def_files


let rec get_responds_to_selector stmt =
  let open Clang_ast_t in
  let responToSelectorMethods = ["respondsToSelector:"; "instancesRespondToSelector:"] in
  match stmt with
  | ObjCMessageExpr (_, [_; ObjCSelectorExpr (_, _, _, method_name)], _, mdi)
  | ObjCMessageExpr (_, [ObjCSelectorExpr (_, _, _, method_name)], _, mdi)
    when List.mem ~equal:String.equal responToSelectorMethods mdi.Clang_ast_t.omei_selector ->
      [method_name]
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PolyVariantEqual.( = ) bo_info.Clang_ast_t.boi_kind `LAnd ->
      List.append (get_responds_to_selector stmt1) (get_responds_to_selector stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups (_, [stmt], _, _) ->
      get_responds_to_selector stmt
  | _ ->
      []


let rec is_core_foundation_version_number stmt =
  let open Clang_ast_t in
  match stmt with
  | DeclRefExpr (_, _, _, decl_ref_info) -> (
    match decl_ref_info.drti_decl_ref with
    | Some decl_ref_info ->
        let name_info, _, _ = CAst_utils.get_info_from_decl_ref decl_ref_info in
        String.equal name_info.ni_name "kCFCoreFoundationVersionNumber"
    | None ->
        false )
  | ImplicitCastExpr (_, [stmt], _, _) ->
      is_core_foundation_version_number stmt
  | _ ->
      false


let rec current_os_version_constant stmt =
  let open Clang_ast_t in
  match stmt with
  | FloatingLiteral (_, _, _, number) ->
      CiOSVersionNumbers.version_of number
  | IntegerLiteral (_, _, _, info) ->
      CiOSVersionNumbers.version_of info.ili_value
  | ImplicitCastExpr (_, [stmt], _, _) ->
      current_os_version_constant stmt
  | _ ->
      None


let rec get_current_os_version stmt =
  let open Clang_ast_t in
  match stmt with
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PolyVariantEqual.( = ) bo_info.Clang_ast_t.boi_kind `GE
         && is_core_foundation_version_number stmt1 ->
      Option.to_list (current_os_version_constant stmt2)
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PolyVariantEqual.( = ) bo_info.Clang_ast_t.boi_kind `LE
         && is_core_foundation_version_number stmt2 ->
      Option.to_list (current_os_version_constant stmt1)
  | BinaryOperator (_, [stmt1; stmt2], _, bo_info)
    when PolyVariantEqual.( = ) bo_info.Clang_ast_t.boi_kind `LAnd ->
      List.append (get_current_os_version stmt1) (get_current_os_version stmt2)
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups (_, [stmt], _, _) ->
      get_current_os_version stmt
  | _ ->
      []


let rec get_ios_available_version stmt =
  let open Clang_ast_t in
  match stmt with
  | ObjCAvailabilityCheckExpr (_, _, _, oacei) ->
      oacei.oacei_version
  | ImplicitCastExpr (_, [stmt], _, _)
  | ParenExpr (_, [stmt], _)
  | ExprWithCleanups (_, [stmt], _, _) ->
      get_ios_available_version stmt
  | _ ->
      None


let compute_if_context (context : CLintersContext.context) stmt =
  let selector = get_responds_to_selector stmt in
  let receiver_class_method_call =
    match
      (CPredicates.get_selector (Stmt stmt), CPredicates.receiver_class_method_call (Stmt stmt))
    with
    | Some selector, Some receiver when String.equal selector "class" ->
        Option.to_list (CPredicates.declaration_name receiver)
    | _ ->
        []
  in
  let os_version = get_current_os_version stmt in
  let ios_available_version_opt = Option.to_list (get_ios_available_version stmt) in
  let os_version = List.append ios_available_version_opt os_version in
  let within_responds_to_selector_block, within_available_class_block, ios_version_guard =
    match context.if_context with
    | Some if_context ->
        let within_responds_to_selector_block =
          List.append selector if_context.within_responds_to_selector_block
        in
        let within_available_class_block =
          List.append receiver_class_method_call if_context.within_available_class_block
        in
        let ios_version_guard = List.append os_version if_context.ios_version_guard in
        (within_responds_to_selector_block, within_available_class_block, ios_version_guard)
    | None ->
        (selector, receiver_class_method_call, os_version)
  in
  Some
    ( {within_responds_to_selector_block; within_available_class_block; ios_version_guard}
      : CLintersContext.if_context )


let rec do_frontend_checks_stmt linters (context : CLintersContext.context) stmt =
  let open Clang_ast_t in
  let an = Ctl_parser_types.Stmt stmt in
  (*L.(debug Linters Medium)
    "@\n >>>>>>Visit node %i <<<<<@\n" (Ctl_parser_types.ast_node_pointer an) ; *)
  let do_all_checks_on_stmts context stmt =
    ( match stmt with
    | DeclStmt (_, _, decl_list) ->
        List.iter ~f:(do_frontend_checks_decl linters context) decl_list
    | BlockExpr (_, _, _, decl) ->
        List.iter ~f:(do_frontend_checks_decl linters context) [decl]
    | _ ->
        () ) ;
    do_frontend_checks_stmt linters context stmt
  in
  ALIssues.invoke_set_of_checkers_on_node linters context an ;
  (* The map should be visited when we enter the node before visiting children *)
  let stmt_context_list =
    match stmt with
    | ObjCAtSynchronizedStmt (_, stmt_list) ->
        [({context with CLintersContext.in_synchronized_block= true}, stmt_list)]
    | OpaqueValueExpr (_, lstmt, _, opaque_value_expr_info) -> (
      match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
      | Some stmt ->
          [(context, lstmt @ [stmt])]
      | _ ->
          [(context, lstmt)] )
    | IfStmt (stmt_info, _, {Clang_ast_t.isi_cond; isi_cond_var; isi_then; isi_else}) ->
        let cond_stmt = CAst_utils.get_stmt_exn isi_cond stmt_info.si_source_range in
        let inside_then_stmt_context =
          {context with CLintersContext.if_context= compute_if_context context cond_stmt}
        in
        let then_stmt = CAst_utils.get_stmt_exn isi_then stmt_info.si_source_range in
        let other_stmts =
          Option.to_list isi_cond_var
          @ ( Option.map isi_else ~f:(fun (else_body, _) ->
                  CAst_utils.get_stmt_exn else_body stmt_info.si_source_range )
            |> Option.to_list )
        in
        (* distinguish between then and else branch as they need different context *)
        [(context, cond_stmt :: other_stmts); (inside_then_stmt_context, [then_stmt])]
    | ForStmt (_, stmt1 :: stmts) ->
        let inside_for_stmt_decl_context =
          {context with CLintersContext.in_for_loop_declaration= true}
        in
        [(context, stmts); (inside_for_stmt_decl_context, [stmt1])]
    | _ ->
        [(context, snd (Clang_ast_proj.get_stmt_tuple stmt))]
  in
  List.iter
    ~f:(fun (cxt, stmts) -> List.iter ~f:(do_all_checks_on_stmts cxt) stmts)
    stmt_context_list


(* Visit nodes via a transition *)
and do_frontend_checks_via_transition linters context an trans =
  let succs = CTL.next_state_via_transition an trans in
  List.iter
    ~f:(fun an' ->
      (*L.(debug Linters Medium)
        "@\n---- Going from %i to %i via transition %a ---- @\n"
        (Ctl_parser_types.ast_node_pointer an) (Ctl_parser_types.ast_node_pointer an')
        CTL.Debug.pp_transition (Some trans) ;*)
      match an' with
      | Ctl_parser_types.Decl d ->
          do_frontend_checks_decl linters context d
      | Ctl_parser_types.Stmt st ->
          do_frontend_checks_stmt linters context st )
    succs


and do_frontend_checks_decl linters (context : CLintersContext.context) decl =
  let open Clang_ast_t in
  if not (CAst_utils.is_implicit_decl decl) (* do not analyze implicit declarations *) then
    let an = Ctl_parser_types.Decl decl in
    (* The map should be visited when we enter the node before visiting children *)
    match decl with
    | FunctionDecl _
    | CXXMethodDecl _
    | CXXConstructorDecl _
    | CXXConversionDecl _
    | CXXDestructorDecl _
    | BlockDecl _
    | ObjCMethodDecl _ -> (
        let context' = CLintersContext.update_current_method context decl in
        ALIssues.invoke_set_of_checkers_on_node linters context' an ;
        (* We need to visit explicitly nodes reachable via Parameters transitions
           because they won't be visited during the evaluation of the formula *)
        do_frontend_checks_via_transition linters context' an CTLTypes.Parameters ;
        match CAst_utils.get_method_body_opt decl with
        | Some stmt ->
            do_frontend_checks_stmt linters context' stmt
        | None ->
            () )
    | ObjCImplementationDecl (_, _, decls, _, _) | ObjCInterfaceDecl (_, _, decls, _, _) ->
        ALIssues.invoke_set_of_checkers_on_node linters context an ;
        let context' = {context with current_objc_class= Some decl} in
        List.iter ~f:(do_frontend_checks_decl linters context') decls
    | ObjCCategoryImplDecl (_, _, decls, _, _) | ObjCCategoryDecl (_, _, decls, _, _) ->
        ALIssues.invoke_set_of_checkers_on_node linters context an ;
        let context' = {context with current_objc_category= Some decl} in
        List.iter ~f:(do_frontend_checks_decl linters context') decls
    | ObjCProtocolDecl (_, _, decls, _, _) ->
        ALIssues.invoke_set_of_checkers_on_node linters context an ;
        let context' = {context with current_objc_protocol= Some decl} in
        List.iter ~f:(do_frontend_checks_decl linters context') decls
    | _ -> (
        ALIssues.invoke_set_of_checkers_on_node linters context an ;
        match Clang_ast_proj.get_decl_context_tuple decl with
        | Some (decls, _) ->
            List.iter ~f:(do_frontend_checks_decl linters context) decls
        | None ->
            () )


let context_with_ck_set context decl_list =
  let is_ck =
    context.CLintersContext.is_ck_translation_unit || ComponentKit.contains_ck_impl decl_list
  in
  if is_ck then {context with CLintersContext.is_ck_translation_unit= true} else context


let find_linters_files () =
  List.concat_map
    ~f:(fun folder -> Utils.find_files ~path:folder ~extension:".al")
    Config.linters_def_folder


let linters_files =
  List.dedup_and_sort ~compare:String.compare (find_linters_files () @ Config.linters_def_file)


let is_decl_allowed lcxt decl =
  let decl_info = Clang_ast_proj.get_decl_tuple decl in
  CLocation.should_do_frontend_check lcxt.CLintersContext.translation_unit_context.source_file
    decl_info.Clang_ast_t.di_source_range


let do_frontend_checks (trans_unit_ctx : CFrontend_config.translation_unit_context) ast =
  ALIssues.issue_log := IssueLog.empty ;
  L.(debug Capture Quiet)
    "Loading the following linters files: %a@\n"
    (Pp.comma_seq Format.pp_print_string)
    linters_files ;
  CTL.create_ctl_evaluation_tracker trans_unit_ctx.source_file ;
  let parsed_linters =
    let parsed_linters = parse_ctl_files linters_files in
    ALIssues.filter_parsed_linters parsed_linters trans_unit_ctx.source_file
  in
  let source_file = trans_unit_ctx.CFrontend_config.source_file in
  L.(debug Linters Medium)
    "Start linting file %a with rules: @\n%a@\n" SourceFile.pp source_file ALIssues.pp_linters
    parsed_linters ;
  if Config.print_active_checkers then
    L.progress "Linting file %a, active linters: @\n%a@\n" SourceFile.pp source_file
      ALIssues.pp_linters parsed_linters ;
  match ast with
  | Clang_ast_t.TranslationUnitDecl (_, decl_list, _, _) ->
      let context = context_with_ck_set (CLintersContext.empty trans_unit_ctx) decl_list in
      let allowed_decls = List.filter ~f:(is_decl_allowed context) decl_list in
      (* We analyze the top level and then all the allowed declarations *)
      ALIssues.invoke_set_of_checkers_on_node parsed_linters context (Ctl_parser_types.Decl ast) ;
      List.iter ~f:(do_frontend_checks_decl parsed_linters context) allowed_decls ;
      IssueLog.store !ALIssues.issue_log ~entry:LintIssues ~file:source_file ;
      L.(debug Linters Medium) "End linting file %a@\n" SourceFile.pp source_file ;
      CTL.save_dotty_when_in_debug_mode trans_unit_ctx.CFrontend_config.source_file
      (*if CFrontend_config.tableaux_evaluation then (
        Tableaux.print_table_size () ;
        Tableaux.print_global_valuation_map ()) *)
  | _ (* NOTE: Assumes that an AST always starts with a TranslationUnitDecl *) ->
      assert false
