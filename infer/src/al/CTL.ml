(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ctl_parser_types
module L = Logging

(* This module defines a language to define checkers. These checkers
   are intepreted over the AST of the program. A checker is defined by a
   CTL formula which express a condition saying when the checker should
    report a problem *)

(* "set" clauses are used for defining mandatory variables that will be used
   by when reporting issues: eg for defining the condition.

   "desc" clauses are used for defining the error message,
   the suggestion, the severity.

   "let" clauses are used to define temporary formulas which are then
   used to abbreviate the another formula. For example

   let f = a And B

   set formula  = f OR f

   set message = "bla"

*)

type clause =
  | CLet of ALVar.formula_id * ALVar.t list * CTLTypes.t
  (* Let clause: let id = definifion;  *)
  | CSet of ALVar.keyword * CTLTypes.t
  (* Set clause: set id = definition *)
  | CDesc of ALVar.keyword * string
  (* Description clause eg: set message = "..." *)
  | CPath of [`WhitelistPath | `BlacklistPath] * ALVar.t list

type ctl_checker =
  {id: string; (* Checker's id *) definitions: clause list (* A list of let/set definitions *)}

type al_file =
  { import_files: string list
  ; global_macros: clause list
  ; global_paths: (string * ALVar.alexp list) list
  ; checkers: ctl_checker list }

let print_checker c =
  L.(debug Linters Medium) "@\n-------------------- @\n" ;
  L.(debug Linters Medium) "@\nChecker name: %s@\n" c.id ;
  List.iter
    ~f:(fun d ->
      match d with
      | CSet (keyword, phi) ->
          let cn_str = ALVar.keyword_to_string keyword in
          L.(debug Linters Medium) "    %s=  @\n    %a@\n@\n" cn_str CTLTypes.pp_formula phi
      | CLet (exp, _, phi) ->
          let cn_str = ALVar.formula_id_to_string exp in
          L.(debug Linters Medium) "    %s=  @\n    %a@\n@\n" cn_str CTLTypes.pp_formula phi
      | CDesc (keyword, s) ->
          let cn_str = ALVar.keyword_to_string keyword in
          L.(debug Linters Medium) "    %s=  @\n    %s@\n@\n" cn_str s
      | CPath (paths_keyword, paths) ->
          let keyword =
            match paths_keyword with `WhitelistPath -> "whitelist_path" | _ -> "blacklist_path"
          in
          let paths_str = String.concat ~sep:"," (List.map ~f:ALVar.alexp_to_string paths) in
          L.(debug Linters Medium) "    %s=  @\n    %s@\n@\n" keyword paths_str )
    c.definitions ;
  L.(debug Linters Medium) "@\n-------------------- @\n"


let ctl_evaluation_tracker = ref None

let create_ctl_evaluation_tracker source_file =
  match (Config.linters_developer_mode, !ctl_evaluation_tracker) with
  | true, None ->
      ctl_evaluation_tracker := Some (ALDebugger.EvaluationTracker.create source_file)
  | true, _ ->
      L.(die InternalError) "A CTL evaluation tracker has already been created"
  | _ ->
      ()


let debug_create_payload ast_node phi lcxt =
  match !ctl_evaluation_tracker with
  | Some _ ->
      Some (ALDebugger.EvaluationTracker.create_content ast_node phi lcxt)
  | None ->
      None


let debug_eval_begin payload =
  match (!ctl_evaluation_tracker, payload) with
  | Some tracker, Some payload ->
      ctl_evaluation_tracker := Some (ALDebugger.EvaluationTracker.eval_begin tracker payload)
  | _ ->
      ()


let debug_eval_end result =
  match !ctl_evaluation_tracker with
  | Some tracker ->
      ctl_evaluation_tracker := Some (ALDebugger.EvaluationTracker.eval_end tracker result)
  | None ->
      ()


let save_dotty_when_in_debug_mode source_file =
  match !ctl_evaluation_tracker with
  | Some tracker ->
      let dotty_dir = ResultsDir.get_path LintDotty in
      Utils.create_dir dotty_dir ;
      let source_file_basename = Filename.basename (SourceFile.to_abs_path source_file) in
      let file = dotty_dir ^/ source_file_basename ^ ".dot" in
      let dotty = ALDebugger.EvaluationTracker.DottyPrinter.dotty_of_ctl_evaluation tracker in
      Utils.with_file_out file ~f:(fun oc -> Out_channel.output_string oc dotty)
  | _ ->
      ()


(* Helper functions *)
(* given a decl returns a stmt such that decl--->stmt via label trs *)
let transition_decl_to_stmt d trs =
  let open Clang_ast_t in
  let open CTLTypes in
  let temp_res =
    match (trs, d) with
    | Body, ObjCMethodDecl (_, _, omdi) ->
        [omdi.omdi_body]
    | Body, FunctionDecl (_, _, _, fdi)
    | Body, CXXMethodDecl (_, _, _, fdi, _)
    | Body, CXXConstructorDecl (_, _, _, fdi, _)
    | Body, CXXConversionDecl (_, _, _, fdi, _)
    | Body, CXXDestructorDecl (_, _, _, fdi, _) ->
        [fdi.fdi_body]
    | Body, BlockDecl (_, bdi) ->
        [bdi.bdi_body]
    | InitExpr, VarDecl (_, _, _, vdi) ->
        [vdi.vdi_init_expr]
    | InitExpr, ObjCIvarDecl (_, _, _, fldi, _)
    | InitExpr, FieldDecl (_, _, _, fldi)
    | InitExpr, ObjCAtDefsFieldDecl (_, _, _, fldi) ->
        [fldi.fldi_init_expr]
    | InitExpr, CXXMethodDecl (_, _, _, _, mdi)
    | InitExpr, CXXConstructorDecl (_, _, _, _, mdi)
    | InitExpr, CXXConversionDecl (_, _, _, _, mdi)
    | InitExpr, CXXDestructorDecl (_, _, _, _, mdi) ->
        List.map ~f:(fun ci -> ci.xci_init_expr) mdi.xmdi_cxx_ctor_initializers
    | InitExpr, EnumConstantDecl (_, _, _, ecdi) ->
        [ecdi.ecdi_init_expr]
    | _, _ ->
        [None]
  in
  List.fold ~f:(fun l e -> match e with Some st -> Stmt st :: l | _ -> l) temp_res ~init:[]


let transition_decl_to_decl_via_accessor_for_property d desired_kind =
  let open Clang_ast_t in
  let find_property_for_accessor decl_opt predicate =
    let decl_matches decl =
      match decl with ObjCPropertyDecl (_, _, opdi) -> predicate opdi | _ -> false
    in
    match decl_opt with
    | Some (ObjCCategoryImplDecl (_, _, _, _, ocidi)) ->
        let category_decls =
          match CAst_utils.get_decl_opt_with_decl_ref_opt ocidi.ocidi_category_decl with
          | Some (ObjCCategoryDecl (_, _, decls, _, _)) ->
              List.filter ~f:decl_matches decls
          | _ ->
              []
        in
        let class_decls =
          match CAst_utils.get_decl_opt_with_decl_ref_opt ocidi.ocidi_class_interface with
          | Some (ObjCInterfaceDecl (_, _, decls, _, _)) ->
              List.filter ~f:decl_matches decls
          | _ ->
              []
        in
        category_decls @ class_decls
    | Some (ObjCImplementationDecl (_, _, _, _, oidi)) -> (
      match CAst_utils.get_decl_opt_with_decl_ref_opt oidi.oidi_class_interface with
      | Some (ObjCInterfaceDecl (_, _, decls, _, _)) ->
          List.filter ~f:decl_matches decls
      | _ ->
          [] )
    | _ ->
        []
  in
  match d with
  | ObjCMethodDecl (di, method_decl_name, mdi) -> (
      (* infer whether this method may be a getter or setter (or
         neither) from its argument list *)
      let num_params = List.length mdi.omdi_parameters in
      let actual_kind, accessor_decl_ref_of_property_decl_info =
        match num_params with
        | 0 ->
            ("getter", fun opdi -> opdi.opdi_getter_method)
        | 1 ->
            ("setter", fun opdi -> opdi.opdi_setter_method)
        | _ ->
            ("", fun _ -> None)
      in
      if not (ALVar.compare_str_with_alexp actual_kind desired_kind) then []
      else
        match CAst_utils.get_decl_opt_with_decl_ref_opt mdi.omdi_property_decl with
        | Some property_decl ->
            (* clang handles most cases: property declarations with
               accessor method declarations in the inferface; property
               declarations in base classes; etc. *)
            [Decl property_decl]
        | None ->
            (* search the interface for a matching property *)
            let name_check opdi =
              match accessor_decl_ref_of_property_decl_info opdi with
              | None ->
                  false
              | Some dr -> (
                match dr.dr_name with
                | Some ni ->
                    String.equal method_decl_name.ni_name ni.ni_name
                | _ ->
                    false )
            in
            let impl_decl_opt = CAst_utils.get_decl_opt di.di_parent_pointer in
            List.map ~f:(fun x -> Decl x) (find_property_for_accessor impl_decl_opt name_check) )
  | _ ->
      []


let transition_decl_to_decl_via_super d =
  let decl_opt_to_ast_node_opt d_opt = match d_opt with Some d' -> [Decl d'] | None -> [] in
  let do_ObjCImplementationDecl d =
    match CAst_utils.get_impl_decl_info d with
    | Some idi ->
        decl_opt_to_ast_node_opt (CAst_utils.get_super_ObjCImplementationDecl idi)
    | None ->
        []
  in
  match d with
  | Clang_ast_t.ObjCImplementationDecl _ ->
      do_ObjCImplementationDecl d
  | Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, idi) ->
      decl_opt_to_ast_node_opt (CAst_utils.get_decl_opt_with_decl_ref_opt idi.otdi_super)
  | _ ->
      []


let transition_decl_to_decl_via_protocol d =
  let open Clang_ast_t in
  let get_nodes dr =
    match CAst_utils.get_decl dr.dr_decl_pointer with Some d -> Some (Decl d) | None -> None
  in
  match d with
  | Clang_ast_t.ObjCProtocolDecl (_, _, _, _, opdi) ->
      List.filter_map ~f:get_nodes opdi.opcdi_protocols
  | _ ->
      []


let transition_stmt_to_stmt_via_condition st =
  let open Clang_ast_t in
  match st with
  | IfStmt (stmt_info, _, {isi_cond; _}) ->
      let cond = CAst_utils.get_stmt_exn isi_cond stmt_info.si_source_range in
      [Stmt cond]
  | ConditionalOperator (_, cond :: _, _)
  | ForStmt (_, [_; _; cond; _; _])
  | WhileStmt (_, ([_; cond; _] | [cond; _])) ->
      [Stmt cond]
  | _ ->
      []


let transition_stmt_to_decl_via_pointer stmt =
  let open Clang_ast_t in
  match stmt with
  | ObjCMessageExpr (_, _, _, obj_c_message_expr_info) -> (
    match CAst_utils.get_decl_opt obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
    | Some decl ->
        [Decl decl]
    | None ->
        [] )
  | DeclRefExpr (_, _, _, decl_ref_expr_info) -> (
    match
      CAst_utils.get_decl_opt_with_decl_ref_opt decl_ref_expr_info.Clang_ast_t.drti_decl_ref
    with
    | Some decl ->
        [Decl decl]
    | None ->
        [] )
  | _ ->
      []


let transition_stmt_to_stmt_via_source_expr stmt =
  let open Clang_ast_t in
  match stmt with
  | OpaqueValueExpr (_, _, _, ovei) -> (
    match ovei.ovei_source_expr with Some st -> [Stmt st] | None -> [] )
  | _ ->
      []


let transition_via_parameters an =
  let open Clang_ast_t in
  match an with
  | Decl (ObjCMethodDecl (_, _, omdi)) ->
      List.map ~f:(fun d -> Decl d) omdi.omdi_parameters
  | Stmt (ObjCMessageExpr (_, stmt_list, _, _))
  | Stmt (CallExpr (_, _ :: stmt_list, _))
  | Stmt (CXXMemberCallExpr (_, stmt_list, _)) ->
      List.map ~f:(fun stmt -> Stmt stmt) stmt_list
  | _ ->
      []


let parameter_of_corresp_name method_name args name =
  let names =
    List.filter (String.split ~on:':' method_name) ~f:(fun label -> not (String.is_empty label))
  in
  match List.zip names args with
  | Ok names_args -> (
      let names_arg_opt =
        List.find names_args ~f:(fun (arg_label, _) -> ALVar.compare_str_with_alexp arg_label name)
      in
      match names_arg_opt with Some (_, arg) -> Some arg | None -> None )
  | Unequal_lengths ->
      None


let parameter_of_corresp_pos args pos =
  let pos_int =
    match pos with ALVar.Const n -> ( try int_of_string n with Failure _ -> -1 ) | _ -> -1
  in
  List.nth args pos_int


let transition_via_specified_parameter ~pos an key =
  let invalid_param_name_use () =
    Logging.die InternalError "Transition ParameterName is only available for ObjC methods"
  in
  let node_opt_to_ast_node_list f arg_stmt_opt =
    match arg_stmt_opt with Some arg -> [f arg] | None -> []
  in
  let apply_decl arg = Decl arg in
  let apply_stmt arg = Stmt arg in
  let compute_nodes parameters =
    let parameter_of_corresp_key =
      if pos then parameter_of_corresp_pos else invalid_param_name_use ()
    in
    let arg_stmt_opt = parameter_of_corresp_key parameters key in
    node_opt_to_ast_node_list apply_stmt arg_stmt_opt
  in
  match an with
  | Stmt (ObjCMessageExpr (_, stmt_list, _, omei)) ->
      let method_name = omei.omei_selector in
      let parameter_of_corresp_key =
        if pos then parameter_of_corresp_pos else parameter_of_corresp_name method_name
      in
      let arg_stmt_opt = parameter_of_corresp_key stmt_list key in
      node_opt_to_ast_node_list apply_stmt arg_stmt_opt
  | Stmt (CallExpr (_, _ :: args, _)) ->
      compute_nodes args
  | Stmt (CXXMemberCallExpr (_, stmt_list, _)) ->
      compute_nodes stmt_list
  | Decl (ObjCMethodDecl (_, named_decl_info, omdi)) ->
      let method_name = named_decl_info.ni_name in
      let parameter_of_corresp_key =
        if pos then parameter_of_corresp_pos else parameter_of_corresp_name method_name
      in
      let arg_decl_opt = parameter_of_corresp_key omdi.omdi_parameters key in
      node_opt_to_ast_node_list apply_decl arg_decl_opt
  | Decl (FunctionDecl (_, _, _, fdi))
  | Decl (CXXMethodDecl (_, _, _, fdi, _))
  | Decl (CXXConstructorDecl (_, _, _, fdi, _)) ->
      let parameter_of_corresp_key =
        if pos then parameter_of_corresp_pos else invalid_param_name_use ()
      in
      let arg_decl_opt = parameter_of_corresp_key fdi.fdi_parameters key in
      node_opt_to_ast_node_list apply_decl arg_decl_opt
  | _ ->
      []


let transition_via_parameter_name an name = transition_via_specified_parameter an name ~pos:false

let transition_via_parameter_pos an pos = transition_via_specified_parameter an pos ~pos:true

let transition_via_fields an =
  let open Clang_ast_t in
  match an with
  | Decl (RecordDecl (_, _, _, decls, _, _, _)) | Decl (CXXRecordDecl (_, _, _, decls, _, _, _, _))
    ->
      List.filter_map ~f:(fun d -> match d with FieldDecl _ -> Some (Decl d) | _ -> None) decls
  | Stmt (InitListExpr (_, stmts, _)) ->
      List.map ~f:(fun stmt -> Stmt stmt) stmts
  | _ ->
      []


let field_has_name name node =
  match node with
  | Decl (FieldDecl (_, name_info, _, _)) ->
      ALVar.compare_str_with_alexp name_info.Clang_ast_t.ni_name name
  | _ ->
      false


let field_of_name name nodes = List.filter ~f:(field_has_name name) nodes

let field_of_corresp_name_from_init_list_expr name init_nodes (expr_info : Clang_ast_t.expr_info) =
  match CAst_utils.get_decl_from_typ_ptr expr_info.ei_qual_type.qt_type_ptr with
  | Some decl -> (
      let fields = transition_via_fields (Decl decl) in
      match List.zip init_nodes fields with
      | Ok init_nodes_fields ->
          List.filter ~f:(fun (_, field) -> field_has_name name field) init_nodes_fields
          |> List.map ~f:(fun (node, _) -> node)
      | Unequal_lengths ->
          [] )
  | None ->
      []


let transition_via_field_name node name =
  let open Clang_ast_t in
  match node with
  | Decl (RecordDecl _) | Decl (CXXRecordDecl _) ->
      let fields = transition_via_fields node in
      field_of_name name fields
  | Stmt (InitListExpr (_, stmts, expr_info)) ->
      let nodes = List.map ~f:(fun stmt -> Stmt stmt) stmts in
      field_of_corresp_name_from_init_list_expr name nodes expr_info
  | _ ->
      []


let transition_via_sibling node =
  let open Clang_ast_t in
  match node with
  | Decl orig_decl -> (
      let decl_info = Clang_ast_proj.get_decl_tuple orig_decl in
      match CAst_utils.get_decl_opt decl_info.Clang_ast_t.di_parent_pointer with
      | Some (TranslationUnitDecl (_, decls, _, _))
      | Some (ObjCImplementationDecl (_, _, decls, _, _))
      | Some (ObjCInterfaceDecl (_, _, decls, _, _))
      | Some (ObjCProtocolDecl (_, _, decls, _, _))
      | Some (ObjCCategoryDecl (_, _, decls, _, _))
      | Some (ObjCCategoryImplDecl (_, _, decls, _, _))
      | Some (RecordDecl (_, _, _, decls, _, _, _)) ->
          List.filter_map
            ~f:(fun decl -> if not (phys_equal decl orig_decl) then Some (Decl decl) else None)
            decls
      | _ ->
          [] )
  | Stmt _ ->
      []


(* given a node an returns a list of nodes an' such that an transition to an' via label trans *)
let next_state_via_transition an trans =
  let open CTLTypes in
  match (an, trans) with
  | Decl d, Super ->
      transition_decl_to_decl_via_super d
  | _, FieldName name ->
      transition_via_field_name an name
  | _, Fields ->
      transition_via_fields an
  | _, Parameters ->
      transition_via_parameters an
  | _, Sibling ->
      transition_via_sibling an
  | Decl d, InitExpr | Decl d, Body ->
      transition_decl_to_stmt d trans
  | Decl d, Protocol ->
      transition_decl_to_decl_via_protocol d
  | Stmt st, Cond ->
      transition_stmt_to_stmt_via_condition st
  | Stmt st, PointerToDecl ->
      transition_stmt_to_decl_via_pointer st
  | Stmt st, SourceExpr ->
      transition_stmt_to_stmt_via_source_expr st
  | an, ParameterName name ->
      transition_via_parameter_name an name
  | an, ParameterPos pos ->
      transition_via_parameter_pos an pos
  | Decl d, AccessorForProperty name ->
      transition_decl_to_decl_via_accessor_for_property d name
  | _, _ ->
      []


let choose_one_witness an1 an2 =
  if Ctl_parser_types.ast_node_equal an1 an2 then an1
  else if Ctl_parser_types.is_node_successor_of an1 ~is_successor:an2 then an2
  else an1


let choose_witness_opt witness_opt1 witness_opt2 =
  match (witness_opt1, witness_opt2) with
  | Some witness1, Some witness2 ->
      Some (choose_one_witness witness1 witness2)
  | Some witness, None | None, Some witness ->
      Some witness
  | None, None ->
      None


(* Evaluation of formulas *)
(* evaluate an atomic formula (i.e. a predicate) on a ast node an and a
   linter context lcxt. That is:  an, lcxt |= pred_name(params) *)
let eval_Atomic pred_name_ args an lcxt =
  let pred_name = ALVar.formula_id_to_string pred_name_ in
  match (pred_name, args, an) with
  | "call_class_method", [m], an ->
      CPredicates.call_class_method an m
  | "call_function", [m], an ->
      CPredicates.call_function an m
  | "call_qualified_function", [m], an ->
      CPredicates.call_qualified_function an m
  | "call_instance_method", [m], an ->
      CPredicates.call_instance_method an m
  | "call_method", [m], an ->
      CPredicates.call_method an m
  | "call_cxx_method", [m], an ->
      CPredicates.call_cxx_method an m
  | "captures_cxx_references", [], _ ->
      CPredicates.captures_cxx_references an
  | "objc_block_is_capturing_values", [], _ ->
      CPredicates.objc_block_is_capturing_values an
  | "context_in_synchronized_block", [], _ ->
      CPredicates.context_in_synchronized_block lcxt
  | "declaration_has_name", [decl_name], an ->
      CPredicates.declaration_has_name an decl_name
  | "has_cxx_fully_qualified_name", [qual_name_re], an ->
      CPredicates.has_cxx_fully_qualified_name an qual_name_re
  | "has_cxx_fully_qualified_name_in_custom_symbols", [list_name], an -> (
    match list_name with
    | ALVar.Const s ->
        CPredicates.has_cxx_fully_qualified_name_in_custom_symbols an s
    | _ ->
        assert false )
  | "declaration_ref_name", [decl_name], an ->
      CPredicates.declaration_ref_name an decl_name
  | "has_cast_kind", [name], an ->
      CPredicates.has_cast_kind an name
  | "has_type", [typ], an ->
      CPredicates.has_type an typ
  | "has_value", [typ], an ->
      CPredicates.has_value an typ
  | "isa", [classname], an ->
      CPredicates.isa an classname
  | "is_assign_property", [], an ->
      CPredicates.is_assign_property an
  | "is_binop_with_kind", [kind], an ->
      CPredicates.is_binop_with_kind an kind
  | "is_class", [cname], an ->
      CPredicates.is_class an cname
  | "is_const_expr_var", [], an ->
      CPredicates.is_const_expr_var an
  | "is_init_integral_constant_expr", [], an ->
      CPredicates.is_init_integral_constant_expr an
  | "is_qual_type_const", [], an ->
      CPredicates.is_qual_type_const an
  | "has_init_list_const_expr", [], an ->
      CPredicates.has_init_list_const_expr an
  | "is_decl", [], an ->
      CPredicates.is_decl an
  | "is_enum_constant", [cname], an ->
      CPredicates.is_enum_constant an cname
  | "is_enum_constant_of_enum", [name], an ->
      CPredicates.is_enum_constant_of_enum an name
  | "is_global_var", [], an ->
      CPredicates.is_global_var an
  | "is_static_local_var", [], an ->
      CPredicates.is_static_local_var an
  | "is_static_var", [], an ->
      CPredicates.is_static_var an
  | "is_extern_var", [], an ->
      CPredicates.is_extern_var an
  | "adhere_to_protocol", [], an ->
      CPredicates.adhere_to_protocol an
  | "is_in_block", [], _ ->
      CPredicates.is_in_block lcxt
  | "is_optional_objc_method", [], an ->
      CPredicates.is_optional_objc_method an
  | "is_call_to_optional_objc_method", [], an ->
      CPredicates.is_call_to_optional_objc_method an
  | "is_in_cxx_constructor", [name], _ ->
      CPredicates.is_in_cxx_constructor lcxt name
  | "is_in_cxx_destructor", [name], _ ->
      CPredicates.is_in_cxx_destructor lcxt name
  | "is_in_cxx_method", [name], _ ->
      CPredicates.is_in_cxx_method lcxt name
  | "is_in_function", [name], _ ->
      CPredicates.is_in_function lcxt name
  | "is_in_objc_class_method", [name], _ ->
      CPredicates.is_in_objc_class_method lcxt name
  | "is_in_objc_instance_method", [name], _ ->
      CPredicates.is_in_objc_instance_method lcxt name
  | "is_in_objc_method", [name], _ ->
      CPredicates.is_in_objc_method lcxt name
  | "is_in_objc_interface_named", [name], _ ->
      CPredicates.is_in_objc_interface_named lcxt name
  | "is_in_objc_implementation_named", [name], _ ->
      CPredicates.is_in_objc_implementation_named lcxt name
  | "is_in_objc_class_named", [name], _ ->
      CPredicates.is_in_objc_class_named lcxt name
  | "is_in_objc_subclass_of", [name], _ ->
      CPredicates.is_in_objc_subclass_of lcxt name
  | "is_in_objc_category_interface_on_class_named", [name], _ ->
      CPredicates.is_in_objc_category_interface_on_class_named lcxt name
  | "is_in_objc_category_implementation_on_class_named", [name], _ ->
      CPredicates.is_in_objc_category_implementation_on_class_named lcxt name
  | "is_in_objc_category_on_class_named", [name], _ ->
      CPredicates.is_in_objc_category_on_class_named lcxt name
  | "is_in_objc_category_interface_on_subclass_of", [name], _ ->
      CPredicates.is_in_objc_category_interface_on_subclass_of lcxt name
  | "is_in_objc_category_implementation_on_subclass_of", [name], _ ->
      CPredicates.is_in_objc_category_implementation_on_subclass_of lcxt name
  | "is_in_objc_category_on_subclass_of", [name], _ ->
      CPredicates.is_in_objc_category_on_subclass_of lcxt name
  | "is_in_objc_category_interface_named", [name], _ ->
      CPredicates.is_in_objc_category_interface_named lcxt name
  | "is_in_objc_category_implementation_named", [name], _ ->
      CPredicates.is_in_objc_category_implementation_named lcxt name
  | "is_in_objc_category_named", [name], _ ->
      CPredicates.is_in_objc_category_named lcxt name
  | "is_in_objc_protocol_named", [name], _ ->
      CPredicates.is_in_objc_protocol_named lcxt name
  | "is_ivar_readonly", [], an ->
      CPredicates.is_ivar_readonly an
  | "is_ivar_atomic", [], an ->
      CPredicates.is_ivar_atomic an
  | "is_method_property_accessor_of_ivar", [], an ->
      CPredicates.is_method_property_accessor_of_ivar an lcxt
  | "is_node", [nodename], an ->
      CPredicates.is_node an nodename
  | "is_objc_constructor", [], _ ->
      CPredicates.is_objc_constructor lcxt
  | "objc_class_has_only_one_constructor_method_named", [name], an ->
      CPredicates.objc_class_has_only_one_constructor_method_named an name
  | "is_objc_dealloc", [], _ ->
      CPredicates.is_objc_dealloc lcxt
  | "is_objc_extension", [], _ ->
      CPredicates.is_objc_extension lcxt
  | "is_objc_interface_named", [name], an ->
      CPredicates.is_objc_interface_named an name
  | "is_objc_implementation_named", [name], an ->
      CPredicates.is_objc_implementation_named an name
  | "is_objc_class_named", [name], an ->
      CPredicates.is_objc_class_named an name
  | "is_objc_category_interface_on_class_named", [name], an ->
      CPredicates.is_objc_category_interface_on_class_named an name
  | "is_objc_category_implementation_on_class_named", [name], an ->
      CPredicates.is_objc_category_implementation_on_class_named an name
  | "is_objc_category_on_class_named", [cname], an ->
      CPredicates.is_objc_category_on_class_named an cname
  | "is_objc_category_interface_named", [name], an ->
      CPredicates.is_objc_category_interface_named an name
  | "is_objc_category_implementation_named", [name], an ->
      CPredicates.is_objc_category_implementation_named an name
  | "is_objc_category_named", [cname], an ->
      CPredicates.is_objc_category_named an cname
  | "is_objc_category_interface_on_subclass_of", [name], an ->
      CPredicates.is_objc_category_interface_on_subclass_of an name
  | "is_objc_category_implementation_on_subclass_of", [name], an ->
      CPredicates.is_objc_category_implementation_on_subclass_of an name
  | "is_objc_category_on_subclass_of", [name], an ->
      CPredicates.is_objc_category_on_subclass_of an name
  | "is_objc_protocol_named", [name], an ->
      CPredicates.is_objc_protocol_named an name
  | "is_objc_class_method_named", [name], an ->
      CPredicates.is_objc_class_method_named an name
  | "is_objc_instance_method_named", [name], an ->
      CPredicates.is_objc_instance_method_named an name
  | "is_objc_method_named", [name], an ->
      CPredicates.is_objc_method_named an name
  | "is_objc_method_overriding", [], an ->
      CPredicates.is_objc_method_overriding an
  | "is_objc_method_exposed", [], an ->
      CPredicates.is_objc_method_exposed lcxt an
  | "is_property_pointer_type", [], an ->
      CPredicates.is_property_pointer_type an
  | "is_strong_property", [], an ->
      CPredicates.is_strong_property an
  | "is_strong_ivar", [], an ->
      CPredicates.is_strong_ivar an
  | "is_unop_with_kind", [kind], an ->
      CPredicates.is_unop_with_kind an kind
  | "is_weak_property", [], an ->
      CPredicates.is_weak_property an
  | "is_receiver_objc_class_type", [], an ->
      CPredicates.is_receiver_objc_class_type an
  | "is_receiver_objc_id_type", [], an ->
      CPredicates.is_receiver_objc_id_type an
  | "is_receiver_subclass_of", [name], an ->
      CPredicates.is_receiver_subclass_of lcxt an name
  | "is_receiver_class_named", [name], an ->
      CPredicates.is_receiver_class_named lcxt an name
  | "is_receiver_super", [], an ->
      CPredicates.is_receiver_super an
  | "is_receiver_self", [], an ->
      CPredicates.is_receiver_self an
  | "method_return_type", [typ], an ->
      CPredicates.method_return_type an typ
  | "within_responds_to_selector_block", [], an ->
      CPredicates.within_responds_to_selector_block lcxt an
  | "objc_method_call_within_responds_to_selector_block", [], an ->
      CPredicates.objc_method_call_within_responds_to_selector_block lcxt an
  | "using_namespace", [namespace], an ->
      CPredicates.using_namespace an namespace
  | "is_at_selector_with_name", [name], an ->
      CPredicates.is_at_selector_with_name an name
  | "cxx_construct_expr_has_name", [name], an ->
      CPredicates.cxx_construct_expr_has_name an name
  | "has_type_const_ptr_to_objc_class", [], an ->
      CPredicates.has_type_const_ptr_to_objc_class an
  | "has_type_subprotocol_of", [protname], an ->
      CPredicates.has_type_subprotocol_of an protname
  | "has_visibility_attribute", [vis], an ->
      CPredicates.has_visibility_attribute an vis
  | "has_used_attribute", [], an ->
      CPredicates.has_used_attribute an
  | "has_no_escape_attribute", [], an ->
      CPredicates.has_no_escape_attribute an
  | "has_unavailable_attribute", [], an ->
      CPredicates.has_unavailable_attribute an
  | "within_available_class_block", [], an ->
      CPredicates.within_available_class_block lcxt an
  | "is_method_called_by_superclass", [], an ->
      CPredicates.is_method_called_by_superclass an
  | "is_cxx_copy_constructor", [], an ->
      CPredicates.is_cxx_copy_constructor an
  | "is_cxx_method_overriding", [], an ->
      CPredicates.is_cxx_method_overriding an None
  | "is_cxx_method_overriding", [qual_name_re], an ->
      CPredicates.is_cxx_method_overriding an (Some qual_name_re)
  | "is_init_expr_cxx11_constant", [], an ->
      CPredicates.is_init_expr_cxx11_constant an
  | "cxx_construct_expr_has_no_parameters", [], an ->
      CPredicates.cxx_construct_expr_has_no_parameters an
  | "is_in_source_file", [path_re], an ->
      CPredicates.is_in_source_file an path_re
  | "is_referencing_decl_from_source_file", [path_re], an ->
      CPredicates.is_referencing_decl_from_source_file an path_re
  | "objc_block_is_capturing_var_of_type", [typ], an ->
      CPredicates.objc_block_is_capturing_var_of_type an typ
  | _ ->
      L.(die ExternalError) "Undefined Predicate or wrong set of arguments: '%s'" pred_name


let eval_Atomic_with_witness pred_name_ args witness1 witness2 _ =
  let pred_name = ALVar.formula_id_to_string pred_name_ in
  match (pred_name, args) with
  | "decl_name_is_contained_in_name_of_decl", [] ->
      CPredicatesOnTwoNodes.decl_name_is_contained_in_name_of_decl witness1 witness2
  | _ ->
      L.(die ExternalError) "Undefined Predicate or wrong set of arguments: '%s'" pred_name


let rec eval_AndWithW an lcxt f1 f2 =
  match eval_formula f1 an lcxt with
  | Some witness1 -> (
    match eval_formula ~keep_witness:true f2 an lcxt with
    | Some witness2 ->
        Some (witness1, witness2)
    | _ ->
        None )
  | None (* we short-circuit the AND evaluation *) ->
      None


and eval_AndWithWitnesses an lcxt f1 f2 pred_name_ args =
  match eval_AndWithW an lcxt f1 f2 with
  | Some (witness1, witness2) -> (
    try if eval_Atomic_with_witness pred_name_ args witness1 witness2 lcxt then Some an else None
    with CFrontend_errors.IncorrectAssumption _ -> None )
  | None ->
      None


and eval_AND ?keep_witness an lcxt f1 f2 =
  match eval_formula ?keep_witness f1 an lcxt with
  | Some witness1 -> (
    match eval_formula ?keep_witness f2 an lcxt with
    | Some witness2 ->
        Some (choose_one_witness witness1 witness2)
    | _ ->
        None )
  | None (* we short-circuit the AND evaluation *) ->
      None


and eval_OR an lcxt f1 f2 = choose_witness_opt (eval_formula f1 an lcxt) (eval_formula f2 an lcxt)

and eval_Implies an lcxt f1 f2 =
  let witness1 = if Option.is_some (eval_formula f1 an lcxt) then None else Some an in
  let witness2 = eval_formula f2 an lcxt in
  choose_witness_opt witness1 witness2


(* an, lcxt |= EF phi  <=>
   an, lcxt |= phi or exists an' in Successors(st): an', lcxt |= EF phi

   That is: a (an, lcxt) satifies EF phi if and only if
   either (an,lcxt) satifies phi or there is a child an' of the node an
   such that (an', lcxt) satifies EF phi
*)
and eval_EF phi an lcxt trans =
  let open CTLTypes in
  match (trans, an) with
  | Some _, _ ->
      (* Using equivalence EF[->trans] phi = phi OR EX[->trans](EF[->trans] phi)*)
      let phi' = Or (phi, EX (trans, EF (trans, phi))) in
      eval_formula phi' an lcxt
  | None, _ ->
      let witness_opt = eval_formula phi an lcxt in
      if Option.is_some witness_opt then witness_opt
      else
        List.fold_left (Ctl_parser_types.get_direct_successor_nodes an) ~init:witness_opt
          ~f:(fun acc node -> choose_witness_opt (eval_EF phi node lcxt trans) acc)


(* an, lcxt |= EX phi  <=> exists an' in Successors(st): an', lcxt |= phi

   That is: a (an, lcxt) satifies EX phi if and only if
   there exists is a child an' of the node an
   such that (an', lcxt) satifies phi
*)
and eval_EX ?(keep_witness = false) phi an lcxt trans =
  let succs =
    match trans with
    | Some l ->
        next_state_via_transition an l
    | None ->
        Ctl_parser_types.get_direct_successor_nodes an
  in
  let witness_opt =
    List.fold_left succs ~init:None ~f:(fun acc node ->
        choose_witness_opt (eval_formula phi node lcxt) acc )
  in
  if keep_witness then witness_opt
  else
    match (witness_opt, trans) with
    | Some _, Some trans when not (CTLTypes.is_transition_to_successor trans) ->
        Some an (* We want to limit the witnesses to the successors of the original node. *)
    | _ ->
        witness_opt


(* an, lcxt |= E(phi1 U phi2) evaluated using the equivalence
   an, lcxt |= E(phi1 U phi2) <=> an, lcxt |= phi2 or (phi1 and EX(E(phi1 U phi2)))

   That is: a (an,lcxt) satifies E(phi1 U phi2) if and only if
   an,lcxt satifies the formula phi2 or (phi1 and EX(E(phi1 U phi2)))
*)
and eval_EU phi1 phi2 an lcxt trans =
  let open CTLTypes in
  let f = Or (phi2, And (phi1, EX (trans, EU (trans, phi1, phi2)))) in
  eval_formula f an lcxt


(* an |= A(phi1 U phi2) evaluated using the equivalence
   an |= A(phi1 U phi2) <=> an |= phi2 or (phi1 and AX(A(phi1 U phi2)))

   Same as EU but for the all path quantifier A
*)
and eval_AU phi1 phi2 an lcxt trans =
  let open CTLTypes in
  let f = Or (phi2, And (phi1, AX (trans, AU (trans, phi1, phi2)))) in
  eval_formula f an lcxt


(* an, lcxt |= InNode[node_type_list] phi <=>
   an is a node of type in node_type_list and an satifies phi
*)
and in_node node_type_list phi an lctx =
  let holds_for_one_node n =
    match lctx.CLintersContext.et_evaluation_node with
    | Some id ->
        if String.equal id (Ctl_parser_types.ast_node_unique_string_id an) then
          eval_formula phi an lctx
        else None
    | None ->
        if Ctl_parser_types.ast_node_has_kind [n] an then eval_formula phi an lctx else None
  in
  (* This is basically an OR of formula holds in the various nodes in the list *)
  List.fold_left node_type_list ~init:None ~f:(fun acc node ->
      choose_witness_opt (holds_for_one_node node) acc )


(* Intuitive meaning: (an,lcxt) satifies EH[Classes] phi
   if the node an is among the declaration specified by the list Classes and
   there exists a super class in its hierarchy whose declaration satisfy phi.

   an, lcxt |= EH[Classes] phi <=>
   the node an is in Classes and there exists a declaration d in Hierarchy(an)
   such that d,lcxt |= phi *)
and eval_EH classes phi an lcxt =
  let open CTLTypes in
  (* Define EH[Classes] phi = ET[Classes](EF[->Super] phi) *)
  let f = ET (classes, None, EX (Some Super, EF (Some Super, phi))) in
  eval_formula f an lcxt


(* an, lcxt |= ET[T][->l]phi <=>
   eventually we reach a node an' such that an' is among the types defined in T
   and:

   an'-l->an''
   ("an' transitions" to another node an'' via an edge labelled l)
   and an'',lcxt |= phi

   or l is unspecified and an,lcxt |= phi
*)
and eval_ET tl trs phi an lcxt =
  let open CTLTypes in
  let f =
    match trs with
    | Some _ ->
        EF (None, InNode (tl, EX (trs, phi)))
    | None ->
        EF (None, InNode (tl, phi))
  in
  eval_formula f an lcxt


and eval_InObjCClass an lcxt f1 f2 =
  match an with
  | Ctl_parser_types.Decl (Clang_ast_t.ObjCInterfaceDecl (_, _, _, _, otdi)) ->
      let open Option.Monad_infix in
      eval_formula f1 an lcxt
      >>= fun _ ->
      otdi.otdi_implementation
      >>= fun dr ->
      CAst_utils.get_decl dr.Clang_ast_t.dr_decl_pointer
      >>= fun n -> eval_formula f2 (Ctl_parser_types.Decl n) lcxt
  | _ ->
      None


(* Formulas are evaluated on a AST node an and a linter context lcxt *)
and eval_formula ?keep_witness f an lcxt : Ctl_parser_types.ast_node option =
  let open CTLTypes in
  debug_eval_begin (debug_create_payload an f lcxt) ;
  let res =
    match f with
    | True ->
        Some an
    | False ->
        None
    | Atomic (name, params) -> (
      try if eval_Atomic name params an lcxt then Some an else None
      with CFrontend_errors.IncorrectAssumption _ -> None )
    | InNode (node_type_list, f1) ->
        in_node node_type_list f1 an lcxt
    | Not f1 -> (
      match eval_formula f1 an lcxt with Some _ -> None | None -> Some an )
    | And (f1, f2) ->
        eval_AND an lcxt f1 f2
    | AndWithWitnesses (f1, f2, (name, params)) ->
        eval_AndWithWitnesses an lcxt f1 f2 name params
    | Or (f1, f2) ->
        eval_OR an lcxt f1 f2
    | Implies (f1, f2) ->
        eval_Implies an lcxt f1 f2
    | AU (trans, f1, f2) ->
        eval_AU f1 f2 an lcxt trans
    | EU (trans, f1, f2) ->
        eval_EU f1 f2 an lcxt trans
    | EF (trans, f1) ->
        eval_EF f1 an lcxt trans
    | AF (trans, f1) ->
        eval_formula (AU (trans, True, f1)) an lcxt
    | AG (trans, f1) ->
        eval_formula (Not (EF (trans, Not f1))) an lcxt
    | EX (trans, f1) ->
        eval_EX ?keep_witness f1 an lcxt trans
    | AX (trans, f1) ->
        eval_formula (Not (EX (trans, Not f1))) an lcxt
    | EH (cl, phi) ->
        eval_EH cl phi an lcxt
    | EG (trans, f1) ->
        (* st |= EG f1 <=> st |= f1 /\ EX EG f1 *)
        eval_formula (And (f1, EX (trans, EG (trans, f1)))) an lcxt
    | ET (tl, sw, phi) ->
        eval_ET tl sw phi an lcxt
    | InObjCClass (f1, f2) ->
        eval_InObjCClass an lcxt f1 f2
  in
  debug_eval_end res ;
  res
