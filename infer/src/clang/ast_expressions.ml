(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module creates extra ast constructs that are needed for the translation *)

open! IStd

let stmt_info_with_fresh_pointer stmt_info =
  { Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()
  ; si_source_range= stmt_info.Clang_ast_t.si_source_range }


let create_qual_type ?(quals = Typ.mk_type_quals ()) qt_type_ptr =
  { Clang_ast_t.qt_type_ptr
  ; qt_is_const= Typ.is_const quals
  ; qt_is_volatile= Typ.is_volatile quals
  ; qt_is_restrict= Typ.is_restrict quals }


let builtin_to_qual_type kind = create_qual_type (Clang_ast_extend.Builtin kind)

let create_pointer_qual_type ?quals typ = create_qual_type ?quals (Clang_ast_extend.PointerOf typ)

let create_reference_qual_type ?quals typ =
  create_qual_type ?quals (Clang_ast_extend.ReferenceOf typ)


let create_int_type = builtin_to_qual_type `Int

let create_void_type = builtin_to_qual_type `Void

let create_void_star_type = create_pointer_qual_type create_void_type

let create_id_type = create_pointer_qual_type (builtin_to_qual_type `ObjCId)

let create_char_type = builtin_to_qual_type `Char_S

let create_char_star_type ?quals () = create_pointer_qual_type ?quals create_char_type

let create_class_qual_type ?quals typename =
  create_qual_type ?quals (Clang_ast_extend.ClassType typename)


let create_integer_literal n =
  let stmt_info = CAst_utils.dummy_stmt_info () in
  let expr_info =
    {Clang_ast_t.ei_qual_type= create_int_type; ei_value_kind= `RValue; ei_object_kind= `Ordinary}
  in
  let integer_literal_info = {Clang_ast_t.ili_is_signed= true; ili_bitwidth= 32; ili_value= n} in
  Clang_ast_t.IntegerLiteral (stmt_info, [], expr_info, integer_literal_info)


let create_cstyle_cast_expr stmt_info stmts qt =
  let expr_info =
    { Clang_ast_t.ei_qual_type= create_void_star_type
    ; ei_value_kind= `RValue
    ; ei_object_kind= `Ordinary }
  in
  let cast_expr = {Clang_ast_t.cei_cast_kind= `NullToPointer; cei_base_path= []} in
  Clang_ast_t.CStyleCastExpr (stmt_info, stmts, expr_info, cast_expr, qt)


let create_parent_expr stmt_info stmts =
  let expr_info =
    { Clang_ast_t.ei_qual_type= create_void_star_type
    ; ei_value_kind= `RValue
    ; ei_object_kind= `Ordinary }
  in
  Clang_ast_t.ParenExpr (stmt_info, stmts, expr_info)


let create_implicit_cast_expr stmt_info stmts typ cast_kind =
  let expr_info =
    {Clang_ast_t.ei_qual_type= typ; ei_value_kind= `RValue; ei_object_kind= `Ordinary}
  in
  let cast_expr_info = {Clang_ast_t.cei_cast_kind= cast_kind; cei_base_path= []} in
  Clang_ast_t.ImplicitCastExpr (stmt_info, stmts, expr_info, cast_expr_info)


let create_nil stmt_info =
  let integer_literal = create_integer_literal "0" in
  let cstyle_cast_expr = create_cstyle_cast_expr stmt_info [integer_literal] create_int_type in
  let paren_expr = create_parent_expr stmt_info [cstyle_cast_expr] in
  create_implicit_cast_expr stmt_info [paren_expr] create_id_type `NullToPointer


let make_expr_info qt vk objc_kind =
  {Clang_ast_t.ei_qual_type= qt; ei_value_kind= vk; ei_object_kind= objc_kind}


let make_expr_info_with_objc_kind qt objc_kind = make_expr_info qt `LValue objc_kind

let make_obj_c_message_expr_info_instance sel =
  { Clang_ast_t.omei_selector= sel
  ; omei_receiver_kind= `Instance
  ; omei_is_definition_found= false
  ; omei_decl_pointer= None (* TODO look into it *) }


let make_obj_c_message_expr_info_class selector tname pointer =
  { Clang_ast_t.omei_selector= selector
  ; omei_receiver_kind= `Class (create_class_qual_type tname)
  ; omei_is_definition_found= false
  ; omei_decl_pointer= pointer }


let make_decl_ref k decl_ptr name is_hidden qt_opt =
  { Clang_ast_t.dr_kind= k
  ; dr_decl_pointer= decl_ptr
  ; dr_name= Some name
  ; dr_is_hidden= is_hidden
  ; dr_qual_type= qt_opt }


let make_decl_ref_qt k decl_ptr name is_hidden qt =
  make_decl_ref k decl_ptr name is_hidden (Some qt)


let make_decl_ref_expr_info decl_ref =
  {Clang_ast_t.drti_decl_ref= Some decl_ref; drti_found_decl_ref= None}


let make_message_expr param_qt selector decl_ref_exp stmt_info add_cast =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  let parameters =
    if add_cast then
      let cast_expr =
        create_implicit_cast_expr stmt_info [decl_ref_exp] param_qt `LValueToRValue
      in
      [cast_expr]
    else [decl_ref_exp]
  in
  let obj_c_message_expr_info = make_obj_c_message_expr_info_instance selector in
  let expr_info = make_expr_info_with_objc_kind param_qt `ObjCProperty in
  Clang_ast_t.ObjCMessageExpr (stmt_info, parameters, expr_info, obj_c_message_expr_info)


let make_binary_stmt stmt1 stmt2 stmt_info expr_info boi =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  Clang_ast_t.BinaryOperator (stmt_info, [stmt1; stmt2], expr_info, boi)


let make_next_object_exp stmt_info item items =
  let rec get_decl_ref item =
    match item with
    | Clang_ast_t.DeclStmt (_, _, [Clang_ast_t.VarDecl (di, name_info, var_qual_type, _)]) ->
        let decl_ptr = di.Clang_ast_t.di_pointer in
        let decl_ref = make_decl_ref_qt `Var decl_ptr name_info false var_qual_type in
        let stmt_info_var =
          { Clang_ast_t.si_pointer= di.Clang_ast_t.di_pointer
          ; si_source_range= di.Clang_ast_t.di_source_range }
        in
        let expr_info = make_expr_info_with_objc_kind var_qual_type `ObjCProperty in
        let decl_ref_expr_info = make_decl_ref_expr_info decl_ref in
        (Clang_ast_t.DeclRefExpr (stmt_info_var, [], expr_info, decl_ref_expr_info), var_qual_type)
    | Clang_ast_t.DeclRefExpr (_, _, expr_info, _) ->
        (item, expr_info.Clang_ast_t.ei_qual_type)
    | stmt -> (
        let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
        match stmts with
        | [stmt] ->
            get_decl_ref stmt
        | _ ->
            CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
              "unexpected item %a"
              (Pp.to_string ~f:Clang_ast_j.string_of_stmt)
              item )
  in
  let var_decl_ref, var_type = get_decl_ref item in
  let message_call =
    make_message_expr create_id_type CFrontend_config.next_object items stmt_info false
  in
  let boi = {Clang_ast_t.boi_kind= `Assign} in
  let expr_info = make_expr_info_with_objc_kind var_type `ObjCProperty in
  let assignment = make_binary_stmt var_decl_ref message_call stmt_info expr_info boi in
  let boi' = {Clang_ast_t.boi_kind= `NE} in
  let cast = create_implicit_cast_expr stmt_info [var_decl_ref] var_type `LValueToRValue in
  let nil_exp = create_nil stmt_info in
  let loop_cond = make_binary_stmt cast nil_exp stmt_info expr_info boi' in
  (assignment, loop_cond)


(* We translate an expression with a conditional*)
(* x <=> x?1:0 *)
let trans_with_conditional stmt_info expr_info stmt_list =
  let stmt_list_cond = stmt_list @ [create_integer_literal "1"] @ [create_integer_literal "0"] in
  Clang_ast_t.ConditionalOperator (stmt_info, stmt_list_cond, expr_info)


(* We translate the logical negation of an expression with a conditional*)
(* !x <=> x?0:1 *)
let trans_negation_with_conditional stmt_info expr_info stmt_list =
  let stmt_list_cond = stmt_list @ [create_integer_literal "0"] @ [create_integer_literal "1"] in
  Clang_ast_t.ConditionalOperator (stmt_info, stmt_list_cond, expr_info)
