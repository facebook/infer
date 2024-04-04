(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module creates extra ast constructs that are needed for the translation *)

open! IStd

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


let create_class_pointer_qual_type ?quals typename =
  create_pointer_qual_type ?quals (create_class_qual_type ?quals typename)


let create_decl_info stmt_info pointer =
  { Clang_ast_t.di_pointer= pointer
  ; di_parent_pointer= None
  ; di_source_range= stmt_info.Clang_ast_t.si_source_range
  ; di_owning_module= None
  ; di_is_hidden= false
  ; di_is_implicit= false
  ; di_is_used= true
  ; di_is_this_declaration_referenced= true
  ; di_is_invalid_decl= false
  ; di_attributes= []
  ; di_full_comment= None
  ; di_access= `None }


let default_var_decl_info =
  { Clang_ast_t.vdi_is_global= false
  ; vdi_is_extern= false
  ; vdi_is_static= false
  ; vdi_is_static_local= false
  ; vdi_is_static_data_member= false
  ; vdi_is_constexpr= false
  ; vdi_is_init_ice= false
  ; vdi_init_expr= None
  ; vdi_is_init_expr_cxx11_constant= false
  ; vdi_parm_index_in_function= None }


let create_named_decl_info name = {Clang_ast_t.ni_name= name; ni_qual_name= [name]}

let create_decl_ref pointer ni qual_type =
  { Clang_ast_t.dr_kind= `Var
  ; dr_decl_pointer= pointer
  ; dr_name= Some ni
  ; dr_is_hidden= false
  ; dr_qual_type= Some qual_type }


let create_decl_ref_expr stmt_info pointer ni qual_type =
  Clang_ast_t.DeclRefExpr
    ( stmt_info
    , []
    , {ei_qual_type= qual_type; ei_value_kind= `LValue; ei_object_kind= `Ordinary}
    , {drti_decl_ref= Some (create_decl_ref pointer ni qual_type); drti_found_decl_ref= None} )


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
  Clang_ast_t.ImplicitCastExpr (stmt_info, stmts, expr_info, cast_expr_info, false)


let create_nil stmt_info =
  let integer_literal = create_integer_literal "0" in
  let cstyle_cast_expr = create_cstyle_cast_expr stmt_info [integer_literal] create_int_type in
  let paren_expr = create_parent_expr stmt_info [cstyle_cast_expr] in
  create_implicit_cast_expr stmt_info [paren_expr] create_id_type `NullToPointer


let make_obj_c_message_expr_info_class selector tname pointer =
  { Clang_ast_t.omei_selector= selector
  ; omei_receiver_kind= `Class (create_class_qual_type tname)
  ; omei_is_definition_found= false
  ; omei_decl_pointer= pointer }


let create_obj_c_message_expr stmt_info qual_type selector args =
  Clang_ast_t.ObjCMessageExpr
    ( stmt_info
    , args
    , {ei_qual_type= qual_type; ei_value_kind= `RValue; ei_object_kind= `Ordinary}
    , { omei_selector= selector
      ; omei_is_definition_found= false
      ; omei_decl_pointer= None
      ; omei_receiver_kind= `Instance } )


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
