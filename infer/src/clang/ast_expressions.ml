(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

open Clang_ast_t
open CFrontend_utils

(** This module creates extra ast constructs that are needed for the translation *)

let dummy_source_range () =
  let dummy_source_loc = {
    sl_file = None;
    sl_line = None;
    sl_column = None
  } in
  (dummy_source_loc, dummy_source_loc)

let dummy_stmt_info () =
  {
    Clang_ast_t.si_pointer = Ast_utils.get_fresh_pointer ();
    Clang_ast_t.si_source_range = dummy_source_range ()
  }

let dummy_decl_info decl_info =
  {
    decl_info with
    Clang_ast_t.di_pointer = Ast_utils.get_fresh_pointer ();
    Clang_ast_t.di_source_range = dummy_source_range ();
  }

let empty_decl_info = {
  Clang_ast_t.di_pointer = "";
  Clang_ast_t.di_parent_pointer = None;
  Clang_ast_t.di_previous_decl = `None;
  Clang_ast_t.di_source_range = dummy_source_range ();
  Clang_ast_t.di_owning_module = None;
  Clang_ast_t.di_is_hidden = false;
  Clang_ast_t.di_is_implicit = false;
  Clang_ast_t.di_is_used = true;
  Clang_ast_t.di_is_this_declaration_referenced = true;
  Clang_ast_t.di_is_invalid_decl = false;
  Clang_ast_t.di_attributes = [];
  Clang_ast_t.di_full_comment = None;
}

let stmt_info_with_fresh_pointer stmt_info =
  {
    Clang_ast_t.si_pointer = Ast_utils.get_fresh_pointer ();
    Clang_ast_t.si_source_range = stmt_info.si_source_range
  }

let create_qual_type s =
  {
    Clang_ast_t.qt_raw = s;
    Clang_ast_t.qt_desugared = Some s
  }

let create_pointer_type s =
  create_qual_type (s^" *")

let create_int_type () = create_qual_type "int"

let create_void_type () = create_qual_type "void *"

let create_id_type () = create_qual_type "id"

let create_char_type () = create_qual_type "char *"

let create_integer_literal stmt_info n =
  let stmt_info = dummy_stmt_info () in
  let expr_info = {
    Clang_ast_t.ei_qual_type = create_int_type ();
    Clang_ast_t.ei_value_kind = `RValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let integer_literal_info = {
    Clang_ast_t.ili_is_signed = true;
    Clang_ast_t.ili_bitwidth = 32;
    Clang_ast_t.ili_value = n
  } in
  IntegerLiteral (stmt_info, [], expr_info, integer_literal_info)

let create_cstyle_cast_expr stmt_info stmts qt =
  let expr_info = {
    Clang_ast_t.ei_qual_type = create_void_type ();
    Clang_ast_t.ei_value_kind = `RValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let cast_expr = {
    Clang_ast_t.cei_cast_kind = `NullToPointer;
    Clang_ast_t.cei_base_path = []
  } in
  CStyleCastExpr (stmt_info, stmts, expr_info, cast_expr, qt)

let create_parent_expr stmt_info stmts =
  let expr_info = {
    Clang_ast_t.ei_qual_type = create_void_type ();
    Clang_ast_t.ei_value_kind = `RValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  ParenExpr (stmt_info, stmts, expr_info)

let create_implicit_cast_expr stmt_info stmts typ cast_kind =
  let expr_info = {
    Clang_ast_t.ei_qual_type = typ;
    Clang_ast_t.ei_value_kind = `RValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let cast_expr_info = {
    Clang_ast_t.cei_cast_kind = cast_kind;
    Clang_ast_t.cei_base_path = []
  } in
  ImplicitCastExpr (stmt_info, stmts, expr_info, cast_expr_info)

let create_nil stmt_info =
  let integer_literal = create_integer_literal stmt_info "0" in
  let cstyle_cast_expr = create_cstyle_cast_expr stmt_info [integer_literal] (create_int_type ()) in
  let paren_expr = create_parent_expr stmt_info [cstyle_cast_expr] in
  let implicit_cast_expr = create_implicit_cast_expr stmt_info [paren_expr] (create_id_type ()) `NullToPointer in
  implicit_cast_expr

let dummy_stmt () =
  let pointer = Ast_utils.get_fresh_pointer () in
  let source_range = dummy_source_range () in
  NullStmt({ Clang_ast_t.si_pointer = pointer; Clang_ast_t.si_source_range = source_range } ,[])

let make_stmt_info di =
  { Clang_ast_t.si_pointer = di.Clang_ast_t.di_pointer; Clang_ast_t.si_source_range = di.Clang_ast_t.di_source_range }

let make_expr_info qt objc_kind = {
  Clang_ast_t.ei_qual_type = qt;
  Clang_ast_t.ei_value_kind = `LValue;
  Clang_ast_t.ei_object_kind = objc_kind;}

let make_method_decl_info mdi body = {
  Clang_ast_t.omdi_is_instance_method = mdi.Clang_ast_t.omdi_is_instance_method;
  Clang_ast_t.omdi_result_type = mdi.Clang_ast_t.omdi_result_type;
  Clang_ast_t.omdi_parameters = mdi.Clang_ast_t.omdi_parameters;
  Clang_ast_t.omdi_is_variadic = mdi.Clang_ast_t.omdi_is_variadic;
  Clang_ast_t.omdi_body = Some body; }

let make_decl_ref_exp stmt_info expr_info drei =
  let stmt_info = {
    Clang_ast_t.si_pointer = Ast_utils.get_fresh_pointer ();
    Clang_ast_t.si_source_range = stmt_info.Clang_ast_t.si_source_range
  } in
  DeclRefExpr(stmt_info, [], expr_info, drei)

let make_obj_c_message_expr_info_instance sel =
  {
    Clang_ast_t.omei_selector = sel;
    Clang_ast_t.omei_receiver_kind = `Instance
  }

let make_obj_c_message_expr_info_class selector qt =
  {
    omei_selector = selector;
    omei_receiver_kind = `Class (create_qual_type qt);
  }

let make_general_decl_ref k name is_hidden qt = {
  Clang_ast_t.dr_kind = k;
  Clang_ast_t.dr_name = Some name;
  Clang_ast_t.dr_is_hidden = is_hidden ;
  Clang_ast_t.dr_qual_type = Some (qt)
}

let make_decl_ref name =
  make_general_decl_ref (`Var) name false (create_int_type ())

let make_decl_ref_self qt = {
  Clang_ast_t.dr_kind = `ImplicitParam;
  Clang_ast_t.dr_name = Some "self";
  Clang_ast_t.dr_is_hidden = false ;
  Clang_ast_t.dr_qual_type = Some qt
}

let make_decl_ref_expr_info decl_ref = {
  Clang_ast_t.drti_decl_ref = Some decl_ref;
  Clang_ast_t.drti_found_decl_ref = None;
}

let make_obj_c_ivar_ref_expr_info k n qt = {
  Clang_ast_t.ovrei_decl_ref = make_general_decl_ref k n false qt;
  Clang_ast_t.ovrei_pointer = Ast_utils.get_fresh_pointer ();
  Clang_ast_t.ovrei_is_free_ivar = true;
}

(* Build an AST cast expression of a decl_ref_expr *)
let make_cast_expr qt di decl_ref_expr_info objc_kind =
  let expr_info = make_expr_info qt objc_kind in
  let stmt_info = make_stmt_info di in
  let decl_ref_exp = make_decl_ref_exp stmt_info expr_info decl_ref_expr_info in
  let cast_expr = {
    Clang_ast_t.cei_cast_kind = `LValueToRValue;
    Clang_ast_t.cei_base_path = []
  } in
  let cast_exp_rhs = ImplicitCastExpr(stmt_info, [decl_ref_exp], expr_info, cast_expr) in
  cast_exp_rhs

(* Build AST expression self.field_name as `LValue *)
let make_self_field class_type di qt field_name =
  let qt_class = create_qual_type class_type in
  let expr_info = make_expr_info qt `ObjCProperty in
  let stmt_info = make_stmt_info di in
  let cast_exp = make_cast_expr qt_class di (make_decl_ref_expr_info (make_decl_ref_self qt_class)) `ObjCProperty in
  let obj_c_ivar_ref_expr_info = make_obj_c_ivar_ref_expr_info (`ObjCIvar) field_name qt in
  let ivar_ref_exp = ObjCIvarRefExpr(stmt_info, [cast_exp], expr_info, obj_c_ivar_ref_expr_info) in
  ivar_ref_exp

(* Build AST expression for self.field_name casted as `RValue. *)
let make_deref_self_field class_decl_opt di qt field_name =
  let stmt_info = make_stmt_info di in
  let ivar_ref_exp = make_self_field class_decl_opt di qt field_name in
  let expr_info' = make_expr_info qt `ObjCProperty in
  let cast_exp_info =
    {
      Clang_ast_t.cei_cast_kind = `LValueToRValue;
      Clang_ast_t.cei_base_path = []
    } in
  let cast_exp' = ImplicitCastExpr(stmt_info, [ivar_ref_exp], expr_info', cast_exp_info) in
  cast_exp'

let make_objc_ivar_decl decl_info qt property_impl_decl_info =
  let name = Ast_utils.property_name property_impl_decl_info in
  let qt = match qt with
    | Some qt' -> qt'
    | None -> (* a qual_type was not found by the caller, so we try to get it out of property_impl_decl_info *)
        (match property_impl_decl_info.Clang_ast_t.opidi_ivar_decl with
          | Some decl_ref -> (match decl_ref.Clang_ast_t.dr_qual_type with
                | Some qt' -> qt'
                | None -> assert false)
          | _ -> assert false) in
  let field_decl_info = {
    Clang_ast_t.fldi_is_mutable = true;
    Clang_ast_t.fldi_is_module_private = true;
    Clang_ast_t.fldi_init_expr = None;
    Clang_ast_t.fldi_bit_width_expr = None } in
  let obj_c_ivar_decl_info = {
    Clang_ast_t.ovdi_is_synthesize = true; (* NOTE: We set true here because we use this definition to synthesize the getter/setter*)
    Clang_ast_t.ovdi_access_control = `Private } in
  ObjCIvarDecl(decl_info, name, qt, field_decl_info, obj_c_ivar_decl_info)

let make_expr_info qt =
  {
    Clang_ast_t.ei_qual_type = qt;
    Clang_ast_t.ei_value_kind = `LValue;
    Clang_ast_t.ei_object_kind = `ObjCProperty
  }

let make_decl_ref_exp_var (var_name, var_qt) var_kind stmt_info =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  let decl_ref = make_general_decl_ref var_kind var_name false var_qt in
  let expr_info = make_expr_info var_qt in
  make_decl_ref_exp stmt_info expr_info (make_decl_ref_expr_info decl_ref)

let make_message_expr param_qt selector decl_ref_exp stmt_info add_cast =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  let parameters =
    if add_cast then
      let cast_expr = create_implicit_cast_expr stmt_info [decl_ref_exp] param_qt `LValueToRValue in
      [cast_expr]
    else [decl_ref_exp] in
  let obj_c_message_expr_info = make_obj_c_message_expr_info_instance selector in
  let expr_info = make_expr_info param_qt in
  ObjCMessageExpr (stmt_info, parameters, expr_info, obj_c_message_expr_info)

let make_compound_stmt stmts stmt_info =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  CompoundStmt (stmt_info, stmts)

let make_binary_stmt stmt1 stmt2 stmt_info expr_info boi =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  BinaryOperator(stmt_info, [stmt1; stmt2], expr_info, boi)

let make_next_object_exp stmt_info item items =
  let var_decl_ref, var_type =
    match item with
    | DeclStmt (stmt_info, _, [VarDecl(di, var_name, var_type, _)]) ->
        let decl_ref = make_general_decl_ref `Var var_name false var_type in
        let stmt_info_var = {
          si_pointer = di.Clang_ast_t.di_pointer;
          si_source_range = di.Clang_ast_t.di_source_range
        } in
        DeclRefExpr(stmt_info_var, [], (make_expr_info var_type), (make_decl_ref_expr_info decl_ref)),
        var_type
    | _ -> assert false in
  let message_call = make_message_expr (create_qual_type CFrontend_config.id_cl)
      CFrontend_config.next_object items stmt_info false in
  let boi = { Clang_ast_t.boi_kind = `Assign } in
  make_binary_stmt var_decl_ref message_call stmt_info (make_expr_info var_type) boi

let empty_var_decl = {
  vdi_storage_class = None;
  vdi_tls_kind =`Tls_none;
  vdi_is_module_private = false;
  vdi_is_nrvo_variable = false;
  vdi_init_expr = None
}

(* dispatch_once(v,block_def) is transformed as: *)
(* void (^block_var)()=block_def; block_var() *)
let translate_dispatch_function block_name stmt_info stmt_list ei n =
  let block_expr =
    try Utils.list_nth stmt_list (n + 1)
    with Not_found -> assert false in
  match block_expr with BlockExpr(bsi, bsl, bei, bd) ->
      let qt = bei.Clang_ast_t.ei_qual_type in
      let cast_info = { cei_cast_kind = `BitCast; cei_base_path =[]} in
      let block_def = ImplicitCastExpr(stmt_info,[block_expr], bei, cast_info) in
      let decl_info = { empty_decl_info
        with di_pointer = stmt_info.si_pointer; di_source_range = stmt_info.si_source_range } in
      let var_decl_info = { empty_var_decl with vdi_init_expr = Some block_def } in
      let block_var_decl = VarDecl(decl_info, block_name, ei.ei_qual_type, var_decl_info) in
      let decl_stmt = DeclStmt(stmt_info,[], [block_var_decl]) in
      let expr_info_call = {
        Clang_ast_t.ei_qual_type = create_void_type ();
        Clang_ast_t.ei_value_kind = `XValue;
        Clang_ast_t.ei_object_kind = `Ordinary
      } in
      let expr_info_dre = {
        Clang_ast_t.ei_qual_type = qt;
        Clang_ast_t.ei_value_kind = `LValue;
        Clang_ast_t.ei_object_kind = `Ordinary
      } in
      let decl_ref = {
        dr_kind = `Var;
        dr_name = Some block_name;
        dr_is_hidden = false;
        dr_qual_type = Some qt;
      } in
      let decl_ref_expr_info = {
        drti_decl_ref = Some decl_ref;
        drti_found_decl_ref = None
      } in
      let cast_info_call = { cei_cast_kind = `LValueToRValue; cei_base_path =[]} in
      let decl_ref_exp = DeclRefExpr(stmt_info, [], expr_info_dre, decl_ref_expr_info) in
      let stmt_call = ImplicitCastExpr(stmt_info, [decl_ref_exp], bei, cast_info_call) in
      let call_block_var = CallExpr(stmt_info, [stmt_call], expr_info_call) in
      CompoundStmt (stmt_info, [decl_stmt; call_block_var]), qt
  | _ -> assert false (* when we call this function we have already checked that this cannot be possible *)

(* We translate the logical negation of an integer with a conditional*)
(* !x <=> x?0:1 *)
let trans_negation_with_conditional stmt_info expr_info stmt_list =
  let stmt_list_cond = stmt_list @ [create_integer_literal stmt_info "0"] @ [create_integer_literal stmt_info "1"] in
  ConditionalOperator(stmt_info, stmt_list_cond, expr_info)
