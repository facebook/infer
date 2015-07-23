(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
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

(* given a stmt_info return the same stmt_info with a fresh pointer *)
let fresh_stmt_info stmt_info =
  { stmt_info with Clang_ast_t.si_pointer = Ast_utils.get_fresh_pointer () }

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

let empty_var_decl_info = {
  Clang_ast_t.vdi_storage_class = None;
  Clang_ast_t.vdi_tls_kind =`Tls_none;
  Clang_ast_t.vdi_is_module_private = false;
  Clang_ast_t.vdi_is_nrvo_variable = false;
  Clang_ast_t.vdi_init_expr = None;
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

let create_BOOL_type () = { qt_raw = "BOOL"; qt_desugared = Some("signed char") }

let create_void_unsigned_long_type () = create_qual_type "void *(unsigned long)"

let create_unsigned_long_type () = create_qual_type "unsigned long"

let create_void_void_type () = create_qual_type "void (void *)"

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

let make_expr_info qt vk objc_kind = {
  Clang_ast_t.ei_qual_type = qt;
  Clang_ast_t.ei_value_kind = vk;
  Clang_ast_t.ei_object_kind = objc_kind;}

let make_expr_info_with_objc_kind qt objc_kind =
  make_expr_info qt `LValue objc_kind

let make_lvalue_obc_prop_expr_info qt =
  make_expr_info qt `LValue `ObjCProperty

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

let make_name_decl name = {
  Clang_ast_t.ni_name = name;
  Clang_ast_t.ni_qual_name = [name];
}

let make_decl_ref k decl_ptr name is_hidden qt_opt = {
  Clang_ast_t.dr_kind = k;
  Clang_ast_t.dr_decl_pointer = decl_ptr;
  Clang_ast_t.dr_name = Some (make_name_decl name);
  Clang_ast_t.dr_is_hidden = is_hidden ;
  Clang_ast_t.dr_qual_type = qt_opt
}

let make_decl_ref_qt k decl_ptr name is_hidden qt =
  make_decl_ref k decl_ptr name is_hidden (Some qt)

let make_decl_ref_no_qt k decl_ptr name is_hidden =
  make_decl_ref k decl_ptr name is_hidden None

let make_decl_ref_invalid k name is_hidden qt =
  make_decl_ref k (Ast_utils.get_invalid_pointer ()) name is_hidden (Some qt)

let make_decl_ref_self ptr qt = {
  Clang_ast_t.dr_kind = `ImplicitParam;
  Clang_ast_t.dr_decl_pointer = ptr;
  Clang_ast_t.dr_name = Some (make_name_decl "self");
  Clang_ast_t.dr_is_hidden = false ;
  Clang_ast_t.dr_qual_type = Some qt
}

let make_decl_ref_expr_info decl_ref = {
  Clang_ast_t.drti_decl_ref = Some decl_ref;
  Clang_ast_t.drti_found_decl_ref = None;
}

let make_obj_c_ivar_ref_expr_info k ptr n qt = {
  Clang_ast_t.ovrei_decl_ref = make_decl_ref_qt k ptr n false qt;
  Clang_ast_t.ovrei_pointer = Ast_utils.get_fresh_pointer ();
  Clang_ast_t.ovrei_is_free_ivar = true;
}

(* Build an AST cast expression of a decl_ref_expr *)
let make_cast_expr qt di decl_ref_expr_info objc_kind =
  let expr_info = make_expr_info_with_objc_kind qt objc_kind in
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
  let expr_info = make_expr_info_with_objc_kind qt `ObjCProperty in
  let stmt_info = make_stmt_info di in
  let cast_exp = make_cast_expr qt_class di (make_decl_ref_expr_info (make_decl_ref_self di.di_pointer qt_class)) `ObjCProperty in
  let obj_c_ivar_ref_expr_info = make_obj_c_ivar_ref_expr_info (`ObjCIvar) di.di_pointer field_name qt in
  let ivar_ref_exp = ObjCIvarRefExpr(stmt_info, [cast_exp], expr_info, obj_c_ivar_ref_expr_info) in
  ivar_ref_exp

(* Build AST expression for self.field_name casted as `RValue. *)
let make_deref_self_field class_decl_opt di qt field_name =
  let stmt_info = make_stmt_info di in
  let ivar_ref_exp = make_self_field class_decl_opt di qt field_name in
  let expr_info' = make_expr_info_with_objc_kind qt `ObjCProperty in
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
  ObjCIvarDecl(decl_info, make_name_decl name, qt, field_decl_info, obj_c_ivar_decl_info)

let make_expr_info qt =
  {
    Clang_ast_t.ei_qual_type = qt;
    Clang_ast_t.ei_value_kind = `LValue;
    Clang_ast_t.ei_object_kind = `ObjCProperty
  }

let make_general_expr_info qt vk ok =
  {
    Clang_ast_t.ei_qual_type = qt;
    Clang_ast_t.ei_value_kind = vk;
    Clang_ast_t.ei_object_kind = ok
  }

let make_ObjCBoolLiteralExpr stmt_info value =
  let ei = make_expr_info (create_BOOL_type ()) in
  ObjCBoolLiteralExpr((fresh_stmt_info stmt_info),[], ei, value)

let make_decl_ref_exp_var (var_name, var_qt, var_ptr) var_kind stmt_info =
  let stmt_info = stmt_info_with_fresh_pointer stmt_info in
  let decl_ref = make_decl_ref_qt var_kind var_ptr var_name false var_qt in
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
  let expr_info = make_expr_info_with_objc_kind param_qt `ObjCProperty in
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
    | DeclStmt (stmt_info, _, [VarDecl(di, name_info, var_type, _)]) ->
        let var_name = name_info.Clang_ast_t.ni_name in
        let decl_ptr = di.Clang_ast_t.di_pointer in
        let decl_ref = make_decl_ref_qt `Var decl_ptr var_name false var_type in
        let stmt_info_var = {
          si_pointer = di.Clang_ast_t.di_pointer;
          si_source_range = di.Clang_ast_t.di_source_range
        } in
        DeclRefExpr(stmt_info_var, [], (make_expr_info_with_objc_kind var_type `ObjCProperty), (make_decl_ref_expr_info decl_ref)),
        var_type
    | _ -> assert false in
  let message_call = make_message_expr (create_qual_type CFrontend_config.id_cl)
      CFrontend_config.next_object items stmt_info false in
  let boi = { Clang_ast_t.boi_kind = `Assign } in
  make_binary_stmt var_decl_ref message_call stmt_info (make_expr_info_with_objc_kind var_type `ObjCProperty) boi

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
  let block_name_info = make_name_decl block_name in
  match block_expr with
  | BlockExpr(bsi, bsl, bei, bd) ->
      let qt = bei.Clang_ast_t.ei_qual_type in
      let cast_info = { cei_cast_kind = `BitCast; cei_base_path =[]} in
      let block_def = ImplicitCastExpr(stmt_info,[block_expr], bei, cast_info) in
      let decl_info = { empty_decl_info
        with di_pointer = stmt_info.si_pointer; di_source_range = stmt_info.si_source_range } in
      let var_decl_info = { empty_var_decl with vdi_init_expr = Some block_def } in
      let block_var_decl = VarDecl(decl_info, block_name_info, ei.ei_qual_type, var_decl_info) in
      let decl_stmt = DeclStmt(stmt_info,[], [block_var_decl]) in

      let expr_info_call = make_general_expr_info (create_void_type ()) `XValue `Ordinary in
      let expr_info_dre = make_expr_info_with_objc_kind qt `Ordinary in
      let decl_ref = make_decl_ref_qt `Var stmt_info.si_pointer block_name false qt in
      let decl_ref_expr_info = make_decl_ref_expr_info decl_ref in
      let cast_info_call = { cei_cast_kind = `LValueToRValue; cei_base_path =[]} in
      let decl_ref_exp = DeclRefExpr(stmt_info, [], expr_info_dre, decl_ref_expr_info) in
      let stmt_call = ImplicitCastExpr(stmt_info, [decl_ref_exp], bei, cast_info_call) in
      let call_block_var = CallExpr(stmt_info, [stmt_call], expr_info_call) in
      CompoundStmt (stmt_info, [decl_stmt; call_block_var]), qt
  | _ -> assert false (* when we call this function we have already checked that this cannot be possible *)

(* Create declaration statement: qt vname = iexp *)
let make_DeclStmt stmt_info di qt vname iexp =
  let init_expr_opt, init_expr_l = match iexp with
    | Some iexp' ->
        let ie = create_implicit_cast_expr stmt_info [iexp'] qt `IntegralCast in
        Some ie, [ie]
    | None -> None, [] in
  let var_decl = VarDecl(di, vname, qt, { empty_var_decl_info with Clang_ast_t.vdi_init_expr = init_expr_opt;}) in
  DeclStmt(stmt_info, init_expr_l, [var_decl])

let build_OpaqueValueExpr si source_expr ei =
  let opaque_value_expr_info = { Clang_ast_t.ovei_source_expr = Some source_expr } in
  OpaqueValueExpr(si, [], ei, opaque_value_expr_info)

let pseudo_object_qt () =
  create_qual_type CFrontend_config.pseudo_object_type

(* Create expression PseudoObjectExpr for 'o.m' *)
let build_PseudoObjectExpr qt_m o_cast_decl_ref_exp mname =
  match o_cast_decl_ref_exp with
  | ImplicitCastExpr(si, stmt_list, ei, cast_expr_info) ->
      let ove = build_OpaqueValueExpr si o_cast_decl_ref_exp ei in
      let ei_opre = make_expr_info (pseudo_object_qt ()) in
      let obj_c_property_ref_expr_info = {
        Clang_ast_t.oprei_kind =
          `PropertyRef (make_decl_ref_no_qt `ObjCProperty si.si_pointer CFrontend_config.count false);
        Clang_ast_t.oprei_is_super_receiver = false;
        Clang_ast_t.oprei_is_messaging_getter = true;
        Clang_ast_t.oprei_is_messaging_setter = false;
      } in
      let opre = ObjCPropertyRefExpr(si, [ove], ei_opre, obj_c_property_ref_expr_info) in
      let ome = make_message_expr qt_m mname o_cast_decl_ref_exp si false in
      let poe_ei = make_general_expr_info qt_m `LValue `Ordinary in
      PseudoObjectExpr(si, [opre; ove; ome], poe_ei)
  | _ -> assert false

let create_call stmt_info decl_pointer function_name qt parameters =
  let expr_info_call = {
    Clang_ast_t.ei_qual_type = create_void_type ();
    Clang_ast_t.ei_value_kind = `XValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let expr_info_dre = make_expr_info_with_objc_kind qt `Ordinary in
  let decl_ref = make_decl_ref_qt `Function decl_pointer function_name false qt in
  let decl_ref_info = make_decl_ref_expr_info decl_ref in
  let decl_ref_exp = DeclRefExpr(stmt_info, [], expr_info_dre, decl_ref_info) in
  let cast = create_implicit_cast_expr (fresh_stmt_info stmt_info) [decl_ref_exp] qt `FunctionToPointerDecay in
  CallExpr(stmt_info, cast:: parameters, expr_info_call)

(* For a of type NSArray* Translate                                                                                              *)
(* [a enumerateObjectsUsingBlock:^(id object, NSUInteger idx, BOOL * stop) {                                 *)
(*      body_block                                                                                           *)
(*     };                                                                                                    *)
(*  ];                                                                                                       *)

(* as follows:                                                                                               *)

(*     NSArray *objects = a;                                                                                 *)
(*    void (^enumerateObjectsUsingBlock)(id, NSUInteger, BOOL* )= ^(id object, NSUInteger idx, BOOL* stop) { *)
(*         body_block                                                                                        *)
(*     };                                                                                                    *)
(*     BOOL *stop = malloc(sizeof(BOOL));                                                                    *)
(*     *stop = NO;                                                                                           *)

(*     for (NSUInteger idx=0; idx<objects.count; idx++) {                                                    *)
(*     id object= objects[idx];                                                                              *)
(*         enumerateObjectsUsingBlock(object, idx, stop);                                                    *)
(*         if ( *stop ==YES) break;                                                                           *)
(*     }                                                                                                     *)
(*     free(stop);                                                                                           *)
(*                                                                                                         *)
let translate_block_enumerate block_name stmt_info stmt_list ei =

  let rec get_name_pointers lp =
    match lp with
    | [] -> []
    | ParmVarDecl(di, name, qt, _):: lp' ->
        (name.Clang_ast_t.ni_name, di.Clang_ast_t.di_pointer, qt):: get_name_pointers lp'
    | _ -> assert false in

  let build_idx_decl pidx =
    match pidx with
    | ParmVarDecl(di_idx, name_idx, qt_idx, _) ->
        let zero = create_integer_literal stmt_info "0" in
        (* qt_idx idx = 0; *)
        let idx_decl_stmt = make_DeclStmt (fresh_stmt_info stmt_info) di_idx qt_idx name_idx (Some zero) in
        let idx_ei = make_expr_info qt_idx in
        let idx_decl_ref = make_decl_ref_qt `Var di_idx.di_pointer name_idx.Clang_ast_t.ni_name false qt_idx in
        let idx_drei = make_decl_ref_expr_info idx_decl_ref in
        let idx_decl_ref_exp = make_decl_ref_exp stmt_info idx_ei idx_drei in
        let idx_cast = create_implicit_cast_expr (fresh_stmt_info stmt_info) [idx_decl_ref_exp] qt_idx `LValueToRValue in
        idx_decl_stmt, idx_decl_ref_exp, idx_cast, qt_idx
    | _ -> assert false in

  let cast_expr decl_ref qt =
    let ei = make_expr_info qt in
    let drei = make_decl_ref_expr_info decl_ref in
    let decl_ref_exp = make_decl_ref_exp (fresh_stmt_info stmt_info) ei drei in
    create_implicit_cast_expr (fresh_stmt_info stmt_info) [decl_ref_exp] qt `LValueToRValue in

  (* build statement BOOL *stop = malloc(sizeof(BOOL)); *)
  let build_stop pstop =
    match pstop with
    | ParmVarDecl(di, name, qt, _) ->
        let qt_fun = create_void_unsigned_long_type () in
        let parameter = UnaryExprOrTypeTraitExpr((fresh_stmt_info stmt_info), [],
            make_expr_info (create_unsigned_long_type ()),
            { Clang_ast_t.uttei_kind = `SizeOf; Clang_ast_t.uttei_qual_type = Some (create_BOOL_type ()) }) in
        let malloc = create_call (fresh_stmt_info stmt_info) di.di_pointer CFrontend_config.malloc qt_fun [parameter] in
        let init_exp = create_implicit_cast_expr (fresh_stmt_info stmt_info) [malloc] qt `BitCast in
        make_DeclStmt (fresh_stmt_info stmt_info) di qt name (Some init_exp)
    | _ -> assert false in

  (* BOOL *stop =NO; *)
  let stop_equal_no pstop =
    match pstop with
    | ParmVarDecl(di, name, qt, _) ->
        let decl_ref = make_decl_ref_qt `Var di.di_pointer name.Clang_ast_t.ni_name false qt in
        let cast = cast_expr decl_ref qt in
        let lhs = UnaryOperator((fresh_stmt_info stmt_info), [cast], ei, { uoi_kind = `Deref; uoi_is_postfix = true }) in
        let bool_NO = make_ObjCBoolLiteralExpr stmt_info 0 in
        BinaryOperator((fresh_stmt_info stmt_info), [lhs; bool_NO], ei, { boi_kind = `Assign })
    | _ -> assert false in

  (* build statement free(stop); *)
  let free_stop pstop =
    match pstop with
    | ParmVarDecl(di, name, qt, _) ->
        let qt_fun = create_void_void_type () in
        let decl_ref = make_decl_ref_qt `Var di.di_pointer name.Clang_ast_t.ni_name false qt in
        let cast = cast_expr decl_ref qt in
        let parameter =
          create_implicit_cast_expr (fresh_stmt_info stmt_info) [cast] (create_void_type ()) `BitCast in
        create_call (fresh_stmt_info stmt_info) di.di_pointer CFrontend_config.free qt_fun [parameter]
    | _ -> assert false in

  (* idx<a.count *)
  let bin_op pidx array_decl_ref_exp =
    let idx_decl_stmt, idx_decl_ref_exp, idx_cast, idx_qt = build_idx_decl pidx in
    let rhs = build_PseudoObjectExpr idx_qt array_decl_ref_exp CFrontend_config.count in
    BinaryOperator((fresh_stmt_info stmt_info), [idx_cast; rhs], make_expr_info (create_int_type ()), { boi_kind = `LT }) in

  (*  idx++ *)
  let un_op idx_decl_ref_expr qt_idx =
    let idx_ei = make_expr_info qt_idx in
    UnaryOperator((fresh_stmt_info stmt_info), [idx_decl_ref_expr], idx_ei, { uoi_kind = `PostInc; uoi_is_postfix = true }) in

  let get_ei_from_cast cast =
    match cast with
    | ImplicitCastExpr(_, _, ei, _) -> ei
    | _ -> assert false in

  (* id object= objects[idx]; *)
  let build_object_DeclStmt pobj decl_ref_expr_array decl_ref_expr_idx qt_idx =
    match pobj with
    | ParmVarDecl(di_obj, name_obj, qt_obj, _) ->
        let poe_ei = make_general_expr_info qt_obj `LValue `Ordinary in
        let ei_array = get_ei_from_cast decl_ref_expr_array in
        let ove_array = build_OpaqueValueExpr (fresh_stmt_info stmt_info) decl_ref_expr_array ei_array in
        let ei_idx = get_ei_from_cast decl_ref_expr_idx in
        let ove_idx = build_OpaqueValueExpr (fresh_stmt_info stmt_info) decl_ref_expr_idx ei_idx in
        let objc_sre = ObjCSubscriptRefExpr((fresh_stmt_info stmt_info), [ove_array; ove_idx],
            make_expr_info (pseudo_object_qt ()),
            { osrei_kind =`ArraySubscript; osrei_getter = None; osrei_setter = None; }) in
        let obj_c_message_expr_info = { omei_selector = CFrontend_config.object_at_indexed_subscript_m; omei_receiver_kind =`Instance } in
        let ome = ObjCMessageExpr((fresh_stmt_info stmt_info), [ove_array; ove_idx], poe_ei, obj_c_message_expr_info) in
        let pseudo_obj_expr = PseudoObjectExpr((fresh_stmt_info stmt_info), [objc_sre; ove_array; ove_idx; ome], poe_ei) in
        let vdi = { empty_var_decl_info with vdi_init_expr = Some (pseudo_obj_expr) } in
        let var_decl = VarDecl(di_obj, name_obj, qt_obj, vdi) in
        DeclStmt((fresh_stmt_info stmt_info), [pseudo_obj_expr], [var_decl])
    | _ -> assert false in

  (* NSArray *objects = a *)
  let objects_array_DeclStmt init =
    let di = { empty_decl_info with Clang_ast_t.di_pointer = Ast_utils.get_fresh_pointer () } in
    let qt = create_qual_type CFrontend_config.ns_array_ptr in
    (* init should be ImplicitCastExpr of array a *)
    let vdi = { empty_var_decl_info with vdi_init_expr = Some (init) } in
    let var_decl = VarDecl(di, make_name_decl CFrontend_config.objects, qt, vdi) in
    DeclStmt((fresh_stmt_info stmt_info), [init], [var_decl]), [(CFrontend_config.objects, di.Clang_ast_t.di_pointer, qt)] in

  let make_object_cast_decl_ref_expr objects =
    match objects with
    | DeclStmt(si, _, [VarDecl(di, name, qt, vdi)]) ->
        let decl_ref = make_decl_ref_qt `Var si.si_pointer name.Clang_ast_t.ni_name false qt in
        cast_expr decl_ref qt
    | _ -> assert false in

  let build_cast_decl_ref_expr_from_parm p =
    match p with
    | ParmVarDecl(di, name, qt, _) ->
        let decl_ref = make_decl_ref_qt `Var di.di_pointer name.Clang_ast_t.ni_name false qt in
        cast_expr decl_ref qt
    | _ -> assert false in

  let make_block_decl be =
    match be with
    | BlockExpr(bsi, _, bei, _) ->
        let di = { empty_decl_info with Clang_ast_t.di_pointer = Ast_utils.get_fresh_pointer () } in
        let vdi = { empty_var_decl_info with vdi_init_expr = Some (be) } in
        let var_decl = VarDecl(di, make_name_decl block_name, bei.Clang_ast_t.ei_qual_type, vdi) in
        DeclStmt(bsi, [be], [var_decl]), [(block_name, di.Clang_ast_t.di_pointer, bei.Clang_ast_t.ei_qual_type)]
    | _ -> assert false in

  let make_block_call block_qt object_cast idx_cast stop_cast =
    let decl_ref = make_decl_ref_invalid `Var block_name false block_qt in
    let fun_cast = cast_expr decl_ref block_qt in
    let ei_call = make_expr_info (create_void_type ()) in
    CallExpr((fresh_stmt_info stmt_info), [fun_cast; object_cast; idx_cast; stop_cast], ei_call) in

  (* build statement "if (stop) break;" *)
  let build_if_stop stop_cast =
    let bool_qt = create_BOOL_type () in
    let ei = make_expr_info bool_qt in
    let unary_op = UnaryOperator((fresh_stmt_info stmt_info), [stop_cast], ei, { uoi_kind = `Deref; uoi_is_postfix = true }) in
    let cond = create_implicit_cast_expr (fresh_stmt_info stmt_info) [unary_op] bool_qt `LValueToRValue in
    let break_stmt = BreakStmt((fresh_stmt_info stmt_info),[]) in
    IfStmt((fresh_stmt_info stmt_info), [dummy_stmt (); cond; break_stmt; dummy_stmt ()]) in

  let translate params array_cast_decl_ref_exp block_decl block_qt =
    match params with
    | [pobj; pidx; pstop] ->
        let objects_decl, op = objects_array_DeclStmt array_cast_decl_ref_exp in
        let decl_stop = build_stop pstop in
        let assign_stop = stop_equal_no pstop in
        let objects = make_object_cast_decl_ref_expr objects_decl in
        let idx_decl_stmt, idx_decl_ref_exp, idx_cast, qt_idx = build_idx_decl pidx in
        let guard = bin_op pidx objects in
        let incr = un_op idx_decl_ref_exp qt_idx in
        let obj_assignment = build_object_DeclStmt pobj objects idx_cast qt_idx in
        let object_cast = build_cast_decl_ref_expr_from_parm pobj in
        let stop_cast = build_cast_decl_ref_expr_from_parm pstop in
        let call_block = make_block_call block_qt object_cast idx_cast stop_cast in
        let if_stop = build_if_stop stop_cast in
        let free_stop = free_stop pstop in
        [ objects_decl; block_decl; decl_stop; assign_stop;
        ForStmt(stmt_info, [idx_decl_stmt; dummy_stmt (); guard; incr;
          CompoundStmt(stmt_info, [obj_assignment; call_block; if_stop])]); free_stop], op
    | _ -> assert false in

  match stmt_list with
  | [s; BlockExpr(_, _, bei, BlockDecl(_, _, _, bdi)) as be] ->
      let block_decl, bv = make_block_decl be in
      let vars_to_register = get_name_pointers bdi.Clang_ast_t.bdi_parameters in
      let translated_stmt, op = translate bdi.Clang_ast_t.bdi_parameters s block_decl bei.Clang_ast_t.ei_qual_type in
      CompoundStmt(stmt_info, translated_stmt), vars_to_register@op@bv
  | _ -> (* When it is not the method we expect with only one parameter, we don't translate *)
    Printing.log_out "WARNING: Block Enumeration called at %s not translated." (Clang_ast_j.string_of_stmt_info stmt_info);
    CompoundStmt(stmt_info, stmt_list), []

(* We translate the logical negation of an integer with a conditional*)
(* !x <=> x?0:1 *)
let trans_negation_with_conditional stmt_info expr_info stmt_list =
  let stmt_list_cond = stmt_list @ [create_integer_literal stmt_info "0"] @ [create_integer_literal stmt_info "1"] in
  ConditionalOperator(stmt_info, stmt_list_cond, expr_info)

let create_call stmt_info decl_pointer function_name qt parameters =
  let expr_info_call = {
    Clang_ast_t.ei_qual_type = qt;
    Clang_ast_t.ei_value_kind = `XValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let expr_info_dre = make_expr_info_with_objc_kind qt `Ordinary in
  let decl_ref = make_decl_ref_qt `Function decl_pointer function_name false qt in
  let decl_ref_info = make_decl_ref_expr_info decl_ref in
  let decl_ref_exp = DeclRefExpr(stmt_info, [], expr_info_dre, decl_ref_info) in
  CallExpr(stmt_info, decl_ref_exp:: parameters, expr_info_call)

let create_assume_not_null_call decl_info var_name var_type =
  let stmt_info = stmt_info_with_fresh_pointer (make_stmt_info decl_info) in
  let boi = { Clang_ast_t.boi_kind = `NE } in
  let decl_ptr = Ast_utils.get_invalid_pointer () in
  let decl_ref = make_decl_ref_qt `Var decl_ptr var_name false var_type in
  let stmt_info_var = dummy_stmt_info () in
  let decl_ref_info = make_decl_ref_expr_info decl_ref in
  let var_decl_ref = DeclRefExpr(stmt_info_var, [], (make_expr_info var_type), decl_ref_info) in
  let var_decl_ptr = Ast_utils.get_invalid_pointer () in
  let expr_info = {
    Clang_ast_t.ei_qual_type = var_type;
    Clang_ast_t.ei_value_kind = `RValue;
    Clang_ast_t.ei_object_kind = `Ordinary
  } in
  let cast_info_call = { cei_cast_kind = `LValueToRValue; cei_base_path = [] } in
  let decl_ref_exp_cast = ImplicitCastExpr(stmt_info, [var_decl_ref], expr_info, cast_info_call) in
  let null_expr = create_integer_literal stmt_info "0" in
  let bin_op = make_binary_stmt decl_ref_exp_cast null_expr stmt_info (make_lvalue_obc_prop_expr_info var_type) boi in
  let parameters = [bin_op] in
  create_call stmt_info var_decl_ptr (Procname.to_string SymExec.ModelBuiltins.__infer_assume) (create_void_type ()) parameters
