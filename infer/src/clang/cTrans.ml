(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual

(** Translates instructions: (statements and expressions) from the ast into sil *)

open CTrans_utils
open CTrans_utils.Nodes
module L = Logging

module CTrans_funct (F : CModule_type.CFrontend) : CModule_type.CTranslation = struct
  (** Returns the procname and whether is instance, according to the selector information and
     according to the method signature with the following priority:
     1. method is a predefined model
     2. method is found by clang's resolution
     3. Method is found by our resolution *)
  let get_callee_objc_method context obj_c_message_expr_info act_params =
    let open CContext in
    let selector, method_pointer_opt, mc_type =
      CMethod_trans.get_objc_method_data obj_c_message_expr_info
    in
    let is_instance = mc_type <> CMethod_trans.MCStatic in
    let objc_method_kind = Typ.Procname.ObjC_Cpp.objc_method_kind_of_bool is_instance in
    let method_kind =
      if is_instance then ClangMethodKind.OBJC_INSTANCE else ClangMethodKind.OBJC_CLASS
    in
    let ms_opt =
      match method_pointer_opt with
      | Some pointer ->
          CMethod_trans.method_signature_of_pointer context.tenv pointer
      | None ->
          None
    in
    let proc_name =
      match CMethod_trans.get_method_name_from_clang context.tenv ms_opt with
      | Some name ->
          name
      | None ->
          (* fall back to our method resolution if clang's fails *)
          let class_name =
            CMethod_trans.get_class_name_method_call_from_receiver_kind context
              obj_c_message_expr_info act_params
          in
          CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind class_name selector
            objc_method_kind
    in
    let predefined_ms_opt =
      match proc_name with
      | Typ.Procname.ObjC_Cpp objc_cpp ->
          let class_name = Typ.Procname.ObjC_Cpp.get_class_type_name objc_cpp in
          CTrans_models.get_predefined_model_method_signature class_name selector
            CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind
      | _ ->
          None
    in
    match (predefined_ms_opt, ms_opt) with
    | Some ms, _ ->
        ignore
          (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
             context.tenv ms [] []) ;
        (ms.CMethodSignature.name, CMethod_trans.MCNoVirtual)
    | None, Some ms ->
        ignore
          (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
             context.tenv ms [] []) ;
        if CMethodSignature.is_getter ms || CMethodSignature.is_setter ms then
          (proc_name, CMethod_trans.MCNoVirtual)
        else (proc_name, mc_type)
    | _ ->
        CMethod_trans.create_external_procdesc context.translation_unit_context context.cfg
          proc_name method_kind None ;
        (proc_name, mc_type)


  let rec is_block_expr s =
    let open Clang_ast_t in
    match s with
    | BlockExpr _ ->
        true
    (* the block can be wrapped in ExprWithCleanups  or ImplicitCastExpr*)
    | ImplicitCastExpr (_, [s'], _, _) | ExprWithCleanups (_, [s'], _, _) ->
        is_block_expr s'
    | _ ->
        false


  let objc_exp_of_type_block fun_exp_stmt =
    match fun_exp_stmt with
    | Clang_ast_t.ImplicitCastExpr (_, _, ei, _)
      when CType.is_block_type ei.Clang_ast_t.ei_qual_type ->
        true
    | _ ->
        false


  let collect_returns trans_result_list = List.map ~f:(fun {return} -> return) trans_result_list

  (* If e is a block and the calling node has the priority then we need to release the priority to
     allow creation of nodes inside the block.  At the end of block translation, we need to get the
     proirity back.  the parameter f will be called with function instruction *)
  let exec_with_block_priority_exception f trans_state e stmt_info =
    if is_block_expr e && PriorityNode.own_priority_node trans_state.priority stmt_info then (
      L.(debug Capture Verbose) "Translating block expression by freeing the priority" ;
      f {trans_state with priority= Free} e )
    else f trans_state e


  let exec_with_node_creation ~f trans_state stmt =
    let res_trans = f trans_state stmt in
    if res_trans.control.instrs <> [] then
      let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
      let stmt_info' = {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let sil_loc =
        CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
          stmt_info'
      in
      PriorityNode.compute_result_to_parent trans_state_pri sil_loc ~node_name:FallbackNode
        stmt_info' res_trans
    else res_trans


  (* This is the standard way of dealing with self:Class or a call [a class]. We translate it as
     sizeof(<type pf a>) The only time when we want to translate those expressions differently is
     when they are the first argument of method calls. In that case they are not translated as
     expressions, but we take the type and create a static method call from it. This is done in
     objcMessageExpr_trans. *)
  let sizeof_expr_class class_name =
    let typ = Typ.mk (Tstruct class_name) in
    ( Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
    , Typ.mk (Tint IULong) )


  let add_reference_if_glvalue (typ : Typ.t) expr_info =
    (* glvalue definition per C++11:
       http://en.cppreference.com/w/cpp/language/value_category *)
    let is_glvalue =
      match expr_info.Clang_ast_t.ei_value_kind with `LValue | `XValue -> true | `RValue -> false
    in
    match (is_glvalue, typ.desc) with
    | true, Tptr (_, Pk_reference) ->
        (* reference of reference is not allowed in C++ - it's most likely frontend *)
        (* trying to add same reference to same type twice*)
        (* this is hacky and should be fixed (t9838691) *)
        typ
    | true, _ ->
        Typ.mk (Tptr (typ, Pk_reference))
    | _ ->
        typ


  (** Execute translation and then possibly adjust the type of the result of translation:
      In C++, when expression returns reference to type T, it will be lvalue to T, not T&, but
      infer needs it to be T& *)
  let exec_with_glvalue_as_reference f trans_state stmt =
    let expr_info =
      match Clang_ast_proj.get_expr_tuple stmt with
      | Some (_, _, ei) ->
          ei
      | None ->
          let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
          CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Clang_ast_proj.get_expr_tuple stmt returns None, stmt is %a"
            (Pp.to_string ~f:Clang_ast_j.string_of_stmt)
            stmt
    in
    let res_trans = f trans_state stmt in
    let return =
      let exp, typ = res_trans.return in
      (exp, add_reference_if_glvalue typ expr_info)
    in
    {res_trans with return}


  (** Execute translation of e forcing to release priority (if it's not free) and then setting it
     back. This is used in conditional operators where we need to force the priority to be free for
     the computation of the expressions*)
  let exec_with_priority_exception trans_state e f =
    if PriorityNode.is_priority_free trans_state then f trans_state e
    else f {trans_state with priority= Free} e


  let call_translation context decl =
    let open CContext in
    (* translation will reset Ident counter, save it's state and restore it afterwards *)
    let ident_state = Ident.NameGenerator.get_current () in
    F.translate_one_declaration context.translation_unit_context context.tenv context.cfg
      `Translation decl ;
    Ident.NameGenerator.set_current ident_state


  let mk_temp_sil_var procdesc var_name_suffix =
    let procname = Procdesc.get_proc_name procdesc in
    Pvar.mk_tmp var_name_suffix procname


  let mk_temp_sil_var_for_expr tenv procdesc var_name_prefix expr_info =
    let qual_type = expr_info.Clang_ast_t.ei_qual_type in
    let typ = CType_decl.qual_type_to_sil_type tenv qual_type in
    (mk_temp_sil_var procdesc var_name_prefix, typ)


  let create_var_exp_tmp_var trans_state expr_info var_name =
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let pvar, typ = mk_temp_sil_var_for_expr context.CContext.tenv procdesc var_name expr_info in
    let var_data =
      ProcAttributes.{name= Pvar.get_name pvar; typ; modify_in_block= false; is_constexpr= false}
    in
    Procdesc.append_locals procdesc [var_data] ;
    (Exp.Lvar pvar, typ)


  let get_forwarded_params trans_state sil_loc =
    (* forward all parameters of constructor except this *)
    let context = trans_state.context in
    let pdesc = context.procdesc in
    let attr = Procdesc.get_attributes pdesc in
    let procname = Procdesc.get_proc_name pdesc in
    attr.formals (* remove this, which should always be the first formal parameter *)
    |> List.tl_exn
    |> List.fold_left ~init:([], [])
         ~f:(fun (forwarded_params, forwarded_init_exps) (formal, typ) ->
           let pvar = Pvar.mk formal procname in
           let id = Ident.create_fresh Ident.knormal in
           ( (Exp.Var id, typ) :: forwarded_params
           , Sil.Load (id, Exp.Lvar pvar, typ, sil_loc) :: forwarded_init_exps ) )


  let create_call_instr trans_state (return_type : Typ.t) function_sil params_sil sil_loc
      call_flags ~is_objc_method ~is_inherited_ctor =
    let ret_id_typ = (Ident.create_fresh Ident.knormal, return_type) in
    let ret_id', params, initd_exps, ret_exps, call_flags =
      (* Assumption: should_add_return_param will return true only for struct types *)
      if CType_decl.should_add_return_param return_type ~is_objc_method then
        let param_type = Typ.mk (Typ.Tptr (return_type, Typ.Pk_pointer)) in
        let var_exp =
          match trans_state.var_exp_typ with
          | Some (exp, _) ->
              exp
          | _ ->
              let procdesc = trans_state.context.CContext.procdesc in
              let pvar = mk_temp_sil_var procdesc "__temp_return_" in
              let var_data : ProcAttributes.var_data =
                { name= Pvar.get_name pvar
                ; typ= return_type
                ; modify_in_block= false
                ; is_constexpr= false }
              in
              Procdesc.append_locals procdesc [var_data] ;
              Exp.Lvar pvar
        in
        (* It is very confusing - same expression has two different types in two contexts:*)
        (* 1. if passed as parameter it's RETURN_TYPE* since we are passing it as rvalue *)
        (* 2. for return expression it's RETURN_TYPE since backend allows to treat it as lvalue*)
        (*    of RETURN_TYPE *)
        (* Implications: *)
        (* Fields: field_deref_trans relies on it - if exp has RETURN_TYPE then *)
        (*         it means that it's not lvalue in clang's AST (it'd be reference otherwise) *)
        (* Methods: method_deref_trans actually wants a pointer to the object, which is*)
        (*          equivalent of value of ret_param. Since ret_exp has type RETURN_TYPE,*)
        (*          we optionally add pointer there to avoid backend confusion. *)
        (*          It works either way *)
        (* Passing by value: may cause problems - there needs to be extra Sil.Load, but*)
        (*                   doing so would create problems with methods. Passing structs by*)
        (*                   value doesn't work good anyway. This may need to be revisited later*)
        let ret_param = (var_exp, param_type) in
        let ret_exp = (var_exp, return_type) in
        ( mk_fresh_void_id_typ ()
        , params_sil @ [ret_param]
        , [var_exp]
        , ret_exp
        , {call_flags with CallFlags.cf_assign_last_arg= true} )
      else
        ( ret_id_typ
        , params_sil
        , []
        , (let i, t = ret_id_typ in
           (Exp.Var i, t))
        , call_flags )
    in
    let forwarded_params, forwarded_init_instrs =
      if is_inherited_ctor then get_forwarded_params trans_state sil_loc else ([], [])
    in
    let call_instr =
      Sil.Call (ret_id', function_sil, params @ forwarded_params, sil_loc, call_flags)
    in
    let call_instr' = Sil.add_with_block_parameters_flag call_instr in
    mk_trans_result ret_exps
      {empty_control with instrs= forwarded_init_instrs @ [call_instr']; initd_exps}


  (** Given a captured var, return the instruction to assign it to a temp *)
  let assign_captured_var loc (cvar, typ) =
    let id = Ident.create_fresh Ident.knormal in
    let instr = Sil.Load (id, Exp.Lvar cvar, typ, loc) in
    ((Exp.Var id, cvar, typ), instr)


  let closure_trans closure_pname captured_vars context stmt_info expr_info =
    let open CContext in
    let loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let qual_type = expr_info.Clang_ast_t.ei_qual_type in
    let typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let ids_instrs = List.map ~f:(assign_captured_var loc) captured_vars in
    let captured_vars, instrs = List.unzip ids_instrs in
    let closure = Exp.Closure {name= closure_pname; captured_vars} in
    mk_trans_result (closure, typ) {empty_control with instrs}


  let stringLiteral_trans trans_state expr_info str =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cstr str) in
    mk_trans_result (exp, typ) empty_control


  (** FROM CLANG DOCS: "Implements the GNU __null extension, which is a name for a null pointer
     constant that has integral type (e.g., int or long) and is the same size and alignment as a
     pointer. The __null extension is typically only used by system headers, which define NULL as
     __null in C++ rather than using 0 (which is an integer that may not match the size of a
     pointer)".  So we implement it as the constant zero *)
  let gNUNullExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cint IntLit.zero) in
    mk_trans_result (exp, typ) empty_control


  let nullPtrExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    mk_trans_result (Exp.null, typ) empty_control


  let objCSelectorExpr_trans trans_state expr_info selector =
    stringLiteral_trans trans_state expr_info selector


  let objCEncodeExpr_trans trans_state expr_info objc_encode_expr_info =
    let type_raw = objc_encode_expr_info.Clang_ast_t.oeei_raw in
    stringLiteral_trans trans_state expr_info type_raw


  let objCProtocolExpr_trans trans_state expr_info decl_ref =
    let name =
      match decl_ref.Clang_ast_t.dr_name with Some s -> s.Clang_ast_t.ni_name | _ -> ""
    in
    stringLiteral_trans trans_state expr_info name


  let characterLiteral_trans trans_state expr_info n =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cint (IntLit.of_int n)) in
    mk_trans_result (exp, typ) empty_control


  let booleanValue_trans trans_state expr_info b =
    characterLiteral_trans trans_state expr_info (if b then 1 else 0)


  let floatingLiteral_trans trans_state expr_info float_string =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cfloat (float_of_string float_string)) in
    mk_trans_result (exp, typ) empty_control


  (* Note currently we don't have support for different qual type like long, unsigned long, etc. *)
  and integerLiteral_trans trans_state expr_info integer_literal_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp =
      try
        let i = IntLit.of_string integer_literal_info.Clang_ast_t.ili_value in
        let exp = Exp.int i in
        exp
      with Failure _ ->
        (* Parse error: return a nondeterministic value *)
        let id = Ident.create_fresh Ident.knormal in
        Exp.Var id
    in
    mk_trans_result (exp, typ) empty_control


  let cxxScalarValueInitExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    (* constant will be different depending on type *)
    let zero = Exp.zero_of_type typ |> Option.value ~default:Exp.zero in
    mk_trans_result (zero, typ) empty_control


  (** Create instructions to initialize record with zeroes. It needs to traverse
      whole type structure, to assign 0 values to all transitive fields because of
      AST construction in C translation *)
  let implicitValueInitExpr_trans trans_state stmt_info =
    match trans_state.var_exp_typ with
    | Some var_exp_typ ->
        (* This node will always be child of InitListExpr, claiming priority will always fail *)
        let tenv = trans_state.context.CContext.tenv in
        let sil_loc =
          CLocation.location_of_stmt_info
            trans_state.context.CContext.translation_unit_context.source_file stmt_info
        in
        (* Traverse structure of a type and initialize int/float/ptr fields with zero *)
        let rec fill_typ_with_zero ((exp, typ) as exp_typ) =
          match typ.Typ.desc with
          | Tstruct tn ->
              let field_exp_typs =
                match Tenv.lookup tenv tn with
                | Some {fields} ->
                    List.map fields ~f:(fun (fieldname, fieldtype, _) ->
                        (Exp.Lfield (exp, fieldname, typ), fieldtype) )
                | None ->
                    assert false
              in
              List.map field_exp_typs ~f:(fun exp_typ -> (fill_typ_with_zero exp_typ).control)
              |> collect_controls trans_state.context.procdesc
              |> mk_trans_result exp_typ
          | Tarray {elt= field_typ; length= Some n} ->
              let size = IntLit.to_int_exn n in
              let indices = CGeneral_utils.list_range 0 (size - 1) in
              List.map indices ~f:(fun i ->
                  let idx_exp = Exp.Const (Const.Cint (IntLit.of_int i)) in
                  let field_exp = Exp.Lindex (exp, idx_exp) in
                  (fill_typ_with_zero (field_exp, field_typ)).control )
              |> collect_controls trans_state.context.procdesc
              |> mk_trans_result exp_typ
          | Tint _ | Tfloat _ | Tptr _ ->
              let zero_exp = Exp.zero_of_type_exn typ in
              let instrs = [Sil.Store (exp, typ, zero_exp, sil_loc)] in
              mk_trans_result (exp, typ) {empty_control with instrs}
          | Tfun _ | Tvoid | Tarray _ | TVar _ ->
              CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
                "fill_typ_with_zero on type %a" (Typ.pp Pp.text) typ
        in
        let res_trans = fill_typ_with_zero var_exp_typ in
        {res_trans with control= {res_trans.control with initd_exps= [fst var_exp_typ]}}
    | None ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "Retrieving var from non-InitListExpr parent"


  let no_op_trans succ_nodes =
    mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= succ_nodes}


  (* The stmt seems to be always empty *)
  let unaryExprOrTypeTraitExpr_trans trans_state unary_expr_or_type_trait_expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ =
      CType_decl.qual_type_to_sil_type tenv
        unary_expr_or_type_trait_expr_info.Clang_ast_t.uttei_qual_type
    in
    match unary_expr_or_type_trait_expr_info.Clang_ast_t.uttei_kind with
    | (`SizeOf | `SizeOfWithSize _) as size ->
        let nbytes = match size with `SizeOfWithSize nbytes -> Some nbytes | _ -> None in
        let sizeof_data = {Exp.typ; nbytes; dynamic_length= None; subtype= Subtype.exact} in
        mk_trans_result (Exp.Sizeof sizeof_data, typ) empty_control
    | `AlignOf | `OpenMPRequiredSimdAlign | `VecStep ->
        let nondet = (Exp.Var (Ident.create_fresh Ident.knormal), typ) in
        mk_trans_result nondet empty_control


  (* search the label into the hashtbl - create a fake node eventually *)
  (* connect that node with this stmt *)
  let gotoStmt_trans trans_state stmt_info label_name =
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes= [root_node']; leaf_nodes= trans_state.succ_nodes}


  let get_builtin_pname_opt trans_unit_ctx qual_name decl_opt =
    let get_annotate_attr_arg decl =
      let open Clang_ast_t in
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      let get_attr_opt = function AnnotateAttr a -> Some a | _ -> None in
      match List.find_map ~f:get_attr_opt decl_info.di_attributes with
      | Some attribute_info -> (
        match attribute_info.ai_parameters with
        | [_; arg; _] ->
            Some arg
        | _ ->
            (* it's not supposed to happen due to hardcoded exporting logic
                  coming from ASTExporter.h in facebook-clang-plugins *)
            assert false )
      | None ->
          None
    in
    let name = QualifiedCppName.to_qual_string qual_name in
    let function_attr_opt = Option.bind decl_opt ~f:get_annotate_attr_arg in
    match function_attr_opt with
    | Some attr when CTrans_models.is_modeled_attribute attr ->
        Some (Typ.Procname.from_string_c_fun attr)
    | _ when CTrans_models.is_modeled_builtin name ->
        Some (Typ.Procname.from_string_c_fun (CFrontend_config.infer ^ name))
    | _
      when String.equal name CFrontend_config.malloc
           && CGeneral_utils.is_objc_extension trans_unit_ctx ->
        Some BuiltinDecl.malloc_no_fail
    | _ ->
        None


  let function_deref_trans trans_state decl_ref =
    let open CContext in
    let context = trans_state.context in
    let name_info, decl_ptr, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let decl_opt = CAst_utils.get_function_decl_with_body decl_ptr in
    Option.iter ~f:(call_translation context) decl_opt ;
    let qual_name = CAst_utils.get_qualified_name name_info in
    let typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let pname =
      match get_builtin_pname_opt context.translation_unit_context qual_name decl_opt with
      | Some builtin_pname ->
          builtin_pname
      | None ->
          let name = QualifiedCppName.to_qual_string qual_name in
          CMethod_trans.create_procdesc_with_pointer context decl_ptr None name
    in
    mk_trans_result (Exp.Const (Const.Cfun pname), typ) empty_control


  let field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init =
    let open CContext in
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let name_info, decl_ptr, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let field_string = name_info.Clang_ast_t.ni_name in
    L.(debug Capture Verbose) "Translating field '%s'@\n" field_string ;
    let field_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let obj_sil, class_typ = pre_trans_result.return in
    let is_pointer_typ = Typ.is_pointer class_typ in
    let class_typ = match class_typ.Typ.desc with Typ.Tptr (t, _) -> t | _ -> class_typ in
    L.(debug Capture Verbose) "Type is  '%s' @." (Typ.to_string class_typ) ;
    let class_tname =
      match CAst_utils.get_decl decl_ptr with
      | Some (FieldDecl ({di_parent_pointer}, _, _, _))
      | Some (ObjCIvarDecl ({di_parent_pointer}, _, _, _, _)) -> (
        match CAst_utils.get_decl_opt di_parent_pointer with
        | Some decl ->
            CType_decl.get_record_typename ~tenv:context.tenv decl
        | _ ->
            assert false )
      | _ as decl ->
          (* FIXME(t21762295): we do not expect this to happen but it does *)
          CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "di_parent_pointer should be always set for fields/ivars, but got %a"
            (Pp.option (Pp.to_string ~f:Clang_ast_j.string_of_decl))
            decl
    in
    let field_name = CGeneral_utils.mk_class_field_name class_tname field_string in
    let field_exp = Exp.Lfield (obj_sil, field_name, class_typ) in
    (* In certain cases, there is be no LValueToRValue cast, but backend needs dereference*)
    (* there either way:*)
    (* 1. Class is not a pointer type - it means that it's rvalue struct most likely coming from*)
    (*    create_call_instr - more info there*)
    (* 2. Field has reference type - we need to add extra dereference in same fashion*)
    (*    it's done in var_deref_trans. The only exception is during field initialization in*)
    (*    constructor's initializer list (when reference itself is initialized) *)
    let should_add_deref =
      (not is_pointer_typ) || ((not is_constructor_init) && CType.is_reference_type qual_type)
    in
    let exp, deref_instrs =
      if should_add_deref then
        let id = Ident.create_fresh Ident.knormal in
        let deref_instr = Sil.Load (id, field_exp, field_typ, sil_loc) in
        (Exp.Var id, [deref_instr])
      else (field_exp, [])
    in
    let instrs = pre_trans_result.control.instrs @ deref_instrs in
    { pre_trans_result with
      control= {pre_trans_result.control with instrs}; return= (exp, field_typ) }


  type decl_ref_context = MemberOrIvar of trans_result | DeclRefExpr

  let method_deref_trans ?(is_inner_destructor = false) trans_state ~context:decl_ref_context
      decl_ref stmt_info decl_kind =
    let open CContext in
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let name_info, decl_ptr, _ = CAst_utils.get_info_from_decl_ref decl_ref in
    let decl_opt = CAst_utils.get_function_decl_with_body decl_ptr in
    Option.iter ~f:(call_translation context) decl_opt ;
    let method_name = CAst_utils.get_unqualified_name name_info in
    L.(debug Capture Verbose) "!!!!! Dealing with method '%s' @." method_name ;
    let ms_opt = CMethod_trans.method_signature_of_pointer context.tenv decl_ptr in
    let is_instance_method =
      match ms_opt with
      | Some {CMethodSignature.method_kind= CPP_INSTANCE | OBJC_INSTANCE} ->
          true
      | Some {CMethodSignature.method_kind= CPP_CLASS | OBJC_CLASS | BLOCK | C_FUNCTION} ->
          false
      | None ->
          (* might happen for methods that are not exported yet (some templates) *)
          true
    in
    let this_exp_typ, this_instrs =
      if is_instance_method then
        match
          decl_ref_context
          (* the result exps of [context] may contain expr for 'this' parameter: if it comes from
             CXXMemberCallExpr it will be there if it comes from CXXOperatorCallExpr it won't be
             there and will be added later In case of CXXMemberCallExpr it's possible that type of
             'this' parameter won't have a pointer - if that happens add a pointer to type of the
             object *)
        with
        | MemberOrIvar {return= (exp, {Typ.desc= Tptr (typ, _)}) as return}
        (* We need to add a dereference before a method call to find null dereferences when
             calling a method with null *)
          when decl_kind <> `CXXConstructor ->
            let no_id = Ident.create_none () in
            let extra_instrs = [Sil.Load (no_id, exp, typ, sil_loc)] in
            (return, extra_instrs)
        | MemberOrIvar {return= (_, {Typ.desc= Tptr _}) as return} ->
            (return, [])
        | MemberOrIvar {return= exp, typ} ->
            ((exp, Typ.mk (Tptr (typ, Typ.Pk_reference))), [])
        | DeclRefExpr ->
            (mk_fresh_void_exp_typ (), [])
      else (* don't add 'this' expression for static methods. *)
        (mk_fresh_void_exp_typ (), [])
    in
    (* unlike field access, for method calls there is no need to expand class type use qualified
       method name for builtin matching, but use unqualified name elsewhere *)
    let qual_method_name = CAst_utils.get_qualified_name name_info in
    let pname =
      match get_builtin_pname_opt context.translation_unit_context qual_method_name decl_opt with
      | Some builtin_pname ->
          builtin_pname
      | None ->
          let class_typename =
            Typ.Name.Cpp.from_qual_name Typ.NoTemplate
              (CAst_utils.get_class_name_from_member name_info)
          in
          if is_inner_destructor then
            match ms_opt with
            | Some ms ->
                let procname = ms.CMethodSignature.name in
                let new_method_name =
                  Config.clang_inner_destructor_prefix ^ Typ.Procname.get_method procname
                in
                let ms' =
                  {ms with name= Typ.Procname.objc_cpp_replace_method_name procname new_method_name}
                in
                ignore
                  (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
                     context.tenv ms' [] []) ;
                ms'.CMethodSignature.name
            | None ->
                CMethod_trans.create_procdesc_with_pointer context decl_ptr (Some class_typename)
                  method_name
          else
            CMethod_trans.create_procdesc_with_pointer context decl_ptr (Some class_typename)
              method_name
    in
    let is_cpp_call_virtual =
      match ms_opt with Some {CMethodSignature.is_cpp_virtual} -> is_cpp_virtual | None -> false
    in
    let context_control_with_this =
      match decl_ref_context with
      | MemberOrIvar pre_trans_result ->
          {pre_trans_result.control with instrs= pre_trans_result.control.instrs @ this_instrs}
      | DeclRefExpr ->
          {empty_control with instrs= this_instrs}
    in
    mk_trans_result ~is_cpp_call_virtual ~method_name:pname this_exp_typ context_control_with_this


  let destructor_deref_trans trans_state pvar_trans_result class_type_ptr si ~is_inner_destructor =
    let open Clang_ast_t in
    let destruct_decl_ref_opt =
      match CAst_utils.get_decl_from_typ_ptr class_type_ptr with
      | Some (CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info))
      | Some (ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _)) ->
          cxx_record_info.xrdi_destructor
      | _ ->
          None
    in
    match destruct_decl_ref_opt with
    | Some decl_ref -> (
      match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
      | Some (CXXDestructorDecl _) ->
          Some
            (method_deref_trans ~is_inner_destructor trans_state
               ~context:(MemberOrIvar pvar_trans_result) decl_ref si `CXXDestructor)
      | _ ->
          None )
    | None ->
        None


  let get_this_pvar_typ stmt_info ?class_qual_type ({CContext.tenv; procdesc} as context) =
    let class_qual_type =
      match class_qual_type with
      | Some class_qual_type ->
          class_qual_type
      | None ->
          let class_ptr =
            CContext.get_curr_class_decl_ptr stmt_info (CContext.get_curr_class context)
          in
          Ast_expressions.create_pointer_qual_type (CAst_utils.qual_type_of_decl_ptr class_ptr)
    in
    let procname = Procdesc.get_proc_name procdesc in
    let name = CFrontend_config.this in
    let pvar = Pvar.mk (Mangled.from_string name) procname in
    (pvar, CType_decl.qual_type_to_sil_type tenv class_qual_type)


  let this_expr_trans stmt_info ?class_qual_type trans_state sil_loc =
    let this_pvar, this_typ = get_this_pvar_typ stmt_info ?class_qual_type trans_state.context in
    let return = (Exp.Lvar this_pvar, this_typ) in
    (* there is no cast operation in AST, but backend needs it *)
    dereference_value_from_result stmt_info.Clang_ast_t.si_source_range sil_loc
      (mk_trans_result return empty_control)


  (** get the [this] of the current procedure *)
  let compute_this_expr trans_state stmt_info =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let this_res_trans = this_expr_trans stmt_info trans_state sil_loc in
    let obj_sil, class_typ = this_res_trans.return in
    let this_qual_type = match class_typ.desc with Typ.Tptr (t, _) -> t | _ -> class_typ in
    (obj_sil, this_qual_type, this_res_trans)


  let cxxThisExpr_trans trans_state stmt_info expr_info =
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    this_expr_trans stmt_info trans_state sil_loc
      ~class_qual_type:expr_info.Clang_ast_t.ei_qual_type


  let rec labelStmt_trans trans_state stmt_info stmt_list label_name =
    let context = trans_state.context in
    let[@warning "-8"] [stmt] = stmt_list in
    let res_trans = instruction trans_state stmt in
    (* create the label root node into the hashtbl *)
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    Procdesc.node_set_succs_exn context.procdesc root_node' res_trans.control.root_nodes [] ;
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes= [root_node']; leaf_nodes= trans_state.succ_nodes}


  and var_deref_trans trans_state stmt_info (decl_ref : Clang_ast_t.decl_ref) =
    let context = trans_state.context in
    let _, _, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let ast_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let typ =
      match ast_typ.Typ.desc with
      | Tstruct _ when decl_ref.dr_kind = `ParmVar ->
          if CGeneral_utils.is_cpp_translation context.translation_unit_context then
            Typ.mk (Tptr (ast_typ, Pk_reference))
          else ast_typ
      | _ ->
          ast_typ
    in
    let procname = Procdesc.get_proc_name context.procdesc in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let pvar =
      CVar_decl.sil_var_of_decl_ref context stmt_info.Clang_ast_t.si_source_range decl_ref procname
    in
    CContext.add_block_static_var context procname (pvar, typ) ;
    let var_exp = Exp.Lvar pvar in
    let return =
      if Self.is_var_self pvar (CContext.is_objc_method context) && CType.is_class typ then
        let class_name = CContext.get_curr_class_typename stmt_info context in
        if trans_state.is_fst_arg_objc_instance_method_call then
          raise
            (Self.SelfClassException
               {class_name; position= __POS__; source_range= stmt_info.Clang_ast_t.si_source_range})
        else
          let exp_typ = sizeof_expr_class class_name in
          exp_typ
      else (var_exp, typ)
    in
    L.(debug Capture Verbose) "@\n@\n PVAR ='%s'@\n@\n" (Pvar.to_string pvar) ;
    let res_trans = mk_trans_result return empty_control in
    match typ.desc with
    | Tptr (_, Pk_reference) ->
        (* dereference pvar due to the behavior of reference types in clang's AST *)
        dereference_value_from_result stmt_info.Clang_ast_t.si_source_range sil_loc res_trans
    | _ ->
        res_trans


  and decl_ref_trans ?(is_constructor_init = false) ~context trans_state stmt_info decl_ref =
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    let decl_kind = decl_ref.Clang_ast_t.dr_kind in
    match (decl_kind, context) with
    | `EnumConstant, _ ->
        enum_constant_trans trans_state decl_ref
    | `Function, _ ->
        function_deref_trans trans_state decl_ref
    | (`Var | `ImplicitParam | `ParmVar), _ ->
        var_deref_trans trans_state stmt_info decl_ref
    | (`Field | `ObjCIvar), MemberOrIvar pre_trans_result ->
        (* a field outside of constructor initialization is probably a pointer to member, which we
           do not support *)
        field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init
    | (`CXXMethod | `CXXConversion | `CXXConstructor | `CXXDestructor), _ ->
        method_deref_trans trans_state ~context decl_ref stmt_info decl_kind
    | _ ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "Decl ref expression %a with pointer %d still needs to be translated"
          (Pp.to_string ~f:Clang_ast_j.string_of_decl_kind)
          decl_kind decl_ref.Clang_ast_t.dr_decl_pointer


  and declRefExpr_trans trans_state stmt_info decl_ref_expr_info =
    L.(debug Capture Verbose)
      "  priority node free = '%b'@\n@."
      (PriorityNode.is_priority_free trans_state) ;
    let decl_ref = Option.value_exn decl_ref_expr_info.Clang_ast_t.drti_decl_ref in
    decl_ref_trans ~context:DeclRefExpr trans_state stmt_info decl_ref


  (** evaluates an enum constant *)
  and enum_const_eval context enum_constant_pointer prev_enum_constant_opt zero =
    match CAst_utils.get_decl enum_constant_pointer with
    | Some (Clang_ast_t.EnumConstantDecl (_, _, _, enum_constant_decl_info)) -> (
      match enum_constant_decl_info.Clang_ast_t.ecdi_init_expr with
      | Some stmt ->
          expression_trans context stmt
      | None -> (
        match prev_enum_constant_opt with
        | Some prev_constant_pointer ->
            let previous_exp = get_enum_constant_expr context prev_constant_pointer in
            CArithmetic_trans.sil_const_plus_one previous_exp
        | None ->
            zero ) )
    | _ ->
        zero


  (** get the sil value of the enum constant from the map or by evaluating it *)
  and get_enum_constant_expr context enum_constant_pointer =
    let zero = Exp.Const (Const.Cint IntLit.zero) in
    try
      let prev_enum_constant_opt, sil_exp_opt =
        CAst_utils.get_enum_constant_exp enum_constant_pointer
      in
      match sil_exp_opt with
      | Some exp ->
          exp
      | None ->
          let exp = enum_const_eval context enum_constant_pointer prev_enum_constant_opt zero in
          CAst_utils.update_enum_map enum_constant_pointer exp ;
          exp
    with Caml.Not_found -> zero


  and enum_constant_trans trans_state decl_ref =
    let context = trans_state.context in
    let _, _, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
    let const_exp = get_enum_constant_expr context decl_ref.Clang_ast_t.dr_decl_pointer in
    mk_trans_result (const_exp, typ) empty_control


  and arraySubscriptExpr_trans trans_state expr_info stmt_list =
    let context = trans_state.context in
    let typ = CType_decl.get_type_from_expr_info expr_info context.tenv in
    let array_stmt, idx_stmt =
      match stmt_list with
      | [a; i] ->
          (* Assumption: the statement list contains 2 elements, the first is the array expr and the
             second the index *)
          (a, i)
      | _ ->
          assert false
    in
    let res_trans_a = instruction trans_state array_stmt in
    let res_trans_idx = instruction trans_state idx_stmt in
    let a_exp, _ = res_trans_a.return in
    let i_exp, _ = res_trans_idx.return in
    let array_exp = Exp.Lindex (a_exp, i_exp) in
    let root_nodes =
      if res_trans_a.control.root_nodes <> [] then res_trans_a.control.root_nodes
      else res_trans_idx.control.root_nodes
    in
    let leaf_nodes =
      if res_trans_idx.control.leaf_nodes <> [] then res_trans_idx.control.leaf_nodes
      else res_trans_a.control.leaf_nodes
    in
    if res_trans_idx.control.root_nodes <> [] then
      List.iter
        ~f:(fun n ->
          Procdesc.node_set_succs_exn context.procdesc n res_trans_idx.control.root_nodes [] )
        res_trans_a.control.leaf_nodes ;
    (* Note the order of res_trans_idx.ids @ res_trans_a.ids is important. *)
    (* We expect to use only res_trans_idx.ids in construction of other operation. *)
    (* res_trans_a.ids is passed to be Removed.*)
    let control =
      { root_nodes
      ; leaf_nodes
      ; instrs= res_trans_a.control.instrs @ res_trans_idx.control.instrs
      ; initd_exps= res_trans_idx.control.initd_exps @ res_trans_a.control.initd_exps }
    in
    mk_trans_result (array_exp, typ) control


  and binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list =
    L.(debug Capture Verbose)
      "  BinaryOperator '%a' "
      (Pp.to_string ~f:Clang_ast_j.string_of_binary_operator_kind)
      binary_operator_info.Clang_ast_t.boi_kind ;
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let node_name =
      Procdesc.Node.BinaryOperatorStmt (CArithmetic_trans.bin_op_to_string binary_operator_info)
    in
    let trans_state' = {trans_state_pri with succ_nodes= []} in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let res_typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    match stmt_list with
    | [s1; s2] ->
        (* Assumption: We expect precisely 2 stmt corresponding to the 2 operands*)
        (* NOTE: we create a node only if required. In that case this node *)
        (* becomes the successor of the nodes that may be created when     *)
        (* translating the operands.                                       *)
        let res_trans_e1 = instruction trans_state' s1 in
        let ((exp1, typ1) as exp_typ1) = res_trans_e1.return in
        let var_exp_typ =
          match binary_operator_info.Clang_ast_t.boi_kind with
          | `Assign ->
              Some exp_typ1
          | _ ->
              None
        in
        let trans_state'' = {trans_state' with var_exp_typ} in
        let res_trans_e2 =
          (* translation of s2 is done taking care of block special case *)
          exec_with_block_priority_exception instruction trans_state'' s2 stmt_info
        in
        let exp_typ2 = res_trans_e2.return in
        let binop_control, return =
          if List.exists ~f:(Exp.equal exp1) res_trans_e2.control.initd_exps then ([], exp_typ1)
          else
            let exp_op, instr_bin =
              CArithmetic_trans.binary_operation_instruction stmt_info.Clang_ast_t.si_source_range
                binary_operator_info exp_typ1 res_typ exp_typ2 sil_loc
            in
            (* Create a node if the priority if free and there are instructions *)
            let creating_node =
              PriorityNode.own_priority_node trans_state_pri.priority stmt_info
              && List.length instr_bin > 0
            in
            let extra_instrs, exp_to_parent =
              if
                is_binary_assign_op binary_operator_info
                (* assignment operator result is lvalue in CPP, rvalue in C, *)
                (* hence the difference *)
                && (not (CGeneral_utils.is_cpp_translation context.translation_unit_context))
                && ((not creating_node) || is_return_temp trans_state.continuation)
              then
                (* We are in this case when an assignment is inside        *)
                (* another operator that creates a node. Eg. another       *)
                (* assignment.  *)
                (* As no node is created here ids are passed to the parent *)
                let id = Ident.create_fresh Ident.knormal in
                let res_instr = Sil.Load (id, exp1, typ1, sil_loc) in
                ([res_instr], Exp.Var id)
              else ([], exp_op)
            in
            let return = (exp_to_parent, typ1) in
            let binop_control = {empty_control with instrs= instr_bin @ extra_instrs} in
            ([binop_control], return)
        in
        let all_res_trans = [res_trans_e1.control; res_trans_e2.control] @ binop_control in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name stmt_info
          all_res_trans
        |> mk_trans_result return
    | _ ->
        (* Binary operator should have two operands *)
        assert false


  and callExpr_trans trans_state si stmt_list expr_info =
    let context = trans_state.context in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file si
    in
    (* First stmt is the function expr and the rest are params *)
    let fun_exp_stmt, params_stmt =
      match stmt_list with fe :: params -> (fe, params) | _ -> assert false
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    (* claim priority if no ancestors has claimed priority before *)
    let trans_state_callee = {trans_state_pri with succ_nodes= []} in
    let res_trans_callee = instruction trans_state_callee fun_exp_stmt in
    let sil_fe =
      match res_trans_callee.method_name with
      | Some pname ->
          (* back from a CXX method call, the [method_name] field is set but the return expression
             is the object instance's [this] *)
          Exp.Const (Const.Cfun pname)
      | None ->
          (* more boring function call, the function name is the return expression *)
          fst res_trans_callee.return
    in
    let callee_pname_opt =
      match sil_fe with Exp.Const (Const.Cfun pn) -> Some pn | _ -> (* function pointer *) None
    in
    (* we cannot translate the arguments of __builtin_object_size because preprocessing copies
       them verbatim from a call to a different function, and they might be side-effecting *)
    let should_translate_args =
      not
        (Option.value_map ~f:CTrans_models.is_builtin_object_size ~default:false callee_pname_opt)
    in
    let params_stmt = if should_translate_args then params_stmt else [] in
    (* As we may have nodes coming from different parameters we need to  *)
    (* call instruction for each parameter and collect the results       *)
    (* afterwards. The 'instructions' function does not do that          *)
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let result_trans_params =
      let instruction' = exec_with_glvalue_as_reference instruction in
      List.map ~f:(instruction' trans_state_param) params_stmt
    in
    match
      Option.bind callee_pname_opt
        ~f:
          (CTrans_utils.builtin_trans trans_state_pri si.Clang_ast_t.si_source_range sil_loc
             (res_trans_callee :: result_trans_params))
    with
    | Some builtin ->
        builtin
    | None ->
        let act_params = collect_returns result_trans_params in
        let res_trans_call =
          let is_call_to_block = objc_exp_of_type_block fun_exp_stmt in
          let call_flags = {CallFlags.default with CallFlags.cf_is_objc_block= is_call_to_block} in
          create_call_instr trans_state function_type sil_fe act_params sil_loc call_flags
            ~is_objc_method:false ~is_inherited_ctor:false
        in
        let node_name = Procdesc.Node.Call (Exp.to_string sil_fe) in
        let all_res_trans = res_trans_callee :: (result_trans_params @ [res_trans_call]) in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name si
          ~return:res_trans_call.return all_res_trans


  and cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt si
      function_type is_cpp_call_virtual extra_res_trans ~is_inherited_ctor =
    let context = trans_state_pri.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file si
    in
    let callee_pname = Option.value_exn result_trans_callee.method_name in
    (* As we may have nodes coming from different parameters we need to call instruction for each
       parameter and collect the results afterwards. The 'instructions' function does not do that *)
    let result_trans_params =
      let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
      let instruction' = exec_with_glvalue_as_reference instruction in
      let res_trans_p = List.map ~f:(instruction' trans_state_param) params_stmt in
      result_trans_callee :: res_trans_p
    in
    (* params including 'this' parameter *)
    let actual_params = collect_returns result_trans_params in
    match
      cxx_method_builtin_trans trans_state_pri si.Clang_ast_t.si_source_range sil_loc
        result_trans_params callee_pname
    with
    | Some builtin ->
        builtin
    | None ->
        let sil_method = Exp.Const (Const.Cfun callee_pname) in
        let call_flags = {CallFlags.default with CallFlags.cf_virtual= is_cpp_call_virtual} in
        let res_trans_call =
          create_call_instr trans_state_pri function_type sil_method actual_params sil_loc
            call_flags ~is_objc_method:false ~is_inherited_ctor
        in
        let node_name = Procdesc.Node.Call (Exp.to_string sil_method) in
        let all_res_trans =
          result_trans_params @ (res_trans_call :: Option.to_list extra_res_trans)
        in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name si
          ~return:res_trans_call.return all_res_trans


  and cxxMemberCallExpr_trans trans_state si stmt_list expr_info =
    let context = trans_state.context in
    (* Structure is the following: *)
    (* CXXMemberCallExpr: first stmt is method+this expr and the rest are normal params *)
    (* CXXOperatorCallExpr: First stmt is method/function deref without this expr and the *)
    (*                      rest are params, possibly including 'this' *)
    let fun_exp_stmt, params_stmt =
      match stmt_list with fe :: params -> (fe, params) | _ -> assert false
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    (* claim priority if no ancestors has claimed priority before *)
    let trans_state_callee = {trans_state_pri with succ_nodes= []} in
    let result_trans_callee = instruction trans_state_callee fun_exp_stmt in
    let is_cpp_call_virtual = result_trans_callee.is_cpp_call_virtual in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt si
      function_type is_cpp_call_virtual None ~is_inherited_ctor:false


  and cxxConstructExpr_trans trans_state si params_stmt ei cxx_constr_info ~is_inherited_ctor =
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file si
    in
    let decl_ref = cxx_constr_info.Clang_ast_t.xcei_decl_ref in
    let var_exp, class_type =
      match trans_state.var_exp_typ with
      | Some exp_typ ->
          exp_typ
      | None ->
          let procdesc = trans_state.context.CContext.procdesc in
          let pvar = Pvar.mk_tmp "__temp_construct_" (Procdesc.get_proc_name procdesc) in
          let class_type = CType_decl.get_type_from_expr_info ei context.CContext.tenv in
          let var_data : ProcAttributes.var_data =
            {name= Pvar.get_name pvar; typ= class_type; modify_in_block= false; is_constexpr= false}
          in
          Procdesc.append_locals procdesc [var_data] ;
          (Exp.Lvar pvar, class_type)
    in
    let this_type = CType.add_pointer_to_typ class_type in
    let this_res_trans =
      mk_trans_result (var_exp, this_type) {empty_control with initd_exps= [var_exp]}
    in
    let tmp_res_trans = mk_trans_result (var_exp, class_type) empty_control in
    (* When class type is translated as pointer (std::shared_ptr for example), there needs
       to be extra Load instruction before returning the trans_result of constructorExpr.
       There is no LValueToRvalue cast in the AST afterwards since clang doesn't know
       that class type is translated as pointer type. It gets added here instead. *)
    let extra_res_trans =
      let do_extra_deref =
        match class_type.desc with
        | Typ.Tptr _ ->
            (* do not inject the extra dereference for procedures generated to record the
                      initialization code of globals *)
            Procdesc.get_proc_name trans_state.context.procdesc
            |> Typ.Procname.get_global_name_of_initializer |> Option.is_none
        | _ ->
            false
      in
      if do_extra_deref then
        dereference_value_from_result si.Clang_ast_t.si_source_range sil_loc tmp_res_trans
      else tmp_res_trans
    in
    let res_trans_callee =
      decl_ref_trans ~context:(MemberOrIvar this_res_trans) trans_state si decl_ref
    in
    let res_trans =
      cxx_method_construct_call_trans trans_state_pri res_trans_callee params_stmt si
        (Typ.mk Tvoid) false (Some extra_res_trans) ~is_inherited_ctor
    in
    {res_trans with return= extra_res_trans.return}


  and cxx_destructor_call_trans trans_state si this_res_trans class_type_ptr ~is_inner_destructor =
    (* cxx_method_construct_call_trans claims a priority with the same `si`. A new pointer is
       generated to avoid premature node creation *)
    let si' = {si with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si' in
    let this_exp, this_typ = this_res_trans.return in
    let this_res_trans' =
      {this_res_trans with return= (this_exp, CType.add_pointer_to_typ this_typ)}
    in
    match
      destructor_deref_trans trans_state this_res_trans' class_type_ptr si' ~is_inner_destructor
    with
    | Some res_trans_callee when Option.is_some res_trans_callee.method_name ->
        let is_cpp_call_virtual = res_trans_callee.is_cpp_call_virtual in
        Some
          (cxx_method_construct_call_trans trans_state_pri res_trans_callee [] si' (Typ.mk Tvoid)
             is_cpp_call_virtual None ~is_inherited_ctor:false)
    | _ ->
        None


  and is_receiver_instance = function `Instance | `SuperInstance -> true | _ -> false

  and objCMessageExpr_trans_special_cases trans_state si obj_c_message_expr_info method_type
      trans_state_pri sil_loc act_params =
    let context = trans_state.context in
    let receiver_kind = obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind in
    let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
    (* class method *)
    if String.equal selector CFrontend_config.class_method && CType.is_class method_type then
      let class_name =
        CMethod_trans.get_class_name_method_call_from_receiver_kind context obj_c_message_expr_info
          act_params
      in
      if trans_state.is_fst_arg_objc_instance_method_call && is_receiver_instance receiver_kind
      then
        raise
          (Self.SelfClassException
             {class_name; position= __POS__; source_range= si.Clang_ast_t.si_source_range})
      else
        let exp, typ = sizeof_expr_class class_name in
        Some (mk_trans_result (exp, typ) empty_control)
    else if
      (* alloc or new *)
      String.equal selector CFrontend_config.alloc
      || String.equal selector CFrontend_config.new_str
    then
      match receiver_kind with
      | `Class qual_type ->
          let class_opt =
            CMethod_trans.get_class_name_method_call_from_clang context.CContext.tenv
              obj_c_message_expr_info
          in
          Some (new_or_alloc_trans trans_state_pri sil_loc si qual_type class_opt selector)
      | _ ->
          None
    else None


  (** If the first argument of the call is self in a static context, remove it as an argument and
     change the call from instance to static *)
  and objCMessageExpr_deal_with_static_self trans_state_param stmt_list obj_c_message_expr_info =
    match stmt_list with
    | stmt :: rest -> (
        let param_trans_results =
          List.map ~f:(exec_with_glvalue_as_reference instruction trans_state_param) rest
        in
        try
          let trans_state_param' =
            if is_receiver_instance obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind then
              {trans_state_param with is_fst_arg_objc_instance_method_call= true}
            else {trans_state_param with is_fst_arg_objc_instance_method_call= false}
          in
          let fst_res_trans = instruction trans_state_param' stmt in
          (obj_c_message_expr_info, fst_res_trans :: param_trans_results)
        with Self.SelfClassException e ->
          let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
          let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
          let obj_c_message_expr_info =
            Ast_expressions.make_obj_c_message_expr_info_class selector e.class_name pointer
          in
          (obj_c_message_expr_info, param_trans_results) )
    | [] ->
        (obj_c_message_expr_info, [])


  and objCMessageExpr_trans trans_state si obj_c_message_expr_info stmt_list expr_info =
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file si
    in
    let method_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let method_type = add_reference_if_glvalue method_type_no_ref expr_info in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let obj_c_message_expr_info, res_trans_subexpr_list =
      objCMessageExpr_deal_with_static_self trans_state_param stmt_list obj_c_message_expr_info
    in
    let subexpr_exprs = collect_returns res_trans_subexpr_list in
    match
      objCMessageExpr_trans_special_cases trans_state si obj_c_message_expr_info method_type
        trans_state_pri sil_loc subexpr_exprs
    with
    | Some res ->
        res
    | None ->
        let procname = Procdesc.get_proc_name context.CContext.procdesc in
        let callee_name, method_call_type =
          get_callee_objc_method context obj_c_message_expr_info subexpr_exprs
        in
        let res_trans_add_self =
          Self.add_self_parameter_for_super_instance si context procname sil_loc
            obj_c_message_expr_info
        in
        let res_trans_subexpr_list = Option.to_list res_trans_add_self @ res_trans_subexpr_list in
        let subexpr_exprs = collect_returns res_trans_subexpr_list in
        let is_virtual =
          CMethod_trans.equal_method_call_type method_call_type CMethod_trans.MCVirtual
        in
        let call_flags = {CallFlags.default with CallFlags.cf_virtual= is_virtual} in
        let method_sil = Exp.Const (Const.Cfun callee_name) in
        let res_trans_call =
          create_call_instr trans_state method_type method_sil subexpr_exprs sil_loc call_flags
            ~is_objc_method:true ~is_inherited_ctor:false
        in
        let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
        let node_name = Procdesc.Node.MessageCall selector in
        let assertion_trans_opt =
          if CTrans_models.is_handleFailureInMethod selector then
            Some (CTrans_utils.trans_assertion trans_state sil_loc)
          else None
        in
        let all_res_trans =
          res_trans_subexpr_list @ (res_trans_call :: Option.to_list assertion_trans_opt)
        in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name si
          ~return:res_trans_call.return all_res_trans


  and inject_base_class_destructor_calls trans_state stmt_info bases obj_sil this_qual_type =
    List.rev_filter_map bases ~f:(fun base ->
        let this_res_trans_destruct = mk_trans_result (obj_sil, this_qual_type) empty_control in
        cxx_destructor_call_trans trans_state stmt_info this_res_trans_destruct base
          ~is_inner_destructor:true )


  and add_this_instrs_if_result_non_empty res_trans this_res_trans =
    if res_trans <> [] then
      mk_trans_result this_res_trans.return
        {empty_control with instrs= this_res_trans.control.instrs}
      :: res_trans
    else res_trans


  and cxx_inject_virtual_base_class_destructors trans_state stmt_info =
    let context = trans_state.context in
    if not (CGeneral_utils.is_cpp_translation context.translation_unit_context) then None
    else
      (* get virtual base classes of the current class *)
      let class_ptr =
        CContext.get_curr_class_decl_ptr stmt_info (CContext.get_curr_class context)
      in
      let decl = Option.value_exn (CAst_utils.get_decl class_ptr) in
      let typ_pointer_opt = CAst_utils.type_of_decl decl in
      let bases = CAst_utils.get_cxx_virtual_base_classes decl in
      let bases = match typ_pointer_opt with Some p -> bases @ [p] | None -> bases in
      let _, sloc2 = stmt_info.Clang_ast_t.si_source_range in
      let stmt_info_loc = {stmt_info with Clang_ast_t.si_source_range= (sloc2, sloc2)} in
      (* compute `this` once that is used for all destructor calls of virtual base class *)
      let obj_sil, this_qual_type, this_res_trans = compute_this_expr trans_state stmt_info_loc in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info_loc in
      let bases_res_trans =
        inject_base_class_destructor_calls trans_state_pri stmt_info_loc bases obj_sil
          this_qual_type
      in
      let all_res_trans = add_this_instrs_if_result_non_empty bases_res_trans this_res_trans in
      let sil_loc =
        CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info_loc
      in
      Some
        (PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:Destruction
           stmt_info_loc ~return:(mk_fresh_void_exp_typ ()) all_res_trans)


  and cxx_inject_field_destructors_in_destructor_body trans_state stmt_info =
    let context = trans_state.context in
    if not (CGeneral_utils.is_cpp_translation context.translation_unit_context) then None
    else
      (* get fields and base classes of the current class *)
      let class_ptr =
        CContext.get_curr_class_decl_ptr stmt_info (CContext.get_curr_class context)
      in
      let decl = Option.value_exn (CAst_utils.get_decl class_ptr) in
      let fields = CAst_utils.get_record_fields decl in
      let bases = CAst_utils.get_cxx_base_classes decl in
      let _, sloc2 = stmt_info.Clang_ast_t.si_source_range in
      let stmt_info_loc = {stmt_info with Clang_ast_t.si_source_range= (sloc2, sloc2)} in
      (* compute `this` once that is used for all destructors of fields and base classes *)
      let obj_sil, this_qual_type, this_res_trans = compute_this_expr trans_state stmt_info_loc in
      (* ReturnStmt claims a priority with the same `stmt_info`.
         New pointer is generated to avoid premature node creation *)
      let stmt_info' =
        {stmt_info_loc with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
      in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let all_res_trans =
        List.rev_filter_map fields ~f:(function
          | Clang_ast_t.FieldDecl ({di_parent_pointer}, {ni_name}, qual_type, _) ->
              let class_tname =
                match CAst_utils.get_decl_opt di_parent_pointer with
                | Some decl ->
                    CType_decl.get_record_typename ~tenv:context.tenv decl
                | _ ->
                    assert false
              in
              let field_name = CGeneral_utils.mk_class_field_name class_tname ni_name in
              let field_exp = Exp.Lfield (obj_sil, field_name, this_qual_type) in
              let field_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
              let this_res_trans_destruct = mk_trans_result (field_exp, field_typ) empty_control in
              cxx_destructor_call_trans trans_state_pri stmt_info_loc this_res_trans_destruct
                qual_type.Clang_ast_t.qt_type_ptr ~is_inner_destructor:false
          | _ ->
              assert false )
      in
      let bases_res_trans =
        inject_base_class_destructor_calls trans_state_pri stmt_info_loc bases obj_sil
          this_qual_type
      in
      let all_res_trans =
        add_this_instrs_if_result_non_empty (all_res_trans @ bases_res_trans) this_res_trans
      in
      let sil_loc =
        CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
      in
      Some
        (PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:Destruction
           stmt_info' ~return:(mk_fresh_void_exp_typ ()) all_res_trans)


  and inject_destructors trans_state stmt_info =
    let context = trans_state.context in
    if not (CGeneral_utils.is_cpp_translation context.translation_unit_context) then None
    else
      let procname = Procdesc.get_proc_name context.CContext.procdesc in
      (* The source location of destructor should reflect the end of the statement *)
      let _, sloc2 = stmt_info.Clang_ast_t.si_source_range in
      let stmt_info_loc = {stmt_info with Clang_ast_t.si_source_range= (sloc2, sloc2)} in
      (* ReturnStmt claims a priority with the same `stmt_info`.
         New pointer is generated to avoid premature node creation *)
      let stmt_info' =
        {stmt_info_loc with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
      in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let all_res_trans =
        try
          let map = context.CContext.vars_to_destroy in
          let vars_to_destroy = CContext.StmtMap.find_exn map stmt_info.Clang_ast_t.si_pointer in
          List.filter_map
            ~f:(function
              | Clang_ast_t.VarDecl (_, _, qual_type, _) as decl ->
                  let pvar = CVar_decl.sil_var_of_decl context decl procname in
                  if Pvar.is_static_local pvar then (* don't call destructors on static vars *)
                    None
                  else
                    let exp = Exp.Lvar pvar in
                    let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
                    let this_res_trans_destruct = mk_trans_result (exp, typ) empty_control in
                    cxx_destructor_call_trans trans_state_pri stmt_info_loc this_res_trans_destruct
                      qual_type.Clang_ast_t.qt_type_ptr ~is_inner_destructor:false
              | _ ->
                  assert false)
            vars_to_destroy
        with Caml.Not_found ->
          L.(debug Capture Verbose) "@\n Variables that go out of scope are not found...@\n@." ;
          []
      in
      let sil_loc =
        CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
      in
      Some
        (PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:Destruction
           stmt_info' ~return:(mk_fresh_void_exp_typ ()) all_res_trans)


  and compoundStmt_trans trans_state stmt_info stmt_list =
    (* Computing destructor call nodes to inject at the end of the compound statement,
       except if the statement ends with Return statemenent *)
    let destr_trans_result =
      match List.last stmt_list with
      | Some (Clang_ast_t.ReturnStmt _) ->
          None
      | _ ->
          inject_destructors trans_state stmt_info
    in
    (* Injecting destructor call nodes at the end of the compound statement *)
    let succ_nodes =
      match destr_trans_result with
      | Some {control= {root_nodes= []}} | None ->
          trans_state.succ_nodes
      | Some {control= {root_nodes}} ->
          root_nodes
    in
    let trans_state' = {trans_state with succ_nodes} in
    let compound_control, returns = instructions trans_state' stmt_list in
    let compound_control' =
      match destr_trans_result with
      | Some {control= {leaf_nodes= []}} | None ->
          compound_control
      | Some {control= {leaf_nodes}} ->
          {compound_control with leaf_nodes}
    in
    mk_trans_result (last_or_mk_fresh_void_exp_typ returns) compound_control'


  and conditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let procdesc = context.CContext.procdesc in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let do_branch branch stmt var_typ prune_nodes join_node pvar =
      let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
      let trans_state' = {trans_state_pri with succ_nodes= []} in
      let res_trans_b = instruction trans_state' stmt in
      let e' =
        match res_trans_b.return with
        | _, {Typ.desc= Tvoid} ->
            (* void return *) Exp.Var (Ident.create_fresh Ident.knormal)
        | e', _ ->
            e'
      in
      let temp_var = Exp.Lvar pvar in
      let set_temp_var = [Sil.Store (temp_var, var_typ, e', sil_loc)] in
      let temp_return = (temp_var, var_typ) in
      let tmp_var_res_trans =
        mk_trans_result temp_return {empty_control with instrs= set_temp_var}
      in
      let trans_state'' = {trans_state' with succ_nodes= [join_node]} in
      let all_res_trans = [res_trans_b; tmp_var_res_trans] in
      let res_trans =
        PriorityNode.compute_results_to_parent trans_state'' sil_loc
          ~node_name:ConditionalStmtBranch stmt_info ~return:temp_return all_res_trans
      in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n res_trans.control.root_nodes [])
        prune_nodes' ;
      res_trans
    in
    match stmt_list with
    | [cond; exp1; exp2] ->
        let typ =
          CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
        in
        let var_typ = add_reference_if_glvalue typ expr_info in
        let join_node =
          Procdesc.create_node trans_state.context.CContext.procdesc sil_loc
            Procdesc.Node.Join_node []
        in
        Procdesc.node_set_succs_exn context.procdesc join_node succ_nodes [] ;
        let pvar = mk_temp_sil_var procdesc "SIL_temp_conditional___" in
        let var_data =
          ProcAttributes.
            {name= Pvar.get_name pvar; typ= var_typ; modify_in_block= false; is_constexpr= false}
        in
        Procdesc.append_locals procdesc [var_data] ;
        let continuation' = mk_cond_continuation trans_state.continuation in
        let trans_state' = {trans_state with continuation= continuation'; succ_nodes= []} in
        let res_trans_cond =
          exec_with_priority_exception trans_state' cond
            (cond_trans ~if_kind:Sil.Ik_bexp ~negate_cond:false)
        in
        (* Note: by contruction prune nodes are leafs_nodes_cond *)
        let _ : trans_result =
          do_branch true exp1 var_typ res_trans_cond.control.leaf_nodes join_node pvar
        in
        let _ : trans_result =
          do_branch false exp2 var_typ res_trans_cond.control.leaf_nodes join_node pvar
        in
        let id = Ident.create_fresh Ident.knormal in
        let instrs = [Sil.Load (id, Exp.Lvar pvar, var_typ, sil_loc)] in
        mk_trans_result (Exp.Var id, typ)
          { root_nodes= res_trans_cond.control.root_nodes
          ; leaf_nodes= [join_node]
          ; instrs
          ; initd_exps= [] (* TODO we should get exps from branches+cond *) }
    | _ ->
        assert false


  (** The GNU extension to the conditional operator which allows the middle operand to be
     omitted. *)
  and binaryConditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    match stmt_list with
    | [stmt1; ostmt1; ostmt2; stmt2]
      when contains_opaque_value_expr ostmt1 && contains_opaque_value_expr ostmt2 ->
        let sil_loc =
          CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
            stmt_info
        in
        let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
        let trans_state_cond =
          {trans_state_pri with continuation= mk_cond_continuation trans_state_pri.continuation}
        in
        (* evaluate stmt1 once. Then, use it as replacement for OpaqueValueExpr*)
        (* when translating ostmt1 and ostmt2 *)
        let init_res_trans = instruction trans_state_cond stmt1 in
        let opaque_exp = init_res_trans.return in
        let trans_state' = {trans_state_pri with opaque_exp= Some opaque_exp} in
        let op_res_trans =
          conditionalOperator_trans trans_state' stmt_info [ostmt1; ostmt2; stmt2] expr_info
        in
        let trans_state'' = {trans_state_cond with succ_nodes= op_res_trans.control.root_nodes} in
        let init_res_trans' =
          PriorityNode.compute_result_to_parent trans_state'' sil_loc
            ~node_name:BinaryConditionalStmtInit stmt_info init_res_trans
        in
        let root_nodes = init_res_trans'.control.root_nodes in
        if root_nodes <> [] then {op_res_trans with control= {op_res_trans.control with root_nodes}}
        else op_res_trans
    | _ ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "BinaryConditionalOperator not translated"


  (** Translate a condition for if/loops statement. It shorts-circuit and/or. The invariant is that
     the translation of a condition always contains (at least) the prune nodes. Moreover these are
     always the leaf nodes of the translation. *)
  and cond_trans ~if_kind ~negate_cond trans_state cond : trans_result =
    let context = trans_state.context in
    let cond_source_range = source_range_of_stmt cond in
    let sil_loc =
      CLocation.location_of_source_range context.translation_unit_context.source_file
        cond_source_range
    in
    let mk_prune_node ~branch ~negate_cond e ins =
      create_prune_node context.procdesc ~branch ~negate_cond e ins sil_loc if_kind
    in
    (* this function translate cond without doing shortcircuit *)
    let no_short_circuit_cond ~is_cmp =
      L.(debug Capture Verbose) " No short-circuit condition@\n" ;
      let res_trans_cond =
        if is_null_stmt cond then
          mk_trans_result
            (Exp.Const (Const.Cint IntLit.one), Typ.mk (Tint Typ.IBool))
            empty_control
          (* Assumption: If it's a null_stmt, it is a loop with no bound, so we set condition to 1 *)
        else if is_cmp then
          let open Clang_ast_t in
          (* If we have a comparision here, do not dispatch it to `instruction` function, which
           * invokes binaryOperator_trans_with_cond -> conditionalOperator_trans -> cond_trans.
           * This will throw the translation process into an infinite loop immediately.
           * Instead, dispatch to binaryOperator_trans directly. *)
          (* If one wants to add a new kind of `BinaryOperator` that will have the same behavior,
           * she need to change both the codes here and the `match` in
           * binaryOperator_trans_with_cond *)
          match cond with
          | BinaryOperator (si, ss, ei, boi) ->
              binaryOperator_trans trans_state boi si ei ss
          | _ ->
              instruction trans_state cond
        else instruction trans_state cond
      in
      let ((e', _) as return), instrs' =
        define_condition_side_effects res_trans_cond.return res_trans_cond.control.instrs sil_loc
      in
      let prune_t = mk_prune_node ~branch:true ~negate_cond e' instrs' in
      let prune_f = mk_prune_node ~branch:false ~negate_cond:(not negate_cond) e' instrs' in
      List.iter
        ~f:(fun n' -> Procdesc.node_set_succs_exn context.procdesc n' [prune_t; prune_f] [])
        res_trans_cond.control.leaf_nodes ;
      let root_nodes =
        if List.is_empty res_trans_cond.control.root_nodes then [prune_t; prune_f]
        else res_trans_cond.control.root_nodes
      in
      mk_trans_result return
        {empty_control with root_nodes; leaf_nodes= [prune_t; prune_f]; instrs= instrs'}
    in
    (* This function translate (s1 binop s2) doing shortcircuit for '&&' and '||' *)
    (* At the high level it does cond_trans s1; cond_trans s2; glue_nodes *)
    (* The glue_nodes partitions the prune nodes of s1's translation.*)
    (* Some of them need to go to the statement to be executed after the *)
    (* condition (prune_to_short_c) and others to the root nodes of the *)
    (* translation of s2 (i.e., the case when we need to fully evaluate*)
    (* the condition to decide its truth value). *)
    let short_circuit binop s1 s2 =
      let res_trans_s1 = cond_trans ~if_kind ~negate_cond trans_state s1 in
      let prune_nodes_t, prune_nodes_f =
        List.partition_tf ~f:is_true_prune_node res_trans_s1.control.leaf_nodes
      in
      let res_trans_s2 = cond_trans ~if_kind ~negate_cond trans_state s2 in
      (* prune_to_s2 is the prune node that is connected with the root node of the *)
      (* translation of s2.*)
      (* prune_to_short_c is the prune node that is connected directly with the branch *)
      (* where the control flow goes in case of short circuit *)
      let prune_to_s2, prune_to_short_c =
        match binop with
        | Binop.LAnd ->
            (prune_nodes_t, prune_nodes_f)
        | Binop.LOr ->
            (prune_nodes_f, prune_nodes_t)
        | _ ->
            assert false
      in
      List.iter
        ~f:(fun n ->
          Procdesc.node_set_succs_exn context.procdesc n res_trans_s2.control.root_nodes [] )
        prune_to_s2 ;
      let root_nodes_to_parent =
        if List.is_empty res_trans_s1.control.root_nodes then res_trans_s1.control.leaf_nodes
        else res_trans_s1.control.root_nodes
      in
      let exp1, typ1 = res_trans_s1.return in
      let exp2, _ = res_trans_s2.return in
      let e_cond = Exp.BinOp (binop, exp1, exp2) in
      mk_trans_result (e_cond, typ1)
        { root_nodes= root_nodes_to_parent
        ; leaf_nodes= prune_to_short_c @ res_trans_s2.control.leaf_nodes
        ; instrs= res_trans_s1.control.instrs @ res_trans_s2.control.instrs
        ; initd_exps= [] }
    in
    L.(debug Capture Verbose)
      "Translating Condition for If-then-else/Loop/Conditional Operator @\n" ;
    let open Clang_ast_t in
    match cond with
    | BinaryOperator (_, [s1; s2], _, boi) -> (
      match boi.boi_kind with
      | `LAnd ->
          short_circuit (if negate_cond then Binop.LOr else Binop.LAnd) s1 s2
      | `LOr ->
          short_circuit (if negate_cond then Binop.LAnd else Binop.LOr) s1 s2
      | `LT | `GT | `LE | `GE | `EQ | `NE ->
          no_short_circuit_cond ~is_cmp:true
      | _ ->
          no_short_circuit_cond ~is_cmp:false )
    | ParenExpr (_, [s], _) ->
        (* condition can be wrapped in parenthesys *)
        cond_trans ~if_kind ~negate_cond trans_state s
    | UnaryOperator (_, [s], _, {uoi_kind= `LNot}) ->
        cond_trans ~if_kind ~negate_cond:(not negate_cond) trans_state s
    | _ ->
        no_short_circuit_cond ~is_cmp:false


  and declStmt_in_condition_trans trans_state decl_stmt res_trans_cond =
    match decl_stmt with
    | Clang_ast_t.DeclStmt (stmt_info, _, decl_list) ->
        let trans_state_decl = {trans_state with succ_nodes= res_trans_cond.control.root_nodes} in
        declStmt_trans trans_state_decl decl_list stmt_info
    | _ ->
        res_trans_cond


  and ifStmt_trans trans_state stmt_info stmt_list =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let join_node = Procdesc.create_node context.procdesc sil_loc Procdesc.Node.Join_node [] in
    Procdesc.node_set_succs_exn context.procdesc join_node succ_nodes [] ;
    let trans_state' = {trans_state with succ_nodes= [join_node]} in
    let do_branch branch stmt_branch prune_nodes =
      (* leaf nodes are ignored here as they will be already attached to join_node *)
      let res_trans_b = instruction trans_state' stmt_branch in
      let nodes_branch =
        match res_trans_b.control.root_nodes with
        | [] ->
            [ Procdesc.create_node context.procdesc sil_loc (Procdesc.Node.Stmt_node IfStmtBranch)
                res_trans_b.control.instrs ]
        | _ ->
            res_trans_b.control.root_nodes
      in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n nodes_branch [])
        prune_nodes'
    in
    match stmt_list with
    | [_; decl_stmt; cond; stmt1; stmt2] ->
        (* set the flat to inform that we are translating a condition of a if *)
        let continuation' = mk_cond_continuation trans_state.continuation in
        let trans_state'' = {trans_state with continuation= continuation'; succ_nodes= []} in
        let res_trans_cond = cond_trans ~if_kind:Sil.Ik_if ~negate_cond:false trans_state'' cond in
        let res_trans_decl = declStmt_in_condition_trans trans_state decl_stmt res_trans_cond in
        (* Note: by contruction prune nodes are leafs_nodes_cond *)
        do_branch true stmt1 res_trans_cond.control.leaf_nodes ;
        do_branch false stmt2 res_trans_cond.control.leaf_nodes ;
        mk_trans_result (mk_fresh_void_exp_typ ())
          { empty_control with
            root_nodes= res_trans_decl.control.root_nodes; leaf_nodes= [join_node] }
    | _ ->
        assert false


  and caseStmt_trans trans_state stmt_info case_stmt_list =
    (* ignore the [case lhs ... rhs: body] form, only support the [case condition: body] form *)
    let[@warning "-8"] [condition; _rhs; body] = case_stmt_list in
    let body_trans_result = instruction trans_state body in
    (let open SwitchCase in
    add {condition= Case condition; stmt_info; root_nodes= body_trans_result.control.root_nodes}) ;
    body_trans_result


  and defaultStmt_trans trans_state stmt_info default_stmt_list =
    let[@warning "-8"] [body] = default_stmt_list in
    let body_trans_result = instruction trans_state body in
    (let open SwitchCase in
    add {condition= Default; stmt_info; root_nodes= body_trans_result.control.root_nodes}) ;
    body_trans_result


  and switchStmt_trans trans_state stmt_info switch_stmt_list =
    (* overview: translate the body of the switch statement, which automatically collects the
       various cases at the same time, then link up the cases together and together with the switch
       condition variable *)
    (* unsupported: initialization *)
    let[@warning "-8"] [_initialization; variable; condition; body] = switch_stmt_list in
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state' = {trans_state_pri with succ_nodes= []} in
    let res_trans_cond_tmp = instruction trans_state' condition in
    let switch_node =
      let node_kind = Procdesc.Node.Stmt_node SwitchStmt in
      Procdesc.create_node context.procdesc sil_loc node_kind res_trans_cond_tmp.control.instrs
    in
    List.iter
      ~f:(fun n' -> Procdesc.node_set_succs_exn context.procdesc n' [switch_node] [])
      res_trans_cond_tmp.control.leaf_nodes ;
    let root_nodes =
      if res_trans_cond_tmp.control.root_nodes <> [] then res_trans_cond_tmp.control.root_nodes
      else [switch_node]
    in
    let condition_exp, _ = res_trans_cond_tmp.return in
    let condition_result =
      { res_trans_cond_tmp with
        control= {res_trans_cond_tmp.control with root_nodes; leaf_nodes= [switch_node]} }
    in
    let variable_result = declStmt_in_condition_trans trans_state variable condition_result in
    let trans_state_no_pri =
      if PriorityNode.own_priority_node trans_state_pri.priority stmt_info then
        {trans_state_pri with priority= Free}
      else trans_state_pri
    in
    let continuation' =
      let switch_exit_point = trans_state.succ_nodes in
      match trans_state.continuation with
      | Some cont ->
          Some {cont with break= switch_exit_point}
      | None ->
          Some {break= switch_exit_point; continue= []; return_temp= false}
    in
    let inner_trans_state = {trans_state_no_pri with continuation= continuation'} in
    let switch_cases, (_ : trans_result) =
      SwitchCase.in_switch_body ~f:(instruction inner_trans_state) body
    in
    let link_up_switch_cases curr_succ_nodes = function
      | {SwitchCase.condition= Case case_condition; stmt_info; root_nodes} ->
          (* create case prune nodes, link the then branch to [root_nodes], the else branch to
             [curr_succ_nodes] *)
          let trans_state_pri = PriorityNode.try_claim_priority_node inner_trans_state stmt_info in
          let res_trans_case_const = instruction trans_state_pri case_condition in
          let e_const, _ = res_trans_case_const.return in
          let sil_eq_cond = Exp.BinOp (Binop.Eq, condition_exp, e_const) in
          let sil_loc =
            CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
          in
          let true_prune_node =
            create_prune_node context.procdesc ~branch:true ~negate_cond:false sil_eq_cond
              res_trans_case_const.control.instrs sil_loc Sil.Ik_switch
          in
          let false_prune_node =
            create_prune_node context.procdesc ~branch:false ~negate_cond:true sil_eq_cond
              res_trans_case_const.control.instrs sil_loc Sil.Ik_switch
          in
          Procdesc.node_set_succs_exn context.procdesc true_prune_node root_nodes [] ;
          Procdesc.node_set_succs_exn context.procdesc false_prune_node curr_succ_nodes [] ;
          (* return prune nodes as next roots *)
          [true_prune_node; false_prune_node]
      | {SwitchCase.condition= Default; root_nodes} ->
          (* just return the [root_nodes] to be linked to the previous case's fallthrough *)
          root_nodes
    in
    let cases_root_nodes =
      List.fold switch_cases ~init:trans_state.succ_nodes ~f:link_up_switch_cases
    in
    Procdesc.node_set_succs_exn context.procdesc switch_node cases_root_nodes [] ;
    let top_nodes = variable_result.control.root_nodes in
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes= top_nodes; leaf_nodes= trans_state.succ_nodes}


  and stmtExpr_trans trans_state source_range stmt_list =
    let stmt =
      extract_stmt_from_singleton stmt_list source_range "StmtExpr should have only one statement."
    in
    let trans_state' = {trans_state with priority= Free} in
    instruction trans_state' stmt


  and tryStmt_trans trans_state stmts =
    let open Clang_ast_t in
    let translate_catch catch_root_nodes_acc = function
      | CXXCatchStmt (catch_stmt_info, catch_body_stmts, _) ->
          let catch_trans_result =
            compoundStmt_trans trans_state catch_stmt_info catch_body_stmts
          in
          (* no risk of duplicates because two catch blocks should never have the same root nodes
             (they have to be in different syntactic locations, after all!) *)
          catch_trans_result.control.root_nodes @ catch_root_nodes_acc
      | _ ->
          assert false
    in
    match stmts with
    | try_body_stmt :: catch_stmts ->
        let try_trans_result = instruction trans_state try_body_stmt in
        let catch_start_nodes = List.fold catch_stmts ~f:translate_catch ~init:[] in
        (* add catch block as exceptional successor to end of try block. not ideal, but we will at
           least reach the code in the catch block this way *)
        (* TODO (T28898377): instead, we should extend trans_state with a list of maybe-throwing
           blocks, and add transitions from those to the catch block instead *)
        let try_control = try_trans_result.control in
        let try_ends =
          if List.is_empty try_control.leaf_nodes then
            (* try ends in return; transition from beginning instead *)
            try_control.root_nodes
          else try_control.leaf_nodes
        in
        (* do not add exceptional successors to return nodes *)
        List.iter
          ~f:(fun try_end ->
            match Procdesc.Node.get_kind try_end with
            | Procdesc.Node.Stmt_node ReturnStmt ->
                ()
            | _ ->
                Procdesc.set_succs_exn_only try_end catch_start_nodes )
          try_ends ;
        try_trans_result
    | _ ->
        (* try should always have a catch statement *)
        assert false


  and loop_instruction trans_state loop_kind stmt_info =
    let outer_continuation = trans_state.continuation in
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let join_node = Procdesc.create_node context.procdesc sil_loc Procdesc.Node.Join_node [] in
    let continuation = Some {break= succ_nodes; continue= [join_node]; return_temp= false} in
    (* set the flag to inform that we are translating a condition of a if *)
    let continuation_cond = mk_cond_continuation outer_continuation in
    let init_incr_nodes =
      match loop_kind with
      | Loops.For {init; increment} ->
          let trans_state' = {trans_state with succ_nodes= [join_node]; continuation} in
          let res_trans_init = instruction trans_state' init in
          let res_trans_incr = instruction trans_state' increment in
          Some (res_trans_init.control.root_nodes, res_trans_incr.control.root_nodes)
      | _ ->
          None
    in
    let cond_stmt = Loops.get_cond loop_kind in
    let trans_state_cond = {trans_state with continuation= continuation_cond; succ_nodes= []} in
    let if_kind =
      match loop_kind with
      | Loops.For _ ->
          Sil.Ik_for
      | Loops.While _ ->
          Sil.Ik_while
      | Loops.DoWhile _ ->
          Sil.Ik_dowhile
    in
    let res_trans_cond = cond_trans ~if_kind ~negate_cond:false trans_state_cond cond_stmt in
    let res_trans_decl =
      match loop_kind with
      | Loops.For {decl_stmt} | Loops.While {decl_stmt} ->
          declStmt_in_condition_trans trans_state decl_stmt res_trans_cond
      | Loops.DoWhile _ ->
          res_trans_cond
    in
    let body_succ_nodes =
      match loop_kind with
      | Loops.For _ -> (
        match init_incr_nodes with Some (_, nodes_incr) -> nodes_incr | None -> assert false )
      | Loops.While _ ->
          [join_node]
      | Loops.DoWhile _ ->
          res_trans_cond.control.root_nodes
    in
    let body_continuation =
      match (loop_kind, continuation, init_incr_nodes) with
      | Loops.DoWhile _, Some c, _ ->
          Some {c with continue= res_trans_cond.control.root_nodes}
      | _, Some c, Some (_, nodes_incr) ->
          Some {c with continue= nodes_incr}
      | _ ->
          continuation
    in
    let res_trans_body =
      let trans_state_body =
        {trans_state with succ_nodes= body_succ_nodes; continuation= body_continuation}
      in
      instruction trans_state_body (Loops.get_body loop_kind)
    in
    let join_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ ->
          res_trans_decl.control.root_nodes
      | Loops.DoWhile _ ->
          res_trans_body.control.root_nodes
    in
    (* Note: prune nodes are by contruction the res_trans_cond.control.leaf_nodes *)
    let prune_nodes_t, prune_nodes_f =
      List.partition_tf ~f:is_true_prune_node res_trans_cond.control.leaf_nodes
    in
    let prune_t_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ ->
          res_trans_body.control.root_nodes
      | Loops.DoWhile _ ->
          [join_node]
    in
    Procdesc.node_set_succs_exn context.procdesc join_node join_succ_nodes [] ;
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n prune_t_succ_nodes [])
      prune_nodes_t ;
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n succ_nodes [])
      prune_nodes_f ;
    let root_nodes =
      match loop_kind with
      | Loops.For _ -> (
        match init_incr_nodes with Some (nodes_init, _) -> nodes_init | None -> assert false )
      | Loops.While _ | Loops.DoWhile _ ->
          [join_node]
    in
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes; leaf_nodes= prune_nodes_f}


  and forStmt_trans trans_state ~init ~decl_stmt ~condition ~increment ~body stmt_info =
    let for_kind = Loops.For {init; decl_stmt; condition; increment; body} in
    loop_instruction trans_state for_kind stmt_info


  and whileStmt_trans trans_state ~decl_stmt ~condition ~body stmt_info =
    let while_kind = Loops.While {decl_stmt; condition; body} in
    loop_instruction trans_state while_kind stmt_info


  and doStmt_trans trans_state ~condition ~body stmt_info =
    let dowhile_kind = Loops.DoWhile {condition; body} in
    loop_instruction trans_state dowhile_kind stmt_info


  (** Iteration over collection

      for (v : C) { body; }

     is translated as follows:

      TypeC __range = C;
      for (__begin = __range.begin(), __end = __range.end();
           __begin != __end;
           ++__begin)
       {
          v = *__begin;
          loop_body;
        }
  *)
  and cxxForRangeStmt_trans trans_state stmt_info stmt_list =
    let open Clang_ast_t in
    match stmt_list with
    | [iterator_decl; begin_stmt; end_stmt; exit_cond; increment; assign_current_index; loop_body]
      ->
        let loop_body' = CompoundStmt (stmt_info, [assign_current_index; loop_body]) in
        let null_stmt = NullStmt (stmt_info, []) in
        let beginend_stmt = CompoundStmt (stmt_info, [begin_stmt; end_stmt]) in
        let for_loop =
          ForStmt (stmt_info, [beginend_stmt; null_stmt; exit_cond; increment; loop_body'])
        in
        instruction trans_state (CompoundStmt (stmt_info, [iterator_decl; for_loop]))
    | _ ->
        assert false


  (** Fast iteration for colection
     for (type_it i in collection) { body }
     is translate as
     {
      i = type_next_object();
      while(i != nil) { body; i = type_next_object();}
     }
  *)
  and objCForCollectionStmt_trans trans_state item items body stmt_info =
    ignore (instruction trans_state item) ;
    (* Here we do ast transformation, so we don't need the value of the translation of the *)
    (* variable item but we still need to add the variable to the locals *)
    let assign_next_object, cond = Ast_expressions.make_next_object_exp stmt_info item items in
    let body' = Clang_ast_t.CompoundStmt (stmt_info, [body; assign_next_object]) in
    let null_stmt = Clang_ast_t.NullStmt (stmt_info, []) in
    let loop = Clang_ast_t.WhileStmt (stmt_info, [null_stmt; cond; body']) in
    instruction trans_state (Clang_ast_t.CompoundStmt (stmt_info, [assign_next_object; loop]))


  and initListExpr_array_trans trans_state stmt_info stmts var_exp field_typ =
    let lh_exp idx =
      let idx_exp = Exp.Const (Const.Cint (IntLit.of_int idx)) in
      Exp.Lindex (var_exp, idx_exp)
    in
    let init_field idx stmt =
      init_expr_trans trans_state (lh_exp idx, field_typ) stmt_info (Some stmt)
    in
    (* rest of fields when length(stmts) < size is ignored *)
    List.mapi ~f:init_field stmts


  and initListExpr_struct_trans trans_state stmt_info stmts var_exp var_typ =
    let context = trans_state.context in
    let tenv = context.tenv in
    let tname = match var_typ.Typ.desc with Tstruct tname -> tname | _ -> assert false in
    let field_exps =
      match Tenv.lookup tenv tname with
      | Some {fields} ->
          List.filter_map fields ~f:(fun (fieldname, fieldtype, _) ->
              Some (Exp.Lfield (var_exp, fieldname, var_typ), fieldtype) )
      | None ->
          assert false
    in
    let init_field field_exp_typ stmt =
      init_expr_trans trans_state field_exp_typ stmt_info (Some stmt)
    in
    match List.map2 field_exps stmts ~f:init_field with
    | Ok result ->
        result
    | Unequal_lengths ->
        (* This can happen with union initializers. Skip them for now *) []


  and initListExpr_builtin_trans trans_state stmt_info stmts var_exp var_typ =
    match stmts with
    | [stmt] ->
        [init_expr_trans trans_state (var_exp, var_typ) stmt_info (Some stmt)]
    | _ ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "InitListExpression for var %a type %a with multiple init statements" Exp.pp var_exp
          (Typ.pp_full Pp.text) var_typ


  (** InitListExpr can have following meanings:
      - initialize all record fields
      - initialize array
      - initialize primitive type (int/flaot/pointer/...)
      - perform zero initalization - http://en.cppreference.com/w/cpp/language/zero_initialization
      Decision which case happens is based on the type of the InitListExpr
  *)
  and initListExpr_trans trans_state stmt_info expr_info stmts =
    let var_exp, var_typ =
      match trans_state.var_exp_typ with
      | Some var_exp_typ ->
          var_exp_typ
      | None ->
          create_var_exp_tmp_var trans_state expr_info "SIL_init_list__"
    in
    if List.is_empty stmts then
      (* perform zero initialization of a primitive type, record types will have
         ImplicitValueInitExpr nodes *)
      let return_exp = Exp.zero_of_type var_typ |> Option.value ~default:Exp.zero in
      let return = (return_exp, var_typ) in
      mk_trans_result return {empty_control with root_nodes= trans_state.succ_nodes}
    else
      let sil_loc =
        CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
          stmt_info
      in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
      let init_stmt_info =
        {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
      in
      let all_res_trans =
        match var_typ.Typ.desc with
        | Typ.Tarray {elt} ->
            initListExpr_array_trans trans_state_pri init_stmt_info stmts var_exp elt
        | Tstruct _ ->
            initListExpr_struct_trans trans_state_pri init_stmt_info stmts var_exp var_typ
        | Tint _ | Tfloat _ | Tptr _ ->
            initListExpr_builtin_trans trans_state_pri init_stmt_info stmts var_exp var_typ
        | _ ->
            CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
              "InitListExp for var %a of type %a" Exp.pp var_exp (Typ.pp Pp.text) var_typ
      in
      let res_trans =
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:InitListExp
          stmt_info ~return:(var_exp, var_typ) all_res_trans
      in
      {res_trans with control= {res_trans.control with initd_exps= [var_exp]}}


  and init_dynamic_array trans_state array_exp_typ array_stmt_info dynlength_stmt_pointer =
    let dynlength_stmt =
      Int.Table.find_exn ClangPointers.pointer_stmt_table dynlength_stmt_pointer
    in
    let dynlength_stmt_info, _ = Clang_ast_proj.get_stmt_tuple dynlength_stmt in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state array_stmt_info in
    let dynlength_trans_result = instruction trans_state_pri dynlength_stmt in
    let dynlength_exp_typ = dynlength_trans_result.return in
    let sil_loc =
      CLocation.location_of_stmt_info trans_state_pri.context.translation_unit_context.source_file
        dynlength_stmt_info
    in
    let ret_id_typ, ret_exp_typ = mk_fresh_void_return () in
    let call_instr =
      let call_exp = Exp.Const (Const.Cfun BuiltinDecl.__set_array_length) in
      let actuals = [array_exp_typ; dynlength_exp_typ] in
      Sil.Call (ret_id_typ, call_exp, actuals, sil_loc, CallFlags.default)
    in
    let call_trans_control = {empty_control with instrs= [call_instr]} in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc
      ~node_name:InitializeDynamicArrayLength dynlength_stmt_info
      [dynlength_trans_result.control; call_trans_control]
    |> mk_trans_result ret_exp_typ


  and init_expr_trans trans_state var_exp_typ ?qual_type var_stmt_info init_expr_opt =
    match init_expr_opt with
    | None -> (
      match
        Option.map ~f:(fun qt -> qt.Clang_ast_t.qt_type_ptr) qual_type
        |> Option.find_map ~f:CAst_utils.get_type
      with
      | Some (Clang_ast_t.VariableArrayType (_, _, stmt_pointer)) ->
          (* Set the dynamic length of the variable length array. Variable length array cannot
                 have an initialization expression. *)
          init_dynamic_array trans_state var_exp_typ var_stmt_info stmt_pointer
      | _ ->
          (* Nothing to do if no init expression and not a variable length array *)
          mk_trans_result var_exp_typ {empty_control with root_nodes= trans_state.succ_nodes} )
    | Some ie ->
        (* For init expr, translate how to compute it and assign to the var *)
        let var_exp, _ = var_exp_typ in
        let context = trans_state.context in
        let sil_loc =
          CLocation.location_of_stmt_info context.translation_unit_context.source_file
            var_stmt_info
        in
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state var_stmt_info in
        (* if ie is a block the translation need to be done
           with the block special cases by exec_with_block_priority *)
        let res_trans_ie =
          let trans_state' =
            {trans_state_pri with succ_nodes= []; var_exp_typ= Some var_exp_typ}
          in
          let instruction' = exec_with_glvalue_as_reference instruction in
          exec_with_block_priority_exception instruction' trans_state' ie var_stmt_info
        in
        let assign_trans_control_opt =
          if
            (* variable might be initialized already - do nothing in that case*)
            List.exists ~f:(Exp.equal var_exp) res_trans_ie.control.initd_exps
          then None
          else
            let sil_e1', ie_typ = res_trans_ie.return in
            Some {empty_control with instrs= [Sil.Store (var_exp, ie_typ, sil_e1', sil_loc)]}
        in
        let pre_init_opt =
          match var_exp with
          | Exp.Lvar _ ->
              let sil_fun = Exp.Const (Const.Cfun BuiltinDecl.__variable_initialization) in
              let ret_id = Ident.create_fresh Ident.knormal in
              Some
                { empty_control with
                  instrs=
                    [ Sil.Call
                        ( (ret_id, Typ.void)
                        , sil_fun
                        , [var_exp_typ]
                        , sil_loc
                        , {CallFlags.default with cf_assign_last_arg= true} ) ] }
          | _ ->
              None
        in
        let all_res_trans =
          Option.to_list pre_init_opt
          @ (res_trans_ie.control :: Option.to_list assign_trans_control_opt)
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name:DeclStmt
          var_stmt_info all_res_trans
        |> mk_trans_result var_exp_typ


  and collect_all_decl trans_state var_decls next_nodes stmt_info : trans_result =
    let open Clang_ast_t in
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let do_var_dec var_decl qual_type vdi next_node =
      let pvar = CVar_decl.sil_var_of_decl context var_decl procname in
      let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
      CVar_decl.add_var_to_locals procdesc var_decl typ pvar ;
      let trans_state' = {trans_state with succ_nodes= next_node} in
      init_expr_trans trans_state' (Exp.Lvar pvar, typ) ~qual_type stmt_info
        vdi.Clang_ast_t.vdi_init_expr
    in
    let rec aux : decl list -> trans_result option = function
      | [] ->
          None
      | (VarDecl (_, _, qt, vdi) as var_decl) :: var_decls' ->
          (* Var are defined when procdesc is created, here we only take care of initialization *)
          let res_trans_tl = aux var_decls' in
          let root_nodes_tl, instrs_tl, initd_exps_tl =
            match res_trans_tl with
            | None ->
                (next_nodes, [], [])
            | Some {control= {root_nodes; instrs; initd_exps}} ->
                (root_nodes, instrs, initd_exps)
          in
          let res_trans_tmp = do_var_dec var_decl qt vdi root_nodes_tl in
          (* keep the last return and leaf_nodes from the list *)
          let return, leaf_nodes =
            match res_trans_tl with
            | None ->
                (res_trans_tmp.return, res_trans_tmp.control.leaf_nodes)
            | Some {return; control= {leaf_nodes}} ->
                (return, leaf_nodes)
          in
          Some
            (mk_trans_result return
               { root_nodes= res_trans_tmp.control.root_nodes
               ; leaf_nodes
               ; instrs= res_trans_tmp.control.instrs @ instrs_tl
               ; initd_exps= res_trans_tmp.control.initd_exps @ initd_exps_tl })
      | _ :: var_decls' ->
          (* Here we can get also record declarations or typedef declarations, which are dealt with
             somewhere else.  We just handle the variables here. *)
          aux var_decls'
    in
    match aux var_decls with
    | Some trans_result ->
        trans_result
    | None ->
        mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= next_nodes}


  (* stmt_list is ignored because it contains the same instructions as *)
  (* the init expression. We use the latter info.                      *)
  and declStmt_trans trans_state decl_list stmt_info : trans_result =
    let succ_nodes = trans_state.succ_nodes in
    match (decl_list : Clang_ast_t.decl list) with
    | VarDecl _ :: _ | CXXRecordDecl _ :: _ | RecordDecl _ :: _ ->
        collect_all_decl trans_state decl_list succ_nodes stmt_info
    | (TypedefDecl _ | TypeAliasDecl _ | UsingDecl _ | UsingDirectiveDecl _) :: _ ->
        mk_trans_result (mk_fresh_void_exp_typ ()) empty_control
    | decl :: _ ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "In DeclStmt found an unknown declaration type %s" (Clang_ast_j.string_of_decl decl)
    | [] ->
        assert false


  and objCPropertyRefExpr_trans trans_state stmt_list =
    match stmt_list with [stmt] -> instruction trans_state stmt | _ -> assert false


  (** For OpaqueValueExpr we return the translation generated from its source expression*)
  and opaqueValueExpr_trans trans_state opaque_value_expr_info source_range =
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    match trans_state.opaque_exp with
    | Some exp ->
        mk_trans_result exp empty_control
    | None -> (
      match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
      | Some stmt ->
          instruction trans_state stmt
      | None ->
          CFrontend_config.incorrect_assumption __POS__ source_range
            "Expected source expression for OpaqueValueExpr" )


  (* NOTE: This translation has several hypothesis. Need to be verified when we have more*)
  (* experience with this construct. Assert false will help to see if we encounter programs*)
  (* that do not conform with this hypothesis.*)
  (* Hypotheses:*)
  (* 1. stmt_list is composed by 2 parts: the first element is a syntactic description of the*)
  (* expression. The rest of the list has a semantic caracterization of the expression and*)
  (* defines how that expression is going to be implemented at runtime. *)
  (* 2. the semantic description is composed by a list of OpaqueValueExpr that define the *)
  (* various expressions involved and one finale expression that define how the final value of*)
  (* the PseudoObjectExpr is obtained.
     All the OpaqueValueExpr will be part of the last expression.*)
  (* So they can be skipped. *)
  (* For example: 'x.f = a' when 'f' is a property will be
     translated with a call to f's setter [x f:a]*)
  (* the stmt_list will be [x.f = a; x; a; CallToSetter]
     Among all element of the list we only need*)
  (* to translate the CallToSetter which is
     how x.f = a is actually implemented by the runtime.*)
  and pseudoObjectExpr_trans trans_state stmt_list =
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@\n"
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    let rec do_semantic_elements el =
      let open Clang_ast_t in
      match el with
      | OpaqueValueExpr _ :: el' ->
          do_semantic_elements el'
      | stmt :: _ ->
          instruction trans_state stmt
      | _ ->
          assert false
    in
    match stmt_list with
    | _ :: semantic_form ->
        do_semantic_elements semantic_form
    | _ ->
        assert false


  (** Cast expression are treated the same apart from the cast operation kind *)
  and cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_expr_info =
    let context = trans_state.context in
    L.(debug Capture Verbose)
      "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state)) ;
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let stmt =
      extract_stmt_from_singleton stmt_list stmt_info.Clang_ast_t.si_source_range
        "In CastExpr There must be only one stmt defining the expression to be cast."
    in
    let res_trans_stmt = instruction trans_state stmt in
    let typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    let cast_kind = cast_expr_info.Clang_ast_t.cei_cast_kind in
    let exp_typ = res_trans_stmt.return in
    (* This gives the difference among cast operations kind *)
    let cast_inst, cast_exp = cast_operation cast_kind exp_typ typ sil_loc in
    { res_trans_stmt with
      control= {res_trans_stmt.control with instrs= res_trans_stmt.control.instrs @ cast_inst}
    ; return= cast_exp }


  (** function used in the computation for both Member_Expr and ObjCIVarRefExpr *)
  and do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref =
    let exp_stmt =
      extract_stmt_from_singleton stmt_list stmt_info.Clang_ast_t.si_source_range
        "in MemberExpr there must be only one stmt defining its expression."
    in
    (* Don't pass var_exp_typ to child of MemberExpr - this may lead to initializing variable *)
    (* with wrong value. For example, we don't want p to be initialized with X(1) for:*)
    (* int p = X(1).field; *)
    let trans_state' = {trans_state with var_exp_typ= None} in
    let result_trans_exp_stmt = exec_with_glvalue_as_reference instruction trans_state' exp_stmt in
    decl_ref_trans ~context:(MemberOrIvar result_trans_exp_stmt) trans_state stmt_info decl_ref


  and objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info =
    let decl_ref = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref in
    do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref


  and memberExpr_trans trans_state stmt_info stmt_list member_expr_info =
    let decl_ref = member_expr_info.Clang_ast_t.mei_decl_ref in
    let res_trans = do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref in
    let is_virtual_dispatch = member_expr_info.Clang_ast_t.mei_performs_virtual_dispatch in
    {res_trans with is_cpp_call_virtual= res_trans.is_cpp_call_virtual && is_virtual_dispatch}


  and unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let stmt =
      extract_stmt_from_singleton stmt_list stmt_info.Clang_ast_t.si_source_range
        "We expect only one element in stmt list defining the operand in UnaryOperator."
    in
    let trans_state' = {trans_state_pri with succ_nodes= []} in
    let res_trans_stmt = instruction trans_state' stmt in
    (* Assumption: the operand does not create a cfg node*)
    let sil_e', _ = res_trans_stmt.return in
    let ret_typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    let exp_op, instr_op =
      CArithmetic_trans.unary_operation_instruction context.translation_unit_context
        unary_operator_info sil_e' ret_typ sil_loc
    in
    let unary_op_control = {empty_control with instrs= instr_op} in
    let all_control = [res_trans_stmt.control; unary_op_control] in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name:UnaryOperator
      stmt_info all_control
    |> mk_trans_result (exp_op, ret_typ)


  and returnStmt_trans trans_state stmt_info stmt_list =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let procdesc = context.CContext.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    (* Check that the destructor-injecting functions do not create new nodes for return statement
       This is ensured by creating a fresh pointer in these functions.  *)
    let check_destructor_translation = function
      | Some {control= {root_nodes}} ->
          assert (List.is_empty root_nodes)
      | None ->
          ()
    in
    let mk_ret_node instrs =
      let destr_trans_result = inject_destructors trans_state_pri stmt_info in
      check_destructor_translation destr_trans_result ;
      let is_destructor =
        match procname with
        | Typ.Procname.ObjC_Cpp cpp_pname ->
            Typ.Procname.ObjC_Cpp.is_destructor cpp_pname
        | _ ->
            false
      in
      let destructor_res =
        if is_destructor then
          cxx_inject_field_destructors_in_destructor_body trans_state_pri stmt_info
        else None
      in
      (* `cxx_inject_field_destructors_in_destructor_body` should not create new nodes for return statement,
          this is ensured by creating a fresh pointer in `cxx_inject_field_destructors_in_destructor_body`
      *)
      check_destructor_translation destructor_res ;
      let instrs_of = function Some {control= {instrs}} -> instrs | None -> [] in
      let ret_node =
        Procdesc.create_node context.procdesc sil_loc (Procdesc.Node.Stmt_node ReturnStmt)
          (instrs @ instrs_of destr_trans_result @ instrs_of destructor_res)
      in
      Procdesc.node_set_succs_exn context.procdesc ret_node
        [Procdesc.get_exit_node context.CContext.procdesc]
        [] ;
      ret_node
    in
    let trans_result =
      match stmt_list with
      | [stmt] ->
          (* return exp; *)
          let ret_type = Procdesc.get_ret_type procdesc in
          let ret_exp, ret_typ, var_instrs =
            match context.CContext.return_param_typ with
            | Some ret_param_typ ->
                let name = CFrontend_config.return_param in
                let pvar = Pvar.mk (Mangled.from_string name) procname in
                let id = Ident.create_fresh Ident.knormal in
                let instr = Sil.Load (id, Exp.Lvar pvar, ret_param_typ, sil_loc) in
                let ret_typ =
                  match ret_param_typ.desc with Typ.Tptr (t, _) -> t | _ -> assert false
                in
                (Exp.Var id, ret_typ, [instr])
            | None ->
                (Exp.Lvar (Procdesc.get_ret_var procdesc), ret_type, [])
          in
          let trans_state' =
            {trans_state_pri with succ_nodes= []; var_exp_typ= Some (ret_exp, ret_typ)}
          in
          let res_trans_stmt = instruction trans_state' stmt in
          let ret_instrs =
            if List.exists ~f:(Exp.equal ret_exp) res_trans_stmt.control.initd_exps then []
            else
              let sil_expr, _ = res_trans_stmt.return in
              [Sil.Store (ret_exp, ret_type, sil_expr, sil_loc)]
          in
          let instrs = var_instrs @ res_trans_stmt.control.instrs @ ret_instrs in
          let ret_node = mk_ret_node instrs in
          List.iter
            ~f:(fun n -> Procdesc.node_set_succs_exn procdesc n [ret_node] [])
            res_trans_stmt.control.leaf_nodes ;
          let root_nodes_to_parent =
            if List.length res_trans_stmt.control.root_nodes > 0 then
              res_trans_stmt.control.root_nodes
            else [ret_node]
          in
          mk_trans_result res_trans_stmt.return
            {empty_control with root_nodes= root_nodes_to_parent}
      | [] ->
          (* return; *)
          let ret_node = mk_ret_node [] in
          mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= [ret_node]}
      | _ ->
          assert false
    in
    (* We expect a return with only one expression *)
    trans_result


  (** We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. It may be that
     later on (when we treat ARC) some info can be taken from it.  For ParenExpression we translate
     its body composed by the stmt_list. In paren expression there should be only one stmt that
     defines the expression *)
  and parenExpr_trans trans_state source_range stmt_list =
    let stmt =
      extract_stmt_from_singleton stmt_list source_range
        "WARNING: In ParenExpression there should be only one stmt."
    in
    instruction trans_state stmt


  and objCBoxedExpr_trans trans_state info sel stmt_info stmts =
    let typ =
      CType_decl.class_from_pointer_type trans_state.context.CContext.tenv
        info.Clang_ast_t.ei_qual_type
    in
    let obj_c_message_expr_info =
      Ast_expressions.make_obj_c_message_expr_info_class sel typ None
    in
    let message_stmt =
      Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_message_expr_info)
    in
    instruction trans_state message_stmt


  and objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer =
    let open Clang_ast_t in
    match CAst_utils.get_decl_opt method_pointer with
    | Some (ObjCMethodDecl (decl_info, named_decl_info, _)) ->
        let typ =
          CAst_utils.qual_type_of_decl_ptr (Option.value_exn decl_info.di_parent_pointer)
        in
        let obj_c_mes_expr_info =
          { Clang_ast_t.omei_selector= named_decl_info.Clang_ast_t.ni_name
          ; omei_receiver_kind= `Class typ
          ; omei_is_definition_found= true
          ; omei_decl_pointer= method_pointer }
        in
        let message_stmt =
          Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, expr_info, obj_c_mes_expr_info)
        in
        instruction trans_state message_stmt
    | _ ->
        Logging.die InternalError
          "The method for translating array/dictionary literals is not available at %s"
          (Clang_ast_j.string_of_stmt_info stmt_info)


  and objCArrayLiteral_trans trans_state expr_info stmt_info stmts array_literal_info =
    let method_pointer = array_literal_info.Clang_ast_t.oalei_array_method in
    let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
    objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer


  and objCDictionaryLiteral_trans trans_state expr_info stmt_info stmts dict_literal_info =
    let method_pointer = dict_literal_info.Clang_ast_t.odlei_dict_method in
    let stmts = CGeneral_utils.swap_elements_list stmts in
    let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
    objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer


  and objCStringLiteral_trans trans_state stmt_info stmts info =
    let char_star_typ =
      Ast_expressions.create_char_star_type ~quals:(Typ.mk_type_quals ~is_const:true ()) ()
    in
    let stmts =
      [Ast_expressions.create_implicit_cast_expr stmt_info stmts char_star_typ `ArrayToPointerDecay]
    in
    let typ =
      CType_decl.class_from_pointer_type trans_state.context.CContext.tenv
        info.Clang_ast_t.ei_qual_type
    in
    let meth = CFrontend_config.string_with_utf8_m in
    let obj_c_mess_expr_info = Ast_expressions.make_obj_c_message_expr_info_class meth typ None in
    let message_stmt =
      Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_mess_expr_info)
    in
    instruction trans_state message_stmt


  (* Assumption: stmt_list contains 2 items, the first can be ObjCMessageExpr or ParenExpr *)
  (* We ignore this item since we don't deal with the concurrency problem yet *)
  (* For the same reason we also ignore the stmt_info that
     is related with the ObjCAtSynchronizedStmt construct *)
  (* Finally we recursively work on the CompoundStmt, the second item of stmt_list *)
  and objCAtSynchronizedStmt_trans trans_state stmt_list =
    match stmt_list with
    | [_; compound_stmt] ->
        instruction trans_state compound_stmt
    | _ ->
        assert false


  and blockExpr_trans trans_state stmt_info expr_info decl =
    let context = trans_state.context in
    let outer_proc = Procdesc.get_proc_name context.CContext.procdesc in
    match decl with
    | Clang_ast_t.BlockDecl (_, block_decl_info) ->
        let open CContext in
        let block_return_type = expr_info.Clang_ast_t.ei_qual_type in
        let block_pname =
          CType_decl.CProcname.from_decl decl ~tenv:context.tenv ~block_return_type ~outer_proc
        in
        let captured_pvars =
          CVar_decl.captured_vars_from_block_info context stmt_info.Clang_ast_t.si_source_range
            block_decl_info.Clang_ast_t.bdi_captured_variables
        in
        let res = closure_trans block_pname captured_pvars context stmt_info expr_info in
        let block_data = Some (context, block_return_type, block_pname, captured_pvars) in
        F.function_decl context.translation_unit_context context.tenv context.cfg decl block_data ;
        res
    | _ ->
        (* Block expression with no BlockDecl *)
        assert false


  and lambdaExpr_trans trans_state stmt_info expr_info {Clang_ast_t.lei_lambda_decl} =
    let open CContext in
    let qual_type = expr_info.Clang_ast_t.ei_qual_type in
    let context = trans_state.context in
    call_translation context lei_lambda_decl ;
    let procname = Procdesc.get_proc_name context.procdesc in
    let lambda_pname = CMethod_trans.get_procname_from_cpp_lambda context lei_lambda_decl in
    let typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let get_captured_pvar_typ decl_ref =
      CVar_decl.sil_var_of_captured_var context stmt_info.Clang_ast_t.si_source_range procname
        decl_ref
    in
    let translate_capture_init (pvar, typ) init_decl =
      match init_decl with
      | Clang_ast_t.VarDecl (_, _, _, {vdi_init_expr}) ->
          init_expr_trans trans_state (Exp.Lvar pvar, typ) stmt_info vdi_init_expr
      | _ ->
          CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Capture-init statement without var decl"
    in
    let translate_normal_capture ~is_by_ref ((pvar, typ) as pvar_typ)
        (trans_results_acc, captured_vars_acc) =
      let loc =
        CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
      in
      if is_by_ref then (trans_results_acc, (Exp.Lvar pvar, pvar, typ) :: captured_vars_acc)
      else
        let ((exp, _, typ) as exp_pvar_typ), instr = assign_captured_var loc pvar_typ in
        let trans_results = mk_trans_result (exp, typ) {empty_control with instrs= [instr]} in
        (trans_results :: trans_results_acc, exp_pvar_typ :: captured_vars_acc)
    in
    let translate_captured
        { Clang_ast_t.lci_captured_var
        ; lci_init_captured_vardecl
        ; lci_capture_this
        ; lci_capture_kind } ((trans_results_acc, captured_vars_acc) as acc) =
      let is_by_ref =
        (* see http://en.cppreference.com/w/cpp/language/lambda *)
        match lci_capture_kind with
        | `LCK_ByRef (* explicit with [&x] or implicit with [&] *)
        | `LCK_This (* explicit with [this] or implicit with [&] *)
        | `LCK_VLAType
        (* capture a variable-length array by reference. we probably don't handle
                            this correctly elsewhere, but it's definitely not captured by value! *)
          ->
            true
        | `LCK_ByCopy (* explicit with [x] or implicit with [=] *) ->
            (* [=] captures this by reference and everything else by value *)
            lci_capture_this
        | `LCK_StarThis (* [*this] is special syntax for capturing current object by value *) ->
            false
      in
      match (lci_captured_var, lci_init_captured_vardecl) with
      | Some captured_var_decl_ref, Some init_decl -> (
        (* capture and init *)
        match get_captured_pvar_typ captured_var_decl_ref with
        | Some pvar_typ ->
            ( translate_capture_init pvar_typ init_decl :: trans_results_acc
            , (Exp.Lvar (fst pvar_typ), fst pvar_typ, snd pvar_typ) :: captured_vars_acc )
        | None ->
            (trans_results_acc, captured_vars_acc) )
      | Some captured_var_decl_ref, None -> (
        (* just capture *)
        match get_captured_pvar_typ captured_var_decl_ref with
        | Some pvar_typ ->
            translate_normal_capture ~is_by_ref pvar_typ acc
        | None ->
            (trans_results_acc, captured_vars_acc) )
      | None, None ->
          if lci_capture_this then
            (* captured [this] *)
            let this_typ = get_this_pvar_typ stmt_info context in
            translate_normal_capture ~is_by_ref this_typ acc
          else acc
      | None, Some _ ->
          CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Capture-init with init, but no capture"
    in
    let lei_captures = CMethod_trans.get_captures_from_cpp_lambda lei_lambda_decl in
    let trans_results, captured_vars =
      List.fold_right ~f:translate_captured ~init:([], []) lei_captures
    in
    let closure = Exp.Closure {name= lambda_pname; captured_vars} in
    collect_trans_results context.procdesc ~return:(closure, typ) trans_results


  and cxxNewExpr_trans trans_state stmt_info expr_info cxx_new_expr_info =
    let context = trans_state.context in
    let typ = CType_decl.get_type_from_expr_info expr_info context.tenv in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let is_dyn_array = cxx_new_expr_info.Clang_ast_t.xnei_is_array in
    let source_range = stmt_info.Clang_ast_t.si_source_range in
    let size_exp_opt, control_size =
      if is_dyn_array then
        match
          CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_array_size_expr source_range
        with
        | Some stmt ->
            let trans_state_size = {trans_state_pri with succ_nodes= []} in
            let res_trans_size = instruction trans_state_size stmt in
            (Some (fst res_trans_size.return), Some res_trans_size.control)
        | None ->
            (Some (Exp.Const (Const.Cint IntLit.minus_one)), None)
      else (None, None)
    in
    let placement_args =
      List.filter_map
        ~f:(fun i -> CAst_utils.get_stmt i source_range)
        cxx_new_expr_info.Clang_ast_t.xnei_placement_args
    in
    let trans_state_placement = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let res_trans_placement_control, res_trans_placement_exps =
      instructions trans_state_placement placement_args
    in
    let res_trans_new =
      cpp_new_trans context.translation_unit_context.integer_type_widths sil_loc typ size_exp_opt
        res_trans_placement_exps
    in
    let stmt_opt =
      CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_initializer_expr source_range
    in
    let trans_state_init = {trans_state_pri with succ_nodes= []} in
    let var_exp_typ =
      match res_trans_new.return with
      | var_exp, ({desc= Tptr (t, _)} as var_typ) when is_dyn_array ->
          (* represent dynamic array as Tarray *)
          (var_exp, Typ.mk_array ~default:var_typ t)
      | var_exp, {desc= Tptr (t, _)} when not is_dyn_array ->
          (var_exp, t)
      | _ ->
          assert false
    in
    (* Need a new stmt_info for the translation of the initializer, so that it can create nodes *)
    (* if it needs to, with the same stmt_info it doesn't work. *)
    let init_stmt_info =
      {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
    in
    let res_trans_init =
      match stmt_opt with
      | Some (InitListExpr _) ->
          [init_expr_trans trans_state_init var_exp_typ init_stmt_info stmt_opt]
      | _ when is_dyn_array && Typ.is_pointer_to_cpp_class typ ->
          (* NOTE: this is heuristic to initialize C++ objects when the size of dynamic
             array is constant, it doesn't do anything for non-const lengths, it should be translated as a loop *)
          let rec create_stmts stmt_opt size_exp_opt =
            match (stmt_opt, size_exp_opt) with
            | Some stmt, Some (Exp.Const (Const.Cint n)) when not (IntLit.iszero n) ->
                let n_minus_1 = Some (Exp.Const (Const.Cint (IntLit.sub n IntLit.one))) in
                stmt :: create_stmts stmt_opt n_minus_1
            | _ ->
                []
          in
          let stmts = create_stmts stmt_opt size_exp_opt in
          let var_exp, var_typ = var_exp_typ in
          initListExpr_array_trans trans_state_init init_stmt_info stmts var_exp var_typ
      | _ ->
          [init_expr_trans trans_state_init var_exp_typ init_stmt_info stmt_opt]
    in
    let all_res_trans =
      Option.to_list control_size
      @ [res_trans_placement_control; res_trans_new.control]
      @ List.map ~f:(fun {control} -> control) res_trans_init
    in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name:CXXNewExpr stmt_info
      all_res_trans
    |> mk_trans_result res_trans_new.return


  and cxxDeleteExpr_trans trans_state stmt_info stmt_list delete_expr_info =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let is_array = delete_expr_info.Clang_ast_t.xdei_is_array in
    let fname = if is_array then BuiltinDecl.__delete_array else BuiltinDecl.__delete in
    let param = match stmt_list with [p] -> p | _ -> assert false in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = {trans_state_pri with succ_nodes= []} in
    let result_trans_param = instruction trans_state_param param in
    let ret_id_typ, call_return = mk_fresh_void_return () in
    let call_instr =
      Sil.Call
        ( ret_id_typ
        , Exp.Const (Const.Cfun fname)
        , [result_trans_param.return]
        , sil_loc
        , CallFlags.default )
    in
    let call_res_trans = mk_trans_result call_return {empty_control with instrs= [call_instr]} in
    let all_res_trans =
      if false then
        (* FIXME (t10135167): call destructor on deleted pointer if it's not null *)
        (* Right now it's dead code hidden by the 'false' flag *)
        let deleted_type = delete_expr_info.Clang_ast_t.xdei_destroyed_type in
        (* create stmt_info with new pointer so that destructor call doesn't create a node *)
        let destruct_stmt_info =
          {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
        in
        (* use mk_trans_result to avoid ending up with same instruction twice *)
        (* otherwise it would happen due to structutre of all_res_trans *)
        let this_res_trans_destruct = mk_trans_result result_trans_param.return empty_control in
        let destruct_res_trans =
          cxx_destructor_call_trans trans_state_pri destruct_stmt_info this_res_trans_destruct
            deleted_type.Clang_ast_t.qt_type_ptr ~is_inner_destructor:false
        in
        result_trans_param :: (Option.to_list destruct_res_trans @ [call_res_trans])
        (* --- END OF DEAD CODE --- *)
      else [result_trans_param; call_res_trans]
    in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:(Call "delete")
      stmt_info ~return:call_return all_res_trans


  and materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    (* typ_tmp is 'best guess' type of variable - translation may decide to use different type
       later *)
    let pvar, typ_tmp =
      mk_temp_sil_var_for_expr context.CContext.tenv procdesc Pvar.materialized_cpp_temporary
        expr_info
    in
    let temp_exp = match stmt_list with [p] -> p | _ -> assert false in
    let var_exp_typ = (Exp.Lvar pvar, typ_tmp) in
    let res_trans = init_expr_trans trans_state var_exp_typ stmt_info (Some temp_exp) in
    let _, typ = res_trans.return in
    let var_data =
      ProcAttributes.{name= Pvar.get_name pvar; typ; modify_in_block= false; is_constexpr= false}
    in
    Procdesc.append_locals procdesc [var_data] ;
    res_trans


  and compoundLiteralExpr_trans trans_state stmt_list expr_info =
    let stmt = match stmt_list with [stmt] -> stmt | _ -> assert false in
    let var_exp_typ =
      if Option.is_some trans_state.var_exp_typ then trans_state.var_exp_typ
      else Some (create_var_exp_tmp_var trans_state expr_info "SIL_compound_literal__")
    in
    let trans_state' = {trans_state with var_exp_typ} in
    instruction trans_state' stmt


  and cxxDynamicCastExpr_trans trans_state stmt_info stmts cast_qual_type =
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state' = {trans_state_pri with succ_nodes= []} in
    let context = trans_state.context in
    let subtype = Subtype.subtypes_cast in
    let tenv = context.CContext.tenv in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let cast_type = CType_decl.qual_type_to_sil_type tenv cast_qual_type in
    let sizeof_expr =
      match cast_type.desc with
      | Typ.Tptr (typ, _) ->
          Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype}
      | _ ->
          assert false
    in
    let builtin = Exp.Const (Const.Cfun BuiltinDecl.__cast) in
    let stmt = match stmts with [stmt] -> stmt | _ -> assert false in
    let res_trans_stmt = exec_with_glvalue_as_reference instruction trans_state' stmt in
    let exp = res_trans_stmt.return in
    let args = [exp; (sizeof_expr, Typ.mk Tvoid)] in
    let ret_id = Ident.create_fresh Ident.knormal in
    let call = Sil.Call ((ret_id, cast_type), builtin, args, sil_loc, CallFlags.default) in
    let res_ex = Exp.Var ret_id in
    let res_trans_dynamic_cast = {empty_control with instrs= [call]} in
    let all_res_trans = [res_trans_stmt.control; res_trans_dynamic_cast] in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name:CXXDynamicCast
      stmt_info all_res_trans
    |> mk_trans_result (res_ex, cast_type)


  and cxxDefaultExpr_trans trans_state default_expr_info =
    match default_expr_info.Clang_ast_t.xdaei_init_expr with
    | Some exp ->
        instruction trans_state exp
    | None ->
        assert false


  and call_function_with_args instr_name pname trans_state stmt_info ret_typ stmts =
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = {trans_state_pri with succ_nodes= []} in
    let res_trans_subexpr_list =
      List.map ~f:(exec_with_glvalue_as_reference instruction trans_state_param) stmts
    in
    let params = collect_returns res_trans_subexpr_list in
    let sil_fun = Exp.Const (Const.Cfun pname) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let call_instr = Sil.Call ((ret_id, ret_typ), sil_fun, params, sil_loc, CallFlags.default) in
    let res_trans_call =
      mk_trans_result (Exp.Var ret_id, ret_typ) {empty_control with instrs= [call_instr]}
    in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~node_name:instr_name stmt_info
      ~return:res_trans_call.return all_res_trans


  and gccAsmStmt_trans trans_state stmt_info stmts =
    let pname = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_gcc_asm_stmt in
    call_function_with_args Procdesc.Node.GCCAsmStmt pname trans_state stmt_info (Typ.mk Tvoid)
      stmts


  and genericSelectionExprUnknown_trans trans_state stmt_info stmts =
    let pname = Typ.Procname.from_string_c_fun CFrontend_config.infer_generic_selection_expr in
    call_function_with_args Procdesc.Node.GenericSelectionExpr pname trans_state stmt_info
      (Typ.mk Tvoid) stmts


  and objc_cxx_throw_trans trans_state stmt_info stmts =
    call_function_with_args Procdesc.Node.ObjCCPPThrow BuiltinDecl.objc_cpp_throw trans_state
      stmt_info (Typ.mk Tvoid) stmts


  and cxxPseudoDestructorExpr_trans () =
    let fun_name = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_fun in
    mk_trans_result (Exp.Const (Const.Cfun fun_name), Typ.mk Tvoid) empty_control


  and cxxTypeidExpr_trans trans_state stmt_info stmts expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let res_trans_subexpr =
      match stmts with
      | [stmt] ->
          let trans_state_param = {trans_state_pri with succ_nodes= []} in
          Some (instruction trans_state_param stmt)
      | _ ->
          None
    in
    let fun_name = BuiltinDecl.__cxx_typeid in
    let sil_fun = Exp.Const (Const.Cfun fun_name) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let void_typ = Typ.mk Tvoid in
    let type_info_objc =
      (Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact}, void_typ)
    in
    let class_tname =
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate (QualifiedCppName.of_list ["std"; "type_info"])
    in
    let field_name = CGeneral_utils.mk_class_field_name class_tname "__type_name" in
    let ret_exp = Exp.Var ret_id in
    let field_exp = Exp.Lfield (ret_exp, field_name, typ) in
    let args =
      type_info_objc :: (field_exp, void_typ)
      :: Option.value_map ~default:[] res_trans_subexpr ~f:(fun trans_result ->
             [trans_result.return] )
    in
    let call_instr = Sil.Call ((ret_id, typ), sil_fun, args, sil_loc, CallFlags.default) in
    let res_control = {empty_control with instrs= [call_instr]} in
    let all_res_trans =
      match res_trans_subexpr with
      | Some trans_result ->
          [trans_result.control; res_control]
      | None ->
          [res_control]
    in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ~node_name:CXXTypeidExpr
      stmt_info all_res_trans
    |> mk_trans_result (ret_exp, typ)


  and cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info =
    let context = trans_state.context in
    let tenv = context.CContext.tenv in
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let typ = CType_decl.qual_type_to_sil_type tenv expr_info.Clang_ast_t.ei_qual_type in
    let fun_name = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_fun in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = {trans_state_pri with succ_nodes= []} in
    let res_trans_subexpr_list = List.map ~f:(instruction trans_state_param) stmts in
    let params = collect_returns res_trans_subexpr_list in
    let sil_fun = Exp.Const (Const.Cfun fun_name) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let ret_exp = Exp.Var ret_id in
    let call_instr = Sil.Call ((ret_id, typ), sil_fun, params, sil_loc, CallFlags.default) in
    let res_trans_call =
      mk_trans_result (ret_exp, typ) {empty_control with instrs= [call_instr]}
    in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc
      ~node_name:CXXStdInitializerListExpr stmt_info ~return:res_trans_call.return all_res_trans


  and binaryOperator_trans_with_cond trans_state stmt_info stmt_list expr_info binop_info =
    let open Clang_ast_t in
    match binop_info.boi_kind with
    | `LAnd | `LOr | `LT | `GT | `LE | `GE | `EQ | `NE ->
        (* For LAnd/LOr/comparison operators we compiles a binary expression bo into an semantic
           equivalent conditional operator 'bo ? 1:0'.
           The conditional operator takes care of shortcircuit when/where needed *)
        let bo = BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) in
        let cond = Ast_expressions.trans_with_conditional stmt_info expr_info [bo] in
        instruction trans_state cond
    | _ ->
        binaryOperator_trans trans_state binop_info stmt_info expr_info stmt_list


  and attributedStmt_trans trans_state stmt_info stmts attrs =
    let open Clang_ast_t in
    match (stmts, attrs) with
    | [stmt], [attr] -> (
      match (stmt, attr) with
      | NullStmt _, FallThroughAttr _ ->
          no_op_trans trans_state.succ_nodes
      | _ ->
          CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
            "attributedStmt [stmt] [attr] with:@\nstmt=%s@\nattr=%s@\n"
            (Clang_ast_j.string_of_stmt stmt)
            (Clang_ast_j.string_of_attribute attr) )
    | _ ->
        CFrontend_config.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "attributedStmt with:@\nstmts=[%a]@\nattrs=[%a]@\n"
          (Pp.semicolon_seq (Pp.to_string ~f:Clang_ast_j.string_of_stmt))
          stmts
          (Pp.semicolon_seq (Pp.to_string ~f:Clang_ast_j.string_of_attribute))
          attrs


  and breakStmt_trans trans_state stmt_info =
    match trans_state.continuation with
    | Some bn -> (
        let trans_state' = {trans_state with succ_nodes= bn.break} in
        match inject_destructors trans_state' stmt_info with
        | Some ({control= {root_nodes= _ :: _}} as destr_trans_result) ->
            {destr_trans_result with control= {destr_trans_result.control with leaf_nodes= []}}
        | Some {control= {root_nodes= []}} | None ->
            mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= bn.break} )
    | None (* t21762295 *) ->
        CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
          "Break stmt without continuation: %a"
          (Pp.to_string ~f:Clang_ast_j.string_of_stmt_info)
          stmt_info


  and continueStmt_trans trans_state stmt_info =
    match trans_state.continuation with
    | Some bn -> (
        let trans_state' = {trans_state with succ_nodes= bn.continue} in
        match inject_destructors trans_state' stmt_info with
        | Some ({control= {root_nodes= _ :: _}} as destr_trans_result) ->
            {destr_trans_result with control= {destr_trans_result.control with leaf_nodes= []}}
        | Some {control= {root_nodes= []}} | None ->
            mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= bn.continue}
        )
    | None (* t21762295 *) ->
        CFrontend_config.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
          "Continue stmt without continuation: %a"
          (Pp.to_string ~f:Clang_ast_j.string_of_stmt_info)
          stmt_info


  (* Expect that this doesn't happen *)
  and undefined_expr trans_state expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    mk_trans_result (CTrans_utils.undefined_expression (), typ) empty_control


  (** no-op translated for unsupported instructions that will at least translate subexpressions *)
  and skip_unimplemented ~reason trans_state stmt_info ret_typ stmts =
    call_function_with_args (Procdesc.Node.Skip reason) BuiltinDecl.__infer_skip trans_state
      stmt_info ret_typ stmts


  (** Translates a clang instruction into SIL instructions. It takes a a [trans_state] containing
      current info on the translation and it returns a [trans_result].*)
  and instruction =
    (* log errors only at the innermost recursive call *)
    let logged_error = ref false in
    fun trans_state instr ->
      let pp_pointer f instr =
        let {Clang_ast_t.si_pointer}, _ = Clang_ast_proj.get_stmt_tuple instr in
        Format.pp_print_int f si_pointer
      in
      L.(debug Capture Verbose)
        "Translating statement '%a' (pointer= '%a')@\n@[<hv2>"
        (Pp.to_string ~f:Clang_ast_proj.get_stmt_kind_string)
        instr pp_pointer instr ;
      let trans_result =
        try instruction_aux trans_state instr with e ->
          IExn.reraise_after e ~f:(fun () ->
              let should_log_error = not !logged_error in
              if should_log_error then (
                (* prevent from displaying the same issue multiple times as we climb up the call
                   stack *)
                logged_error := true ;
                let {Clang_ast_t.si_source_range}, _ = Clang_ast_proj.get_stmt_tuple instr in
                let source_file =
                  trans_state.context.CContext.translation_unit_context
                    .CFrontend_config.source_file
                in
                let loc_start =
                  CLocation.location_of_source_range ~pick_location:`Start source_file
                    si_source_range
                in
                let loc_end =
                  CLocation.location_of_source_range ~pick_location:`End source_file
                    si_source_range
                in
                (* Unfortunately this triggers regularly so do not show the message on the console
                   unless asked to do so or if the error will crash the frontend. *)
                let should_display_error =
                  Config.debug_level_capture >= 1
                  ||
                  match e with
                  | CFrontend_config.Unimplemented _ | CFrontend_config.IncorrectAssumption _ ->
                      (* these are caught by default, do not print messages unless asked to do so *)
                      false
                  | _ ->
                      (* we are going to crash, print message for more context *)
                      true
                in
                (if should_display_error then L.internal_error else L.debug Capture Quiet)
                  "%a: ERROR translating statement '%a'@\n" Location.pp_range (loc_start, loc_end)
                  (Pp.to_string ~f:Clang_ast_proj.get_stmt_kind_string)
                  instr ) )
      in
      L.(debug Capture Verbose) "@]" ;
      (* don't forget to reset this so we output messages for future errors too *)
      logged_error := false ;
      trans_result


  and instruction_aux trans_state (instr : Clang_ast_t.stmt) =
    match instr with
    | GotoStmt (stmt_info, _, {Clang_ast_t.gsi_label= label_name; _}) ->
        gotoStmt_trans trans_state stmt_info label_name
    | LabelStmt (stmt_info, stmt_list, label_name) ->
        labelStmt_trans trans_state stmt_info stmt_list label_name
    | ArraySubscriptExpr (_, stmt_list, expr_info) ->
        arraySubscriptExpr_trans trans_state expr_info stmt_list
    | BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) ->
        binaryOperator_trans_with_cond trans_state stmt_info stmt_list expr_info binop_info
    | CallExpr (stmt_info, stmt_list, ei) | UserDefinedLiteral (stmt_info, stmt_list, ei) ->
        callExpr_trans trans_state stmt_info stmt_list ei
    | CXXMemberCallExpr (stmt_info, stmt_list, ei) ->
        cxxMemberCallExpr_trans trans_state stmt_info stmt_list ei
    | CXXOperatorCallExpr (stmt_info, stmt_list, ei) ->
        callExpr_trans trans_state stmt_info stmt_list ei
    | CXXConstructExpr (stmt_info, stmt_list, expr_info, cxx_constr_info)
    | CXXTemporaryObjectExpr (stmt_info, stmt_list, expr_info, cxx_constr_info) ->
        cxxConstructExpr_trans trans_state stmt_info stmt_list expr_info cxx_constr_info
          ~is_inherited_ctor:false
    | CXXInheritedCtorInitExpr (stmt_info, stmt_list, expr_info, cxx_construct_inherited_expr_info)
      ->
        cxxConstructExpr_trans trans_state stmt_info stmt_list expr_info
          cxx_construct_inherited_expr_info ~is_inherited_ctor:true
    | ObjCMessageExpr (stmt_info, stmt_list, expr_info, obj_c_message_expr_info) ->
        objCMessageExpr_trans trans_state stmt_info obj_c_message_expr_info stmt_list expr_info
    | CompoundStmt (stmt_info, stmt_list) ->
        (* No node for this statement. We just collect its statement list*)
        compoundStmt_trans trans_state stmt_info stmt_list
    | ConditionalOperator (stmt_info, stmt_list, expr_info) ->
        (* Ternary operator "cond ? exp1 : exp2" *)
        conditionalOperator_trans trans_state stmt_info stmt_list expr_info
    | IfStmt (stmt_info, stmt_list) ->
        ifStmt_trans trans_state stmt_info stmt_list
    | SwitchStmt (stmt_info, switch_stmt_list) ->
        switchStmt_trans trans_state stmt_info switch_stmt_list
    | CaseStmt (stmt_info, stmt_list) ->
        caseStmt_trans trans_state stmt_info stmt_list
    | DefaultStmt (stmt_info, stmt_list) ->
        defaultStmt_trans trans_state stmt_info stmt_list
    | StmtExpr ({Clang_ast_t.si_source_range}, stmt_list, _) ->
        stmtExpr_trans trans_state si_source_range stmt_list
    | ForStmt (stmt_info, [init; decl_stmt; condition; increment; body]) ->
        forStmt_trans trans_state ~init ~decl_stmt ~condition ~increment ~body stmt_info
    | WhileStmt (stmt_info, [decl_stmt; condition; body]) ->
        whileStmt_trans trans_state ~decl_stmt ~condition ~body stmt_info
    | DoStmt (stmt_info, [body; condition]) ->
        doStmt_trans trans_state ~condition ~body stmt_info
    | CXXForRangeStmt (stmt_info, stmt_list) ->
        cxxForRangeStmt_trans trans_state stmt_info stmt_list
    | ObjCForCollectionStmt (stmt_info, [item; items; body]) ->
        objCForCollectionStmt_trans trans_state item items body stmt_info
    | NullStmt _ ->
        no_op_trans trans_state.succ_nodes
    | CompoundAssignOperator (stmt_info, stmt_list, expr_info, binary_operator_info, _) ->
        binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list
    | DeclStmt (stmt_info, _, decl_list) ->
        declStmt_trans trans_state decl_list stmt_info
    | DeclRefExpr (stmt_info, _, _, decl_ref_expr_info) ->
        declRefExpr_trans trans_state stmt_info decl_ref_expr_info
    | ObjCPropertyRefExpr (_, stmt_list, _, _) ->
        objCPropertyRefExpr_trans trans_state stmt_list
    | CXXThisExpr (stmt_info, _, expr_info) ->
        cxxThisExpr_trans trans_state stmt_info expr_info
    | OpaqueValueExpr (stmt_info, _, _, opaque_value_expr_info) ->
        opaqueValueExpr_trans trans_state opaque_value_expr_info
          stmt_info.Clang_ast_t.si_source_range
    | PseudoObjectExpr (_, stmt_list, _) ->
        pseudoObjectExpr_trans trans_state stmt_list
    | UnaryExprOrTypeTraitExpr (_, _, _, unary_expr_or_type_trait_expr_info) ->
        unaryExprOrTypeTraitExpr_trans trans_state unary_expr_or_type_trait_expr_info
    | ObjCBridgedCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
    | ImplicitCastExpr (stmt_info, stmt_list, expr_info, cast_kind)
    | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
    | CXXReinterpretCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXConstCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXStaticCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXFunctionalCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) ->
        cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind
    | IntegerLiteral (_, _, expr_info, integer_literal_info) ->
        integerLiteral_trans trans_state expr_info integer_literal_info
    | StringLiteral (_, _, expr_info, str_list) ->
        stringLiteral_trans trans_state expr_info (String.concat ~sep:"" str_list)
    | GNUNullExpr (_, _, expr_info) ->
        gNUNullExpr_trans trans_state expr_info
    | CXXNullPtrLiteralExpr (_, _, expr_info) ->
        nullPtrExpr_trans trans_state expr_info
    | ObjCSelectorExpr (_, _, expr_info, selector) ->
        objCSelectorExpr_trans trans_state expr_info selector
    | ObjCEncodeExpr (_, _, expr_info, objc_encode_expr_info) ->
        objCEncodeExpr_trans trans_state expr_info objc_encode_expr_info
    | ObjCProtocolExpr (_, _, expr_info, decl_ref) ->
        objCProtocolExpr_trans trans_state expr_info decl_ref
    | ObjCIvarRefExpr (stmt_info, stmt_list, _, obj_c_ivar_ref_expr_info) ->
        objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info
    | MemberExpr (stmt_info, stmt_list, _, member_expr_info) ->
        memberExpr_trans trans_state stmt_info stmt_list member_expr_info
    | UnaryOperator (stmt_info, stmt_list, expr_info, unary_operator_info) ->
        if
          is_logical_negation_of_int trans_state.context.CContext.tenv expr_info
            unary_operator_info
        then
          let conditional =
            Ast_expressions.trans_negation_with_conditional stmt_info expr_info stmt_list
          in
          instruction trans_state conditional
        else unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info
    | ReturnStmt (stmt_info, stmt_list) ->
        returnStmt_trans trans_state stmt_info stmt_list
    (* We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. *)
    (*  It may be that later on (when we treat ARC) some info can be taken from it. *)
    | ExprWithCleanups ({Clang_ast_t.si_source_range}, stmt_list, _, _)
    | ParenExpr ({Clang_ast_t.si_source_range}, stmt_list, _) ->
        parenExpr_trans trans_state si_source_range stmt_list
    | ObjCBoolLiteralExpr (_, _, expr_info, n)
    | CharacterLiteral (_, _, expr_info, n)
    | CXXBoolLiteralExpr (_, _, expr_info, n) ->
        characterLiteral_trans trans_state expr_info n
    | FixedPointLiteral (_, _, expr_info, float_string)
    | FloatingLiteral (_, _, expr_info, float_string) ->
        floatingLiteral_trans trans_state expr_info float_string
    | CXXScalarValueInitExpr (_, _, expr_info) ->
        cxxScalarValueInitExpr_trans trans_state expr_info
    | ObjCBoxedExpr (stmt_info, stmts, info, boxed_expr_info) -> (
      match boxed_expr_info.Clang_ast_t.obei_boxing_method with
      | Some sel ->
          objCBoxedExpr_trans trans_state info sel stmt_info stmts
      | None ->
          assert false )
    | ObjCArrayLiteral (stmt_info, stmts, expr_info, array_literal_info) ->
        objCArrayLiteral_trans trans_state expr_info stmt_info stmts array_literal_info
    | ObjCDictionaryLiteral (stmt_info, stmts, expr_info, dict_literal_info) ->
        objCDictionaryLiteral_trans trans_state expr_info stmt_info stmts dict_literal_info
    | ObjCStringLiteral (stmt_info, stmts, info) ->
        objCStringLiteral_trans trans_state stmt_info stmts info
    | BreakStmt (stmt_info, _) ->
        breakStmt_trans trans_state stmt_info
    | ContinueStmt (stmt_info, _) ->
        continueStmt_trans trans_state stmt_info
    | ObjCAtSynchronizedStmt (_, stmt_list) ->
        objCAtSynchronizedStmt_trans trans_state stmt_list
    | ObjCIndirectCopyRestoreExpr (_, stmt_list, _) ->
        let control, returns = instructions trans_state stmt_list in
        mk_trans_result (last_or_mk_fresh_void_exp_typ returns) control
    | BlockExpr (stmt_info, _, expr_info, decl) ->
        blockExpr_trans trans_state stmt_info expr_info decl
    | ObjCAutoreleasePoolStmt (stmt_info, stmts) ->
        compoundStmt_trans trans_state stmt_info stmts
    | ObjCAtTryStmt (stmt_info, stmts) ->
        compoundStmt_trans trans_state stmt_info stmts
    | CXXTryStmt (_, try_stmts) ->
        tryStmt_trans trans_state try_stmts
    | CXXCatchStmt _ ->
        (* should by handled by try statement *)
        assert false
    | ObjCAtThrowStmt (stmt_info, stmts) | CXXThrowExpr (stmt_info, stmts, _) ->
        objc_cxx_throw_trans trans_state stmt_info stmts
    | ObjCAtFinallyStmt (stmt_info, stmts) ->
        compoundStmt_trans trans_state stmt_info stmts
    | ObjCAtCatchStmt (stmt_info, _, _) ->
        compoundStmt_trans trans_state stmt_info []
    | PredefinedExpr (_, _, expr_info, _) ->
        stringLiteral_trans trans_state expr_info ""
    | BinaryConditionalOperator (stmt_info, stmts, expr_info) ->
        binaryConditionalOperator_trans trans_state stmt_info stmts expr_info
    | CXXNewExpr (stmt_info, _, expr_info, cxx_new_expr_info) ->
        cxxNewExpr_trans trans_state stmt_info expr_info cxx_new_expr_info
    | CXXDeleteExpr (stmt_info, stmt_list, _, delete_expr_info) ->
        cxxDeleteExpr_trans trans_state stmt_info stmt_list delete_expr_info
    | MaterializeTemporaryExpr (stmt_info, stmt_list, expr_info, _) ->
        materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info
    | CompoundLiteralExpr (_, stmt_list, expr_info) ->
        compoundLiteralExpr_trans trans_state stmt_list expr_info
    | InitListExpr (stmt_info, stmts, expr_info) ->
        initListExpr_trans trans_state stmt_info expr_info stmts
    | CXXBindTemporaryExpr ({Clang_ast_t.si_source_range}, stmt_list, _, _) ->
        (* right now we ignore this expression and try to translate the child node *)
        parenExpr_trans trans_state si_source_range stmt_list
    | CXXDynamicCastExpr (stmt_info, stmts, _, _, qual_type, _) ->
        cxxDynamicCastExpr_trans trans_state stmt_info stmts qual_type
    | CXXDefaultArgExpr (_, _, _, default_expr_info)
    | CXXDefaultInitExpr (_, _, _, default_expr_info) ->
        cxxDefaultExpr_trans trans_state default_expr_info
    | ImplicitValueInitExpr (stmt_info, _, _) ->
        implicitValueInitExpr_trans trans_state stmt_info
    | GenericSelectionExpr (stmt_info, stmts, _, gse_info) -> (
      match gse_info.gse_value with
      | Some value ->
          instruction trans_state value
      | None ->
          genericSelectionExprUnknown_trans trans_state stmt_info stmts )
    | SizeOfPackExpr _ ->
        mk_trans_result (Exp.get_undefined false, Typ.mk Tvoid) empty_control
    | GCCAsmStmt (stmt_info, stmts) ->
        gccAsmStmt_trans trans_state stmt_info stmts
    | CXXPseudoDestructorExpr _ ->
        cxxPseudoDestructorExpr_trans ()
    | CXXTypeidExpr (stmt_info, stmts, expr_info) ->
        cxxTypeidExpr_trans trans_state stmt_info stmts expr_info
    | CXXStdInitializerListExpr (stmt_info, stmts, expr_info) ->
        cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info
    | LambdaExpr (stmt_info, _, expr_info, lambda_expr_info) ->
        let trans_state' = {trans_state with priority= Free} in
        lambdaExpr_trans trans_state' stmt_info expr_info lambda_expr_info
    | AttributedStmt (stmt_info, stmts, attrs) ->
        attributedStmt_trans trans_state stmt_info stmts attrs
    | TypeTraitExpr (_, _, expr_info, type_trait_info) ->
        booleanValue_trans trans_state expr_info type_trait_info.Clang_ast_t.xtti_value
    | CXXNoexceptExpr (_, _, expr_info, cxx_noexcept_expr_info) ->
        booleanValue_trans trans_state expr_info cxx_noexcept_expr_info.Clang_ast_t.xnee_value
    | OffsetOfExpr (_, _, expr_info) | VAArgExpr (_, _, expr_info) ->
        undefined_expr trans_state expr_info
    | ArrayInitIndexExpr _ | ArrayInitLoopExpr _ ->
        no_op_trans trans_state.succ_nodes
    (* vector instructions for OpenCL etc. we basically ignore these for now; just translate the
       sub-expressions *)
    | ObjCAvailabilityCheckExpr (_, _, expr_info, _) ->
        undefined_expr trans_state expr_info
    | SubstNonTypeTemplateParmExpr (_, stmts, _) | SubstNonTypeTemplateParmPackExpr (_, stmts, _)
      ->
        let[@warning "-8"] [expr] = stmts in
        instruction trans_state expr
    (* Infer somehow ended up in templated non instantiated code - right now
       it's not supported and failure in those cases is expected. *)
    | CXXDependentScopeMemberExpr ({Clang_ast_t.si_source_range}, _, _) ->
        CFrontend_config.unimplemented __POS__ si_source_range
          ~ast_node:(Clang_ast_proj.get_stmt_kind_string instr)
          "Translation of templated code is unsupported: %a"
          (Pp.to_string ~f:Clang_ast_j.string_of_stmt)
          instr
    | ForStmt ({Clang_ast_t.si_source_range}, _)
    | WhileStmt ({Clang_ast_t.si_source_range}, _)
    | DoStmt ({Clang_ast_t.si_source_range}, _)
    | ObjCForCollectionStmt ({Clang_ast_t.si_source_range}, _) ->
        CFrontend_config.incorrect_assumption __POS__ si_source_range "Unexpected shape for %a: %a"
          (Pp.to_string ~f:Clang_ast_proj.get_stmt_kind_string)
          instr
          (Pp.to_string ~f:Clang_ast_j.string_of_stmt)
          instr
    | MSAsmStmt _
    | CapturedStmt _
    | CoreturnStmt _
    | CoroutineBodyStmt _
    | AddrLabelExpr _
    | ArrayTypeTraitExpr _
    | AsTypeExpr _
    | AtomicExpr _
    | CXXFoldExpr _
    | CXXUnresolvedConstructExpr _
    | CXXUuidofExpr _
    | CUDAKernelCallExpr _
    | ChooseExpr _
    | ConvertVectorExpr _
    | CoawaitExpr _
    | CoyieldExpr _
    | DependentCoawaitExpr _
    | DependentScopeDeclRefExpr _
    | DesignatedInitExpr _
    | DesignatedInitUpdateExpr _
    | ExpressionTraitExpr _
    | ExtVectorElementExpr _
    | FunctionParmPackExpr _
    | ImaginaryLiteral _
    | MSPropertyRefExpr _
    | MSPropertySubscriptExpr _
    | NoInitExpr _
    | OMPArraySectionExpr _
    | ObjCIsaExpr _
    | ObjCSubscriptRefExpr _
    | UnresolvedLookupExpr _
    | UnresolvedMemberExpr _
    | PackExpansionExpr _
    | ParenListExpr _
    | TypoExpr _
    | IndirectGotoStmt _
    | MSDependentExistsStmt _
    | OMPAtomicDirective _
    | OMPBarrierDirective _
    | OMPCancelDirective _
    | OMPCancellationPointDirective _
    | OMPCriticalDirective _
    | OMPFlushDirective _
    | OMPDistributeDirective _
    | OMPDistributeParallelForDirective _
    | OMPDistributeParallelForSimdDirective _
    | OMPDistributeSimdDirective _
    | OMPForDirective _
    | OMPForSimdDirective _
    | OMPParallelForDirective _
    | OMPParallelForSimdDirective _
    | OMPSimdDirective _
    | OMPTargetParallelForSimdDirective _
    | OMPTargetSimdDirective _
    | OMPTargetTeamsDistributeDirective _
    | OMPTargetTeamsDistributeParallelForDirective _
    | OMPTargetTeamsDistributeParallelForSimdDirective _
    | OMPTargetTeamsDistributeSimdDirective _
    | OMPTaskLoopDirective _
    | OMPTaskLoopSimdDirective _
    | OMPTeamsDistributeDirective _
    | OMPTeamsDistributeParallelForDirective _
    | OMPTeamsDistributeParallelForSimdDirective _
    | OMPTeamsDistributeSimdDirective _
    | OMPMasterDirective _
    | OMPOrderedDirective _
    | OMPParallelDirective _
    | OMPParallelSectionsDirective _
    | OMPSectionDirective _
    | OMPSectionsDirective _
    | OMPSingleDirective _
    | OMPTargetDataDirective _
    | OMPTargetDirective _
    | OMPTargetEnterDataDirective _
    | OMPTargetExitDataDirective _
    | OMPTargetParallelDirective _
    | OMPTargetParallelForDirective _
    | OMPTargetTeamsDirective _
    | OMPTargetUpdateDirective _
    | OMPTaskDirective _
    | OMPTaskgroupDirective _
    | OMPTaskwaitDirective _
    | OMPTaskyieldDirective _
    | OMPTeamsDirective _
    | SEHExceptStmt _
    | SEHFinallyStmt _
    | SEHLeaveStmt _
    | SEHTryStmt _
    | ShuffleVectorExpr _ ->
        let (stmt_info, stmts), ret_typ =
          match Clang_ast_proj.get_expr_tuple instr with
          | Some (stmt_info, stmts, expr_info) ->
              let ret_typ =
                CType_decl.get_type_from_expr_info expr_info trans_state.context.tenv
              in
              ((stmt_info, stmts), ret_typ)
          | None ->
              let stmt_tuple = Clang_ast_proj.get_stmt_tuple instr in
              (stmt_tuple, Typ.mk Tvoid)
        in
        skip_unimplemented
          ~reason:
            (Printf.sprintf "unimplemented construct: %s, found at %s"
               (Clang_ast_proj.get_stmt_kind_string instr)
               (Clang_ast_j.string_of_source_range stmt_info.Clang_ast_t.si_source_range))
          trans_state stmt_info ret_typ stmts


  (** Function similar to instruction function, but it takes C++ constructor initializer as
      an input parameter. *)
  and cxx_constructor_init_trans ctor_init trans_state =
    let context = trans_state.context in
    let source_range = ctor_init.Clang_ast_t.xci_source_range in
    let sil_loc =
      CLocation.location_of_source_range context.CContext.translation_unit_context.source_file
        source_range
    in
    (* its pointer will be used in PriorityNode *)
    let this_stmt_info = CAst_utils.dummy_stmt_info () in
    (* this will be used to avoid creating node in init_expr_trans *)
    let child_stmt_info =
      {(CAst_utils.dummy_stmt_info ()) with Clang_ast_t.si_source_range= source_range}
    in
    let trans_state' = PriorityNode.try_claim_priority_node trans_state this_stmt_info in
    let this_res_trans = this_expr_trans child_stmt_info trans_state' sil_loc in
    let var_res_trans =
      match ctor_init.Clang_ast_t.xci_subject with
      | `Delegating _ | `BaseClass _ ->
          let this_exp, this_typ = this_res_trans.return in
          (* Hack: Strip pointer from type here since cxxConstructExpr_trans expects it this way *)
          (* it will add pointer back before making it a parameter to a call *)
          let class_typ = match this_typ.Typ.desc with Tptr (t, _) -> t | _ -> assert false in
          {this_res_trans with return= (this_exp, class_typ)}
      | `Member decl_ref ->
          decl_ref_trans ~is_constructor_init:true ~context:(MemberOrIvar this_res_trans)
            trans_state' child_stmt_info decl_ref
    in
    let var_exp_typ = var_res_trans.return in
    let init_expr = ctor_init.Clang_ast_t.xci_init_expr in
    let init_res_trans = init_expr_trans trans_state' var_exp_typ child_stmt_info init_expr in
    PriorityNode.compute_results_to_parent trans_state' sil_loc ~node_name:ConstructorInit
      this_stmt_info ~return:init_res_trans.return [var_res_trans; init_res_trans]


  (** Given a translation state and list of translation functions it executes translation *)
  and exec_trans_instrs trans_state trans_stmt_fun_list : control * (Exp.t * Typ.t) list =
    let rec exec_trans_instrs_rev trans_state rev_trans_fun_list =
      match rev_trans_fun_list with
      | [] ->
          ({empty_control with root_nodes= trans_state.succ_nodes}, [])
      | trans_stmt_fun :: trans_stmt_fun_list' ->
          let res_trans_s = trans_stmt_fun trans_state in
          let trans_state' =
            if res_trans_s.control.root_nodes <> [] then
              {trans_state with succ_nodes= res_trans_s.control.root_nodes}
            else trans_state
          in
          let control_tail_rev, returns_tail_rev =
            exec_trans_instrs_rev trans_state' trans_stmt_fun_list'
          in
          ( { root_nodes= control_tail_rev.root_nodes
            ; leaf_nodes= res_trans_s.control.leaf_nodes
            ; instrs= List.rev_append res_trans_s.control.instrs control_tail_rev.instrs
            ; initd_exps=
                List.rev_append res_trans_s.control.initd_exps control_tail_rev.initd_exps }
          , res_trans_s.return :: returns_tail_rev )
    in
    let rev_control, rev_returns =
      exec_trans_instrs_rev trans_state (List.rev trans_stmt_fun_list)
    in
    ( { rev_control with
        instrs= List.rev rev_control.instrs; initd_exps= List.rev rev_control.initd_exps }
    , List.rev rev_returns )


  and get_clang_stmt_trans stmt trans_state =
    exec_with_node_creation ~f:instruction trans_state stmt


  (* TODO write translate function for cxx constructor exprs *)
  and get_custom_stmt_trans stmt =
    match stmt with
    | `ClangStmt stmt ->
        get_clang_stmt_trans stmt
    | `CXXConstructorInit instr ->
        cxx_constructor_init_trans instr


  (** Given a translation state, this function translates a list of clang statements. *)
  and instructions trans_state stmt_list =
    let stmt_trans_fun = List.map ~f:get_clang_stmt_trans stmt_list in
    exec_trans_instrs trans_state stmt_trans_fun


  and expression_trans context stmt =
    let trans_state = CTrans_utils.default_trans_state context in
    let res_trans_stmt = instruction trans_state stmt in
    fst res_trans_stmt.return


  let instructions_trans context body extra_instrs exit_node ~is_destructor_wrapper =
    let default_trans_state = CTrans_utils.default_trans_state context in
    let trans_state = {default_trans_state with succ_nodes= [exit_node]} in
    let procname = Procdesc.get_proc_name context.CContext.procdesc in
    let is_destructor =
      match procname with
      | Typ.Procname.ObjC_Cpp cpp_pname ->
          Typ.Procname.ObjC_Cpp.is_destructor cpp_pname
      | _ ->
          false
    in
    let stmt_info, _ = Clang_ast_proj.get_stmt_tuple body in
    let destructor_res, body =
      if is_destructor_wrapper then
        let stmt_info' = {stmt_info with si_pointer= CAst_utils.get_fresh_pointer ()} in
        ( cxx_inject_virtual_base_class_destructors trans_state stmt_info'
          (* destructor wrapper only have calls to virtual base class destructors in its body *)
        , Clang_ast_t.CompoundStmt (stmt_info', []) )
      else if is_destructor then
        (cxx_inject_field_destructors_in_destructor_body trans_state stmt_info, body)
      else (None, body)
    in
    (* Injecting destructor call nodes of fields at the end of the body *)
    let succ_nodes =
      match destructor_res with
      | Some {control= {root_nodes= _ :: _ as root_nodes}} ->
          root_nodes
      | Some {control= {root_nodes= []}} | None ->
          trans_state.succ_nodes
    in
    let trans_state' = {trans_state with succ_nodes} in
    let instrs = extra_instrs @ [`ClangStmt body] in
    let instrs_trans = List.map ~f:get_custom_stmt_trans instrs in
    let res_control, _ = exec_trans_instrs trans_state' instrs_trans in
    res_control.root_nodes
end
