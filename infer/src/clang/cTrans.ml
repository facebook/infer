(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
      according to the method signature with the following priority: 1. method is a predefined model
      2. method is found by clang's resolution 3. Method is found by our resolution *)
  let get_callee_objc_method context obj_c_message_expr_info callee_ms_opt act_params =
    let open CContext in
    let selector, mc_type = CMethod_trans.get_objc_method_data obj_c_message_expr_info in
    let is_instance = not (CMethod_trans.equal_method_call_type mc_type MCStatic) in
    let objc_method_kind = Procname.ObjC_Cpp.objc_method_kind_of_bool is_instance in
    let method_kind =
      if is_instance then ClangMethodKind.OBJC_INSTANCE else ClangMethodKind.OBJC_CLASS
    in
    let proc_name =
      match CMethod_trans.get_method_name_from_clang callee_ms_opt with
      | Some name ->
          name
      | None ->
          (* fall back to our method resolution if clang's fails *)
          let class_name =
            CMethod_trans.get_class_name_method_call_from_receiver_kind context
              obj_c_message_expr_info act_params
          in
          let parameters = List.map act_params ~f:(fun (_, typ) -> Procname.Parameter.of_typ typ) in
          CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind class_name selector
            objc_method_kind parameters
    in
    let predefined_ms_opt =
      match proc_name with
      | Procname.ObjC_Cpp objc_cpp ->
          let class_name = Procname.ObjC_Cpp.get_class_type_name objc_cpp in
          CTrans_models.get_predefined_model_method_signature class_name selector
            CType_decl.CProcname.NoAstDecl.objc_method_of_string_kind
      | _ ->
          None
    in
    match (predefined_ms_opt, callee_ms_opt) with
    | Some ms, _ ->
        ignore
          (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
             context.tenv ms [] [] ) ;
        (ms.CMethodSignature.name, CMethod_trans.MCNoVirtual)
    | None, Some ms ->
        ignore
          (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
             context.tenv ms [] [] ) ;
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
    | ImplicitCastExpr (_, [s'], _, _, _) | ExprWithCleanups (_, [s'], _, _) ->
        is_block_expr s'
    | _ ->
        false


  let objc_exp_of_type_block fun_exp_stmt =
    match fun_exp_stmt with
    | Clang_ast_t.ImplicitCastExpr (_, _, ei, _, _) | Clang_ast_t.PseudoObjectExpr (_, _, ei) ->
        CType.is_block_type ei.Clang_ast_t.ei_qual_type
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


  let exec_with_node_creation node_name ~f trans_state stmt =
    let res_trans = f trans_state stmt in
    if not (List.is_empty res_trans.control.instrs) then
      let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
      let stmt_info' = {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let sil_loc =
        CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
          stmt_info'
      in
      PriorityNode.compute_result_to_parent trans_state_pri sil_loc node_name stmt_info' res_trans
    else res_trans


  (* This is the standard way of dealing with self:Class or a call [a class]. We translate it as
     sizeof(<type pf a>) The only time when we want to translate those expressions differently is
     when they are the first argument of method calls. In that case they are not translated as
     expressions, but we take the type and create a static method call from it. This is done in
     objcMessageExpr_trans. *)
  let sizeof_expr_class class_name =
    let typ = Typ.mk (Tstruct class_name) in
    ( Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
    , Typ.mk (Tint IULong) )


  let add_reference_if_glvalue (typ : Typ.t) expr_info =
    (* glvalue definition per C++11:
       http://en.cppreference.com/w/cpp/language/value_category *)
    let is_xvalue =
      match expr_info.Clang_ast_t.ei_value_kind with `XValue -> true | `LValue | `RValue -> false
    in
    let is_lvalue =
      match expr_info.Clang_ast_t.ei_value_kind with `LValue -> true | `RValue | `XValue -> false
    in
    match (is_xvalue || is_lvalue, typ.desc) with
    | true, Tptr (_, (Pk_lvalue_reference | Pk_rvalue_reference)) ->
        (* reference of reference is not allowed in C++ - it's most likely frontend *)
        (* trying to add same reference to same type twice*)
        (* this is hacky and should be fixed (t9838691) *)
        typ
    | true, _ ->
        if is_lvalue then
          let quals = Typ.mk_type_quals ~is_reference:true () in
          Typ.mk ~quals (Tptr (typ, Pk_lvalue_reference))
        else Typ.mk (Tptr (typ, Pk_lvalue_reference))
    | _ ->
        typ


  (** Execute translation and then possibly adjust the type of the result of translation: In C++,
      when expression returns reference to type T, it will be lvalue to T, not T&, but infer needs
      it to be T& *)
  let exec_with_glvalue_as_reference f trans_state stmt =
    let expr_info =
      match Clang_ast_proj.get_expr_tuple stmt with
      | Some (_, _, ei) ->
          ei
      | None ->
          let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
          CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Clang_ast_proj.get_expr_tuple stmt returns None, stmt is %a"
            (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
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


  (* Translation will reset Ident counter, save it's state and restore it afterwards *)
  let keep_ident_counter ~f =
    let ident_state = Ident.NameGenerator.get_current () in
    f () ;
    Ident.NameGenerator.set_current ident_state


  let call_translation ?(is_cpp_lambda_expr = false) context decl =
    let open CContext in
    keep_ident_counter ~f:(fun () ->
        let decl_context = if is_cpp_lambda_expr then `CppLambdaExprTranslation else `Translation in
        F.translate_one_declaration context.translation_unit_context context.tenv context.cfg
          decl_context decl )


  let global_var_decl_translation context decl_ref =
    let open CContext in
    keep_ident_counter ~f:(fun () ->
        CAst_utils.get_decl_opt_with_decl_ref decl_ref
        |> Option.iter ~f:(fun decl ->
               F.translate_one_declaration context.translation_unit_context context.tenv context.cfg
                 `Translation decl ) )


  let create_var_exp_tmp_var trans_state expr_info ~clang_pointer ~var_name =
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let pvar, typ =
      CVar_decl.mk_temp_sil_var_for_expr context ~name:var_name ~clang_pointer expr_info
    in
    let var_data = ProcAttributes.default_var_data pvar typ in
    Procdesc.append_locals procdesc [var_data] ;
    (Exp.Lvar pvar, typ)


  let get_forwarded_params trans_state sil_loc =
    (* forward all parameters of constructor except this *)
    let context = trans_state.context in
    let pdesc = context.procdesc in
    let attr = Procdesc.get_attributes pdesc in
    let procname = Procdesc.get_proc_name pdesc in
    attr.formals (* remove this, which should always be the first formal parameter *) |> List.tl_exn
    |> List.fold_right ~init:([], [])
         ~f:(fun (formal, typ, _) (forwarded_params, forwarded_init_exps) ->
           let pvar = Pvar.mk formal procname in
           let id = Ident.create_fresh Ident.knormal in
           ( (Exp.Var id, typ) :: forwarded_params
           , Sil.Load {id; e= Exp.Lvar pvar; typ; loc= sil_loc} :: forwarded_init_exps ) )


  let create_call_instr trans_state (return_type : Typ.t) function_sil params_sil sil_loc call_flags
      ~is_inherited_ctor =
    let ret_id_typ = (Ident.create_fresh Ident.knormal, return_type) in
    let ret_id', params, initd_exps, ret_exps, call_flags =
      (* Assumption: should_add_return_param will return true only for struct types *)
      if CType_decl.should_add_return_param return_type then
        let param_type = Typ.mk (Typ.Tptr (return_type, Typ.Pk_pointer)) in
        let var_exp =
          match trans_state.var_exp_typ with
          | Some (exp, _) ->
              exp
          | _ ->
              let procdesc = trans_state.context.CContext.procdesc in
              let pvar = CVar_decl.mk_temp_sil_var procdesc ~name:"__temp_return_" in
              let var_data = ProcAttributes.default_var_data pvar return_type in
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
           (Exp.Var i, t) )
        , call_flags )
    in
    let forwarded_params, forwarded_init_instrs =
      if is_inherited_ctor then get_forwarded_params trans_state sil_loc else ([], [])
    in
    let call_instr =
      match function_sil with
      | Exp.Var _ ->
          let closure_param = (function_sil, Typ.mk Typ.Tfun) in
          let builtin =
            if call_flags.CallFlags.cf_is_objc_block then BuiltinDecl.__call_objc_block
            else BuiltinDecl.__call_c_function_ptr
          in
          Sil.Call
            ( ret_id'
            , Exp.Const (Const.Cfun builtin)
            , closure_param :: (params @ forwarded_params)
            , sil_loc
            , call_flags )
      | _ ->
          Sil.Call (ret_id', function_sil, params @ forwarded_params, sil_loc, call_flags)
    in
    mk_trans_result ret_exps
      {empty_control with instrs= forwarded_init_instrs @ [call_instr]; initd_exps}


  (** Given a captured var, return the instruction to assign it to a temp *)
  let assign_captured_var loc (cvar, typ, mode) =
    match (mode : CapturedVar.capture_mode) with
    | ByReference ->
        ((Exp.Lvar cvar, cvar, typ, mode), None)
    | ByValue ->
        let id = Ident.create_fresh Ident.knormal in
        let instr = Sil.Load {id; e= Exp.Lvar cvar; typ; loc} in
        ((Exp.Var id, cvar, typ, mode), Some instr)


  let closure_trans closure_pname captured_vars context stmt_info expr_info =
    let open CContext in
    let loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let qual_type = expr_info.Clang_ast_t.ei_qual_type in
    let typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let ids_instrs = List.map ~f:(assign_captured_var loc) captured_vars in
    let captured_vars, instrs = List.unzip ids_instrs in
    let instrs = List.filter_opt instrs in
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
      pointer)". So we implement it as the constant zero *)
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
        let sizeof_data =
          {Exp.typ; nbytes; dynamic_length= None; subtype= Subtype.exact; nullable= false}
        in
        mk_trans_result (Exp.Sizeof sizeof_data, typ) empty_control
    | `AlignOf | `OpenMPRequiredSimdAlign | `PreferredAlignOf | `VecStep | `VectorElements ->
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
      let get_attr_opt = function `AnnotateAttr (_, annotation) -> Some annotation | _ -> None in
      List.find_map ~f:get_attr_opt decl_info.di_attributes
    in
    let name = QualifiedCppName.to_qual_string qual_name in
    let function_attr_opt = Option.bind decl_opt ~f:get_annotate_attr_arg in
    match function_attr_opt with
    | Some attr when CTrans_models.is_modeled_attribute attr ->
        Some (Procname.from_string_c_fun attr)
    | _ when CTrans_models.is_modeled_builtin name ->
        Some (Procname.from_string_c_fun (CFrontend_config.infer ^ name))
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
    let callee_ms_opt = CMethod_trans.method_signature_of_pointer context.tenv decl_ptr in
    mk_trans_result
      (Exp.Const (Const.Cfun pname), typ)
      ?method_signature:callee_ms_opt empty_control


  let field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init
      ~is_member_of_const =
    let open CContext in
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let name_info, decl_ptr, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let field_string = name_info.Clang_ast_t.ni_name in
    L.debug Capture Verbose "Translating field '%s'@\n" field_string ;
    let field_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    (* If the object is const, field member will also be const *)
    let field_typ = if is_member_of_const then Typ.set_to_const field_typ else field_typ in
    let obj_sil, class_typ = pre_trans_result.return in
    let is_pointer_typ = Typ.is_pointer class_typ in
    let class_typ = match class_typ.Typ.desc with Typ.Tptr (t, _) -> t | _ -> class_typ in
    L.debug Capture Verbose "Type is  '%s' @\n" (Typ.to_string class_typ) ;
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
          CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "di_parent_pointer should be always set for fields/ivars, but got %a"
            (Pp.option (Pp.of_string ~f:Clang_ast_j.string_of_decl))
            decl
    in
    let field_name =
      let fieldname_no_weak_info = Fieldname.make class_tname field_string in
      if Typ.Name.is_objc_class class_tname then
        match Tenv.lookup trans_state.context.tenv class_tname with
        | Some {fields} ->
            let fieldname_opt =
              List.find_map
                ~f:(fun {Struct.name} ->
                  if String.equal (Fieldname.get_field_name name) field_string then Some name
                  else None )
                fields
            in
            Option.value ~default:fieldname_no_weak_info fieldname_opt
        | None ->
            fieldname_no_weak_info
      else fieldname_no_weak_info
    in
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
        let deref_instr = Sil.Load {id; e= field_exp; typ= field_typ; loc= sil_loc} in
        (Exp.Var id, [deref_instr])
      else (field_exp, [])
    in
    let instrs = pre_trans_result.control.instrs @ deref_instrs in
    {pre_trans_result with control= {pre_trans_result.control with instrs}; return= (exp, field_typ)}


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
    L.debug Capture Verbose "!!!!! Dealing with method '%s'@\n" method_name ;
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
            let extra_instrs = [Sil.Load {id= no_id; e= exp; typ; loc= sil_loc}] in
            (return, extra_instrs)
        | MemberOrIvar {return= (_, {Typ.desc= Tptr _}) as return} ->
            (return, [])
        | MemberOrIvar {return= exp, typ} ->
            ((exp, Typ.mk (Tptr (typ, Typ.Pk_lvalue_reference))), [])
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
            Typ.Name.Cpp.from_qual_name Typ.NoTemplate ~is_union:false
              (CAst_utils.get_class_name_from_member name_info)
          in
          if is_inner_destructor then
            match ms_opt with
            | Some ms ->
                let procname = ms.CMethodSignature.name in
                let new_method_name =
                  Config.clang_inner_destructor_prefix ^ Procname.get_method procname
                in
                let ms' =
                  {ms with name= Procname.objc_cpp_replace_method_name procname new_method_name}
                in
                ignore
                  (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
                     context.tenv ms' [] [] ) ;
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
    mk_trans_result ~is_cpp_call_virtual ~method_name:pname ?method_signature:ms_opt this_exp_typ
      context_control_with_this


  let get_destructor_decl_ref class_type_ptr =
    let open IOption.Let_syntax in
    let* decl_ref =
      match CAst_utils.get_decl_from_typ_ptr class_type_ptr with
      | Some (CXXRecordDecl (_, _, _, _, _, _, _, cxx_record_info))
      | Some (ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _, _, _)) ->
          cxx_record_info.xrdi_destructor
      | _ ->
          None
    in
    match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
    | Some (CXXDestructorDecl ({di_is_implicit= true}, _, _, {fdi_body= None}, _)) ->
        (* trivial destructor (i.e. the destructor isn't user-defined and is a no-op as none of the
           fields of the object need to be destructed, e.g. the object is POD): pretend there is no
           destructor at all as we won't get a chance to translate it due to its empty body (which
           makes it "undefined" according to the frontend's logic for such things) and doing nothing
           at all is equivalent anyway. Note that returning [Some _] here would cause "unknown
           calls" at the call site.

           Note: For some reason empty implicit trivial constructors *do* get a body in the AST so
           do not suffer from this issue. *)
        None
    | Some (CXXDestructorDecl _) ->
        Some decl_ref
    | _ ->
        None


  let destructor_deref_trans trans_state pvar_trans_result destructor_decl_ref si
      ~is_inner_destructor =
    method_deref_trans ~is_inner_destructor trans_state ~context:(MemberOrIvar pvar_trans_result)
      destructor_decl_ref si `CXXDestructor


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


  let get_arrayWithObjects_count_infos method_pointer =
    Option.bind (CAst_utils.get_decl_opt method_pointer) ~f:(function
      | Clang_ast_t.ObjCMethodDecl
          (decl_info, {ni_name}, {omdi_parameters= ParmVarDecl (_, _, objects_qual_typ, _) :: _})
        when String.equal ni_name CFrontend_config.arrayWithObjects_count ->
          Some (decl_info, objects_qual_typ)
      | _ ->
          None )


  let get_dictionaryWithObjects_forKeys_count_infos method_pointer =
    Option.bind (CAst_utils.get_decl_opt method_pointer) ~f:(function
      | Clang_ast_t.ObjCMethodDecl
          (decl_info, {ni_name}, {omdi_parameters= ParmVarDecl (_, _, objects_qual_typ, _) :: _})
        when String.equal ni_name CFrontend_config.dictionaryWithObjects_forKeys_count ->
          Some (decl_info, objects_qual_typ)
      | _ ->
          None )


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


  let add_block_as_arg_attributes trans_state_param callee_ms_opt i =
    match callee_ms_opt with
    | Some callee_ms when not trans_state_param.is_fst_arg_objc_instance_method_call ->
        let ms_param_type_i =
          let find_arg j _ = Int.equal i j in
          List.findi ~f:find_arg callee_ms.CMethodSignature.params
        in
        let block_as_arg_attributes =
          match ms_param_type_i with
          | Some (_, {is_no_escape_block_arg}) ->
              Some
                { ProcAttributes.passed_to= callee_ms.CMethodSignature.name
                ; passed_as_noescape_block= is_no_escape_block_arg }
          | None ->
              None
        in
        {trans_state_param with block_as_arg_attributes}
    | _ ->
        trans_state_param


  let rec labelStmt_trans trans_state stmt_info stmt_list label_name =
    let context = trans_state.context in
    let[@warning "-partial-match"] [stmt] = stmt_list in
    let res_trans = instruction trans_state stmt in
    (* create the label root node into the hashtbl *)
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    Procdesc.node_set_succs context.procdesc root_node' ~normal:res_trans.control.root_nodes ~exn:[] ;
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes= [root_node']; leaf_nodes= trans_state.succ_nodes}


  (** Create instructions to initialize record with zeroes. It needs to traverse whole type
      structure, to assign 0 values to all transitive fields because of AST construction in C
      translation *)
  and implicitValueInitExpr_trans trans_state stmt_info =
    match trans_state.var_exp_typ with
    | None ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "Retrieving var from non-InitListExpr parent"
    | Some var_exp_typ ->
        (* This node will always be child of InitListExpr, claiming priority will always fail *)
        let tenv = trans_state.context.CContext.tenv in
        let sil_loc =
          CLocation.location_of_stmt_info
            trans_state.context.CContext.translation_unit_context.source_file stmt_info
        in
        (* Traverse structure of a type and initialize int/float/ptr fields with zero *)
        let rec fill_typ_with_zero ((exp, typ) as exp_typ) =
          let init_with_builtin =
            match typ.Typ.desc with
            | Tstruct tn -> (
              match Tenv.lookup tenv tn with
              | Some {fields} ->
                  List.length fields > Config.clang_compound_literal_init_limit
              | None ->
                  assert false )
            | Tarray {length= Some n} ->
                IntLit.gt n (IntLit.of_int Config.clang_compound_literal_init_limit)
            | Tint _ | Tfloat _ | Tptr _ ->
                false
            | Tfun | Tvoid | Tarray _ | TVar _ ->
                true
          in
          if init_with_builtin then
            let ret_id = Ident.create_fresh Ident.knormal in
            let call_instr =
              Sil.Call
                ( (ret_id, StdTyp.void)
                , Const (Cfun BuiltinDecl.zero_initialization)
                , [exp_typ]
                , sil_loc
                , CallFlags.default )
            in
            mk_trans_result exp_typ {empty_control with instrs= [call_instr]}
          else
            match typ.Typ.desc with
            | Tstruct tn ->
                let field_exp_typs =
                  match Tenv.lookup tenv tn with
                  | Some {fields} ->
                      List.map fields ~f:(fun {Struct.name= fieldname; typ= fieldtype} ->
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
                let instrs = [Sil.Store {e1= exp; typ; e2= zero_exp; loc= sil_loc}] in
                mk_trans_result (exp, typ) {empty_control with instrs}
            | Tfun | Tvoid | Tarray _ | TVar _ ->
                CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
                  "fill_typ_with_zero on type %a" (Typ.pp Pp.text) typ
        in
        let res_trans = fill_typ_with_zero var_exp_typ in
        {res_trans with control= {res_trans.control with initd_exps= [fst var_exp_typ]}}


  and var_deref_trans trans_state stmt_info (decl_ref : Clang_ast_t.decl_ref) =
    let context = trans_state.context in
    let _, _, qual_type = CAst_utils.get_info_from_decl_ref decl_ref in
    let ast_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let typ =
      match ast_typ.Typ.desc with
      | Tstruct _ when decl_ref.dr_kind = `ParmVar ->
          if CGeneral_utils.is_cpp_translation context.translation_unit_context then
            Typ.mk (Tptr (ast_typ, Pk_lvalue_reference))
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
    (* Captured variables without initialization do not have the correct types
       inside of lambda bodies. The same issue happens for variables captured by reference
       inside objc blocks. Use the types stored in the procdesc. *)
    let typ =
      if Procname.is_cpp_lambda procname || Procname.is_objc_block procname then
        let pvar_name = Pvar.get_name pvar in
        List.find (Procdesc.get_captured context.procdesc) ~f:(fun {CapturedVar.pvar= captured} ->
            Mangled.equal (Pvar.get_name captured) pvar_name )
        |> Option.value_map ~f:(fun {CapturedVar.typ} -> typ) ~default:typ
      else typ
    in
    let return =
      if Self.is_var_self pvar (CContext.is_objc_method context) && CType.is_class typ then
        sizeof_expr_class (CContext.get_curr_class_typename stmt_info context)
      else (var_exp, typ)
    in
    L.debug Capture Verbose "var_deref_trans for %a:%a@\n@\n" (Pvar.pp Pp.text) pvar
      (Typ.pp Pp.text) typ ;
    let res_trans = mk_trans_result return empty_control in
    let res_trans =
      if Typ.is_reference typ then
        (* dereference pvar due to the behavior of reference types in clang's AST *)
        dereference_value_from_result stmt_info.Clang_ast_t.si_source_range sil_loc res_trans
      else res_trans
    in
    (* TODO: For now, it does not generate the initializers for static local variables. This is
       unsound because a static local varaible should be initialized once globally.  On the other
       hand, currently static local variables are initialized in the function body, i.e.,
       initializing every time the function is called.

       While we should move the initialization out the function body to solve the issue, it is not
       good at the moment due to the name conflict of the initializers.  Infer introduces the
       initializer names like [__infer_globals_initializer_\[var\]].  Thus, if multiple functions
       have static local variables with the same name, their initializer names will conflict. *)
    if Pvar.is_global pvar && not (Pvar.is_static_local pvar) then
      global_var_decl_translation context decl_ref ;
    res_trans


  and decl_ref_trans ?(is_constructor_init = false) ?(is_member_of_const = false) ~context
      trans_state stmt_info decl_ref =
    let decl_kind = decl_ref.Clang_ast_t.dr_kind in
    match (decl_kind, context) with
    | `EnumConstant, _ ->
        enum_constant_trans trans_state decl_ref
    | `Function, _ ->
        function_deref_trans trans_state decl_ref
    | (`Var | `VarTemplateSpecialization | `ImplicitParam | `ParmVar | `Binding | `Decomposition), _
      ->
        var_deref_trans trans_state stmt_info decl_ref
    | (`Field | `ObjCIvar), MemberOrIvar pre_trans_result ->
        (* a field outside of constructor initialization is probably a pointer to member, which we
           do not support *)
        field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init
          ~is_member_of_const
    | (`CXXMethod | `CXXConversion | `CXXConstructor | `CXXDestructor), _ ->
        method_deref_trans trans_state ~context decl_ref stmt_info decl_kind
    | _ ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "Decl ref expression %a with pointer %d still needs to be translated"
          (Pp.of_string ~f:Clang_ast_j.string_of_decl_kind)
          decl_kind decl_ref.Clang_ast_t.dr_decl_pointer


  and declRefExpr_trans trans_state stmt_info decl_ref_expr_info =
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
        CAst_utils.get_enum_constant_exp_exn enum_constant_pointer
      in
      match sil_exp_opt with
      | Some exp ->
          exp
      | None ->
          let exp = enum_const_eval context enum_constant_pointer prev_enum_constant_opt zero in
          CAst_utils.update_enum_map_exn enum_constant_pointer exp ;
          exp
    with Not_found_s _ | Caml.Not_found -> zero


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
    let trans_state_sub_exprs = {trans_state with var_exp_typ= None} in
    let res_trans_a = instruction trans_state_sub_exprs array_stmt in
    let res_trans_idx = instruction trans_state_sub_exprs idx_stmt in
    let a_exp, _ = res_trans_a.return in
    let i_exp, _ = res_trans_idx.return in
    let array_exp = Exp.Lindex (a_exp, i_exp) in
    let root_nodes =
      if not (List.is_empty res_trans_a.control.root_nodes) then res_trans_a.control.root_nodes
      else res_trans_idx.control.root_nodes
    in
    let leaf_nodes =
      if not (List.is_empty res_trans_idx.control.leaf_nodes) then res_trans_idx.control.leaf_nodes
      else res_trans_a.control.leaf_nodes
    in
    if not (List.is_empty res_trans_idx.control.root_nodes) then
      List.iter
        ~f:(fun n ->
          Procdesc.node_set_succs context.procdesc n ~normal:res_trans_idx.control.root_nodes
            ~exn:[] )
        res_trans_a.control.leaf_nodes ;
    (* Note the order of res_trans_idx.ids @ res_trans_a.ids is important. *)
    (* We expect to use only res_trans_idx.ids in construction of other operation. *)
    (* res_trans_a.ids is passed to be Removed.*)
    let control =
      { root_nodes
      ; leaf_nodes
      ; instrs= res_trans_a.control.instrs @ res_trans_idx.control.instrs
      ; initd_exps= res_trans_idx.control.initd_exps @ res_trans_a.control.initd_exps
      ; cxx_temporary_markers_set=
          res_trans_idx.control.cxx_temporary_markers_set
          @ res_trans_a.control.cxx_temporary_markers_set }
    in
    mk_trans_result (array_exp, typ) control


  and binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list =
    L.debug Capture Verbose "  BinaryOperator '%a' "
      (Pp.of_string ~f:Clang_ast_j.string_of_binary_operator_kind)
      binary_operator_info.Clang_ast_t.boi_kind ;
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let node_name =
      Procdesc.Node.BinaryOperatorStmt (CArithmetic_trans.bin_op_to_string binary_operator_info)
    in
    let trans_state' = {trans_state_pri with var_exp_typ= None} in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let res_typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    match (stmt_list, res_typ.desc, binary_operator_info.Clang_ast_t.boi_kind) with
    | ( [s1; Clang_ast_t.ImplicitCastExpr (_, [s2], _, _, _)]
      , Tstruct (CStruct _ as struct_name)
      , `Assign ) ->
        let res_trans_e1, res_trans_e2 =
          (instruction trans_state' s1, instruction trans_state' s2)
        in
        let instrs =
          CStructUtils.struct_copy context.CContext.tenv sil_loc (fst res_trans_e1.return)
            (fst res_trans_e2.return) ~typ:res_typ ~struct_name
        in
        [res_trans_e1.control; res_trans_e2.control; {empty_control with instrs}]
        |> PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
        |> mk_trans_result res_trans_e1.return
    | [s1; s2], _, `Comma ->
        let ({control= {instrs}} as res_trans) =
          CTrans_utils.PriorityNode.force_sequential sil_loc node_name trans_state stmt_info
            ~mk_first_opt:(fun trans_state _ -> Some (instruction trans_state s1))
            ~mk_second:(fun trans_state _ -> instruction trans_state s2)
            ~mk_return:(fun ~fst:_ ~snd -> snd.return)
        in
        (* HACK: With the empty instruction, the control is not correctly connected to the parent in
           some cases, e.g. the comma binary operator is used on the top-level.  See
           [CTrans_utils.PriorityNode.compute_controls_to_parent]; the condition [create_node] is
           true only when the instruction is non-empty. *)
        if List.is_empty instrs then
          {res_trans with control= {res_trans.control with instrs= [Metadata Skip]}}
        else res_trans
    | [s1; s2], _, _ ->
        let {control= control1; return= (exp1, typ1) as exp_typ1} = instruction trans_state' s1 in
        let {control= control2; return= exp_typ2} =
          (* translation of s2 is done taking care of block special case *)
          exec_with_block_priority_exception instruction trans_state' s2 stmt_info
        in
        let binop_control, return =
          let exp_op, instr_bin =
            CArithmetic_trans.binary_operation_instruction stmt_info.Clang_ast_t.si_source_range
              binary_operator_info exp_typ1 res_typ exp_typ2 sil_loc
          in
          (* Create a node if the priority if free and there are instructions *)
          let creating_node =
            PriorityNode.own_priority_node trans_state_pri.priority stmt_info
            && not (List.is_empty instr_bin)
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
              let res_instr = Sil.Load {id; e= exp1; typ= typ1; loc= sil_loc} in
              ([res_instr], Exp.Var id)
            else ([], exp_op)
          in
          let return = (exp_to_parent, typ1) in
          let binop_control = {empty_control with instrs= instr_bin @ extra_instrs} in
          ([binop_control], return)
        in
        let all_res_trans =
          (* Evaluate e2 first when assignment.

             > The assignment operator (=) and the compound assignment operators all group
             right-to-left. [...] The right operand is sequenced before the left operand.  (Section
             7.6.19 [expr.ass] in the C++23 standard.) *)
          ( if is_binary_assign_op binary_operator_info then [control2; control1]
            else [control1; control2] )
          @ binop_control
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_res_trans
        |> mk_trans_result return
    | _, _, _ ->
        (* Binary operator should have two operands *)
        assert false


  and atomicExpr_trans trans_state atomic_expr_info stmt_info expr_info stmt_list =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let ret_typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    let node_name = Procdesc.Node.AtomicExpr in
    let handle_unimplemented builtin =
      call_function_with_args node_name builtin trans_state stmt_info ret_typ stmt_list
    in
    match atomic_expr_info.Clang_ast_t.aei_kind with
    | `AO__atomic_add_fetch
    | `AO__atomic_and_fetch
    | `AO__atomic_fetch_add
    | `AO__atomic_fetch_and
    | `AO__atomic_fetch_or
    | `AO__atomic_fetch_sub
    | `AO__atomic_fetch_xor
    | `AO__atomic_or_fetch
    | `AO__atomic_sub_fetch
    | `AO__atomic_xor_fetch
    | `AO__c11_atomic_fetch_add
    | `AO__c11_atomic_fetch_and
    | `AO__c11_atomic_fetch_nand
    | `AO__c11_atomic_fetch_or
    | `AO__c11_atomic_fetch_sub
    | `AO__c11_atomic_fetch_xor
    | `AO__hip_atomic_compare_exchange_strong
    | `AO__hip_atomic_compare_exchange_weak
    | `AO__hip_atomic_exchange
    | `AO__hip_atomic_fetch_add
    | `AO__hip_atomic_fetch_sub
    | `AO__hip_atomic_fetch_and
    | `AO__hip_atomic_fetch_max
    | `AO__hip_atomic_fetch_min
    | `AO__hip_atomic_fetch_or
    | `AO__hip_atomic_fetch_xor
    | `AO__hip_atomic_load
    | `AO__hip_atomic_store
    | `AO__opencl_atomic_fetch_add
    | `AO__opencl_atomic_fetch_or
    | `AO__opencl_atomic_fetch_sub
    | `AO__opencl_atomic_fetch_xor
    | `AO__opencl_atomic_fetch_and
    | `AO__scoped_atomic_add_fetch
    | `AO__scoped_atomic_and_fetch
    | `AO__scoped_atomic_fetch_add
    | `AO__scoped_atomic_fetch_and
    | `AO__scoped_atomic_fetch_or
    | `AO__scoped_atomic_fetch_sub
    | `AO__scoped_atomic_fetch_xor
    | `AO__scoped_atomic_or_fetch
    | `AO__scoped_atomic_sub_fetch
    | `AO__scoped_atomic_xor_fetch ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let s1, s2, mem_controls =
          match stmt_list with
          | [s1; memorder; s2] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (s1, s2, [res_trans_memorder.control])
          | [s1; memorder; s2; memscope] ->
              let res_trans_memorder = instruction trans_state' memorder in
              let res_trans_memscope = instruction trans_state' memscope in
              (s1, s2, [res_trans_memorder.control; res_trans_memscope.control])
          | _ ->
              assert false
        in
        let res_trans_s1 = instruction trans_state' s1 in
        let sil_e1, _ = res_trans_s1.return in
        let res_trans_s2 = instruction trans_state' s2 in
        let sil_e2, _ = res_trans_s2.return in
        let exp_op, instr_op =
          CArithmetic_trans.atomic_operation_instruction atomic_expr_info sil_e1 sil_e2 ret_typ
            sil_loc
        in
        let atomic_op_control = {empty_control with instrs= instr_op} in
        let all_control =
          mem_controls @ [res_trans_s1.control; res_trans_s2.control; atomic_op_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (exp_op, ret_typ)
    | `AO__atomic_load_n
    | `AO__c11_atomic_load
    | `AO__opencl_atomic_load
    | `AO__scoped_atomic_load_n ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, mem_controls =
          match stmt_list with
          | [ptr; memorder] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, [res_trans_memorder.control])
          | [ptr; memorder; memscope] ->
              let res_trans_memorder = instruction trans_state' memorder in
              let res_trans_memscope = instruction trans_state' memscope in
              (ptr, [res_trans_memorder.control; res_trans_memscope.control])
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let e, _ = res_trans_ptr.return in
        let id = Ident.create_fresh Ident.knormal in
        let instrs = [Sil.Load {id; e; typ= ret_typ; loc= sil_loc}] in
        let load_control = {empty_control with instrs} in
        let all_control = mem_controls @ [res_trans_ptr.control; load_control] in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (Exp.Var id, ret_typ)
    | `AO__atomic_load | `AO__scoped_atomic_load ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, ret, mem_controls =
          match stmt_list with
          | [ptr; memorder; ret] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, ret, [res_trans_memorder.control])
          | _ ->
              assert false
        in
        let res_trans_ret = instruction trans_state' ret in
        let ret_exp, ret_typ = res_trans_ret.return in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, ptr_typ = res_trans_ptr.return in
        let ptr_id = Ident.create_fresh Ident.knormal in
        let instrs =
          [ Sil.Load {id= ptr_id; e= ptr_exp; typ= Typ.strip_ptr ptr_typ; loc= sil_loc}
          ; Sil.Store {e1= ret_exp; e2= Exp.Var ptr_id; typ= Typ.strip_ptr ret_typ; loc= sil_loc} ]
        in
        let load_control = {empty_control with instrs} in
        let all_control =
          mem_controls @ [res_trans_ret.control; res_trans_ptr.control; load_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (mk_fresh_void_exp_typ ())
    | `AO__atomic_store_n
    | `AO__c11_atomic_store
    | `AO__opencl_atomic_store
    | `AO__scoped_atomic_store_n
    | `AO__c11_atomic_init
    | `AO__opencl_atomic_init ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, value, mem_controls =
          match stmt_list with
          | [ptr; value] ->
              (ptr, value, [])
          | [ptr; memorder; value] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, value, [res_trans_memorder.control])
          | [ptr; memorder; value; memscope] ->
              let res_trans_memorder = instruction trans_state' memorder in
              let res_trans_memscope = instruction trans_state' memscope in
              (ptr, value, [res_trans_memorder.control; res_trans_memscope.control])
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, _ = res_trans_ptr.return in
        let res_trans_value = instruction trans_state' value in
        let value_exp, value_typ = res_trans_value.return in
        let instrs = [Sil.Store {e1= ptr_exp; e2= value_exp; typ= value_typ; loc= sil_loc}] in
        let load_control = {empty_control with instrs} in
        let all_control =
          mem_controls @ [res_trans_ptr.control; res_trans_value.control; load_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (mk_fresh_void_exp_typ ())
    | `AO__atomic_store | `AO__scoped_atomic_store ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, value, mem_controls =
          match stmt_list with
          | [ptr; memorder; value] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, value, [res_trans_memorder.control])
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, _ = res_trans_ptr.return in
        let res_trans_value = instruction trans_state' value in
        let value_exp, value_typ = res_trans_value.return in
        let typ = Typ.strip_ptr value_typ in
        let id = Ident.create_fresh Ident.knormal in
        let instrs =
          [ Sil.Load {id; e= value_exp; typ; loc= sil_loc}
          ; Sil.Store {e1= ptr_exp; e2= Exp.Var id; typ; loc= sil_loc} ]
        in
        let load_control = {empty_control with instrs} in
        let all_control =
          mem_controls @ [res_trans_ptr.control; res_trans_value.control; load_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (mk_fresh_void_exp_typ ())
    | `AO__atomic_exchange_n
    | `AO__c11_atomic_exchange
    | `AO__opencl_atomic_exchange
    | `AO__scoped_atomic_exchange_n ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, value, mem_controls =
          match stmt_list with
          | [ptr; memorder; value] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, value, [res_trans_memorder.control])
          | [ptr; memorder; value; memscope] ->
              let res_trans_memorder = instruction trans_state' memorder in
              let res_trans_memscope = instruction trans_state' memscope in
              (ptr, value, [res_trans_memorder.control; res_trans_memscope.control])
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, _ = res_trans_ptr.return in
        let res_trans_value = instruction trans_state' value in
        let value_exp, value_typ = res_trans_value.return in
        let id = Ident.create_fresh Ident.knormal in
        let instrs =
          [ Sil.Load {id; e= ptr_exp; typ= ret_typ; loc= sil_loc}
          ; Sil.Store {e1= ptr_exp; e2= value_exp; typ= value_typ; loc= sil_loc} ]
        in
        let load_control = {empty_control with instrs} in
        let all_control =
          mem_controls @ [res_trans_ptr.control; res_trans_value.control; load_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (Exp.Var id, ret_typ)
    | `AO__atomic_exchange | `AO__scoped_atomic_exchange ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
        let ptr, value, ret, mem_controls =
          match stmt_list with
          | [ptr; memorder; value; ret] ->
              let res_trans_memorder = instruction trans_state' memorder in
              (ptr, value, ret, [res_trans_memorder.control])
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, _ = res_trans_ptr.return in
        let res_trans_value = instruction trans_state' value in
        let value_exp, value_typ = res_trans_value.return in
        let res_trans_ret = instruction trans_state' ret in
        let ret_exp, _ = res_trans_ret.return in
        let ptr_id = Ident.create_fresh Ident.knormal in
        let value_id = Ident.create_fresh Ident.knormal in
        let typ = Typ.strip_ptr value_typ in
        let instrs =
          [ Sil.Load {id= ptr_id; e= ptr_exp; typ; loc= sil_loc}
          ; Sil.Load {id= value_id; e= value_exp; typ; loc= sil_loc}
          ; Sil.Store {e1= ptr_exp; e2= Exp.Var value_id; typ; loc= sil_loc}
          ; Sil.Store {e1= ret_exp; e2= Exp.Var ptr_id; typ; loc= sil_loc} ]
        in
        let load_control = {empty_control with instrs} in
        let all_control =
          mem_controls
          @ [res_trans_ptr.control; res_trans_value.control; res_trans_ret.control; load_control]
        in
        PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
          all_control
        |> mk_trans_result (mk_fresh_void_exp_typ ())
    | `AO__atomic_compare_exchange
    | `AO__atomic_compare_exchange_n
    | `AO__c11_atomic_compare_exchange_strong
    | `AO__c11_atomic_compare_exchange_weak
    | `AO__opencl_atomic_compare_exchange_strong
    | `AO__opencl_atomic_compare_exchange_weak
    | `AO__scoped_atomic_compare_exchange
    | `AO__scoped_atomic_compare_exchange_n ->
        let trans_state = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' = {trans_state with succ_nodes= []; var_exp_typ= None} in
        let ptr, expected, desired, mem_controls =
          match stmt_list with
          | [ptr; success_memorder; expected; failure_memorder; desired; weak_or_memscope] ->
              let res_trans_success_memorder = instruction trans_state' success_memorder in
              let res_trans_failure_memorder = instruction trans_state' failure_memorder in
              let res_trans_weak_or_memscope = instruction trans_state' weak_or_memscope in
              ( ptr
              , expected
              , desired
              , [ res_trans_success_memorder.control
                ; res_trans_failure_memorder.control
                ; res_trans_weak_or_memscope.control ] )
          | [ptr; success_memorder; expected; failure_memorder; desired] ->
              let res_trans_success_memorder = instruction trans_state' success_memorder in
              let res_trans_failure_memorder = instruction trans_state' failure_memorder in
              ( ptr
              , expected
              , desired
              , [res_trans_success_memorder.control; res_trans_failure_memorder.control] )
          | _ ->
              assert false
        in
        let res_trans_ptr = instruction trans_state' ptr in
        let ptr_exp, _ = res_trans_ptr.return in
        let ptr_id = Ident.create_fresh Ident.knormal in
        let res_trans_expected = instruction trans_state' expected in
        let expected_exp, _ = res_trans_expected.return in
        let expected_id = Ident.create_fresh Ident.knormal in
        let res_trans_desired = instruction trans_state' desired in
        let (desired_exp, typ), desired_instrs =
          match atomic_expr_info.Clang_ast_t.aei_kind with
          | `AO__atomic_compare_exchange -> (
            match res_trans_desired.return with
            | exp, typ ->
                let desired_id = Ident.create_fresh Ident.knormal in
                let typ = Typ.strip_ptr typ in
                ((Exp.Var desired_id, typ), [Sil.Load {id= desired_id; e= exp; typ; loc= sil_loc}])
            )
          | _ ->
              (res_trans_desired.return, [])
        in
        let is_expected_cond = Exp.BinOp (Binop.Eq, Exp.Var ptr_id, Exp.Var expected_id) in
        let load_instrs =
          [ Sil.Load {id= ptr_id; e= ptr_exp; typ; loc= sil_loc}
          ; Sil.Load {id= expected_id; e= expected_exp; typ; loc= sil_loc} ]
          @ desired_instrs
        in
        let join_node =
          Procdesc.create_node context.procdesc sil_loc Join_node [Sil.Metadata EndBranches]
        in
        Procdesc.node_set_succs context.procdesc join_node ~normal:trans_state.succ_nodes ~exn:[] ;
        let var_exp_typ =
          match trans_state.var_exp_typ with
          | Some var_exp_typ ->
              `ParentExp var_exp_typ
          | None ->
              let pvar =
                CVar_decl.mk_temp_sil_var context.procdesc ~name:"SIL_temp_compare_exchange___"
              in
              let var_data = ProcAttributes.default_var_data pvar ret_typ in
              Procdesc.append_locals context.procdesc [var_data] ;
              `Temp pvar
        in
        let do_branch trans_state branch exp_to_init =
          let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
          let prune_node =
            create_prune_node context.procdesc ~branch ~negate_cond:(not branch) is_expected_cond []
              sil_loc Sil.Ik_compexch
          in
          let instrs =
            (* NOTE: Added dummy loads to avoid dead store false positives. *)
            if branch then
              [ Sil.Store {e1= ptr_exp; e2= desired_exp; typ; loc= sil_loc}
              ; Sil.Load {id= Ident.create_fresh Ident.knormal; e= ptr_exp; typ; loc= sil_loc}
              ; Sil.Store {e1= exp_to_init; e2= Exp.one; typ= ret_typ; loc= sil_loc} ]
            else
              [ Sil.Store {e1= expected_exp; e2= Exp.Var ptr_id; typ; loc= sil_loc}
              ; Sil.Load {id= Ident.create_fresh Ident.knormal; e= expected_exp; typ; loc= sil_loc}
              ; Sil.Store {e1= exp_to_init; e2= Exp.zero; typ= ret_typ; loc= sil_loc} ]
          in
          let return = (exp_to_init, ret_typ) in
          let res_trans =
            PriorityNode.compute_results_to_parent trans_state_pri sil_loc
              AtomicCompareExchangeBranch stmt_info ~return
              [mk_trans_result return {empty_control with instrs; initd_exps= [exp_to_init]}]
          in
          Procdesc.node_set_succs context.procdesc prune_node ~normal:res_trans.control.root_nodes
            ~exn:[] ;
          prune_node
        in
        let exp_to_init =
          match var_exp_typ with `ParentExp (exp, _) -> exp | `Temp pvar -> Exp.Lvar pvar
        in
        let prune_success = do_branch {trans_state with succ_nodes= [join_node]} true exp_to_init in
        let prune_failure =
          do_branch {trans_state with succ_nodes= [join_node]} false exp_to_init
        in
        let instrs, return =
          match var_exp_typ with
          | `ParentExp var_exp_typ ->
              ([], var_exp_typ)
          | `Temp pvar ->
              let id = Ident.create_fresh Ident.knormal in
              ([Sil.Load {id; e= Lvar pvar; typ= ret_typ; loc= sil_loc}], (Exp.Var id, ret_typ))
        in
        let load_controls =
          mem_controls
          @ [ res_trans_ptr.control
            ; res_trans_expected.control
            ; res_trans_desired.control
            ; {empty_control with instrs= load_instrs} ]
        in
        let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
        let control =
          PriorityNode.compute_controls_to_parent trans_state_pri sil_loc node_name stmt_info
            load_controls
        in
        List.iter
          ~f:(fun n ->
            Procdesc.node_set_succs context.procdesc n ~normal:[prune_success; prune_failure]
              ~exn:[] )
          control.leaf_nodes ;
        mk_trans_result return
          {control with instrs; initd_exps= [exp_to_init]; leaf_nodes= [join_node]}
    | `AO__atomic_fetch_max | `AO__scoped_atomic_fetch_max ->
        handle_unimplemented BuiltinDecl.__atomic_fetch_max
    | `AO__atomic_fetch_min | `AO__scoped_atomic_fetch_min ->
        handle_unimplemented BuiltinDecl.__atomic_fetch_min
    | `AO__atomic_fetch_nand | `AO__scoped_atomic_fetch_nand ->
        handle_unimplemented BuiltinDecl.__atomic_fetch_nand
    | `AO__atomic_max_fetch | `AO__scoped_atomic_max_fetch ->
        handle_unimplemented BuiltinDecl.__atomic_max_fetch
    | `AO__atomic_min_fetch | `AO__scoped_atomic_min_fetch ->
        handle_unimplemented BuiltinDecl.__atomic_min_fetch
    | `AO__atomic_nand_fetch | `AO__scoped_atomic_nand_fetch ->
        handle_unimplemented BuiltinDecl.__atomic_nand_fetch
    | `AO__c11_atomic_fetch_max ->
        handle_unimplemented BuiltinDecl.__c11_atomic_fetch_max
    | `AO__c11_atomic_fetch_min ->
        handle_unimplemented BuiltinDecl.__c11_atomic_fetch_min
    | `AO__opencl_atomic_fetch_max ->
        handle_unimplemented BuiltinDecl.__opencl_atomic_fetch_max
    | `AO__opencl_atomic_fetch_min ->
        handle_unimplemented BuiltinDecl.__opencl_atomic_fetch_min


  and callExpr_trans trans_state si stmt_list expr_info =
    let context = trans_state.context in
    let sil_loc = CLocation.location_of_stmt_info context.translation_unit_context.source_file si in
    (* First stmt is the function expr and the rest are params *)
    let fun_exp_stmt, params_stmt =
      match stmt_list with fe :: params -> (fe, params) | _ -> assert false
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let trans_state_callee = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
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
      not (Option.exists ~f:CTrans_models.is_builtin_object_size callee_pname_opt)
    in
    let params_stmt = if should_translate_args then params_stmt else [] in
    (* As we may have nodes coming from different parameters we need to  *)
    (* call instruction for each parameter and collect the results       *)
    (* afterwards. The 'instructions' function does not do that          *)
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let result_trans_params =
      let instruction' = exec_with_glvalue_as_reference instruction in
      let callee_ms_opt = res_trans_callee.method_signature in
      List.mapi
        ~f:(fun i param_stmt ->
          instruction' (add_block_as_arg_attributes trans_state_param callee_ms_opt i) param_stmt )
        params_stmt
    in
    match
      Option.bind callee_pname_opt
        ~f:
          (CTrans_utils.builtin_trans trans_state_pri si.Clang_ast_t.si_source_range sil_loc
             (res_trans_callee :: result_trans_params) )
    with
    | Some builtin ->
        builtin
    | None ->
        let act_params = collect_returns result_trans_params in
        let ret_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
        let ret_type = add_reference_if_glvalue ret_type_no_ref expr_info in
        let res_trans_call =
          create_call_instr trans_state ret_type sil_fe act_params sil_loc ~is_inherited_ctor:false
            {CallFlags.default with cf_is_objc_block= objc_exp_of_type_block fun_exp_stmt}
        in
        let node_name = Procdesc.Node.Call (Exp.to_string sil_fe) in
        let all_res_trans = res_trans_callee :: (result_trans_params @ [res_trans_call]) in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc node_name si
          ~return:res_trans_call.return all_res_trans


  and va_arg_trans trans_state si stmt expr_info =
    let context = trans_state.context in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    let sil_loc = CLocation.location_of_stmt_info context.translation_unit_context.source_file si in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let sil_fe = Exp.Const (Const.Cfun BuiltinDecl.__builtin_va_arg) in
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let result_trans_param = exec_with_glvalue_as_reference instruction trans_state_param stmt in
    let res_trans_call =
      create_call_instr trans_state function_type sil_fe [result_trans_param.return] sil_loc
        CallFlags.default ~is_inherited_ctor:false
    in
    let node_name = Procdesc.Node.Call (Exp.to_string sil_fe) in
    let all_res_trans = [result_trans_param; res_trans_call] in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc node_name si
      ~return:res_trans_call.return all_res_trans


  and cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt si
      function_type ~is_cpp_call_virtual ~is_injected_destructor extra_res_trans ~is_inherited_ctor
      =
    let context = trans_state_pri.context in
    let sil_loc = CLocation.location_of_stmt_info context.translation_unit_context.source_file si in
    let callee_pname = Option.value_exn result_trans_callee.method_name in
    let callee_ms_opt = result_trans_callee.method_signature in
    (* As we may have nodes coming from different parameters we need to call instruction for each
       parameter and collect the results afterwards. The 'instructions' function does not do that *)
    let result_trans_params =
      let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
      let instruction' = exec_with_glvalue_as_reference instruction in
      let res_trans_p =
        List.mapi
          ~f:(fun i param_stmt ->
            let trans_state_param =
              (* We don't need to check the `this` parameter *)
              if i > 0 then add_block_as_arg_attributes trans_state_param callee_ms_opt i
              else trans_state_param
            in
            instruction' trans_state_param param_stmt )
          params_stmt
      in
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
        let call_flags =
          { CallFlags.default with
            cf_virtual= is_cpp_call_virtual
          ; cf_injected_destructor= is_injected_destructor }
        in
        let res_trans_call =
          create_call_instr trans_state_pri function_type sil_method actual_params sil_loc
            call_flags ~is_inherited_ctor
        in
        let node_name = Procdesc.Node.Call (Exp.to_string sil_method) in
        let all_res_trans =
          result_trans_params @ (res_trans_call :: Option.to_list extra_res_trans)
        in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc node_name si
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
    let trans_state_callee = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let result_trans_callee = instruction trans_state_callee fun_exp_stmt in
    let is_cpp_call_virtual = result_trans_callee.is_cpp_call_virtual in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt si function_type
      ~is_injected_destructor:false ~is_cpp_call_virtual None ~is_inherited_ctor:false


  and cxxConstructExpr_trans trans_state si params_stmt ei cxx_constr_info ~is_inherited_ctor =
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let decl_ref = cxx_constr_info.Clang_ast_t.xcei_decl_ref in
    let var_exp, class_type =
      match trans_state.var_exp_typ with
      | Some exp_typ ->
          exp_typ
      | None ->
          let procdesc = trans_state.context.CContext.procdesc in
          let pvar = Pvar.mk_tmp "__temp_construct_" (Procdesc.get_proc_name procdesc) in
          let class_type = CType_decl.get_type_from_expr_info ei context.CContext.tenv in
          let var_data = ProcAttributes.default_var_data pvar class_type in
          Procdesc.append_locals procdesc [var_data] ;
          (Exp.Lvar pvar, class_type)
    in
    let this_type = CType.add_pointer_to_typ class_type in
    let this_res_trans =
      mk_trans_result (var_exp, this_type) {empty_control with initd_exps= [var_exp]}
    in
    let tmp_res_trans = mk_trans_result (var_exp, class_type) empty_control in
    let res_trans_callee =
      decl_ref_trans ~context:(MemberOrIvar this_res_trans) trans_state si decl_ref
    in
    let res_trans =
      cxx_method_construct_call_trans trans_state_pri res_trans_callee params_stmt si StdTyp.void
        ~is_injected_destructor:false ~is_cpp_call_virtual:false (Some tmp_res_trans)
        ~is_inherited_ctor
    in
    {res_trans with return= tmp_res_trans.return}


  and cxx_destructor_call_trans trans_state si this_res_trans destructor_decl_ref
      ~is_injected_destructor ~is_inner_destructor =
    (* cxx_method_construct_call_trans claims a priority with the same [si]. A new pointer is
       generated to avoid premature node creation *)
    let si' = {si with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si' in
    let this_exp, this_typ = this_res_trans.return in
    let this_res_trans' =
      {this_res_trans with return= (this_exp, CType.add_pointer_to_typ this_typ)}
    in
    let res_trans_callee =
      destructor_deref_trans trans_state this_res_trans' destructor_decl_ref si'
        ~is_inner_destructor
    in
    assert (Option.is_some res_trans_callee.method_name) ;
    let is_cpp_call_virtual = res_trans_callee.is_cpp_call_virtual in
    cxx_method_construct_call_trans trans_state_pri res_trans_callee [] si' StdTyp.void
      ~is_injected_destructor ~is_cpp_call_virtual None ~is_inherited_ctor:false


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
      Some (mk_trans_result (sizeof_expr_class class_name) empty_control)
    else if
      (* alloc or new *)
      String.equal selector CFrontend_config.alloc
      (* allocWithZone is like alloc: This method exists for
         historical reasons; memory zones are no longer used by
         Objective-C. *)
      || String.equal selector CFrontend_config.allocWithZone
      || String.equal selector CFrontend_config.new_str
    then
      match receiver_kind with
      | `Class qual_type ->
          let class_opt =
            CMethod_trans.get_class_name_method_call_from_clang context.CContext.tenv
              obj_c_message_expr_info
          in
          let tenv = trans_state.context.CContext.tenv in
          let function_type = CType_decl.qual_type_to_sil_type tenv qual_type in
          Some (new_or_alloc_trans trans_state_pri sil_loc si function_type class_opt selector)
      | `SuperClass ->
          let class_name =
            CMethod_trans.get_class_name_method_call_from_receiver_kind context
              obj_c_message_expr_info act_params
          in
          let function_type = Typ.mk (Typ.Tstruct class_name) in
          Some
            (new_or_alloc_trans trans_state_pri sil_loc si function_type (Some class_name) selector)
      | _ ->
          None
    else None


  and exec_instruction_with_trans_state trans_state_param callee_ms_opt i stmt =
    let trans_state_param' = add_block_as_arg_attributes trans_state_param callee_ms_opt i in
    exec_with_glvalue_as_reference instruction trans_state_param' stmt


  (** If the first argument of the call is self in a static context, remove it as an argument and
      change the call from instance to static *)
  and objCMessageExpr_translate_args trans_state_param args obj_c_message_expr_info callee_ms_opt =
    match args with
    | stmt :: rest -> (
        let param_trans_results =
          List.mapi ~f:(exec_instruction_with_trans_state trans_state_param callee_ms_opt) rest
        in
        let trans_state_param' =
          { trans_state_param with
            is_fst_arg_objc_instance_method_call=
              is_receiver_instance obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind }
        in
        match CTrans_utils.should_remove_first_param trans_state_param' stmt with
        | Some class_name ->
            let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
            let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
            let obj_c_message_expr_info =
              Ast_expressions.make_obj_c_message_expr_info_class selector class_name pointer
            in
            (obj_c_message_expr_info, param_trans_results)
        | None ->
            let fst_res_trans =
              exec_instruction_with_trans_state trans_state_param' callee_ms_opt 0 stmt
            in
            (obj_c_message_expr_info, fst_res_trans :: param_trans_results) )
    | [] ->
        (obj_c_message_expr_info, [])


  and objCMessageExpr_trans trans_state si obj_c_message_expr_info stmt_list expr_info =
    let context = trans_state.context in
    let sil_loc = CLocation.location_of_stmt_info context.translation_unit_context.source_file si in
    let method_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let method_type = add_reference_if_glvalue method_type_no_ref expr_info in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let callee_ms_opt =
      match obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
      | Some pointer ->
          CMethod_trans.method_signature_of_pointer context.tenv pointer
      | None ->
          None
    in
    let trans_state_param =
      {trans_state_pri with succ_nodes= []; var_exp_typ= None; opaque_exp= None}
    in
    let obj_c_message_expr_info, res_trans_subexpr_list =
      objCMessageExpr_translate_args trans_state_param stmt_list obj_c_message_expr_info
        callee_ms_opt
    in
    let subexpr_exprs = collect_returns res_trans_subexpr_list in
    let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
    let node_name = Procdesc.Node.MessageCall selector in
    match
      objCMessageExpr_trans_special_cases trans_state si obj_c_message_expr_info method_type
        trans_state_pri sil_loc subexpr_exprs
    with
    | Some res ->
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc node_name si
          ~return:res.return (res_trans_subexpr_list @ [res])
    | None ->
        let procname = Procdesc.get_proc_name context.CContext.procdesc in
        let callee_name, method_call_type =
          get_callee_objc_method context obj_c_message_expr_info callee_ms_opt subexpr_exprs
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
            ~is_inherited_ctor:false
        in
        let assertion_trans_opt =
          if CTrans_models.is_handleFailureInMethod selector then
            Some (CTrans_utils.trans_assertion trans_state sil_loc)
          else None
        in
        let all_res_trans =
          res_trans_subexpr_list @ (res_trans_call :: Option.to_list assertion_trans_opt)
        in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc node_name si
          ~return:res_trans_call.return all_res_trans


  and inject_base_class_destructor_calls trans_state stmt_info bases obj_sil this_qual_type =
    List.rev_filter_map bases ~f:(fun base ->
        let this_res_trans_destruct = mk_trans_result (obj_sil, this_qual_type) empty_control in
        get_destructor_decl_ref base
        |> Option.map ~f:(fun base_destructor_decl_ref ->
               cxx_destructor_call_trans trans_state stmt_info this_res_trans_destruct
                 base_destructor_decl_ref ~is_injected_destructor:true ~is_inner_destructor:true ) )


  and add_this_instrs_if_result_non_empty res_trans this_res_trans =
    if not (List.is_empty res_trans) then
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
      (* compute [this] once that is used for all destructor calls of virtual base class *)
      let obj_sil, this_qual_type, this_res_trans =
        compute_this_expr {trans_state with var_exp_typ= None} stmt_info_loc
      in
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
        (PriorityNode.compute_results_to_parent trans_state_pri sil_loc
           (Destruction DestrVirtualBase) stmt_info_loc ~return:(mk_fresh_void_exp_typ ())
           all_res_trans )


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
      (* compute [this] once that is used for all destructors of fields and base classes *)
      let obj_sil, this_qual_type, this_res_trans =
        compute_this_expr {trans_state with var_exp_typ= None} stmt_info_loc
      in
      (* ReturnStmt claims a priority with the same [stmt_info].
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
              let field_name = Fieldname.make class_tname ni_name in
              let field_exp = Exp.Lfield (obj_sil, field_name, this_qual_type) in
              let field_typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
              let this_res_trans_destruct = mk_trans_result (field_exp, field_typ) empty_control in
              get_destructor_decl_ref qual_type.Clang_ast_t.qt_type_ptr
              |> Option.map ~f:(fun destructor_decl_ref ->
                     cxx_destructor_call_trans trans_state_pri stmt_info_loc this_res_trans_destruct
                       destructor_decl_ref ~is_injected_destructor:true ~is_inner_destructor:false )
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
        (PriorityNode.compute_results_to_parent trans_state_pri sil_loc (Destruction DestrFields)
           stmt_info' ~return:(mk_fresh_void_exp_typ ()) all_res_trans )


  and destructor_calls destr_kind trans_state stmt_info vars_to_destroy =
    if List.is_empty vars_to_destroy then None
    else
      let context = trans_state.context in
      (* The source location of destructor should reflect the end of the statement *)
      let _, sloc2 = stmt_info.Clang_ast_t.si_source_range in
      let stmt_info_loc = {stmt_info with Clang_ast_t.si_source_range= (sloc2, sloc2)} in
      (* ReturnStmt claims a priority with the same [stmt_info].
         New pointer is generated to avoid premature node creation *)
      let stmt_info' =
        {stmt_info_loc with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
      in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let all_res_trans =
        L.debug Capture Verbose "Destroying pointer %d@\n" stmt_info.Clang_ast_t.si_pointer ;
        List.filter_map vars_to_destroy ~f:(function {CContext.pvar; typ; qual_type} ->
            let exp = Exp.Lvar pvar in
            let this_res_trans_destruct = mk_trans_result (exp, typ) empty_control in
            get_destructor_decl_ref qual_type.Clang_ast_t.qt_type_ptr
            |> Option.map ~f:(fun destructor_decl_ref ->
                   cxx_destructor_call_trans trans_state_pri stmt_info_loc this_res_trans_destruct
                     destructor_decl_ref ~is_injected_destructor:true ~is_inner_destructor:false ) )
      in
      if List.is_empty all_res_trans then None
      else
        let sil_loc =
          CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info_loc
        in
        Some
          (PriorityNode.compute_results_to_parent trans_state_pri sil_loc (Destruction destr_kind)
             stmt_info' ~return:(mk_fresh_void_exp_typ ()) all_res_trans )


  and inject_destructors destr_kind trans_state stmt_info =
    let context = trans_state.context in
    if not (CGeneral_utils.is_cpp_translation context.translation_unit_context) then None
    else
      match
        CContext.StmtMap.find context.CContext.vars_to_destroy stmt_info.Clang_ast_t.si_pointer
      with
      | None ->
          L.(debug Capture Verbose) "@\nNo variables going out of scope here.@\n" ;
          None
      | Some var_decls_to_destroy ->
          let procname = Procdesc.get_proc_name context.CContext.procdesc in
          let vars_to_destroy =
            List.map var_decls_to_destroy ~f:(function
              | CContext.VarDecl ((_, _, qual_type, _) as var_decl) ->
                  let pvar =
                    CVar_decl.sil_var_of_decl context (Clang_ast_t.VarDecl var_decl) procname
                  in
                  let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
                  {CContext.pvar; typ; qual_type; marker= None}
              | CContext.CXXTemporary cxx_temporary ->
                  cxx_temporary )
          in
          destructor_calls destr_kind trans_state stmt_info vars_to_destroy


  and compoundStmt_trans trans_state stmt_list =
    let compound_control, returns = instructions Procdesc.Node.CompoundStmt trans_state stmt_list in
    mk_trans_result (last_or_mk_fresh_void_exp_typ returns) compound_control


  and conditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let procdesc = context.CContext.procdesc in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let do_branch trans_state branch stmt var_typ prune_nodes set_pvar =
      let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
      let res_trans_b = instruction trans_state_pri stmt in
      let all_res_trans, return =
        match set_pvar with
        | `ParentExp ((exp, _typ) as exp_typ)
          when List.mem ~equal:Exp.equal res_trans_b.control.initd_exps exp ->
            ([res_trans_b], exp_typ)
        | _ ->
            let exp_to_init =
              match set_pvar with `ParentExp (exp, _) -> exp | `Temp pvar -> Exp.Lvar pvar
            in
            let init_exp =
              match res_trans_b.return with
              | _, {Typ.desc= Tvoid} ->
                  (* void return *) Exp.Var (Ident.create_fresh Ident.knormal)
              | exp, _ ->
                  exp
            in
            let set_temp_var =
              match var_typ with
              | {Typ.desc= Tstruct struct_name} ->
                  CStructUtils.struct_copy context.CContext.tenv sil_loc exp_to_init init_exp
                    ~typ:var_typ ~struct_name
              | _ ->
                  [Sil.Store {e1= exp_to_init; typ= var_typ; e2= init_exp; loc= sil_loc}]
            in
            let return = (exp_to_init, var_typ) in
            let tmp_var_res_trans =
              mk_trans_result return
                {empty_control with instrs= set_temp_var; initd_exps= [exp_to_init]}
            in
            ([res_trans_b; tmp_var_res_trans], return)
      in
      let res_trans =
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ConditionalStmtBranch
          stmt_info ~return all_res_trans
      in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter
        ~f:(fun n ->
          Procdesc.node_set_succs context.procdesc n ~normal:res_trans.control.root_nodes ~exn:[] )
        prune_nodes' ;
      res_trans
    in
    let[@warning "-partial-match"] [cond; exp1; exp2] = stmt_list in
    let typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    let var_typ = add_reference_if_glvalue typ expr_info in
    let join_node =
      Procdesc.create_node trans_state.context.CContext.procdesc sil_loc Join_node
        [Sil.Metadata EndBranches]
    in
    Procdesc.node_set_succs context.procdesc join_node ~normal:succ_nodes ~exn:[] ;
    let var_exp_typ =
      match trans_state.var_exp_typ with
      | Some var_exp_typ ->
          `ParentExp var_exp_typ
      | None ->
          let pvar = CVar_decl.mk_temp_sil_var procdesc ~name:"SIL_temp_conditional___" in
          let var_data = ProcAttributes.default_var_data pvar var_typ in
          Procdesc.append_locals procdesc [var_data] ;
          `Temp pvar
    in
    let continuation' = mk_cond_continuation trans_state.continuation in
    let res_trans_cond =
      let trans_state' =
        {trans_state with continuation= continuation'; succ_nodes= []; var_exp_typ= None}
      in
      exec_with_priority_exception trans_state' cond
        (cond_trans ~if_kind:(Sil.Ik_bexp {terminated= true}) ~negate_cond:false)
    in
    let trans_state = {trans_state with succ_nodes= [join_node]} in
    (* Note: by contruction prune nodes are leafs_nodes_cond *)
    let res_then_branch =
      do_branch trans_state true exp1 var_typ res_trans_cond.control.leaf_nodes var_exp_typ
    in
    let res_else_branch =
      do_branch trans_state false exp2 var_typ res_trans_cond.control.leaf_nodes var_exp_typ
    in
    let instrs, return =
      match var_exp_typ with
      | `ParentExp var_exp_typ ->
          ([], var_exp_typ)
      | `Temp pvar ->
          let id = Ident.create_fresh Ident.knormal in
          let instrs = [Sil.Load {id; e= Lvar pvar; typ= var_typ; loc= sil_loc}] in
          (instrs, (Exp.Var id, typ))
    in
    let initd_exps =
      (* Intersect initialized expressions in [then] branch with those in [else] branch. To
         avoid a quadratic time complexity of unsorted list intersection, pre-process one of
         them ([then]) into a set. *)
      let then_initd_exps = Exp.Set.of_list res_then_branch.control.initd_exps in
      let both_branches_initd_exps =
        List.filter res_else_branch.control.initd_exps ~f:(fun e -> Exp.Set.mem e then_initd_exps)
      in
      res_trans_cond.control.initd_exps @ both_branches_initd_exps
    in
    let cxx_temporary_markers_set =
      res_trans_cond.control.cxx_temporary_markers_set
      @ res_then_branch.control.cxx_temporary_markers_set
      @ res_else_branch.control.cxx_temporary_markers_set
    in
    mk_trans_result return
      { root_nodes= res_trans_cond.control.root_nodes
      ; leaf_nodes= [join_node]
      ; instrs
      ; initd_exps
      ; cxx_temporary_markers_set }


  (** The GNU extension to the conditional operator which allows the middle operand to be omitted. *)
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
          { trans_state_pri with
            continuation= mk_cond_continuation trans_state_pri.continuation
          ; var_exp_typ= None }
        in
        (* Evaluate [stmt1] once. Then, use it as replacement for [OpaqueValueExpr] when translating
           [ostmt1] and [ostmt2]. *)
        let init_res_trans = instruction trans_state_cond stmt1 in
        L.debug Capture Verbose "init_res_trans.control=%a@\n" pp_control init_res_trans.control ;
        let opaque_exp = init_res_trans.return in
        let trans_state' = {trans_state_pri with opaque_exp= Some opaque_exp} in
        let op_res_trans =
          conditionalOperator_trans trans_state' stmt_info [ostmt1; ostmt2; stmt2] expr_info
        in
        L.debug Capture Verbose "op_res_trans.control=%a@\n" pp_control op_res_trans.control ;
        let init_res_trans =
          PriorityNode.compute_results_to_parent trans_state_pri sil_loc
            ~return:init_res_trans.return BinaryConditionalStmtInit stmt_info [init_res_trans]
        in
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc ~return:op_res_trans.return
          BinaryConditionalStmtInit stmt_info [init_res_trans; op_res_trans]
    | _ ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "BinaryConditionalOperator not translated"


  (** Translate a condition for if/loops statement. It shorts-circuit and/or. The invariant is that
      the translation of a condition always contains (at least) the prune nodes. Moreover these are
      always the leaf nodes of the translation. *)
  and cond_trans ~if_kind ~negate_cond trans_state cond : trans_result =
    L.debug Capture Verbose "cond_trans trans_state: %a@\n" pp_trans_state trans_state ;
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
    let no_short_circuit_cond ~is_cmp cond =
      L.debug Capture Verbose "No short-circuit condition@\n" ;
      let res_trans_cond =
        if is_null_stmt cond then mk_trans_result (Exp.one, Typ.mk (Tint IBool)) empty_control
          (* Assumption: If it's a null_stmt, it is a loop with no bound, so we set condition to 1 *)
        else if is_cmp then
          let open Clang_ast_t in
          (* If we have a comparison here, do not dispatch it to [instruction] function, which
             invokes binaryOperator_trans_with_cond -> conditionalOperator_trans -> cond_trans.
             This will throw the translation process into an infinite loop immediately.  Instead,
             dispatch to binaryOperator_trans directly. *)
          (* If one wants to add a new kind of [BinaryOperator] that will have the same behavior,
             she need to change both the codes here and the [match] in
             binaryOperator_trans_with_cond *)
          match cond with
          | BinaryOperator (si, ss, ei, boi)
          | ExprWithCleanups (_, [BinaryOperator (si, ss, ei, boi)], _, _) ->
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
        ~f:(fun n' -> Procdesc.node_set_succs context.procdesc n' ~normal:[prune_t; prune_f] ~exn:[])
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
          Procdesc.node_set_succs context.procdesc n ~normal:res_trans_s2.control.root_nodes ~exn:[] )
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
        ; initd_exps= []
        ; cxx_temporary_markers_set= [] }
    in
    L.debug Capture Verbose "Translating Condition for If-then-else/Loop/Conditional Operator@\n" ;
    let open Clang_ast_t in
    match cond with
    | BinaryOperator (_, [s1; s2], _, boi)
    | ExprWithCleanups (_, [BinaryOperator (_, [s1; s2], _, boi)], _, _) -> (
      match boi.boi_kind with
      | `LAnd ->
          short_circuit (if negate_cond then Binop.LOr else Binop.LAnd) s1 s2
      | `LOr ->
          short_circuit (if negate_cond then Binop.LAnd else Binop.LOr) s1 s2
      | `LT | `GT | `LE | `GE | `EQ | `NE ->
          no_short_circuit_cond ~is_cmp:true cond
      | _ ->
          no_short_circuit_cond ~is_cmp:false cond )
    | ParenExpr (_, [s], _) ->
        (* condition can be wrapped in parentheses *)
        cond_trans ~if_kind ~negate_cond trans_state s
    | UnaryOperator (_, [s], _, {uoi_kind= `LNot}) ->
        cond_trans ~if_kind ~negate_cond:(not negate_cond) trans_state s
    | ExprWithCleanups (_, [s], _, _)
    (* Skip destructors of temporaries inside conditionals otherwise
       we would destroy them before dereferencing them in the prune
       nodes. A better fix probably exists. *)
    | s ->
        no_short_circuit_cond ~is_cmp:false s


  and declStmt_in_condition_trans trans_state decl_stmt res_trans_cond =
    match decl_stmt with
    | Clang_ast_t.DeclStmt (stmt_info, _, decl_list) ->
        let trans_state_decl = {trans_state with succ_nodes= res_trans_cond.control.root_nodes} in
        declStmt_trans trans_state_decl decl_list stmt_info
    | _ ->
        res_trans_cond


  and ifStmt_trans trans_state stmt_info (if_stmt_info : Clang_ast_t.if_stmt_info) =
    let context = trans_state.context in
    let source_range = stmt_info.Clang_ast_t.si_source_range in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let do_branch branch stmt_branch prune_nodes trans_state =
      (* leaf nodes are ignored here as they will be already attached to join_node *)
      let res_trans_b = instruction trans_state stmt_branch in
      let nodes_branch =
        match res_trans_b.control.root_nodes with
        | [] ->
            [ Procdesc.create_node context.procdesc sil_loc (Stmt_node IfStmtBranch)
                res_trans_b.control.instrs ]
        | _ ->
            res_trans_b.control.root_nodes
      in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter prune_nodes' ~f:(fun n ->
          Procdesc.node_set_succs context.procdesc n ~normal:nodes_branch ~exn:[] )
    in
    let join_node =
      Procdesc.create_node context.procdesc sil_loc Join_node [Sil.Metadata EndBranches]
    in
    Procdesc.node_set_succs context.procdesc join_node ~normal:trans_state.succ_nodes ~exn:[] ;
    let trans_state_join_succ = {trans_state with succ_nodes= [join_node]} in
    (* translate the condition expression *)
    let res_trans_cond =
      (* set the flag to inform that we are translating a condition of an "if" *)
      let continuation' = mk_cond_continuation trans_state.continuation in
      let trans_state'' = {trans_state with continuation= continuation'; succ_nodes= []} in
      let cond_stmt = CAst_utils.get_stmt_exn if_stmt_info.isi_cond source_range in
      cond_trans ~if_kind:(Sil.Ik_if {terminated= true}) ~negate_cond:false trans_state'' cond_stmt
    in
    (* translate the variable declaration inside the condition if present *)
    let res_trans_cond_var =
      match if_stmt_info.isi_cond_var with
      | Some cond_var ->
          declStmt_in_condition_trans trans_state cond_var res_trans_cond
      | None ->
          res_trans_cond
    in
    let then_body = CAst_utils.get_stmt_exn if_stmt_info.isi_then source_range in
    (* Note: by contruction prune nodes are leafs_nodes_cond *)
    do_branch true then_body res_trans_cond.control.leaf_nodes trans_state_join_succ ;
    let else_body =
      match if_stmt_info.isi_else with
      | None ->
          Clang_ast_t.NullStmt ({stmt_info with si_pointer= CAst_utils.get_fresh_pointer ()}, [])
      | Some (else_body_ptr, _) ->
          CAst_utils.get_stmt_exn else_body_ptr source_range
    in
    do_branch false else_body res_trans_cond.control.leaf_nodes trans_state_join_succ ;
    (* translate the initialisation if present *)
    let res_trans_init =
      match if_stmt_info.isi_init with
      | Some init_stmt_ptr ->
          let init_stmt = CAst_utils.get_stmt_exn init_stmt_ptr source_range in
          instruction {trans_state with succ_nodes= res_trans_cond_var.control.root_nodes} init_stmt
      | None ->
          res_trans_cond_var
    in
    let root_nodes = res_trans_init.control.root_nodes in
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes; leaf_nodes= [join_node]}


  and caseStmt_trans trans_state stmt_info case_stmt_list =
    let condition, body =
      match case_stmt_list with
      | [condition; body] ->
          (condition, body)
      | [condition; _rhs; body] ->
          (* ignore the [case lhs ... rhs: body] form, only support the [case condition: body] form *)
          (condition, body)
      | _ ->
          assert false
    in
    L.debug Capture Verbose "translating a caseStmt@\n" ;
    let body_trans_result = exec_with_node_creation CaseStmt ~f:instruction trans_state body in
    L.debug Capture Verbose "result of translating a caseStmt: %a@\n" pp_control
      body_trans_result.control ;
    SwitchCase.add
      {condition= Case condition; stmt_info; root_nodes= body_trans_result.control.root_nodes} ;
    body_trans_result


  and defaultStmt_trans trans_state stmt_info default_stmt_list =
    let[@warning "-partial-match"] [body] = default_stmt_list in
    let body_trans_result = instruction trans_state body in
    (let open SwitchCase in
     add {condition= Default; stmt_info; root_nodes= body_trans_result.control.root_nodes} ) ;
    body_trans_result


  and switchStmt_trans trans_state stmt_info switch_stmt_info =
    (* overview: translate the body of the switch statement, which automatically collects the
       various cases at the same time, then link up the cases together and together with the switch
       condition variable *)
    (* unsupported: initialization *)
    let condition =
      CAst_utils.get_stmt_exn switch_stmt_info.Clang_ast_t.ssi_cond
        stmt_info.Clang_ast_t.si_source_range
    in
    let body =
      CAst_utils.get_stmt_exn switch_stmt_info.Clang_ast_t.ssi_body
        stmt_info.Clang_ast_t.si_source_range
    in
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
      ~f:(fun n' -> Procdesc.node_set_succs context.procdesc n' ~normal:[switch_node] ~exn:[])
      res_trans_cond_tmp.control.leaf_nodes ;
    let root_nodes =
      if not (List.is_empty res_trans_cond_tmp.control.root_nodes) then
        res_trans_cond_tmp.control.root_nodes
      else [switch_node]
    in
    let condition_exp, _ = res_trans_cond_tmp.return in
    let condition_result =
      { res_trans_cond_tmp with
        control= {res_trans_cond_tmp.control with root_nodes; leaf_nodes= [switch_node]} }
    in
    let variable_result =
      match switch_stmt_info.Clang_ast_t.ssi_cond_var with
      | None ->
          condition_result
      | Some variable ->
          declStmt_in_condition_trans trans_state variable condition_result
    in
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
    let is_all_enum_cases_covered = switch_stmt_info.Clang_ast_t.ssi_is_all_enum_cases_covered in
    let link_up_switch_cases (curr_succ_nodes, is_last_case) case =
      L.debug Capture Verbose "switch: curr_succ_nodes=[%a], linking case %a@\n"
        (Pp.semicolon_seq Procdesc.Node.pp)
        curr_succ_nodes SwitchCase.pp case ;
      let curr_succ_nodes =
        match (case : SwitchCase.t) with
        | {SwitchCase.condition= Case case_condition; stmt_info; root_nodes} ->
            (* create case prune nodes, link the then branch to [root_nodes], the else branch to
               [curr_succ_nodes] *)
            let trans_state_pri =
              PriorityNode.try_claim_priority_node inner_trans_state stmt_info
            in
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
            Procdesc.node_set_succs context.procdesc true_prune_node ~normal:root_nodes ~exn:[] ;
            if is_last_case && is_all_enum_cases_covered then
              (* return only the true branch as next roots because the false branch is infeasible *)
              [true_prune_node]
            else
              let false_prune_node =
                create_prune_node context.procdesc ~branch:false ~negate_cond:true sil_eq_cond
                  res_trans_case_const.control.instrs sil_loc Sil.Ik_switch
              in
              Procdesc.node_set_succs context.procdesc false_prune_node ~normal:curr_succ_nodes
                ~exn:[] ;
              (* return prune nodes as next roots *)
              [true_prune_node; false_prune_node]
        | {SwitchCase.condition= Default; root_nodes} ->
            (* just return the [root_nodes] to be linked to the previous case's fallthrough *)
            root_nodes
      in
      (curr_succ_nodes, false)
    in
    let switch_cases =
      (* move the default case to the last in the list of cases, which is the first in
         [switch_cases] since the list is reversed by default *)
      let default, cases =
        List.partition_tf switch_cases ~f:(function
          | {SwitchCase.condition= Default} ->
              true
          | {SwitchCase.condition= Case _} ->
              false )
      in
      if is_all_enum_cases_covered then
        (* when all enum cases covered, the default case is infeasible *)
        cases
      else default @ cases
    in
    let cases_root_nodes, _ =
      List.fold switch_cases ~init:(trans_state.succ_nodes, true) ~f:link_up_switch_cases
    in
    Procdesc.node_set_succs context.procdesc switch_node ~normal:cases_root_nodes ~exn:[] ;
    let top_nodes = variable_result.control.root_nodes in
    mk_trans_result (mk_fresh_void_exp_typ ())
      {empty_control with root_nodes= top_nodes; leaf_nodes= []}


  and stmtExpr_trans trans_state source_range stmt_list =
    let stmt =
      extract_stmt_from_singleton stmt_list source_range "StmtExpr should have only one statement."
    in
    let trans_state' = {trans_state with priority= Free} in
    instruction trans_state' stmt


  and tryStmt_trans ({context= {procdesc; translation_unit_context= {source_file}}} as trans_state)
      ({Clang_ast_t.si_pointer= try_id; si_source_range} as stmt_info) stmts =
    let open Clang_ast_t in
    let translate_catch catch_root_nodes_acc = function
      | CXXCatchStmt (_, catch_body_stmts, _) ->
          let catch_trans_result = compoundStmt_trans trans_state catch_body_stmts in
          (* no risk of duplicates because two catch blocks should never have the same root nodes
             (they have to be in different syntactic locations, after all!) *)
          catch_trans_result.control.root_nodes @ catch_root_nodes_acc
      | _ ->
          assert false
    in
    match stmts with
    | try_body_stmt :: catch_stmts ->
        let try_loc =
          CLocation.location_of_source_range ~pick_location:`Start source_file si_source_range
        in
        let try_exit_node =
          Procdesc.create_node procdesc try_loc (Stmt_node CXXTry)
            [Metadata (TryExit {try_id; loc= try_loc})]
        in
        let try_trans_result =
          PriorityNode.force_sequential try_loc CXXTry trans_state stmt_info
            ~mk_first_opt:(fun _ _ ->
              Some
                (mk_trans_result (mk_fresh_void_exp_typ ())
                   {empty_control with instrs= [Metadata (TryEntry {try_id; loc= try_loc})]} ) )
            ~mk_second:(fun _ _ ->
              instruction {trans_state with succ_nodes= [try_exit_node]} try_body_stmt )
            ~mk_return:(fun ~fst:_ ~snd -> snd.return)
        in
        let catch_start_nodes = List.fold catch_stmts ~f:translate_catch ~init:[] in
        let catch_entry_node =
          Procdesc.create_node procdesc try_loc (Stmt_node CXXTry)
            [Metadata (CatchEntry {try_id; loc= try_loc})]
        in
        (* add catch block as exceptional successor to end of try block. not ideal, but we will at
           least reach the code in the catch block this way *)
        (* TODO (T28898377): instead, we should extend trans_state with a list of maybe-throwing
           blocks, and add transitions from those to the catch block instead *)
        if List.is_empty (Procdesc.Node.get_preds try_exit_node) then (
          (* Add exception edges forcibly when [try_exit_node] does not have a predecessor.  This
             can happen when the [try_body_stmt] ends with [return] always.  Note that [TryEntry]
             and [TryExit] may mismatch in this case. *)
          List.iter try_trans_result.control.leaf_nodes ~f:(fun leaf ->
              Procdesc.set_succs leaf ~normal:None ~exn:(Some [catch_entry_node]) ) ;
          Procdesc.remove_node procdesc try_exit_node )
        else
          Procdesc.set_succs try_exit_node ~normal:(Some trans_state.succ_nodes)
            ~exn:(Some [catch_entry_node]) ;
        Procdesc.set_succs catch_entry_node ~normal:(Some catch_start_nodes) ~exn:None ;
        let try_control = try_trans_result.control in
        {try_trans_result with control= {try_control with leaf_nodes= [try_exit_node]}}
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
    let join_node = Procdesc.create_node context.procdesc sil_loc Join_node [] in
    let continuation = {break= succ_nodes; continue= [join_node]; return_temp= false} in
    (* set the flag to inform that we are translating a condition of a if *)
    let continuation_cond = mk_cond_continuation outer_continuation in
    let init_nodes, incr_nodes =
      match loop_kind with
      | Loops.For {init; increment} ->
          let trans_state' =
            {trans_state with succ_nodes= [join_node]; continuation= Some continuation}
          in
          let res_trans_init =
            exec_with_node_creation LoopIterInit ~f:instruction trans_state' init
          in
          let res_trans_incr =
            exec_with_node_creation LoopIterIncr ~f:instruction trans_state' increment
          in
          (Some res_trans_init.control.root_nodes, Some res_trans_incr.control.root_nodes)
      | _ ->
          (None, None)
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
      | Loops.For {decl_stmt} | Loops.While {decl_stmt= Some decl_stmt} ->
          declStmt_in_condition_trans trans_state decl_stmt res_trans_cond
      | Loops.While {decl_stmt= None} | Loops.DoWhile _ ->
          res_trans_cond
    in
    let body_succ_nodes =
      match loop_kind with
      | Loops.For _ ->
          Option.value_exn incr_nodes
      | Loops.While _ ->
          [join_node]
      | Loops.DoWhile _ ->
          res_trans_cond.control.root_nodes
    in
    let body_continuation =
      match loop_kind with
      | Loops.DoWhile _ ->
          {continuation with continue= res_trans_cond.control.root_nodes}
      | Loops.For _ ->
          {continuation with continue= Option.value_exn incr_nodes}
      | _ ->
          continuation
    in
    let res_trans_body =
      let trans_state_body =
        {trans_state with succ_nodes= body_succ_nodes; continuation= Some body_continuation}
      in
      exec_with_node_creation LoopBody ~f:instruction trans_state_body (Loops.get_body loop_kind)
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
    Procdesc.node_set_succs context.procdesc join_node ~normal:join_succ_nodes ~exn:[] ;
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs context.procdesc n ~normal:prune_t_succ_nodes ~exn:[])
      prune_nodes_t ;
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs context.procdesc n ~normal:succ_nodes ~exn:[])
      prune_nodes_f ;
    let root_nodes =
      match loop_kind with
      | Loops.For _ ->
          let init_nodes = Option.value_exn init_nodes in
          if List.is_empty init_nodes then [join_node] else init_nodes
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


  (** Iteration over collections

      [for (v : C) { body; }] is translated as:

      {[
        TypeC __range = C;
        for (__begin = __range.begin(), __end = __range.end();
             __begin != __end;
             ++__begin)
            {
              v = *__begin;
              loop_body;
            }
      ]} *)
  and cxxForRangeStmt_trans trans_state stmt_info stmt_list =
    let open Clang_ast_t in
    match stmt_list with
    | [ _init
      ; iterator_decl
      ; begin_stmt
      ; end_stmt
      ; exit_cond
      ; increment
      ; assign_current_index
      ; loop_body ] ->
        (* do not use [stmt_info]'s pointer again because we use a pointer map to assign lists of
           variables to destruct to each statement and so re-using pointers can be problematic and
           lead to multiple destructions of the same variables *)
        let stmt_info = {stmt_info with si_pointer= CAst_utils.get_fresh_pointer ()} in
        let loop_body' = CompoundStmt (stmt_info, [assign_current_index; loop_body]) in
        let null_stmt = NullStmt (stmt_info, []) in
        let beginend_stmt = CompoundStmt (stmt_info, [begin_stmt; end_stmt]) in
        let for_loop =
          ForStmt (stmt_info, [beginend_stmt; null_stmt; exit_cond; increment; loop_body'])
        in
        instruction trans_state (CompoundStmt (stmt_info, [iterator_decl; for_loop]))
    | _ ->
        assert false


  (** Fast iteration for collections

      [for (item_type item in items) { body }] is translated as

      {[
        NSEnumerator *enumerator = [items objectEnumerator];
        item_type item;
        while (item = [enumerator nextObject]) { body }
      ]} *)
  and objCForCollectionStmt_trans ({context= {procdesc}} as trans_state) item items body stmt_info =
    match item with
    | Clang_ast_t.DeclRefExpr
        ( _
        , _
        , _
        , { drti_decl_ref=
              Some
                { dr_decl_pointer= item_pointer
                ; dr_name= Some item_ni
                ; dr_qual_type= Some item_qual_type } } )
    | Clang_ast_t.DeclStmt (_, _, [VarDecl ({di_pointer= item_pointer}, item_ni, item_qual_type, _)])
      ->
        let enumerator_type =
          Ast_expressions.create_class_pointer_qual_type
            (Typ.Name.Objc.from_string CFrontend_config.nsenumerator_cl)
        in
        let enumerator_pointer = CAst_utils.get_fresh_pointer () in
        let enumerator_ni =
          let pvar = Pvar.mk_tmp "__enumerator_" (Procdesc.get_proc_name procdesc) in
          Ast_expressions.create_named_decl_info (Pvar.to_string pvar)
        in
        let enumerator_decl =
          let object_enumerator_objc_message_expr =
            Ast_expressions.create_obj_c_message_expr stmt_info enumerator_type
              CFrontend_config.object_enumerator [items]
          in
          let var_decl =
            Clang_ast_t.VarDecl
              ( Ast_expressions.create_decl_info stmt_info enumerator_pointer
              , enumerator_ni
              , enumerator_type
              , { Ast_expressions.default_var_decl_info with
                  vdi_init_expr= Some object_enumerator_objc_message_expr } )
          in
          Clang_ast_t.DeclStmt (stmt_info, [object_enumerator_objc_message_expr], [var_decl])
        in
        let while_stmt =
          let item_expr =
            Ast_expressions.create_decl_ref_expr stmt_info item_pointer item_ni item_qual_type
          in
          let enumerator_expr =
            let enumerator_decl_ref_expr =
              Ast_expressions.create_decl_ref_expr stmt_info enumerator_pointer enumerator_ni
                enumerator_type
            in
            Ast_expressions.create_implicit_cast_expr stmt_info [enumerator_decl_ref_expr]
              enumerator_type `LValueToRValue
          in
          let next_object_call =
            Ast_expressions.create_obj_c_message_expr stmt_info enumerator_type
              CFrontend_config.next_object [enumerator_expr]
          in
          let cond =
            Clang_ast_t.BinaryOperator
              ( stmt_info
              , [item_expr; next_object_call]
              , {ei_qual_type= item_qual_type; ei_value_kind= `RValue; ei_object_kind= `Ordinary}
              , {boi_kind= `Assign} )
          in
          Clang_ast_t.WhileStmt (stmt_info, [cond; body])
        in
        instruction trans_state
          (Clang_ast_t.CompoundStmt (stmt_info, [enumerator_decl; item; while_stmt]))
    | _ ->
        L.debug Capture Medium "Couldn't translate ObjCForCollectionStmt properly: %s@\n"
          (Clang_ast_j.string_of_stmt_info stmt_info) ;
        mk_trans_result (mk_fresh_void_exp_typ ()) empty_control


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


  and initListExpr_struct_trans trans_state stmt_info stmts init_expr_typ var_exp var_typ =
    let context = trans_state.context in
    let tenv = context.tenv in
    let tname = match init_expr_typ.Typ.desc with Tstruct tname -> tname | _ -> assert false in
    let field_exps, supers =
      match Tenv.lookup tenv tname with
      | Some {fields; supers} ->
          ( List.map fields ~f:(fun {Struct.name= fieldname; typ= fieldtype} ->
                (Exp.Lfield (var_exp, fieldname, init_expr_typ), fieldtype) )
          , supers )
      | None ->
          assert false
    in
    let init_field field_exp_typ stmt =
      init_expr_trans trans_state field_exp_typ stmt_info (Some stmt)
    in
    match (supers, stmts) with
    | [super], stmt :: stmts when List.length field_exps >= List.length stmts ->
        (* When [var_typ] has a super class, the lengths of [field_exps] and [stmts] can be
           different.

           - [field_exps] contains all fields including that of the super class, e.g.
             [\[SuperClass.fld1; SuperClass.fld2; CurrentClass.fld1; CurrentClass.fld2\]].

           - On the other hand, [stmts] contains one [stmt] for initializing the super class
             fields, e.g. [\[init_super_class_fields; init_fld1; init_fld2\]]. *)
        let res_super =
          if Int.equal (List.length field_exps) (List.length stmts) then
            (* There is no fields left to initialize. *)
            []
          else if Option.is_none (Tenv.lookup tenv super) then
            (* If there is no information of the super type in tenv, give up the translation. This
               can happen when the super class is declared in a deep header chain. *)
            []
          else
            [ init_expr_trans ~is_declare_variable:false trans_state
                (var_exp, Typ.mk (Tstruct super))
                stmt_info (Some stmt) ]
        in
        let field_exps = List.drop field_exps (List.length field_exps - List.length stmts) in
        res_super @ List.map2_exn field_exps stmts ~f:init_field
    | [], stmts when Int.equal (List.length field_exps) (List.length stmts) ->
        List.map2_exn field_exps stmts ~f:init_field
    | [], [stmt] ->
        (* This handles the case when a single element with a reference type is given.  In that
           case, it loads/store the argument. *)
        [init_expr_trans trans_state (var_exp, var_typ) stmt_info (Some stmt)]
    | _, _ ->
        (* This happens with some braced-init-list for instance; translate each sub-statement so
           as not to lose instructions (we might even get the translation right) *)
        L.debug Capture Medium
          "couldn't translate initListExpr properly: list lengths do not match:@\n\
          \  field_exps is %d: [%a]@\n\
          \  stmts      is %d: [%a]@\n"
          (List.length field_exps)
          (Pp.seq ~sep:"," (Pp.pair ~fst:Exp.pp ~snd:(Typ.pp Pp.text)))
          field_exps (List.length stmts)
          (Pp.seq ~sep:"," (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string))
          stmts ;
        let control, _ = instructions Procdesc.Node.InitListExp trans_state stmts in
        [mk_trans_result (var_exp, var_typ) control]


  and initListExpr_builtin_trans trans_state stmt_info stmts var_exp var_typ =
    match stmts with
    | [stmt] ->
        [ init_expr_trans ~is_declare_variable:false trans_state (var_exp, var_typ) stmt_info
            (Some stmt) ]
    | _ ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "InitListExpression for var %a type %a with multiple init statements" Exp.pp var_exp
          (Typ.pp_full Pp.text) var_typ


  (** InitListExpr can have following meanings:

      - initialize all record fields
      - initialize array
      - initialize primitive type (int/flaot/pointer/...)
      - perform zero initalization -
        {:http://en.cppreference.com/w/cpp/language/zero_initialization} Decision which case happens
        is based on the type of the InitListExpr *)
  and initListExpr_trans ({context= {tenv}} as trans_state) stmt_info
      ({Clang_ast_t.ei_qual_type} as expr_info) stmts =
    let var_exp, var_typ =
      match trans_state.var_exp_typ with
      | Some var_exp_typ ->
          var_exp_typ
      | None ->
          create_var_exp_tmp_var trans_state expr_info ~var_name:"SIL_init_list__"
            ~clang_pointer:stmt_info.Clang_ast_t.si_pointer
    in
    if List.is_empty stmts then
      (* perform zero initialization of a primitive type, record types will have
         ImplicitValueInitExpr nodes *)
      let return_exp = Exp.zero_of_type var_typ |> Option.value ~default:Exp.zero in
      let return = (return_exp, var_typ) in
      mk_trans_result return empty_control
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
        let init_expr_typ = CType_decl.qual_type_to_sil_type tenv ei_qual_type in
        match init_expr_typ.Typ.desc with
        | Tarray {elt} ->
            initListExpr_array_trans trans_state_pri init_stmt_info stmts var_exp elt
        | Tstruct _ ->
            initListExpr_struct_trans trans_state_pri init_stmt_info stmts init_expr_typ var_exp
              var_typ
        | Tint _ | Tfloat _ | Tptr _ ->
            initListExpr_builtin_trans trans_state_pri init_stmt_info stmts var_exp var_typ
        | _ ->
            CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
              "InitListExp for var %a of type %a" Exp.pp var_exp (Typ.pp Pp.text) var_typ
      in
      let res_trans =
        PriorityNode.compute_results_to_parent trans_state_pri sil_loc InitListExp stmt_info
          ~return:(var_exp, var_typ) all_res_trans
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
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc InitializeDynamicArrayLength
      dynlength_stmt_info
      [dynlength_trans_result.control; call_trans_control]
    |> mk_trans_result ret_exp_typ


  and init_expr_trans_aux ~is_structured_binding trans_state var_exp_typ var_stmt_info init_expr =
    (* For init expr, translate how to compute it and assign to the var *)
    let var_exp, var_typ = var_exp_typ in
    let cstruct_name_opt =
      match var_typ.Typ.desc with
      | Tstruct (CStruct _ as struct_name) ->
          Some struct_name
      | _ ->
          None
    in
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file var_stmt_info
    in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state var_stmt_info in
    (* if ie is a block the translation need to be done
       with the block special cases by exec_with_block_priority *)
    let res_trans_ie =
      let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= Some var_exp_typ} in
      let instruction' = exec_with_glvalue_as_reference instruction in
      let init_expr =
        match init_expr with
        | Clang_ast_t.ImplicitCastExpr (_, [init_expr], _, _, _)
          when Option.is_some cstruct_name_opt ->
            init_expr
        | _ ->
            init_expr
      in
      exec_with_block_priority_exception instruction' trans_state' init_expr var_stmt_info
    in
    L.debug Capture Verbose "init expr result: %a@\n" pp_control res_trans_ie.control ;
    let assign_trans_control_opt =
      if
        (* variable might be initialized already - do nothing in that case*)
        List.exists ~f:(Exp.equal var_exp) res_trans_ie.control.initd_exps
      then (
        L.debug Capture Verbose "Sub-expr already initialized %a@\n" Exp.pp var_exp ;
        (* If no node is going to be created for the sub-expr instructions and no node is created
           for initializing [var_exp] then [compute_controls_to_parent] is not going to connect the
           translation of the sub-expression to the [trans_state.succ_nodes] so do it ourselves
           here. Maybe that's a bug in [compute_controls_to_parent] though. *)
        if List.is_empty res_trans_ie.control.instrs then
          List.iter res_trans_ie.control.leaf_nodes ~f:(fun node ->
              Procdesc.node_set_succs context.procdesc node ~normal:trans_state.succ_nodes ~exn:[] ) ;
        None )
      else
        let ((sil_e1', ie_typ) as sil_e1'_typ) = res_trans_ie.return in
        L.debug Capture Verbose "Sub-expr did not initialize %a, initializing with %a@\n" Exp.pp
          var_exp Exp.pp sil_e1' ;
        let instrs =
          if is_structured_binding then
            [ Sil.Call
                ( mk_fresh_void_id_typ ()
                , Const (Cfun BuiltinDecl.__infer_structured_binding)
                , [var_exp_typ; sil_e1'_typ]
                , sil_loc
                , CallFlags.default ) ]
          else
            match cstruct_name_opt with
            | Some struct_name ->
                CStructUtils.struct_copy context.CContext.tenv sil_loc var_exp sil_e1' ~typ:var_typ
                  ~struct_name
            | None ->
                [Sil.Store {e1= var_exp; typ= ie_typ; e2= sil_e1'; loc= sil_loc}]
        in
        Some {empty_control with instrs}
    in
    let all_res_trans = res_trans_ie.control :: Option.to_list assign_trans_control_opt in
    L.debug Capture Verbose "sending init_expr_trans results to parent@\n" ;
    let control =
      PriorityNode.compute_controls_to_parent trans_state_pri sil_loc DeclStmt var_stmt_info
        all_res_trans
    in
    mk_trans_result var_exp_typ control


  and init_expr_trans ?(is_declare_variable = true) ?(is_structured_binding = false) trans_state
      var_exp_typ ?qual_type var_stmt_info init_expr_opt =
    L.debug Capture Verbose "init_expr_trans %b %a %a <?qual_type> <stmt_info> %a@\n"
      is_declare_variable pp_trans_state trans_state
      (Pp.pair ~fst:Exp.pp ~snd:(Typ.pp Pp.text_break))
      var_exp_typ
      (Pp.option (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string))
      init_expr_opt ;
    match init_expr_opt with
    | None -> (
      match Option.bind qual_type ~f:(fun qt -> CAst_utils.get_type qt.Clang_ast_t.qt_type_ptr) with
      | Some (Clang_ast_t.VariableArrayType (_, _, stmt_pointer)) ->
          (* Set the dynamic length of the variable length array. Variable length array cannot
             have an initialization expression. *)
          init_dynamic_array trans_state var_exp_typ var_stmt_info stmt_pointer
      | _ ->
          (* Nothing to do if no init expression and not a variable length array *)
          mk_trans_result var_exp_typ {empty_control with root_nodes= trans_state.succ_nodes} )
    | Some init_expr ->
        let sil_loc =
          CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
            var_stmt_info
        in
        (* force a node creation to place the [VariableLifetimeBegins] instruction before the
           execution of the instructions needed for the initializer, but only if the initializer
           created some nodes, to avoid having the [VariableLifetimeBegins] instruction ending up
           *after* the initialization CFG fragment, as it could itself modify the variable (which
           would then be undone by the [VariableLifetimeBegins] instruction!); otherwise, just
           return the instructions together *)
        PriorityNode.force_sequential sil_loc DeclStmt trans_state var_stmt_info
          ~mk_first_opt:(fun _trans_state _stmt_info ->
            match var_exp_typ with
            | Exp.Lvar pvar, var_typ when is_declare_variable ->
                (* Do not add duplicated variable declaration when we translate InitListExpr
                   as it will be added in the translation of DeclStmt *)
                (* TODO: we shouldn't add that for global variables? *)
                Some
                  (mk_trans_result var_exp_typ
                     { empty_control with
                       instrs=
                         [ Sil.Metadata
                             (VariableLifetimeBegins
                                { pvar
                                ; typ= var_typ
                                ; loc= sil_loc
                                ; is_cpp_structured_binding= is_structured_binding } ) ] } )
            | _ ->
                None )
          ~mk_second:(fun trans_state stmt_info ->
            init_expr_trans_aux ~is_structured_binding trans_state var_exp_typ stmt_info init_expr )
          ~mk_return:(fun ~fst:_ ~snd -> snd.return)


  and collect_all_decl trans_state var_decls next_nodes stmt_info : trans_result =
    let open Clang_ast_t in
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let do_var_dec var_decl qual_type vdi next_node trans_state =
      let pvar = CVar_decl.sil_var_of_decl context var_decl procname in
      let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
      CVar_decl.add_var_to_locals procdesc var_decl typ pvar ;
      let trans_state = {trans_state with succ_nodes= next_node} in
      let var_exp_typ = (Exp.Lvar pvar, typ) in
      let is_structured_binding = match var_decl with BindingDecl _ -> true | _ -> false in
      init_expr_trans ~is_structured_binding trans_state var_exp_typ ~qual_type stmt_info
        vdi.Clang_ast_t.vdi_init_expr
    in
    let aux_var res_trans_tl var_decl qt vdi =
      (* Var are defined when procdesc is created, here we only take care of initialization *)
      let root_nodes_tl, instrs_tl, initd_exps_tl, markers_tl =
        match res_trans_tl with
        | None ->
            (next_nodes, [], [], [])
        | Some {control= {root_nodes; instrs; initd_exps; cxx_temporary_markers_set}} ->
            (root_nodes, instrs, initd_exps, cxx_temporary_markers_set)
      in
      let res_trans_tmp = do_var_dec var_decl qt vdi root_nodes_tl trans_state in
      L.debug Capture Verbose "res_trans_tmp.control=%a@\n" pp_control res_trans_tmp.control ;
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
           ; initd_exps= res_trans_tmp.control.initd_exps @ initd_exps_tl
           ; cxx_temporary_markers_set= res_trans_tmp.control.cxx_temporary_markers_set @ markers_tl
           } )
    in
    let rec aux : decl list -> trans_result option = function
      | [] ->
          None
      | ( ( VarDecl (_, _, qt, vdi)
          | BindingDecl (_, _, qt, Clang_ast_t.{hvdi_binding_var= Some vdi})
          | VarTemplateSpecializationDecl (_, _, _, qt, vdi) ) as var_decl )
        :: var_decls' ->
          let res_trans_tl = aux var_decls' in
          aux_var res_trans_tl var_decl qt vdi
      | (DecompositionDecl (_, _, qt, vdi, list) as var_decl) :: var_decls' ->
          let res_trans_tl = aux (list @ var_decls') in
          aux_var res_trans_tl var_decl qt vdi
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
    | VarDecl _ :: _
    | VarTemplateSpecializationDecl _ :: _
    | CXXRecordDecl _ :: _
    | BindingDecl _ :: _
    | DecompositionDecl _ :: _
    | RecordDecl _ :: _ ->
        collect_all_decl trans_state decl_list succ_nodes stmt_info
    | (NamespaceAliasDecl _ | TypedefDecl _ | TypeAliasDecl _ | UsingDecl _ | UsingDirectiveDecl _)
      :: _ ->
        mk_trans_result (mk_fresh_void_exp_typ ()) empty_control
    | decl :: _ ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "In DeclStmt found an unknown declaration type %s" (Clang_ast_j.string_of_decl decl)
    | [] ->
        assert false


  and objCPropertyRefExpr_trans trans_state stmt_list =
    match stmt_list with [stmt] -> instruction trans_state stmt | _ -> assert false


  (** For OpaqueValueExpr we return the translation generated from its source expression*)
  and opaqueValueExpr_trans trans_state opaque_value_expr_info source_range =
    match trans_state.opaque_exp with
    | Some exp ->
        mk_trans_result exp empty_control
    | None -> (
      match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
      | Some stmt ->
          instruction trans_state stmt
      | None ->
          CFrontend_errors.incorrect_assumption __POS__ source_range
            "Expected source expression for OpaqueValueExpr" )


  (** NOTE: This translation has several hypothesis. Need to be verified when we have more
      experience with this construct. [assert false] will help to see if we encounter programs that
      do not conform with this hypothesis.

      Hypotheses:

      + [stmt_list] is composed of 2 parts: the first element is a syntactic description of the
        expression. The rest of the list has a semantic caracterization of the expression and
        defines how that expression is going to be implemented at runtime.
      + The semantic description is composed by a list of OpaqueValueExpr that define the various
        expressions involved and one finale expression that define how the final value of the
        PseudoObjectExpr is obtained.

      All the OpaqueValueExpr will be part of the last expression. So they can be skipped. For
      example: [x.f = a] when [f] is a property will be translated with a call to [f]'s setter
      [x f:a] the [stmt_list] will be [x.f = a; x; a; CallToSetter]. Among all element of the list
      we only need to translate the CallToSetter which is how [x.f = a] is actually implemented by
      the runtime.*)
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
  and cast_exprs_trans trans_state ?cxx_static_cast stmt_info stmt_list expr_info
      ?objc_bridge_cast_kind cast_expr_info =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let stmt =
      extract_stmt_from_singleton stmt_list stmt_info.Clang_ast_t.si_source_range
        "In CastExpr There must be only one stmt defining the expression to be cast."
    in
    let stmt =
      match stmt with
      | ImplicitCastExpr
          ( inner_stmt_info
          , inner_stmt_list
          , inner_expr_info
          , inner_cast_expr_info
          , part_of_explicit_cast )
        when part_of_explicit_cast ->
          Clang_ast_t.ImplicitCastExpr
            ( inner_stmt_info
            , inner_stmt_list
            , {inner_expr_info with ei_qual_type= expr_info.Clang_ast_t.ei_qual_type}
            , inner_cast_expr_info
            , part_of_explicit_cast )
      | _ ->
          stmt
    in
    let cast_kind = cast_expr_info.Clang_ast_t.cei_cast_kind in
    let trans_state =
      match cast_kind with
      | `LValueToRValue -> (
        match stmt with
        | CompoundLiteralExpr _ ->
            (* HACK: we don't want to apply the reasoning below in the specific case where the
               sub-expression is a struct literal, for reasons unknown but probably "SIL is weird with
               structs-as-values" *)
            trans_state
        | _ ->
            (* reset var_exp_typ because we need to dereference the result, so we cannot have the
               sub-expression initializing [var_exp_typ] for us *)
            {trans_state with var_exp_typ= None} )
      | _ ->
          trans_state
    in
    let res_trans_stmt = instruction trans_state stmt in
    let typ =
      CType_decl.qual_type_to_sil_type context.CContext.tenv
        (Option.value cxx_static_cast ~default:expr_info.Clang_ast_t.ei_qual_type)
    in
    let exp_typ = res_trans_stmt.return in
    (* This gives the difference among cast operations kind *)
    let cast_inst, cast_exp = cast_operation ?objc_bridge_cast_kind cast_kind exp_typ typ sil_loc in
    { res_trans_stmt with
      control= {res_trans_stmt.control with instrs= res_trans_stmt.control.instrs @ cast_inst}
    ; return= cast_exp }


  (** function used in the computation for both Member_Expr and ObjCIVarRefExpr *)
  and do_memb_ivar_ref_exp ~is_member_of_const trans_state stmt_info stmt_list decl_ref =
    let exp_stmt =
      extract_stmt_from_singleton stmt_list stmt_info.Clang_ast_t.si_source_range
        "in MemberExpr there must be only one stmt defining its expression."
    in
    (* Don't pass var_exp_typ to child of MemberExpr - this may lead to initializing variable *)
    (* with wrong value. For example, we don't want p to be initialized with X(1) for:*)
    (* int p = X(1).field; *)
    let trans_state' = {trans_state with var_exp_typ= None} in
    let result_trans_exp_stmt = exec_with_glvalue_as_reference instruction trans_state' exp_stmt in
    decl_ref_trans ~is_member_of_const ~context:(MemberOrIvar result_trans_exp_stmt) trans_state
      stmt_info decl_ref


  and objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info =
    let decl_ref = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref in
    do_memb_ivar_ref_exp ~is_member_of_const:false trans_state stmt_info stmt_list decl_ref


  and memberExpr_trans trans_state stmt_info stmt_list expr_info member_expr_info =
    let decl_ref = member_expr_info.Clang_ast_t.mei_decl_ref in
    let res_trans =
      do_memb_ivar_ref_exp ~is_member_of_const:expr_info.Clang_ast_t.ei_qual_type.qt_is_const
        trans_state stmt_info stmt_list decl_ref
    in
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
    let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
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
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc UnaryOperator stmt_info
      all_control
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
       This is ensured by creating a fresh pointer in these functions. *)
    let check_destructor_translation = function
      | Some {control= {root_nodes}} ->
          assert (List.is_empty root_nodes)
      | None ->
          ()
    in
    let mk_ret_node instrs =
      let destr_trans_result =
        inject_destructors Procdesc.Node.DestrReturnStmt trans_state_pri stmt_info
      in
      check_destructor_translation destr_trans_result ;
      let is_destructor =
        match procname with
        | Procname.ObjC_Cpp cpp_pname ->
            Procname.ObjC_Cpp.is_destructor cpp_pname
        | _ ->
            false
      in
      let destructor_res =
        if is_destructor then
          cxx_inject_field_destructors_in_destructor_body trans_state_pri stmt_info
        else None
      in
      (* [cxx_inject_field_destructors_in_destructor_body] should not create new nodes for return statement,
         this is ensured by creating a fresh pointer in [cxx_inject_field_destructors_in_destructor_body]
      *)
      check_destructor_translation destructor_res ;
      let instrs_of = function Some {control= {instrs}} -> instrs | None -> [] in
      let ret_node =
        Procdesc.create_node context.procdesc sil_loc (Stmt_node ReturnStmt)
          (instrs @ instrs_of destr_trans_result @ instrs_of destructor_res)
      in
      Procdesc.node_set_succs context.procdesc ret_node
        ~normal:[Procdesc.get_exit_node context.CContext.procdesc]
        ~exn:[] ;
      ret_node
    in
    let return_stmt stmt ~mk_ret_instrs =
      let ret_typ_of_pdesc = Procdesc.get_ret_type procdesc in
      let ret_exp, ret_typ, var_control =
        match context.CContext.return_param_typ with
        | Some ret_param_typ ->
            let name = CFrontend_config.return_param in
            let pvar = Pvar.mk (Mangled.from_string name) procname in
            let id = Ident.create_fresh Ident.knormal in
            let instr = Sil.Load {id; e= Exp.Lvar pvar; typ= ret_param_typ; loc= sil_loc} in
            let ret_typ =
              match ret_param_typ.desc with Typ.Tptr (t, _) -> t | _ -> assert false
            in
            (Exp.Var id, ret_typ, Some {empty_control with instrs= [instr]})
        | None ->
            (Exp.Lvar (Procdesc.get_ret_var procdesc), ret_typ_of_pdesc, None)
      in
      let trans_state' =
        {trans_state_pri with succ_nodes= []; var_exp_typ= Some (ret_exp, ret_typ)}
      in
      L.debug Capture Verbose "Evaluating sub-expr of return@\n" ;
      let res_trans_stmt = instruction trans_state' stmt in
      L.debug Capture Verbose "Done evaluating sub-expr of return@\n" ;
      let controls =
        let var_control =
          Option.map var_control ~f:(fun control ->
              (* force node creation to place the load instruction [var_control] before the
                 translation of the sub-expr *)
              let trans_state = PriorityNode.force_claim_priority_node trans_state_pri stmt_info in
              PriorityNode.compute_control_to_parent trans_state sil_loc ReturnStmt stmt_info
                control )
        in
        PriorityNode.compute_controls_to_parent trans_state' sil_loc ReturnStmt stmt_info
          (Option.to_list var_control @ [res_trans_stmt.control])
      in
      let ret_instrs = mk_ret_instrs ret_exp ret_typ res_trans_stmt in
      let ret_node = mk_ret_node ret_instrs in
      L.debug Capture Verbose "Created return node %a with instrs [%a]@\n" Procdesc.Node.pp ret_node
        (Pp.seq ~sep:";" (Sil.pp_instr ~print_types:false Pp.text))
        ret_instrs ;
      assert (List.is_empty controls.instrs) ;
      List.iter controls.leaf_nodes ~f:(fun leaf ->
          Procdesc.set_succs leaf ~normal:(Some [ret_node]) ~exn:None ) ;
      let ret_control =
        {empty_control with root_nodes= [ret_node]; leaf_nodes= [ret_node]; instrs= []}
      in
      PriorityNode.compute_controls_to_parent trans_state_pri sil_loc ReturnStmt stmt_info
        [controls; ret_control]
      |> mk_trans_result res_trans_stmt.return
    in
    match (stmt_list, context.CContext.return_param_typ) with
    | ( ([Clang_ast_t.ImplicitCastExpr (_, [stmt], _, _, _)] | [stmt])
      , Some {desc= Tptr ({desc= Tstruct (CStruct _ as struct_name)}, _)} ) ->
        (* return (exp:struct); *)
        return_stmt stmt ~mk_ret_instrs:(fun ret_exp ret_typ res_trans_stmt ->
            CStructUtils.struct_copy context.CContext.tenv sil_loc ret_exp
              (fst res_trans_stmt.return) ~typ:ret_typ ~struct_name )
    | [stmt], _ ->
        (* return exp; *)
        return_stmt stmt ~mk_ret_instrs:(fun ret_exp ret_typ res_trans_stmt ->
            if List.exists ~f:(Exp.equal ret_exp) res_trans_stmt.control.initd_exps then []
            else
              let sil_expr, _ = res_trans_stmt.return in
              [Sil.Store {e1= ret_exp; typ= ret_typ; e2= sil_expr; loc= sil_loc}] )
    | [], _ ->
        (* return; *)
        let ret_node = mk_ret_node [] in
        mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= [ret_node]}
    | _, _ ->
        assert false


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
    let obj_c_message_expr_info = Ast_expressions.make_obj_c_message_expr_info_class sel typ None in
    let message_stmt =
      Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_message_expr_info)
    in
    instruction trans_state message_stmt


  and objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer =
    let open Clang_ast_t in
    match CAst_utils.get_decl_opt method_pointer with
    | Some (ObjCMethodDecl (decl_info, named_decl_info, _)) ->
        let typ = CAst_utils.qual_type_of_decl_ptr (Option.value_exn decl_info.di_parent_pointer) in
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
    match get_arrayWithObjects_count_infos method_pointer with
    | Some infos ->
        objCArrayLiteral_arrayWithObjects_count_trans trans_state expr_info stmt_info stmts infos
          method_pointer
    | None ->
        let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
        objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer


  (** Translates an array literal [NSArray* a = @[ @2, @3 ];] to

      {[
        n$1=NSNumber.numberWithInt:(2:int)
        n$2=NSNumber.numberWithInt:(3:int)
        _ = objc_insert_value(n$1:objc_object* ) // nil insertion check
        _ = *n$1 // null dereference check
        temp[0]:objc_object*=n$1
        _ = objc_insert_value(n$2:objc_object* ) // nil insertion check
        _ = *n$2 // null dereference check
        temp[1]:objc_object*=n$2
        n$3=NSArray.arrayWithObjects:count:(temp:objc_object* const [2*8],2:int)
        a:NSArray*=n$3
      ]}

      where [temp] is an additional local variable declared as array. *)
  and objCArrayLiteral_arrayWithObjects_count_trans
      ({context= {procdesc; tenv; translation_unit_context} as context} as trans_state) expr_info
      stmt_info stmts (decl_info, objects_qual_typ) method_pointer =
    let loc = CLocation.location_of_stmt_info translation_unit_context.source_file stmt_info in
    (* 1. Add a temporary local variable for an array *)
    let temp = CVar_decl.mk_temp_sil_var procdesc ~name:"SIL_arrayWithObjects_count___" in
    let length = List.length stmts |> IntLit.of_int in
    let ((temp_var, array_typ) as temp_with_typ) =
      let array_typ =
        match CType_decl.qual_type_to_sil_type tenv objects_qual_typ with
        | Typ.{desc= Tptr (t, _)} ->
            Typ.mk_array ~length ~stride:(IntLit.of_int 8) t
        | _ ->
            StdTyp.void_star
      in
      (Exp.Lvar temp, array_typ)
    in
    Procdesc.append_locals procdesc [ProcAttributes.default_var_data temp array_typ] ;
    (* 2. Translate array elements *)
    let res_trans_elems =
      List.mapi
        ~f:(exec_instruction_with_trans_state {trans_state with var_exp_typ= None} None)
        stmts
    in
    (* 3. Add array initialization (elements assignments + nil insertion check) *)
    let res_trans_array =
      let instrs =
        let none_id = Ident.create_none () in
        let objc_insert_value : Exp.t * Typ.t -> Sil.instr =
         fun arg ->
          Sil.Call
            ( (none_id, StdTyp.void)
            , Const (Cfun BuiltinDecl.objc_insert_value)
            , [arg]
            , loc
            , CallFlags.default )
        in
        List.mapi res_trans_elems ~f:(fun i {return= e, typ} ->
            let idx = Exp.Const (Cint (IntLit.of_int i)) in
            [ objc_insert_value (e, typ)
            ; Sil.Load {id= none_id; e; typ; loc}
            ; Sil.Store {e1= Lindex (temp_var, idx); typ; e2= e; loc} ] )
        |> List.concat
      in
      mk_trans_result temp_with_typ {empty_control with instrs}
    in
    (* 4. Add a function call. *)
    let res_trans_call =
      let method_type_no_ref = CType_decl.get_type_from_expr_info expr_info tenv in
      let method_type = add_reference_if_glvalue method_type_no_ref expr_info in
      let actuals = [temp_with_typ; (Exp.Const (Cint length), StdTyp.int)] in
      let callee_name, method_call_type =
        let typ =
          CAst_utils.qual_type_of_decl_ptr
            (Option.value_exn decl_info.Clang_ast_t.di_parent_pointer)
        in
        let obj_c_message_expr_info =
          { Clang_ast_t.omei_selector= CFrontend_config.arrayWithObjects_count
          ; omei_receiver_kind= `Class typ
          ; omei_is_definition_found= true
          ; omei_decl_pointer= method_pointer }
        in
        let callee_ms_opt =
          Option.bind method_pointer ~f:(fun method_pointer ->
              CMethod_trans.method_signature_of_pointer tenv method_pointer )
        in
        get_callee_objc_method context obj_c_message_expr_info callee_ms_opt actuals
      in
      let method_sil = Exp.Const (Cfun callee_name) in
      let call_flags =
        { CallFlags.default with
          cf_virtual= CMethod_trans.equal_method_call_type method_call_type CMethod_trans.MCVirtual
        }
      in
      create_call_instr trans_state method_type method_sil actuals loc call_flags
        ~is_inherited_ctor:false
    in
    collect_trans_results procdesc ~return:res_trans_call.return
      (res_trans_elems @ [res_trans_array; res_trans_call])


  and objCDictionaryLiteral_trans trans_state expr_info stmt_info stmts dict_literal_info =
    let method_pointer = dict_literal_info.Clang_ast_t.odlei_dict_method in
    match get_dictionaryWithObjects_forKeys_count_infos method_pointer with
    | Some infos ->
        objCDictLiteral_dictionaryWithObjects_forKeys_count_trans trans_state expr_info stmt_info
          stmts infos method_pointer
    | None ->
        let stmts = CGeneral_utils.swap_elements_list stmts in
        let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
        objCArrayDictLiteral_trans trans_state expr_info stmt_info stmts method_pointer


  (** Translates an dictionary literal [@[ @"firstName": @"Foo", @"lastName":@"Bar" ]] to

      {[
        n$1=NSString.stringWithUTF8:(@"firstName")
        n$2=NSNumber.stringWithUTF8:(@"Foo")
        n$3=NSNumber.stringWithUTF8:(@"lastName")
        n$4=NSNumber.stringWithUTF8:(@"Bar")
        _ = objc_insert_key(n$1:objc_object* ) // nil insertion check
        _ = *n$1 // null dereference check
        temp1[0]:objc_object*=n$1
        _ = objc_insert_key(n$3:objc_object* ) // nil insertion check
        _ = *n$3 // null dereference check
        temp1[1]:objc_object*=n$3
        _ = objc_insert_value(n$2:objc_object* ) // nil insertion check
        _ = *n$2 // null dereference check
        temp2[0]:objc_object*=n$2
        _ = objc_insert_value(n$4:objc_object* ) // nil insertion check
        _ = *n$4 // null dereference check
        temp2[1]:objc_object*=n$4
        n$3=NSDictionary.dictionaryWithObjects:forKeys:count:(temp2:objc_object* const [2*8],
                                                              temp1:objc_object* const [2*8], 2:int)
      ]}

      where [temp1] [temp2] are additional local variables declared as arrays. *)
  and objCDictLiteral_dictionaryWithObjects_forKeys_count_trans
      ({context= {procdesc; tenv; translation_unit_context} as context} as trans_state) expr_info
      stmt_info stmts (decl_info, objects_qual_typ) method_pointer =
    let loc = CLocation.location_of_stmt_info translation_unit_context.source_file stmt_info in
    (* 1. Add a temporary local variable for an array *)
    let temp1 =
      CVar_decl.mk_temp_sil_var procdesc ~name:"SIL_dictionaryWithObjects_forKeys_count1___"
    in
    let temp2 =
      CVar_decl.mk_temp_sil_var procdesc ~name:"SIL_dictionaryWithObjects_forKeys_count2___"
    in
    let length = List.length stmts |> IntLit.of_int in
    let create_var = function
      | temp ->
          let array_typ =
            match CType_decl.qual_type_to_sil_type tenv objects_qual_typ with
            | Typ.{desc= Tptr (t, _)} ->
                Typ.mk_array ~length ~stride:(IntLit.of_int 8) t
            | _ ->
                StdTyp.void_star
          in
          (Exp.Lvar temp, array_typ)
    in
    let append_var temp array_typ =
      let var_data = ProcAttributes.default_var_data temp array_typ in
      Procdesc.append_locals procdesc [var_data]
    in
    let ((temp1_var, array1_typ) as temp1_with_typ) = create_var temp1 in
    let ((temp2_var, array2_typ) as temp2_with_typ) = create_var temp2 in
    append_var temp1 array1_typ ;
    append_var temp2 array2_typ ;
    (* 2. Translate array elements *)
    let res_trans_elems =
      let trans_state = {trans_state with var_exp_typ= None} in
      List.mapi ~f:(exec_instruction_with_trans_state trans_state None) stmts
    in
    (* 3. Add array initialization (elements assignments) *)
    let array_init temp_var temp_with_typ idx_mod insert_proc =
      let none_id = Ident.create_none () in
      let objc_insert : Exp.t * Typ.t -> Sil.instr =
       fun arg ->
        Sil.Call ((none_id, StdTyp.void), Const (Cfun insert_proc), [arg], loc, CallFlags.default)
      in
      let instrs =
        List.mapi res_trans_elems ~f:(fun i {return= e, typ} ->
            if Int.equal (i % 2) idx_mod then
              let idx = Exp.Const (Cint (IntLit.of_int (i / 2))) in
              [ objc_insert (e, typ)
              ; Sil.Load {id= none_id; e; typ; loc}
              ; Sil.Store {e1= Lindex (temp_var, idx); typ; e2= e; loc} ]
            else [] )
        |> List.concat
      in
      mk_trans_result temp_with_typ {empty_control with instrs}
    in
    let res_trans_array1 = array_init temp1_var temp1_with_typ 1 BuiltinDecl.objc_insert_value in
    let res_trans_array2 = array_init temp2_var temp2_with_typ 0 BuiltinDecl.objc_insert_key in
    (* 4. Add a function call. *)
    let res_trans_call =
      let method_type_no_ref = CType_decl.get_type_from_expr_info expr_info tenv in
      let method_type = add_reference_if_glvalue method_type_no_ref expr_info in
      let actuals =
        [ temp1_with_typ
        ; temp2_with_typ
        ; (Exp.Const (Cint (IntLit.div length (IntLit.of_int 2))), StdTyp.int) ]
      in
      let callee_name, method_call_type =
        let typ =
          CAst_utils.qual_type_of_decl_ptr
            (Option.value_exn decl_info.Clang_ast_t.di_parent_pointer)
        in
        let obj_c_message_expr_info =
          { Clang_ast_t.omei_selector= CFrontend_config.dictionaryWithObjects_forKeys_count
          ; omei_receiver_kind= `Class typ
          ; omei_is_definition_found= true
          ; omei_decl_pointer= method_pointer }
        in
        let callee_ms_opt =
          Option.bind method_pointer ~f:(fun method_pointer ->
              CMethod_trans.method_signature_of_pointer tenv method_pointer )
        in
        get_callee_objc_method context obj_c_message_expr_info callee_ms_opt actuals
      in
      let method_sil = Exp.Const (Cfun callee_name) in
      let call_flags =
        { CallFlags.default with
          cf_virtual= CMethod_trans.equal_method_call_type method_call_type CMethod_trans.MCVirtual
        }
      in
      create_call_instr trans_state method_type method_sil actuals loc call_flags
        ~is_inherited_ctor:false
    in
    collect_trans_results procdesc ~return:res_trans_call.return
      (res_trans_elems @ [res_trans_array1; res_trans_array2; res_trans_call])


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
    let message_stmt = Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_mess_expr_info) in
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


  and objCAutoreleasePoolStmt_trans trans_state stmt_info stmts =
    let procdesc = trans_state.context.procdesc in
    let location =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let mk_call_node pname =
      let ret = mk_fresh_void_id_typ () in
      let instr = Sil.Call (ret, Const (Cfun pname), [], location, CallFlags.default) in
      Procdesc.create_node procdesc location (Stmt_node (Call (Procname.to_string pname))) [instr]
    in
    let push_node = mk_call_node BuiltinDecl.objc_autorelease_pool_push in
    let pop_node = mk_call_node BuiltinDecl.objc_autorelease_pool_pop in
    Procdesc.Node.set_code_block_exit push_node ~code_block_exit:pop_node ;
    let res_trans_body = compoundStmt_trans {trans_state with succ_nodes= [pop_node]} stmts in
    Procdesc.set_succs push_node ~normal:(Some res_trans_body.control.root_nodes) ~exn:None ;
    Procdesc.set_succs pop_node ~normal:(Some trans_state.succ_nodes) ~exn:None ;
    mk_trans_result res_trans_body.return
      {empty_control with root_nodes= [push_node]; leaf_nodes= [pop_node]}


  and blockExpr_trans trans_state stmt_info expr_info decl =
    let context = trans_state.context in
    let outer_proc = Procdesc.get_proc_name context.CContext.procdesc in
    match decl with
    | Clang_ast_t.BlockDecl (_, block_decl_info) ->
        let open CContext in
        let return_type = expr_info.Clang_ast_t.ei_qual_type in
        let procname =
          CType_decl.CProcname.from_decl decl ~tenv:context.tenv ~block_return_type:return_type
            ~outer_proc
        in
        let captured_vars_no_mode =
          CVar_decl.captured_vars_from_block_info context stmt_info.Clang_ast_t.si_source_range
            block_decl_info.Clang_ast_t.bdi_captured_variables
        in
        let block_as_arg_attributes = trans_state.block_as_arg_attributes in
        let captured_vars =
          List.map captured_vars_no_mode ~f:(fun (var, typ, modify_in_block) ->
              let mode, typ =
                if modify_in_block || Pvar.is_global var then
                  (CapturedVar.ByReference, Typ.mk (Tptr (typ, Pk_lvalue_reference)))
                else (CapturedVar.ByValue, typ)
              in
              (var, typ, mode) )
        in
        let res = closure_trans procname captured_vars context stmt_info expr_info in
        let block_data =
          Some {CModule_type.captured_vars; context; block_as_arg_attributes; procname; return_type}
        in
        F.function_decl context.translation_unit_context context.tenv context.cfg decl block_data ;
        res
    | _ ->
        (* Block expression with no BlockDecl *)
        assert false


  and lambdaExpr_trans trans_state stmt_info expr_info {Clang_ast_t.lei_lambda_decl} =
    let open CContext in
    let qual_type = expr_info.Clang_ast_t.ei_qual_type in
    let context = trans_state.context in
    let procname = Procdesc.get_proc_name context.procdesc in
    let typ = CType_decl.qual_type_to_sil_type context.tenv qual_type in
    let get_captured_pvar_typ decl_ref =
      CVar_decl.sil_var_of_captured_var context stmt_info.Clang_ast_t.si_source_range procname
        decl_ref
      |> Option.map ~f:(fun (var, typ, _modify_in_block) -> (var, typ))
    in
    let loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let translate_captured_var_assign exp pvar typ mode =
      (* Structs always have reference if passed as parameters *)
      let typ =
        match typ.Typ.desc with Tstruct _ -> Typ.mk (Tptr (typ, Pk_lvalue_reference)) | _ -> typ
      in
      let instr, exp = CTrans_utils.dereference_var_sil (exp, typ) loc in
      let trans_results = mk_trans_result (exp, typ) {empty_control with instrs= [instr]} in
      (trans_results, (exp, pvar, typ, mode))
    in
    let translate_capture_init mode (pvar, typ) init_decl (trans_results_acc, captured_vars_acc) =
      match init_decl with
      | Clang_ast_t.VarDecl (_, _, _, {vdi_init_expr}) ->
          let init_trans_results =
            init_expr_trans trans_state (Exp.Lvar pvar, typ) stmt_info vdi_init_expr
            :: trans_results_acc
          in
          let trans_result, captured_var =
            translate_captured_var_assign (Lvar pvar) pvar typ mode
          in
          (trans_result :: init_trans_results, captured_var :: captured_vars_acc)
      | _ ->
          CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Capture-init statement without var decl"
    in
    let translate_normal_capture mode (pvar, typ) (trans_results_acc, captured_vars_acc) =
      match (mode : CapturedVar.capture_mode) with
      | ByReference -> (
        match typ.Typ.desc with
        | Tptr (_, Typ.Pk_lvalue_reference) ->
            let trans_result, captured_var =
              translate_captured_var_assign (Exp.Lvar pvar) pvar typ mode
            in
            (trans_result :: trans_results_acc, captured_var :: captured_vars_acc)
        | _ when Pvar.is_this pvar ->
            (* Special case for this *)
            (trans_results_acc, (Exp.Lvar pvar, pvar, typ, mode) :: captured_vars_acc)
        | _ ->
            (* A variable captured by ref (except ref variables) is missing ref in its type *)
            ( trans_results_acc
            , (Exp.Lvar pvar, pvar, Typ.mk (Tptr (typ, Pk_lvalue_reference)), mode)
              :: captured_vars_acc ) )
      | ByValue -> (
          let init, exp, typ_new =
            match typ.Typ.desc with
            (* TODO: Structs are missing copy constructor instructions when passed by value *)
            | Tptr (typ_no_ref, Pk_lvalue_reference) when not (Typ.is_struct typ_no_ref) ->
                let return = (Exp.Lvar pvar, typ) in
                (* We need to dereference ref variable as usual when we read its value *)
                let init_trans_results =
                  dereference_value_from_result stmt_info.Clang_ast_t.si_source_range loc
                    (mk_trans_result return empty_control)
                in
                let exp, _ = init_trans_results.return in
                (Some init_trans_results, exp, typ_no_ref)
            | _ ->
                (None, Exp.Lvar pvar, typ)
          in
          let trans_result, captured_var = translate_captured_var_assign exp pvar typ_new mode in
          let trans_results, captured_vars =
            (trans_result :: trans_results_acc, captured_var :: captured_vars_acc)
          in
          match init with
          | Some init ->
              (init :: trans_results, captured_vars)
          | None ->
              (trans_results, captured_vars) )
    in
    let translate_captured
        {Clang_ast_t.lci_captured_var; lci_init_captured_vardecl; lci_capture_this; lci_capture_kind}
        ((trans_results_acc, captured_vars_acc) as acc) =
      let mode = CAst_utils.get_captured_mode ~lci_capture_this ~lci_capture_kind in
      match (lci_captured_var, lci_init_captured_vardecl) with
      | Some captured_var_decl_ref, Some init_decl -> (
        (* capture and init *)
        match get_captured_pvar_typ captured_var_decl_ref with
        | Some pvar_typ ->
            translate_capture_init mode pvar_typ init_decl acc
        | None ->
            (trans_results_acc, captured_vars_acc) )
      | Some captured_var_decl_ref, None -> (
        (* just capture *)
        match get_captured_pvar_typ captured_var_decl_ref with
        | Some pvar_typ ->
            translate_normal_capture mode pvar_typ acc
        | None ->
            (trans_results_acc, captured_vars_acc) )
      | None, None ->
          if lci_capture_this then
            (* captured [this] *)
            let this_typ = get_this_pvar_typ stmt_info context in
            translate_normal_capture mode this_typ acc
          else acc
      | None, Some _ ->
          CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
            "Capture-init with init, but no capture"
    in
    let lei_captures = CMethod_trans.get_captures_from_cpp_lambda lei_lambda_decl in
    let trans_results, captured_vars =
      List.fold_right ~f:translate_captured ~init:([], []) lei_captures
    in
    let captured_var_names =
      List.map ~f:(fun (_, var, typ, mode) -> (var, typ, mode)) captured_vars
    in
    let lambda_pname =
      CMethod_trans.get_procname_from_cpp_lambda context lei_lambda_decl captured_var_names
    in
    (* We want to translate `operator()` after translating captured variables
       so we can correct type of variables captured by reference *)
    call_translation ~is_cpp_lambda_expr:true context lei_lambda_decl ;
    let closure = Exp.Closure {name= lambda_pname; captured_vars} in
    collect_trans_results context.procdesc ~return:(closure, typ) trans_results


  and cxxNewExpr_trans trans_state stmt_info expr_info cxx_new_expr_info =
    let context = trans_state.context in
    let typ = CType_decl.get_type_from_expr_info expr_info context.tenv in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let is_dyn_array = cxx_new_expr_info.Clang_ast_t.xnei_is_array in
    let source_range = stmt_info.Clang_ast_t.si_source_range in
    let mk_call_new_result trans_state stmt_info =
      let size_exp_opt, control_size =
        if is_dyn_array then
          match
            CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_array_size_expr source_range
          with
          | Some stmt ->
              let trans_state_size = {trans_state with succ_nodes= []; var_exp_typ= None} in
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
      let trans_state_placement = {trans_state with succ_nodes= []; var_exp_typ= None} in
      let res_trans_placement_control, res_trans_placement_exps =
        instructions Procdesc.Node.CXXNewExpr trans_state_placement placement_args
      in
      let res_trans_new =
        cpp_new_trans context.translation_unit_context.integer_type_widths sil_loc typ size_exp_opt
          res_trans_placement_exps
      in
      let controls =
        Option.to_list control_size @ [res_trans_placement_control; res_trans_new.control]
      in
      let trans_result =
        PriorityNode.compute_controls_to_parent trans_state sil_loc CXXNewExpr stmt_info controls
        |> mk_trans_result res_trans_new.return
      in
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
      (trans_result, (var_exp_typ, size_exp_opt))
    in
    let mk_init_result (var_exp_typ, size_exp_opt) trans_state_init init_stmt_info =
      let stmt_opt =
        CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_initializer_expr source_range
      in
      match stmt_opt with
      | Some (InitListExpr _) ->
          init_expr_trans trans_state_init var_exp_typ init_stmt_info stmt_opt
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
          |> PriorityNode.compute_results_to_parent trans_state_init sil_loc CXXNewExpr
               ~return:var_exp_typ init_stmt_info
      | _ ->
          init_expr_trans trans_state_init var_exp_typ init_stmt_info stmt_opt
    in
    PriorityNode.force_sequential_with_acc sil_loc CXXNewExpr trans_state stmt_info
      ~mk_first:mk_call_new_result ~mk_second:mk_init_result ~mk_return:(fun ~fst ~snd:_ ->
        fst.return )


  and cxxDeleteExpr_trans trans_state stmt_info stmt_list delete_expr_info =
    let context = trans_state.context in
    let sil_loc =
      CLocation.location_of_stmt_info context.translation_unit_context.source_file stmt_info
    in
    let is_array = delete_expr_info.Clang_ast_t.xdei_is_array in
    let fname = if is_array then BuiltinDecl.__delete_array else BuiltinDecl.__delete in
    let param = match stmt_list with [p] -> p | _ -> assert false in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
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
          get_destructor_decl_ref deleted_type.Clang_ast_t.qt_type_ptr
          |> Option.map ~f:(fun destructor_decl_ref ->
                 cxx_destructor_call_trans trans_state_pri destruct_stmt_info
                   this_res_trans_destruct destructor_decl_ref ~is_injected_destructor:false
                   ~is_inner_destructor:false )
        in
        result_trans_param :: (Option.to_list destruct_res_trans @ [call_res_trans])
        (* --- END OF DEAD CODE --- *)
      else [result_trans_param; call_res_trans]
    in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc (Call "delete") stmt_info
      ~return:call_return all_res_trans


  and materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let temp_exp = match stmt_list with [p] -> p | _ -> assert false in
    let procdesc = context.CContext.procdesc in
    let pvar, typ_tmp = CVar_decl.materialize_cpp_temporary context stmt_info expr_info in
    let var_exp_typ = (Exp.Lvar pvar, typ_tmp) in
    let trans_state = {trans_state with var_exp_typ= Some var_exp_typ} in
    let res_trans = init_expr_trans trans_state var_exp_typ stmt_info (Some temp_exp) in
    let _, typ = res_trans.return in
    let var_data = ProcAttributes.default_var_data pvar typ in
    Procdesc.append_locals procdesc [var_data] ;
    res_trans


  and cxxBindTemporaryExpr_trans trans_state stmt_info stmt_list expr_info =
    (* Only create a temporary if there *isn't* already a variable to store the result into, eg [X x
       = X();] will generate a [CXXBindTemporaryExpr] to hold [X()] but clang doesn't expect a
       temporary to be actually created here as we can store the result in [x] directly (and have to
       since C++17 –in contexts where clang does expect the temporary to be created we'll get an
       intermediate copy or move constructor call, which in particular has the effect of setting
       [trans_state.var_exp_typ] to [None]).

       Since the presence of [trans_state.var_exp_typ] is highly dependent on the current context,
       [CScope.CXXTemporaries.get_destroyable_temporaries] will compute an over-approximation of the
       C++ temporaries needed by a given expression. We can tell which ones are actually used as
       they are recorded as local variables by [materializeTemporaryExpr_trans]. This trick is used
       by [exprWithCleanups_trans] to compute the accurate set of C++ temporaries to destroy. *)
    if Option.is_none trans_state.var_exp_typ then
      (* actually create a C++ temporary to hold the result of the sub-expression *)
      materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info
    else
      (* do nothing and translate the sub-expression directly if we already have a variable to
         initialize *)
      parenExpr_trans trans_state stmt_info.Clang_ast_t.si_source_range stmt_list


  and compoundLiteralExpr_trans trans_state stmt_list stmt_info expr_info =
    let stmt = match stmt_list with [stmt] -> stmt | _ -> assert false in
    let var_exp_typ =
      if Option.is_some trans_state.var_exp_typ then trans_state.var_exp_typ
      else
        Some
          (create_var_exp_tmp_var trans_state expr_info ~var_name:"SIL_compound_literal__"
             ~clang_pointer:stmt_info.Clang_ast_t.si_pointer )
    in
    let trans_state' = {trans_state with var_exp_typ} in
    instruction trans_state' stmt


  and cxxDynamicCastExpr_trans trans_state stmt_info stmts cast_qual_type =
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state' = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
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
          Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype; nullable= false}
      | _ ->
          assert false
    in
    let builtin = Exp.Const (Const.Cfun BuiltinDecl.__cast) in
    let stmt = match stmts with [stmt] -> stmt | _ -> assert false in
    let res_trans_stmt = exec_with_glvalue_as_reference instruction trans_state' stmt in
    let exp = res_trans_stmt.return in
    let args = [exp; (sizeof_expr, StdTyp.void)] in
    let ret_id = Ident.create_fresh Ident.knormal in
    let call = Sil.Call ((ret_id, cast_type), builtin, args, sil_loc, CallFlags.default) in
    let res_ex = Exp.Var ret_id in
    let res_trans_dynamic_cast = {empty_control with instrs= [call]} in
    let all_res_trans = [res_trans_stmt.control; res_trans_dynamic_cast] in
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc CXXDynamicCast stmt_info
      all_res_trans
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
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let res_trans_subexpr_list =
      List.map ~f:(exec_with_glvalue_as_reference instruction trans_state_param) stmts
    in
    let args = collect_returns res_trans_subexpr_list in
    let sil_fun = Exp.Const (Const.Cfun pname) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let call_instr = Sil.Call ((ret_id, ret_typ), sil_fun, args, sil_loc, CallFlags.default) in
    let res_trans_call =
      mk_trans_result (Exp.Var ret_id, ret_typ) {empty_control with instrs= [call_instr]}
    in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc instr_name stmt_info
      ~return:res_trans_call.return all_res_trans


  and gccAsmStmt_trans trans_state stmt_info stmts =
    call_function_with_args Procdesc.Node.GCCAsmStmt BuiltinDecl.__infer_skip_gcc_asm_stmt
      trans_state stmt_info StdTyp.void stmts


  and offsetOf_trans trans_state expr_info offset_of_expr_info stmt_info =
    match offset_of_expr_info.Clang_ast_t.ooe_literal with
    | Some integer_literal_info ->
        integerLiteral_trans trans_state expr_info integer_literal_info
    | None ->
        (* [offsetof] couldn't be evaluated as integer, return as a call to a builtin *)
        let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
        (* We don't provide arguments to the builtin because due to translation we get gibberish.
           Ex: offsetof(struct foo, bar[i]) gets only [i] as stmt_list and no mentions of foo or bar. *)
        call_function_with_args (Procdesc.Node.Call "offsetof") BuiltinDecl.__builtin_offsetof
          trans_state stmt_info typ []


  and genericSelectionExprUnknown_trans trans_state stmt_info stmts =
    call_function_with_args Procdesc.Node.GenericSelectionExpr
      BuiltinDecl.__infer_generic_selection_expr trans_state stmt_info StdTyp.void stmts


  and objc_cxx_throw_trans trans_state stmt_info stmts =
    call_function_with_args Procdesc.Node.ObjCCPPThrow BuiltinDecl.objc_cpp_throw trans_state
      stmt_info StdTyp.void stmts


  and cxxPseudoDestructorExpr_trans () =
    mk_trans_result
      (Exp.Const (Const.Cfun BuiltinDecl.__infer_skip_function), StdTyp.void)
      empty_control


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
          let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
          Some (instruction trans_state_param stmt)
      | _ ->
          None
    in
    let fun_name = BuiltinDecl.__cxx_typeid in
    let sil_fun = Exp.Const (Const.Cfun fun_name) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let void_typ = StdTyp.void in
    let type_info_objc =
      ( Exp.Sizeof {typ; nbytes= None; dynamic_length= None; subtype= Subtype.exact; nullable= false}
      , void_typ )
    in
    let class_tname =
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate ~is_union:false
        (QualifiedCppName.of_list ["std"; "type_info"])
    in
    let field_name = Fieldname.make class_tname "__type_name" in
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
    PriorityNode.compute_controls_to_parent trans_state_pri sil_loc CXXTypeidExpr stmt_info
      all_res_trans
    |> mk_trans_result (ret_exp, typ)


  and cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info =
    let context = trans_state.context in
    let tenv = context.CContext.tenv in
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let typ = CType_decl.qual_type_to_sil_type tenv expr_info.Clang_ast_t.ei_qual_type in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = {trans_state_pri with succ_nodes= []; var_exp_typ= None} in
    let res_trans_subexpr_list = List.map ~f:(instruction trans_state_param) stmts in
    let args = collect_returns res_trans_subexpr_list in
    let ret_id = Ident.create_fresh Ident.knormal in
    let ret_exp = Exp.Var ret_id in
    let res_instr =
      let sil_fun = Exp.Const (Const.Cfun BuiltinDecl.__infer_initializer_list) in
      Sil.Call ((ret_id, typ), sil_fun, args, sil_loc, CallFlags.default)
    in
    let res_trans_call = mk_trans_result (ret_exp, typ) {empty_control with instrs= [res_instr]} in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    PriorityNode.compute_results_to_parent trans_state_pri sil_loc CXXStdInitializerListExpr
      stmt_info ~return:res_trans_call.return all_res_trans


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
      | NullStmt _, `FallThroughAttr _ ->
          no_op_trans trans_state.succ_nodes
      | _ ->
          CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
            "attributedStmt [stmt] [attr] with:@\nstmt=%s@\nattr=%s@\n"
            (Clang_ast_j.string_of_stmt stmt)
            (Clang_ast_j.string_of_attribute attr) )
    | _ ->
        CFrontend_errors.unimplemented __POS__ stmt_info.Clang_ast_t.si_source_range
          "attributedStmt with:@\nstmts=[%a]@\nattrs=[%a]@\n"
          (Pp.semicolon_seq (Pp.of_string ~f:Clang_ast_j.string_of_stmt))
          stmts
          (Pp.semicolon_seq (Pp.of_string ~f:Clang_ast_j.string_of_attribute))
          attrs


  and breakStmt_trans trans_state stmt_info =
    match trans_state.continuation with
    | Some bn -> (
        let trans_state' = {trans_state with succ_nodes= bn.break} in
        match inject_destructors Procdesc.Node.DestrBreakStmt trans_state' stmt_info with
        | Some ({control= {root_nodes= _ :: _}} as destr_trans_result) ->
            {destr_trans_result with control= {destr_trans_result.control with leaf_nodes= []}}
        | Some {control= {root_nodes= []}} | None ->
            mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= bn.break} )
    | None (* t21762295 *) ->
        CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
          "Break stmt without continuation: %a"
          (Pp.of_string ~f:Clang_ast_j.string_of_stmt_info)
          stmt_info


  and continueStmt_trans trans_state stmt_info =
    match trans_state.continuation with
    | Some bn -> (
        let trans_state' = {trans_state with succ_nodes= bn.continue} in
        match inject_destructors Procdesc.Node.DestrContinueStmt trans_state' stmt_info with
        | Some ({control= {root_nodes= _ :: _}} as destr_trans_result) ->
            {destr_trans_result with control= {destr_trans_result.control with leaf_nodes= []}}
        | Some {control= {root_nodes= []}} | None ->
            mk_trans_result (mk_fresh_void_exp_typ ()) {empty_control with root_nodes= bn.continue}
        )
    | None (* t21762295 *) ->
        CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
          "Continue stmt without continuation: %a"
          (Pp.of_string ~f:Clang_ast_j.string_of_stmt_info)
          stmt_info


  and add_cxx_temporaries_dtor_calls destr_kind expr_trans_result trans_state stmt_info temporaries
      =
    (* The source location of destructor should reflect the end of the statement *)
    let _, sloc2 = stmt_info.Clang_ast_t.si_source_range in
    let stmt_info = {stmt_info with Clang_ast_t.si_source_range= (sloc2, sloc2)} in
    (* ReturnStmt claims a priority with the same [stmt_info].
       New pointer is generated to avoid premature node creation *)
    let sil_loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let dtor_call stmt_info trans_state destructor_decl_ref (temporary : CContext.cxx_temporary) =
      L.debug Capture Verbose "Destroying pointer %d@\n" stmt_info.Clang_ast_t.si_pointer ;
      let exp = Exp.Lvar temporary.pvar in
      let this_res_trans_destruct = mk_trans_result (exp, temporary.typ) empty_control in
      cxx_destructor_call_trans trans_state stmt_info this_res_trans_destruct destructor_decl_ref
        ~is_injected_destructor:true ~is_inner_destructor:false
    in
    (* call the destructor for the given temporary, return [(created_conditional, trans_result)]
       where [created_conditional] is true when we needed to create new nodes to conditionally
       destroy the temporary (hence the next instruction should also create its own new node to
       place before that) *)
    let destroy_one stmt_info trans_state (temporary : CContext.cxx_temporary) =
      L.debug Capture Verbose "destructing %a, trans_state=%a@\n" (Pvar.pp Pp.text_break)
        temporary.pvar pp_trans_state trans_state ;
      let open IOption.Let_syntax in
      let+ dtor_decl_ref = get_destructor_decl_ref temporary.qual_type.qt_type_ptr in
      match temporary.marker with
      | None ->
          (* simple case: no marker instrumentation: unconditionally destroy the temporary variable
             now *)
          (false, dtor_call stmt_info trans_state dtor_decl_ref temporary)
      | Some (marker_var, if_kind) ->
          (* generate [if marker_var then destroy_temporary() ;], i.e.:

             - a prune node [prune_true] for the case where [marker_var] is true

             - a prune node [prune_false] for the case where [marker_var] is false

             - a node [dtor_trans_result] for actually calling the destructor, the successor of
             [prune_true]

             - [join_node] to join [dtor_trans_result] and [prune_false] and continue with the
             successor node(s) of the translation [trans_state.succ_nodes] *)
          let proc_desc = trans_state.context.procdesc in
          let join_node =
            Procdesc.create_node proc_desc sil_loc Join_node
            @@ if Sil.is_terminated_if_kind if_kind then [Sil.Metadata EndBranches] else []
          in
          Procdesc.node_set_succs proc_desc join_node ~normal:trans_state.succ_nodes ~exn:[] ;
          (* create dtor call as a new node connected to the join node, force new node creation by
             creating a fresh pointer and calling [force_claim_priority_node] *)
          let dtor_trans_result =
            let stmt_info =
              {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
            in
            let dtor_trans_state =
              let trans_state = PriorityNode.force_claim_priority_node trans_state stmt_info in
              {trans_state with succ_nodes= [join_node]}
            in
            dtor_call stmt_info dtor_trans_state dtor_decl_ref temporary
            |> PriorityNode.compute_result_to_parent dtor_trans_state sil_loc
                 (Destruction destr_kind) stmt_info
          in
          (* create prune nodes *)
          let marker_id = Ident.create_fresh Ident.knormal in
          let deref_marker_var =
            [Sil.Load {id= marker_id; e= Lvar marker_var; typ= StdTyp.boolean; loc= sil_loc}]
          in
          let prune_true =
            create_prune_node proc_desc ~branch:true ~negate_cond:false (Var marker_id)
              deref_marker_var sil_loc if_kind
          in
          let prune_false =
            create_prune_node proc_desc ~branch:false ~negate_cond:true (Var marker_id)
              deref_marker_var sil_loc if_kind
          in
          Procdesc.node_set_succs proc_desc prune_false ~normal:[join_node] ~exn:[] ;
          Procdesc.node_set_succs proc_desc prune_true ~normal:dtor_trans_result.control.root_nodes
            ~exn:[] ;
          let trans_result =
            let control =
              {empty_control with root_nodes= [prune_true; prune_false]; leaf_nodes= [join_node]}
            in
            mk_trans_result (Var marker_id, StdTyp.boolean) control
          in
          (true, trans_result)
    in
    let rec destroy_all stmt_info trans_results_acc trans_state = function
      | [] ->
          trans_results_acc
      | temporary :: temporaries -> (
        match destroy_one stmt_info trans_state temporary with
        | None ->
            destroy_all stmt_info trans_results_acc trans_state temporaries
        | Some (new_node_created, trans_result) ->
            let stmt_info, trans_state =
              if new_node_created then
                (* force the creation of another new node so that it can be placed *before* (in CFG
                   order) the translation of the previous temporary destruction *)
                let stmt_info =
                  {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()}
                in
                (stmt_info, PriorityNode.force_claim_priority_node trans_state stmt_info)
              else (stmt_info, trans_state)
            in
            destroy_all stmt_info (trans_result :: trans_results_acc) trans_state temporaries )
    in
    L.debug Capture Verbose "Destroying [%a] in state %a@\n" CContext.CXXTemporarySet.pp temporaries
      pp_trans_state trans_state ;
    match destroy_all stmt_info [] trans_state (CContext.CXXTemporarySet.elements temporaries) with
    | [] ->
        expr_trans_result
    | _ :: _ as destr_trans_results ->
        (* wire [destr_trans_result] after [expr_trans_result] in CFG order *)
        PriorityNode.compute_results_to_parent trans_state sil_loc (Destruction destr_kind)
          stmt_info ~return:expr_trans_result.return
          (expr_trans_result :: destr_trans_results)


  and exprWithCleanups_trans trans_state stmt_info stmt_list =
    L.debug Capture Verbose "ExprWithCleanups trans_state={succ_nodes=[%a]}@\n"
      (Pp.seq Procdesc.Node.pp) trans_state.succ_nodes ;
    let loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    (* We need to pass a set of temporaries to destroy to the translation of the sub-expression but
       we cannot know exactly what they will be until *after* the translation (see also
       [CXXBindTemporaryExpr])... Fortunately the translation of the sub-expression only needs to
       know a *superset* of the temporaries to destroy, which is easier to compute. We'll get the
       real set of temporaries to destroy after when filtering to keep only those that are local
       variables. *)
    let temporaries_to_destroy =
      CScope.CXXTemporaries.get_destroyable_temporaries trans_state.context stmt_list
    in
    let[@warning "-partial-match"] [stmt] = stmt_list in
    let temporaries_constructor_markers =
      CContext.CXXTemporarySet.fold
        (fun {CContext.pvar; typ; marker} markers ->
          match marker with
          | None ->
              markers
          | Some (marker_pvar, _if_kind) ->
              Pvar.Map.add pvar (marker_pvar, typ) markers )
        temporaries_to_destroy trans_state.context.temporaries_constructor_markers
    in
    (* translate the sub-expression in its own node(s) so we can inject code for managing
       conditional destructor markers before and after its nodes *)
    let trans_state =
      PriorityNode.force_claim_priority_node
        {trans_state with context= {trans_state.context with temporaries_constructor_markers}}
        stmt_info
    in
    let sub_expr_result = instruction trans_state stmt in
    (* HACK: we can know which C++ temporaries were actually used by the translation of the
       sub-expression by inspecting the local variables *)
    let temporaries_to_destroy =
      CContext.CXXTemporarySet.filter
        (fun {CContext.pvar} -> Procdesc.is_local trans_state.context.procdesc pvar)
        temporaries_to_destroy
    in
    L.debug Capture Verbose "sub_expr_result.control=%a@\n" pp_control sub_expr_result.control ;
    let init_markers_to_zero_instrs =
      CContext.CXXTemporarySet.fold
        (fun {CContext.marker} instrs ->
          match marker with
          | None ->
              instrs
          | Some (marker_pvar, _if_kind) ->
              Sil.Metadata
                (VariableLifetimeBegins
                   {pvar= marker_pvar; typ= StdTyp.boolean; loc; is_cpp_structured_binding= false}
                )
              :: Sil.Store {e1= Lvar marker_pvar; e2= Exp.zero; typ= StdTyp.boolean; loc}
              :: instrs )
        temporaries_to_destroy []
    in
    let markers_var_data =
      CContext.CXXTemporarySet.fold
        (fun {CContext.marker} vds ->
          match marker with
          | None ->
              vds
          | Some (marker_pvar, _if_kind) ->
              let var_data = ProcAttributes.default_var_data marker_pvar StdTyp.boolean in
              var_data :: vds )
        temporaries_to_destroy []
    in
    Procdesc.append_locals trans_state.context.procdesc markers_var_data ;
    let init_then_sub_expr_result =
      if List.is_empty init_markers_to_zero_instrs then sub_expr_result
      else
        (* another new node so the initialization of markers really happens before the current node
           *)
        let stmt_info = {stmt_info with Clang_ast_t.si_pointer= CAst_utils.get_fresh_pointer ()} in
        let trans_state = PriorityNode.force_claim_priority_node trans_state stmt_info in
        let markers_init_result =
          PriorityNode.compute_result_to_parent trans_state loc ExprWithCleanups stmt_info
            (mk_trans_result (Exp.zero, StdTyp.boolean)
               {empty_control with instrs= init_markers_to_zero_instrs} )
        in
        PriorityNode.compute_results_to_parent trans_state loc ExprWithCleanups stmt_info
          ~return:sub_expr_result.return
          [markers_init_result; sub_expr_result]
    in
    let result =
      add_cxx_temporaries_dtor_calls Procdesc.Node.DestrTemporariesCleanup init_then_sub_expr_result
        trans_state stmt_info temporaries_to_destroy
    in
    if List.is_empty result.control.cxx_temporary_markers_set then result
    else
      (* we know that there cannot be nested ExprWithCleanups so it's safe to remove all markers *)
      {result with control= {result.control with cxx_temporary_markers_set= []}}


  and coroutineBodyStmt_trans trans_state stmt_info body_ptr promise_ptr return_value =
    let source_range = stmt_info.Clang_ast_t.si_source_range in
    let body = CAst_utils.get_stmt_exn body_ptr source_range in
    let promise = CAst_utils.get_stmt_exn promise_ptr source_range in
    exec_with_node_creation Procdesc.Node.DefineBody trans_state ~f:instruction
      (CompoundStmt (stmt_info, [promise; body; Clang_ast_t.ReturnStmt (stmt_info, [return_value])]))


  and coreturnStmt_trans trans_state stmt_info operand_opt promise_call_opt =
    let args = Option.to_list operand_opt @ Option.to_list promise_call_opt in
    call_function_with_args Procdesc.Node.ReturnStmt BuiltinDecl.__builtin_cxx_co_return trans_state
      stmt_info StdTyp.void args


  and coroutineSuspendExpr_trans trans_state stmt_info expr_info cse_operand =
    (* confuse [co_await] and [co_yield] because for now we don't care about their accurate
       semantics anywhere *)
    let return_type =
      CType_decl.qual_type_to_sil_type trans_state.context.tenv expr_info.Clang_ast_t.ei_qual_type
    in
    call_function_with_args Procdesc.Node.ReturnStmt BuiltinDecl.__builtin_cxx_co_await trans_state
      stmt_info return_type [cse_operand]


  (* Expect that this doesn't happen *)
  and undefined_expr trans_state expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    mk_trans_result (CTrans_utils.undefined_expression (), typ) empty_control


  (** no-op translated for unsupported instructions that will at least translate subexpressions *)
  and skip_unimplemented ~pp_unimplemented trans_state stmt_info ret_typ stmts =
    let has_args =
      (* if the sub-statements are expressions then they can reasonably be assumed to be arguments
         to some unknown effect that the unimplemented statement kind has; if not then said
         statement is something else. In any case if the sub-statements are *not* expressions then
         treating them as arguments in [call_function_with_args] below will crash. *)
      List.for_all stmts ~f:(fun stmt -> Clang_ast_proj.get_expr_tuple stmt |> Option.is_some)
    in
    let args = if has_args then stmts else [] in
    L.debug Capture Medium "Skipping unimplemented %t" pp_unimplemented ;
    call_function_with_args Procdesc.Node.Skip BuiltinDecl.__infer_skip trans_state stmt_info
      ret_typ args


  and instruction trans_state instr = instruction_log trans_state instr

  (** Translates a clang instruction into SIL instructions. It takes a a [trans_state] containing
      current info on the translation and it returns a [trans_result].*)
  and instruction_log =
    (* log errors only at the innermost recursive call *)
    let logged_error = ref false in
    fun trans_state instr ->
      let pp_pointer f instr =
        let {Clang_ast_t.si_pointer}, _ = Clang_ast_proj.get_stmt_tuple instr in
        Format.pp_print_int f si_pointer
      in
      L.debug Capture Verbose
        "Translating statement '%a' (pointer= '%a')@\n@[<hv2>  trans_state=%a@\n"
        (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
        instr pp_pointer instr pp_trans_state trans_state ;
      let trans_result =
        try instruction_scope trans_state instr
        with e ->
          IExn.reraise_after e ~f:(fun () ->
              let should_log_error = not !logged_error in
              if should_log_error then (
                (* prevent from displaying the same issue multiple times as we climb up the call
                   stack *)
                logged_error := true ;
                let {Clang_ast_t.si_source_range}, _ = Clang_ast_proj.get_stmt_tuple instr in
                let source_file =
                  trans_state.context.CContext.translation_unit_context.CFrontend_config.source_file
                in
                let loc_start =
                  CLocation.location_of_source_range ~pick_location:`Start source_file
                    si_source_range
                in
                let loc_end =
                  CLocation.location_of_source_range ~pick_location:`End source_file si_source_range
                in
                (* Unfortunately this triggers regularly so do not show the message on the console
                   unless asked to do so or if the error will crash the frontend. *)
                let should_display_error =
                  Config.debug_level_capture >= 1
                  ||
                  match e with
                  | CFrontend_errors.Unimplemented _ | CFrontend_errors.IncorrectAssumption _ ->
                      (* these are caught by default, do not print messages unless asked to do so *)
                      false
                  | _ ->
                      (* we are going to crash, print message for more context *)
                      true
                in
                (if should_display_error then L.internal_error else L.debug Capture Quiet)
                  "%a: ERROR translating statement '%a'@\n" Location.pp_range (loc_start, loc_end)
                  (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
                  instr ) )
      in
      L.debug Capture Verbose
        "@]@\nResult of translating statement '%a' (pointer= '%a')@\ncontrol=%a@\n"
        (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
        instr pp_pointer instr pp_control trans_result.control ;
      (* don't forget to reset this so we output messages for future errors too *)
      logged_error := false ;
      trans_result


  (** inject destructors at the end of the translation of the statement if the context map says
      there are variables to destruct *)
  and instruction_scope trans_state stmt =
    let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
    let destr_trans_result =
      (* Statements that break control flow will inject appropriate destructor calls themselves and
         trying to insert them again here would be wasteful and produce nodes that are "unconnected"
         (without a predecessor in the CFG) since the inner statement translation will, well.. break
         control flow. *)
      if CScope.breaks_control_flow stmt then (
        L.debug Capture Verbose "break of control flow detected, skipping destructor injection@\n" ;
        None )
      else inject_destructors Procdesc.Node.DestrScope trans_state stmt_info
    in
    (* adjust successors of the translation of [stmt] to point at the injected destructors *)
    let trans_state =
      match destr_trans_result with
      | Some {control= {root_nodes= []}} | None ->
          trans_state
      | Some {control= {root_nodes}} ->
          {trans_state with succ_nodes= root_nodes}
    in
    let stmt_result = instruction_insert_cxx_temporary_markers trans_state stmt in
    match destr_trans_result with
    | None | Some {control= {leaf_nodes= []}} ->
        stmt_result
    | Some {control= {leaf_nodes}} ->
        {stmt_result with control= {stmt_result.control with leaf_nodes}}


  (** Instrument program to know when C++ temporaries created conditionally have indeed been created
      and need to be destroyed. This is a common compilation scheme for temporary variables created
      conditionally in expressions, due to either [a?b:c] or short-circuiting of [&&] or [||].

      Similarly to [exprWithCleanups_trans], only act on the C++ temporaries that we ended up using
      in the translation of the sub-expression. Here the fact we only take variables initialized by
      the sub-expression ensures that this is always the case. *)
  and instruction_insert_cxx_temporary_markers trans_state stmt =
    let stmt_result = instruction_translate trans_state stmt in
    let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
    let loc =
      CLocation.location_of_stmt_info trans_state.context.translation_unit_context.source_file
        stmt_info
    in
    let instrs, cxx_temporary_markers_set =
      List.fold stmt_result.control.initd_exps
        ~init:([], stmt_result.control.cxx_temporary_markers_set)
        ~f:(fun ((instrs, markers_set) as acc) initd_exp ->
          match initd_exp with
          | Exp.Lvar initd_pvar -> (
            match
              Pvar.Map.find_opt initd_pvar trans_state.context.temporaries_constructor_markers
            with
            | Some (marker_var, _)
              when not
                     (List.mem ~equal:Pvar.equal stmt_result.control.cxx_temporary_markers_set
                        marker_var ) ->
                (* to avoid adding the marker-setting instruction for every super-expression of the
                   current one, add it to the list of marker variables set and do not create the
                   instruction if it's already in that list *)
                let store_marker =
                  Sil.Store {e1= Lvar marker_var; typ= StdTyp.boolean; e2= Exp.one; loc}
                in
                (store_marker :: instrs, marker_var :: markers_set)
            | _ ->
                acc )
          | _ ->
              acc )
    in
    if List.is_empty instrs then stmt_result
    else
      let control =
        PriorityNode.compute_controls_to_parent trans_state loc CXXTemporaryMarkerSet stmt_info
          [stmt_result.control; {empty_control with instrs}]
      in
      let control = {control with cxx_temporary_markers_set} in
      {stmt_result with control}


  and instruction_translate trans_state (instr : Clang_ast_t.stmt) =
    match instr with
    | GotoStmt (stmt_info, _, {Clang_ast_t.gsi_label= label_name; _}) ->
        gotoStmt_trans trans_state stmt_info label_name
    | LabelStmt (stmt_info, stmt_list, label_name) ->
        labelStmt_trans trans_state stmt_info stmt_list label_name
    | ArraySubscriptExpr (_, stmt_list, expr_info) ->
        arraySubscriptExpr_trans trans_state expr_info stmt_list
    | BinaryOperator (stmt_info, stmt_list, expr_info, binop_info) ->
        binaryOperator_trans_with_cond trans_state stmt_info stmt_list expr_info binop_info
    | AtomicExpr (stmt_info, stmt_list, expr_info, atomic_info) ->
        atomicExpr_trans trans_state atomic_info stmt_info expr_info stmt_list
    | CallExpr (stmt_info, stmt_list, ei) | UserDefinedLiteral (stmt_info, stmt_list, ei) ->
        callExpr_trans trans_state stmt_info stmt_list ei
    | ConstantExpr (_, stmt_list, _) -> (
      match stmt_list with
      | [stmt] ->
          instruction_translate trans_state stmt
      | stmts ->
          L.die InternalError "Expected exactly one statement in ConstantExpr, got %d"
            (List.length stmts) )
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
    | CompoundStmt (_, stmt_list) ->
        (* No node for this statement. We just collect its statement list*)
        compoundStmt_trans trans_state stmt_list
    | ConditionalOperator (stmt_info, stmt_list, expr_info) ->
        (* Ternary operator "cond ? exp1 : exp2" *)
        conditionalOperator_trans trans_state stmt_info stmt_list expr_info
    | IfStmt (stmt_info, _, if_stmt_info) ->
        ifStmt_trans trans_state stmt_info if_stmt_info
    | SwitchStmt (stmt_info, _, switch_stmt_info) ->
        switchStmt_trans trans_state stmt_info switch_stmt_info
    | CaseStmt (stmt_info, stmt_list) ->
        caseStmt_trans trans_state stmt_info stmt_list
    | DefaultStmt (stmt_info, stmt_list) ->
        defaultStmt_trans trans_state stmt_info stmt_list
    | StmtExpr ({si_source_range}, stmt_list, _)
    | CXXRewrittenBinaryOperator ({si_source_range}, stmt_list, _) ->
        stmtExpr_trans trans_state si_source_range stmt_list
    | ForStmt (stmt_info, [init; decl_stmt; condition; increment; body]) ->
        forStmt_trans trans_state ~init ~decl_stmt ~condition ~increment ~body stmt_info
    | WhileStmt (stmt_info, [condition; body]) ->
        whileStmt_trans trans_state ~decl_stmt:None ~condition ~body stmt_info
    | WhileStmt (stmt_info, [decl_stmt; condition; body]) ->
        whileStmt_trans trans_state ~decl_stmt:(Some decl_stmt) ~condition ~body stmt_info
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
    | ImplicitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
    | BuiltinBitCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
    | CStyleCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _)
    | CXXReinterpretCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXConstCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXFunctionalCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _) ->
        cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind
    | CXXStaticCastExpr (stmt_info, stmt_list, expr_info, cast_kind, qual_type, _) ->
        cast_exprs_trans trans_state ~cxx_static_cast:qual_type stmt_info stmt_list expr_info
          cast_kind
    | ObjCBridgedCastExpr (stmt_info, stmt_list, expr_info, cast_kind, _, objc_bridge_cast_ei) ->
        let objc_bridge_cast_kind = objc_bridge_cast_ei.Clang_ast_t.obcei_cast_kind in
        cast_exprs_trans trans_state stmt_info stmt_list expr_info ~objc_bridge_cast_kind cast_kind
    | IntegerLiteral (_, _, expr_info, integer_literal_info) ->
        integerLiteral_trans trans_state expr_info integer_literal_info
    | OffsetOfExpr (stmt_info, _, expr_info, offset_of_expr_info) ->
        offsetOf_trans trans_state expr_info offset_of_expr_info stmt_info
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
    | MemberExpr (stmt_info, stmt_list, expr_info, member_expr_info) ->
        memberExpr_trans trans_state stmt_info stmt_list expr_info member_expr_info
    | UnaryOperator (stmt_info, stmt_list, expr_info, unary_operator_info) ->
        if
          is_logical_negation_of_int trans_state.context.CContext.tenv expr_info unary_operator_info
        then
          let conditional =
            Ast_expressions.trans_negation_with_conditional stmt_info expr_info stmt_list
          in
          instruction trans_state conditional
        else unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info
    | ReturnStmt (stmt_info, stmt_list) ->
        returnStmt_trans trans_state stmt_info stmt_list
    | ExprWithCleanups (stmt_info, stmt_list, _, _) ->
        exprWithCleanups_trans trans_state stmt_info stmt_list
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
    | ObjCBoxedExpr (stmt_info, stmts, info, boxed_expr_info) ->
        (* Sometimes clang does not return a boxing method (a name of function to apply), e.g.,
           [@("str")].  In that case, it uses "unknownSelector:" instead of giving up the
           translation. *)
        let sel =
          Option.value boxed_expr_info.Clang_ast_t.obei_boxing_method ~default:"unknownSelector:"
        in
        objCBoxedExpr_trans trans_state info sel stmt_info stmts
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
        let control, returns =
          instructions Procdesc.Node.ObjCIndirectCopyRestoreExpr trans_state stmt_list
        in
        mk_trans_result (last_or_mk_fresh_void_exp_typ returns) control
    | BlockExpr (stmt_info, _, expr_info, decl) ->
        blockExpr_trans trans_state stmt_info expr_info decl
    | ObjCAutoreleasePoolStmt (stmt_info, stmts) ->
        objCAutoreleasePoolStmt_trans trans_state stmt_info stmts
    | ObjCAtTryStmt (_, stmts) ->
        compoundStmt_trans trans_state stmts
    | CXXTryStmt (stmt_info, try_stmts) ->
        tryStmt_trans trans_state stmt_info try_stmts
    | CXXCatchStmt _ ->
        (* should by handled by try statement *)
        assert false
    | ObjCAtThrowStmt (stmt_info, stmts) | CXXThrowExpr (stmt_info, stmts, _) ->
        objc_cxx_throw_trans trans_state stmt_info stmts
    | ObjCAtFinallyStmt (_, stmts) ->
        compoundStmt_trans trans_state stmts
    | ObjCAtCatchStmt _ ->
        compoundStmt_trans trans_state []
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
    | CXXBindTemporaryExpr (stmt_info, stmt_list, expr_info, _) ->
        cxxBindTemporaryExpr_trans trans_state stmt_info stmt_list expr_info
    | CompoundLiteralExpr (stmt_info, stmt_list, expr_info) ->
        compoundLiteralExpr_trans trans_state stmt_list stmt_info expr_info
    | InitListExpr (stmt_info, stmts, expr_info) ->
        initListExpr_trans trans_state stmt_info expr_info stmts
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
        mk_trans_result (Exp.get_undefined false, StdTyp.void) empty_control
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
    | VAArgExpr (_, [], expr_info) ->
        undefined_expr trans_state expr_info
    | VAArgExpr (stmt_info, stmt :: _, ei) ->
        va_arg_trans trans_state stmt_info stmt ei
    | ArrayInitIndexExpr _ | ArrayInitLoopExpr _ ->
        no_op_trans trans_state.succ_nodes
    (* vector instructions for OpenCL etc. we basically ignore these for now; just translate the
       sub-expressions *)
    | ObjCAvailabilityCheckExpr (_, _, expr_info, _) ->
        undefined_expr trans_state expr_info
    | SubstNonTypeTemplateParmExpr (_, stmts, _) | SubstNonTypeTemplateParmPackExpr (_, stmts, _) ->
        let[@warning "-partial-match"] [expr] = stmts in
        instruction trans_state expr
    (* Infer somehow ended up in templated non instantiated code - right now
       it's not supported and failure in those cases is expected. *)
    | CXXDependentScopeMemberExpr ({Clang_ast_t.si_source_range}, _, _) ->
        CFrontend_errors.unimplemented __POS__ si_source_range
          ~ast_node:(Clang_ast_proj.get_stmt_kind_string instr)
          "Translation of templated code is unsupported: %a"
          (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
          instr
    | ForStmt ({Clang_ast_t.si_source_range}, _)
    | WhileStmt ({Clang_ast_t.si_source_range}, _)
    | DoStmt ({Clang_ast_t.si_source_range}, _)
    | ObjCForCollectionStmt ({Clang_ast_t.si_source_range}, _) ->
        CFrontend_errors.incorrect_assumption __POS__ si_source_range "Unexpected shape for %a: %a"
          (Pp.of_string ~f:Clang_ast_proj.get_stmt_kind_string)
          instr
          (Pp.of_string ~f:Clang_ast_j.string_of_stmt)
          instr
    | CoroutineBodyStmt (stmt_info, _, {cbs_body; cbs_promise_decl_stmt; cbs_return_value}) ->
        coroutineBodyStmt_trans trans_state stmt_info cbs_body cbs_promise_decl_stmt
          cbs_return_value
    | CoreturnStmt (stmt_info, _, {coret_operand; coret_promise_call}) ->
        coreturnStmt_trans trans_state stmt_info coret_operand coret_promise_call
    | CoawaitExpr (stmt_info, operand :: _, expr_info)
    | CoyieldExpr (stmt_info, operand :: _, expr_info) ->
        coroutineSuspendExpr_trans trans_state stmt_info expr_info operand
    | AddrLabelExpr _
    | ArrayTypeTraitExpr _
    | AsTypeExpr _
    | CapturedStmt _
    | ChooseExpr _
    | CoawaitExpr (_, [], _)
    | ConceptSpecializationExpr _
    | ConvertVectorExpr _
    | CoyieldExpr (_, [], _)
    | CUDAKernelCallExpr _
    | CXXAddrspaceCastExpr _
    | CXXFoldExpr _
    | CXXParenListInitExpr _
    | CXXUnresolvedConstructExpr _
    | CXXUuidofExpr _
    | DependentCoawaitExpr _
    | DependentScopeDeclRefExpr _
    | DesignatedInitExpr _
    | DesignatedInitUpdateExpr _
    | ExpressionTraitExpr _
    | ExtVectorElementExpr _
    | FunctionParmPackExpr _
    | ImaginaryLiteral _
    | IndirectGotoStmt _
    | MatrixSubscriptExpr _
    | MSAsmStmt _
    | MSDependentExistsStmt _
    | MSPropertyRefExpr _
    | MSPropertySubscriptExpr _
    | NoInitExpr _
    | ObjCIsaExpr _
    | ObjCSubscriptRefExpr _
    | OMPArraySectionExpr _
    | OMPArrayShapingExpr _
    | OMPAtomicDirective _
    | OMPBarrierDirective _
    | OMPCancelDirective _
    | OMPCancellationPointDirective _
    | OMPCanonicalLoop _
    | OMPCriticalDirective _
    | OMPDepobjDirective _
    | OMPDispatchDirective _
    | OMPDistributeDirective _
    | OMPDistributeParallelForDirective _
    | OMPDistributeParallelForSimdDirective _
    | OMPDistributeSimdDirective _
    | OMPErrorDirective _
    | OMPFlushDirective _
    | OMPForDirective _
    | OMPForSimdDirective _
    | OMPGenericLoopDirective _
    | OMPInteropDirective _
    | OMPIteratorExpr _
    | OMPMaskedDirective _
    | OMPMaskedTaskLoopDirective _
    | OMPMaskedTaskLoopSimdDirective _
    | OMPMasterDirective _
    | OMPMasterTaskLoopDirective _
    | OMPMasterTaskLoopSimdDirective _
    | OMPMetaDirective _
    | OMPOrderedDirective _
    | OMPParallelDirective _
    | OMPParallelForDirective _
    | OMPParallelForSimdDirective _
    | OMPParallelGenericLoopDirective _
    | OMPParallelMaskedDirective _
    | OMPParallelMaskedTaskLoopDirective _
    | OMPParallelMaskedTaskLoopSimdDirective _
    | OMPParallelMasterDirective _
    | OMPParallelMasterTaskLoopDirective _
    | OMPParallelMasterTaskLoopSimdDirective _
    | OMPParallelSectionsDirective _
    | OMPScanDirective _
    | OMPScopeDirective _
    | OMPSectionDirective _
    | OMPSectionsDirective _
    | OMPSimdDirective _
    | OMPSingleDirective _
    | OMPTargetDataDirective _
    | OMPTargetDirective _
    | OMPTargetEnterDataDirective _
    | OMPTargetExitDataDirective _
    | OMPTargetParallelDirective _
    | OMPTargetParallelForDirective _
    | OMPTargetParallelForSimdDirective _
    | OMPTargetParallelGenericLoopDirective _
    | OMPTargetSimdDirective _
    | OMPTargetTeamsDirective _
    | OMPTargetTeamsDistributeDirective _
    | OMPTargetTeamsDistributeParallelForDirective _
    | OMPTargetTeamsDistributeParallelForSimdDirective _
    | OMPTargetTeamsDistributeSimdDirective _
    | OMPTargetTeamsGenericLoopDirective _
    | OMPTargetUpdateDirective _
    | OMPTaskDirective _
    | OMPTaskgroupDirective _
    | OMPTaskLoopDirective _
    | OMPTaskLoopSimdDirective _
    | OMPTaskwaitDirective _
    | OMPTaskyieldDirective _
    | OMPTeamsDirective _
    | OMPTeamsDistributeDirective _
    | OMPTeamsDistributeParallelForDirective _
    | OMPTeamsDistributeParallelForSimdDirective _
    | OMPTeamsDistributeSimdDirective _
    | OMPTeamsGenericLoopDirective _
    | OMPTileDirective _
    | OMPUnrollDirective _
    | PackExpansionExpr _
    | ParenListExpr _
    | RecoveryExpr _
    | RequiresExpr _
    | SEHExceptStmt _
    | SEHFinallyStmt _
    | SEHLeaveStmt _
    | SEHTryStmt _
    | ShuffleVectorExpr _
    | SourceLocExpr _
    | SYCLUniqueStableNameExpr _
    | TypoExpr _
    | UnresolvedLookupExpr _
    | UnresolvedMemberExpr _ ->
        let (stmt_info, stmts), ret_typ =
          match Clang_ast_proj.get_expr_tuple instr with
          | Some (stmt_info, stmts, expr_info) ->
              let ret_typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.tenv in
              ((stmt_info, stmts), ret_typ)
          | None ->
              let stmt_tuple = Clang_ast_proj.get_stmt_tuple instr in
              (stmt_tuple, StdTyp.void)
        in
        skip_unimplemented
          ~pp_unimplemented:(fun fmt ->
            Format.fprintf fmt "%s, found at %s"
              (Clang_ast_proj.get_stmt_kind_string instr)
              (Clang_ast_j.string_of_source_range stmt_info.Clang_ast_t.si_source_range) )
          trans_state stmt_info ret_typ stmts


  (** Function similar to instruction function, but it takes C++ constructor initializer as an input
      parameter. *)
  and cxx_constructor_init_trans ctor_init trans_state =
    L.debug Capture Verbose "cxx_constructor_init_trans@\n  @[" ;
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
    let this_res_trans =
      this_expr_trans child_stmt_info trans_state' sil_loc
      |> PriorityNode.compute_result_to_parent trans_state' sil_loc ConstructorInit this_stmt_info
    in
    let var_res_trans =
      match ctor_init.Clang_ast_t.xci_subject with
      | `Delegating _ | `BaseClass _ ->
          let this_exp, this_typ = this_res_trans.return in
          (* Hack: Strip pointer from type here since cxxConstructExpr_trans expects it this way
             it will add pointer back before making it a parameter to a call *)
          let class_typ = match this_typ.Typ.desc with Tptr (t, _) -> t | _ -> assert false in
          {this_res_trans with return= (this_exp, class_typ)}
      | `Member decl_ref ->
          decl_ref_trans ~is_constructor_init:true ~context:(MemberOrIvar this_res_trans)
            trans_state' child_stmt_info decl_ref
    in
    let var_exp_typ = var_res_trans.return in
    let init_expr = ctor_init.Clang_ast_t.xci_init_expr in
    let init_res_trans = init_expr_trans trans_state' var_exp_typ child_stmt_info init_expr in
    let result =
      PriorityNode.compute_results_to_parent trans_state' sil_loc ConstructorInit this_stmt_info
        ~return:init_res_trans.return [var_res_trans; init_res_trans]
    in
    L.debug Capture Verbose "@]@\n" ;
    result


  (** Given a translation state and list of translation functions it executes translation *)
  and exec_trans_instrs trans_state trans_stmt_fun_list : control * (Exp.t * Typ.t) list =
    let rec exec_trans_instrs_rev trans_state rev_trans_fun_list =
      match rev_trans_fun_list with
      | [] ->
          ({empty_control with root_nodes= trans_state.succ_nodes}, [])
      | trans_stmt_fun :: trans_stmt_fun_list' ->
          let res_trans_s = trans_stmt_fun trans_state in
          let trans_state' =
            if not (List.is_empty res_trans_s.control.root_nodes) then
              {trans_state with succ_nodes= res_trans_s.control.root_nodes}
            else trans_state
          in
          let control_tail_rev, returns_tail_rev =
            (* Note: for assignment, we should keep only the value of the last instruction.
               This is handled above, and for all other instructions we must discard the value. *)
            exec_trans_instrs_rev {trans_state' with var_exp_typ= None} trans_stmt_fun_list'
          in
          ( { root_nodes= control_tail_rev.root_nodes
            ; leaf_nodes=
                ( if not (List.is_empty res_trans_s.control.leaf_nodes) then
                    res_trans_s.control.leaf_nodes
                  else control_tail_rev.leaf_nodes )
            ; instrs= List.rev_append res_trans_s.control.instrs control_tail_rev.instrs
            ; initd_exps= List.rev_append res_trans_s.control.initd_exps control_tail_rev.initd_exps
            ; cxx_temporary_markers_set=
                List.rev_append res_trans_s.control.cxx_temporary_markers_set
                  control_tail_rev.cxx_temporary_markers_set }
          , res_trans_s.return :: returns_tail_rev )
    in
    let rev_control, rev_returns =
      exec_trans_instrs_rev trans_state (List.rev trans_stmt_fun_list)
    in
    ({rev_control with instrs= List.rev rev_control.instrs}, List.rev rev_returns)


  and get_clang_stmt_trans node_name stmt trans_state =
    exec_with_node_creation ~f:instruction node_name trans_state stmt


  and get_custom_stmt_trans stmt trans_state =
    match (stmt : CFrontend_config.instr_type) with
    | ClangStmt (node_name, stmt) ->
        get_clang_stmt_trans node_name stmt trans_state
    | CXXConstructorInit instr ->
        cxx_constructor_init_trans instr trans_state


  (** Given a translation state, this function translates a list of clang statements. *)
  and instructions node_name trans_state stmt_list =
    let stmt_trans_fun = List.map ~f:(get_clang_stmt_trans node_name) stmt_list in
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
      | Procname.ObjC_Cpp cpp_pname ->
          Procname.ObjC_Cpp.is_destructor cpp_pname
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
        (* Injecting destructor call nodes of fields at the end of the body *)
        (cxx_inject_field_destructors_in_destructor_body trans_state stmt_info, body)
      else (None, body)
    in
    let succ_nodes =
      match destructor_res with
      | Some {control= {root_nodes= _ :: _ as root_nodes}} ->
          root_nodes
      | Some {control= {root_nodes= []}} | None ->
          trans_state.succ_nodes
    in
    let trans_state' = {trans_state with succ_nodes} in
    let instrs = extra_instrs @ [CFrontend_config.ClangStmt (DefineBody, body)] in
    let instrs_trans = List.map ~f:get_custom_stmt_trans instrs in
    let res_control, _ = exec_trans_instrs trans_state' instrs_trans in
    res_control.root_nodes
end
