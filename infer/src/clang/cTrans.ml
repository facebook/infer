(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Translates instructions: (statements and expressions) from the ast into sil *)

open CTrans_utils
open CTrans_utils.Nodes

module L = Logging

module CTrans_funct(F: CModule_type.CFrontend) : CModule_type.CTranslation =
struct
  (* Returns the procname and whether is instance, according to the selector information and
     according to the method signature with the following priority:
     1. method is a predefined model
     2. method is found by clang's resolution
     3. Method is found by our resolution *)
  let get_callee_objc_method context obj_c_message_expr_info act_params =
    let open CContext in
    let (selector, method_pointer_opt, mc_type) =
      CMethod_trans.get_objc_method_data obj_c_message_expr_info in
    let is_instance = mc_type <> CMethod_trans.MCStatic in
    let method_kind = Typ.Procname.objc_method_kind_of_bool is_instance in
    let ms_opt =
      match method_pointer_opt with
      | Some pointer ->
          CMethod_trans.method_signature_of_pointer context.translation_unit_context context.tenv
            pointer
      | None -> None in
    let proc_name =
      match CMethod_trans.get_method_name_from_clang context.tenv ms_opt with
      | Some ms ->
          CMethod_signature.ms_get_name ms
      | None ->  (* fall back to our method resolution if clang's fails *)
          let class_name = CMethod_trans.get_class_name_method_call_from_receiver_kind context
              obj_c_message_expr_info act_params in
          CProcname.NoAstDecl.objc_method_of_string_kind class_name selector method_kind in
    let predefined_ms_opt = match proc_name with
      | Typ.Procname.ObjC_Cpp objc_cpp ->
          let class_name = Typ.Procname.objc_cpp_get_class_type_name objc_cpp in
          CTrans_models.get_predefined_model_method_signature class_name selector
            CProcname.NoAstDecl.objc_method_of_string_kind CFrontend_config.ObjC
      | _ ->
          None in
    match predefined_ms_opt, ms_opt with
    | Some ms, _ ->
        ignore (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
                  context.tenv ms [] [] is_instance);
        CMethod_signature.ms_get_name ms, CMethod_trans.MCNoVirtual
    | None, Some ms ->
        ignore (CMethod_trans.create_local_procdesc context.translation_unit_context context.cfg
                  context.tenv ms [] [] is_instance);
        if CMethod_signature.ms_is_getter ms || CMethod_signature.ms_is_setter ms then
          proc_name, CMethod_trans.MCNoVirtual
        else
          proc_name, mc_type
    | _ ->
        CMethod_trans.create_external_procdesc context.cfg proc_name is_instance None;
        proc_name, mc_type


  let add_autorelease_call context exp typ sil_loc =
    let method_name = Typ.Procname.get_method (Procdesc.get_proc_name context.CContext.procdesc) in
    if !Config.arc_mode &&
       not (CTrans_utils.is_owning_name method_name) &&
       ObjcInterface_decl.is_pointer_to_objc_class typ then
      let fname = BuiltinDecl.__set_autorelease_attribute in
      let ret_id = Some (Ident.create_fresh Ident.knormal, Typ.Tvoid) in
      (* TODO(jjb): change ret_id to None? *)
      let stmt_call =
        Sil.Call (ret_id, Exp.Const (Const.Cfun fname), [(exp, typ)], sil_loc, CallFlags.default) in
      [stmt_call]
    else []

  let rec is_block_expr s =
    let open Clang_ast_t in
    match s with
    | BlockExpr _ -> true
    (* the block can be wrapped in ExprWithCleanups  or ImplicitCastExpr*)
    | ImplicitCastExpr(_, [s'], _, _)
    | ExprWithCleanups(_, [s'], _, _) -> is_block_expr s'
    | _ -> false

  let objc_exp_of_type_block fun_exp_stmt =
    match fun_exp_stmt with
    | Clang_ast_t.ImplicitCastExpr(_, _, ei, _)
      when CType.is_block_type ei.Clang_ast_t.ei_type_ptr -> true
    | _ -> false

  (* This function add in tenv a class representing an objc block. *)
  (* An object of this class has type:*)
  (* name_of_block |-> {capture_var1:typ_of_capture_var1,... capture_varn:typ_of_capture_varn} *)
  (* It allocates one element and sets its fields with the current values of the *)
  (* captured variables. This allocated instance
     is used to detect retain cycles involving the block.*)
  let allocate_block trans_state block_name captured_vars loc =
    let tenv = trans_state.context.CContext.tenv in
    let procdesc = trans_state.context.CContext.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let mk_field_from_captured_var (var, typ) =
      let vname = Pvar.get_name var in
      let qual_name = CAst_utils.make_qual_name_decl [block_name] (Mangled.to_string vname) in
      let fname = CGeneral_utils.mk_class_field_name qual_name in
      let item_annot = Annot.Item.empty in
      fname, typ, item_annot in
    let fields = List.map ~f:mk_field_from_captured_var captured_vars in
    Logging.out_debug "Block %s field:\n" block_name;
    List.iter ~f:(fun (fn, _, _) ->
        Logging.out_debug "-----> field: '%s'\n" (Ident.fieldname_to_string fn)) fields;
    let block_typename = Typ.Name.Objc.from_string block_name in
    ignore (Tenv.mk_struct tenv ~fields block_typename);
    let block_type = Typ.Tstruct block_typename in
    let trans_res =
      CTrans_utils.alloc_trans
        trans_state loc (Ast_expressions.dummy_stmt_info ()) block_type true None in
    let id_block = match trans_res.exps with
      | [(Exp.Var id, _)] -> id
      | _ -> assert false in
    let mblock = Mangled.from_string block_name in
    let block_var = Pvar.mk mblock procname in
    let declare_block_local =
      Sil.Declare_locals ([(block_var, Typ.Tptr (block_type, Typ.Pk_pointer))], loc) in
    let set_instr = Sil.Store (Exp.Lvar block_var, block_type, Exp.Var id_block, loc) in
    let create_field_exp (var, typ) =
      let id = Ident.create_fresh Ident.knormal in
      id, Sil.Load (id, Exp.Lvar var, typ, loc) in
    let ids, captured_instrs = List.unzip (List.map ~f:create_field_exp captured_vars) in
    let fields_ids = List.zip_exn fields ids in
    let set_fields = List.map ~f:(fun ((f, t, _), id) ->
        Sil.Store (Exp.Lfield (Exp.Var id_block, f, block_type), t, Exp.Var id, loc)) fields_ids in
    (declare_block_local :: trans_res.instrs) @
    [set_instr] @
    captured_instrs @
    set_fields

  (* From a list of expression extract blocks from tuples and *)
  (* returns block names and assignment to temp vars  *)
  let extract_block_from_tuple procname exps loc =
    let insts = ref [] in
    let make_function_name typ bn =
      let bn'= Typ.Procname.to_string bn in
      let bn''= Mangled.from_string bn' in
      let block = Exp.Lvar (Pvar.mk bn'' procname) in
      let id = Ident.create_fresh Ident.knormal in
      insts := Sil.Load (id, block, typ, loc) :: !insts;
      (Exp.Var id, typ) in
    let make_arg typ (id, _, _) = (id, typ) in
    let rec f es =
      match es with
      | [] -> []
      | (Exp.Closure {name; captured_vars},
         (Typ.Tptr((Typ.Tfun _), _ ) as t)) :: es' ->
          let app =
            let function_name = make_function_name t name in
            let args = List.map ~f:(make_arg t) captured_vars in
            function_name :: args in
          app @ (f es')
      | e :: es' -> e :: f es' in
    (f exps, !insts)

  let collect_exprs res_trans_list =
    List.concat_map ~f:(fun res_trans -> res_trans.exps) res_trans_list

  let collect_initid_exprs res_trans_list =
    List.concat_map ~f:(fun res_trans -> res_trans.initd_exps) res_trans_list

  (* If e is a block and the calling node has the priority then *)
  (* we need to release the priority to allow*)
  (* creation of nodes inside the block.*)
  (* At the end of block translation, we need to get the proirity back.*)
  (* the parameter f will be called with function instruction *)
  let exec_with_block_priority_exception f trans_state e stmt_info =
    if (is_block_expr e) && (PriorityNode.own_priority_node trans_state.priority stmt_info) then (
      Logging.out_debug "Translating block expression by freeing the priority";
      f { trans_state with priority = Free } e)
    else f trans_state e

  let exec_with_node_creation f trans_state stmt =
    let res_trans = f trans_state stmt in
    if res_trans.instrs <> [] then
      let stmt_info, _ = Clang_ast_proj.get_stmt_tuple stmt in
      let stmt_info' = { stmt_info with
                         Clang_ast_t.si_pointer = CAst_utils.get_fresh_pointer () } in
      let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info' in
      let sil_loc = CLocation.get_sil_location stmt_info' trans_state.context in
      let to_parent = PriorityNode.compute_results_to_parent
          trans_state_pri sil_loc "Fallback node" stmt_info' [res_trans] in
      { to_parent with exps = res_trans.exps }
    else
      res_trans

  (* This is the standard way of dealing with self:Class or a call [a class]. We translate it as
     sizeof(<type pf a>) The only time when we want to translate those expressions differently is
     when they are the first argument of method calls. In that case they are not translated as
     expressions, but we take the type and create a static method call from it. This is done in
     objcMessageExpr_trans. *)
  let exec_with_self_exception f trans_state stmt =
    try
      f trans_state stmt
    with Self.SelfClassException class_name ->
      let typ = Typ.Tstruct class_name in
      { empty_res_trans with
        exps = [(Exp.Sizeof (typ, None, Subtype.exact), Tint IULong)] }

  let add_reference_if_glvalue typ expr_info =
    (* glvalue definition per C++11:*)
    (* http://en.cppreference.com/w/cpp/language/value_category *)
    let is_glvalue = match expr_info.Clang_ast_t.ei_value_kind with
      | `LValue | `XValue -> true
      | `RValue -> false in
    match is_glvalue, typ with
    | true, Typ.Tptr (_, Typ.Pk_reference) ->
        (* reference of reference is not allowed in C++ - it's most likely frontend *)
        (* trying to add same reference to same type twice*)
        (* this is hacky and should be fixed (t9838691) *)
        typ
    | true, _ -> Typ.Tptr (typ, Typ.Pk_reference)
    | _ -> typ

  (** Execute translation and then possibly adjust the type of the result of translation:
      In C++, when expression returns reference to type T, it will be lvalue to T, not T&, but
      infer needs it to be T& *)
  let exec_with_glvalue_as_reference f trans_state stmt =
    let expr_info = match Clang_ast_proj.get_expr_tuple stmt with
      | Some (_, _, ei) -> ei
      | None -> assert false in
    let res_trans = f trans_state stmt in
    let (exp, typ) = extract_exp_from_list res_trans.exps
        "[Warning] Need exactly one expression to add reference type\n" in
    { res_trans with exps = [(exp, add_reference_if_glvalue typ expr_info)] }

  (* Execute translation of e forcing to release priority
     (if it's not free) and then setting it back.*)
  (* This is used in conditional operators where we need to force
     the priority to be free for the *)
  (* computation of the expressions*)
  let exec_with_priority_exception trans_state e f =
    if PriorityNode.is_priority_free trans_state then
      f trans_state e
    else f { trans_state with priority = Free } e

  let call_translation context decl =
    let open CContext in
    (* translation will reset Ident counter, save it's state and restore it afterwards *)
    let ident_state = Ident.NameGenerator.get_current () in
    F.translate_one_declaration context.translation_unit_context context.tenv context.cg context.cfg
      `Translation decl;
    Ident.NameGenerator.set_current ident_state

  let mk_temp_sil_var procdesc var_name_suffix =
    let procname = Procdesc.get_proc_name procdesc in
    Pvar.mk_tmp var_name_suffix procname

  let mk_temp_sil_var_for_expr tenv procdesc var_name_prefix expr_info =
    let type_ptr = expr_info.Clang_ast_t.ei_type_ptr in
    let typ = CType_decl.type_ptr_to_sil_type tenv type_ptr in
    (mk_temp_sil_var procdesc var_name_prefix, typ)

  let create_var_exp_tmp_var trans_state expr_info var_name =
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let (pvar, typ) = mk_temp_sil_var_for_expr context.CContext.tenv procdesc
        var_name expr_info in
    Procdesc.append_locals procdesc [(Pvar.get_name pvar, typ)];
    Exp.Lvar pvar, typ

  let create_call_instr trans_state return_type function_sil params_sil sil_loc
      call_flags ~is_objc_method =
    let ret_id = if (Typ.equal return_type Typ.Tvoid) then None
      else Some (Ident.create_fresh Ident.knormal, return_type) in
    let ret_id', params, initd_exps, ret_exps =
      (* Assumption: should_add_return_param will return true only for struct types *)
      if CMethod_trans.should_add_return_param return_type ~is_objc_method then
        let param_type = Typ.Tptr (return_type, Typ.Pk_pointer) in
        let var_exp = match trans_state.var_exp_typ with
          | Some (exp, _) -> exp
          | _ ->
              let procdesc = trans_state.context.CContext.procdesc in
              let pvar = mk_temp_sil_var procdesc "__temp_return_" in
              Procdesc.append_locals procdesc [(Pvar.get_name pvar, return_type)];
              Exp.Lvar pvar in
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
        None, params_sil @ [ret_param], [var_exp], [ret_exp]
      else ret_id, params_sil, [], match ret_id with Some (i,t) -> [(Exp.Var i, t)] | None -> [] in
    let call_instr = Sil.Call (ret_id', function_sil, params, sil_loc, call_flags) in
    { empty_res_trans with
      instrs = [call_instr];
      exps = ret_exps;
      initd_exps = initd_exps;}

  let breakStmt_trans trans_state =
    match trans_state.continuation with
    | Some bn -> { empty_res_trans with root_nodes = bn.break }
    | _ -> assert false

  let continueStmt_trans trans_state =
    match trans_state.continuation with
    | Some bn -> { empty_res_trans with root_nodes = bn.continue }
    | _ -> assert false

  let stringLiteral_trans trans_state expr_info str =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cstr (str)) in
    { empty_res_trans with exps = [(exp, typ)]}

  (* FROM CLANG DOCS: "Implements the GNU __null extension,
     which is a name for a null pointer constant *)
     (* that has integral type (e.g., int or long) and is the same
     size and alignment as a pointer. The __null *)
     (* extension is typically  only used by system headers,
     which define NULL as __null in C++ rather than using 0 *)
     (* (which is an integer that may not match the size of a pointer)".
     So we implement it as the constant zero *)
  let gNUNullExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cint (IntLit.zero)) in
    { empty_res_trans with exps = [(exp, typ)]}

  let nullPtrExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    { empty_res_trans with exps = [(Exp.null, typ)]}

  let objCSelectorExpr_trans trans_state expr_info selector =
    stringLiteral_trans trans_state expr_info selector

  let objCEncodeExpr_trans trans_state expr_info objc_encode_expr_info =
    let type_raw = objc_encode_expr_info.Clang_ast_t.oeei_raw in
    stringLiteral_trans trans_state expr_info type_raw

  let objCProtocolExpr_trans trans_state expr_info decl_ref =
    let name = (match decl_ref.Clang_ast_t.dr_name with
        | Some s -> s.Clang_ast_t.ni_name
        | _ -> "") in
    stringLiteral_trans trans_state expr_info name

  let characterLiteral_trans trans_state expr_info n =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cint (IntLit.of_int n)) in
    { empty_res_trans with exps = [(exp, typ)]}

  let booleanValue_trans trans_state expr_info b =
    characterLiteral_trans trans_state expr_info (if b then 1 else 0)

  let floatingLiteral_trans trans_state expr_info float_string =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp = Exp.Const (Const.Cfloat (float_of_string float_string)) in
    { empty_res_trans with exps = [(exp, typ)]}

  (* Note currently we don't have support for different qual     *)
  (* type like long, unsigned long, etc                                *)
  and integerLiteral_trans trans_state expr_info integer_literal_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exp =
      try
        let i = Int64.of_string integer_literal_info.Clang_ast_t.ili_value in
        let exp = Exp.int (IntLit.of_int64 i) in
        exp
      with
      | Failure _ ->
          (* Parse error: return a nondeterministic value *)
          let id = Ident.create_fresh Ident.knormal in
          Exp.Var id in
    { empty_res_trans with
      exps = [(exp, typ)];
    }

  let cxxScalarValueInitExpr_trans trans_state expr_info =
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    (* constant will be different depending on type *)
    let zero_opt = match typ with
      | Typ.Tfloat _  | Typ.Tptr _ | Typ.Tint _ -> Some (Sil.zero_value_of_numerical_type typ)
      | Typ.Tvoid -> None
      | _ -> Some (Exp.Const (Const.Cint IntLit.zero)) in
    match zero_opt with
    | Some zero -> { empty_res_trans with exps = [(zero, typ)] }
    | _ -> empty_res_trans

  let implicitValueInitExpr_trans trans_state expr_info =
    let (var_exp, _) = extract_var_exp_or_fail trans_state in
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info trans_state.context.CContext.tenv in
    let exps = var_or_zero_in_init_list tenv var_exp typ ~return_zero:true in
    { empty_res_trans with exps = exps }

  let nullStmt_trans succ_nodes =
    { empty_res_trans with root_nodes = succ_nodes }

  (* The stmt seems to be always empty *)
  let unaryExprOrTypeTraitExpr_trans trans_state expr_info unary_expr_or_type_trait_expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.type_ptr_to_sil_type tenv expr_info.Clang_ast_t.ei_type_ptr in
    match unary_expr_or_type_trait_expr_info.Clang_ast_t.uttei_kind with
    | `SizeOf ->
        let tp =
          CAst_utils.type_from_unary_expr_or_type_trait_expr_info
            unary_expr_or_type_trait_expr_info in
        let sizeof_typ =
          match tp with
          | Some tp -> CType_decl.type_ptr_to_sil_type tenv tp
          | None -> typ (* Some default type since the type is missing *) in
        { empty_res_trans with
          exps = [(Exp.Sizeof (sizeof_typ, None, Subtype.exact), sizeof_typ)] }
    | k -> Logging.out
             "\nWARNING: Missing translation of Uniry_Expression_Or_Trait of kind: \
              %s . Expression ignored, returned -1... \n"
             (Clang_ast_j.string_of_unary_expr_or_type_trait_kind k);
        { empty_res_trans with exps =[(Exp.minus_one, typ)]}

  (* search the label into the hashtbl - create a fake node eventually *)
  (* connect that node with this stmt *)
  let gotoStmt_trans trans_state stmt_info label_name =
    let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    { empty_res_trans with root_nodes = [root_node']; leaf_nodes = trans_state.succ_nodes }

  let get_builtin_pname_opt trans_unit_ctx name decl_opt type_ptr =
    let get_deprecated_attr_arg decl =
      let open Clang_ast_t in
      let decl_info = Clang_ast_proj.get_decl_tuple decl in
      let get_attr_opt = function DeprecatedAttr a -> Some a | _ -> None in
      match List.find_map ~f:get_attr_opt decl_info.di_attributes with
      | Some attribute_info ->
          (match attribute_info.ai_parameters with
           | [_; arg; _; _; _; _] -> Some arg
           | _ ->
               (* it's not supposed to happen due to hardcoded exporting logic
                  coming from ASTExporter.h in facebook-clang-plugins *)
               assert false)
      | None -> None in
    let function_attr_opt = Option.bind decl_opt get_deprecated_attr_arg in
    match function_attr_opt with
    | Some attr when CTrans_models.is_modeled_attribute attr ->
        Some (Typ.Procname.from_string_c_fun attr)
    | _ when CTrans_models.is_modeled_builtin name ->
        Some (Typ.Procname.from_string_c_fun (CFrontend_config.infer ^ name))
    | _ when CTrans_models.is_release_builtin name type_ptr ->
        Some BuiltinDecl.__objc_release_cf
    | _ when CTrans_models.is_retain_builtin name type_ptr ->
        Some BuiltinDecl.__objc_retain_cf
    | _ when String.equal name CFrontend_config.malloc &&
             CGeneral_utils.is_objc_extension trans_unit_ctx ->
        Some BuiltinDecl.malloc_no_fail
    | _ -> None


  let function_deref_trans trans_state decl_ref =
    let open CContext in
    let context = trans_state.context in
    let name_info, decl_ptr, type_ptr = CAst_utils.get_info_from_decl_ref decl_ref in
    let decl_opt = CAst_utils.get_function_decl_with_body decl_ptr in
    Option.iter ~f:(call_translation context) decl_opt;
    let name = CAst_utils.get_qualified_name name_info in
    let typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
    let pname =
      match get_builtin_pname_opt context.translation_unit_context name decl_opt type_ptr with
      | Some builtin_pname -> builtin_pname
      | None -> CMethod_trans.create_procdesc_with_pointer context decl_ptr None name in
    { empty_res_trans with exps = [(Exp.Const (Const.Cfun pname), typ)] }

  let field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init =
    let open CContext in
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let name_info, _, type_ptr = CAst_utils.get_info_from_decl_ref decl_ref in
    Logging.out_debug "!!!!! Dealing with field '%s' @." name_info.Clang_ast_t.ni_name;
    let field_typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
    let (obj_sil, class_typ) = extract_exp_from_list pre_trans_result.exps
        "WARNING: in Field dereference we expect to know the object\n" in
    let is_pointer_typ = match class_typ with
      | Typ.Tptr _ -> true
      | _ -> false in
    let class_typ =
      match class_typ with
      | Typ.Tptr (t, _) -> t
      | t -> t in
    Logging.out_debug "Type is  '%s' @." (Typ.to_string class_typ);
    let field_name = CGeneral_utils.mk_class_field_name name_info in
    let field_exp = Exp.Lfield (obj_sil, field_name, class_typ) in
    (* In certain cases, there is be no LValueToRValue cast, but backend needs dereference*)
    (* there either way:*)
    (* 1. Class is not a pointer type - it means that it's rvalue struct most likely coming from*)
    (*    create_call_instr - more info there*)
    (* 2. Field has reference type - we need to add extra dereference in same fashion*)
    (*    it's done in var_deref_trans. The only exception is during field initialization in*)
    (*    constructor's initializer list (when reference itself is initialized) *)
    let should_add_deref = (not is_pointer_typ) ||
                           (not is_constructor_init && CType.is_reference_type type_ptr) in
    let exp, deref_instrs = if should_add_deref then
        let id = Ident.create_fresh Ident.knormal in
        let deref_instr = Sil.Load (id, field_exp, field_typ, sil_loc) in
        Exp.Var id, [deref_instr]
      else
        field_exp, [] in
    let instrs = pre_trans_result.instrs @ deref_instrs in
    { pre_trans_result with instrs; exps = [(exp, field_typ)] }

  let method_deref_trans trans_state pre_trans_result decl_ref stmt_info decl_kind =
    let open CContext in
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let name_info, decl_ptr, type_ptr = CAst_utils.get_info_from_decl_ref decl_ref in
    let decl_opt = CAst_utils.get_function_decl_with_body decl_ptr in
    Option.iter ~f:(call_translation context) decl_opt;
    let method_name = CAst_utils.get_unqualified_name name_info in
    Logging.out_debug "!!!!! Dealing with method '%s' @." method_name;
    let method_typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
    let ms_opt = CMethod_trans.method_signature_of_pointer
        context.translation_unit_context context.tenv decl_ptr in
    let is_instance_method = match ms_opt with
      | Some ms -> CMethod_signature.ms_is_instance ms
      | _ -> true (* might happen for methods that are not exported yet (some templates). *) in
    let is_cpp_virtual = match ms_opt with
      | Some ms -> CMethod_signature.ms_is_cpp_virtual ms
      | _ -> false in
    let extra_exps, extra_instrs = if is_instance_method then (
        (* pre_trans_result.exps may contain expr for 'this' parameter:*)
        (* if it comes from CXXMemberCallExpr it will be there *)
        (* if it comes from CXXOperatorCallExpr it won't be there and will be added later *)
        (* In case of CXXMemberCallExpr it's possible that type of 'this' parameter *)
        (* won't have a pointer - if that happens add a pointer to type of the object *)
        match pre_trans_result.exps with
        | [] -> [], []
        (* We need to add a dereference before a method call to find null dereferences when *)
        (* calling a method with null *)
        | [(exp, Typ.Tptr (typ, _) )] when decl_kind <> `CXXConstructor ->
            let no_id = Ident.create_none () in
            let extra_instrs = [Sil.Load (no_id, exp, typ, sil_loc)] in
            pre_trans_result.exps, extra_instrs
        | [(_, Typ.Tptr _ )] -> pre_trans_result.exps, []
        | [(sil, typ)] -> [(sil, Typ.Tptr (typ, Typ.Pk_reference))], []
        | _ -> assert false
      )
      else
        (* don't add 'this' expression for static methods *)
        [], [] in
    (* unlike field access, for method calls there is no need to expand class type *)

    (* use qualified method name for builtin matching, but use unqualified name elsewhere *)
    let qual_method_name = CAst_utils.get_qualified_name name_info in
    let pname =
      match get_builtin_pname_opt context.translation_unit_context qual_method_name decl_opt
              type_ptr with
      | Some builtin_pname -> builtin_pname
      | None ->
          let class_typename = Typ.Name.Cpp.from_string
              (CAst_utils.get_class_name_from_member name_info) in
          CMethod_trans.create_procdesc_with_pointer context decl_ptr (Some class_typename)
            method_name in
    let method_exp = (Exp.Const (Const.Cfun pname), method_typ) in
    { pre_trans_result with
      is_cpp_call_virtual = is_cpp_virtual;
      exps = [method_exp] @ extra_exps;
      instrs = pre_trans_result.instrs @ extra_instrs;
    }

  let destructor_deref_trans trans_state pvar_trans_result class_type_ptr si =
    let open Clang_ast_t in
    let destruct_decl_ref_opt = match CAst_utils.get_decl_from_typ_ptr class_type_ptr with
      | Some CXXRecordDecl (_, _, _ , _, _, _, _, cxx_record_info)
      | Some ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_record_info, _) ->
          cxx_record_info.xrdi_destructor
      | _ -> None in
    match destruct_decl_ref_opt with
    | Some decl_ref ->
        method_deref_trans trans_state pvar_trans_result decl_ref si `CXXDestructor
    | None -> empty_res_trans

  let this_expr_trans trans_state sil_loc class_type_ptr =
    let context = trans_state.context in
    let procname = Procdesc.get_proc_name context.CContext.procdesc in
    let name = CFrontend_config.this in
    let pvar = Pvar.mk (Mangled.from_string name) procname in
    let exp = Exp.Lvar pvar in
    let typ = CType_decl.type_ptr_to_sil_type context.CContext.tenv class_type_ptr in
    let exps =  [(exp, typ)] in
    (* there is no cast operation in AST, but backend needs it *)
    dereference_value_from_result sil_loc { empty_res_trans with exps = exps } ~strip_pointer:false

  let cxxThisExpr_trans trans_state stmt_info expr_info =
    let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
    this_expr_trans trans_state sil_loc expr_info.Clang_ast_t.ei_type_ptr

  let rec labelStmt_trans trans_state stmt_info stmt_list label_name =
    let context = trans_state.context in
    (* go ahead with the translation *)
    let res_trans = match stmt_list with
      | [stmt] ->
          instruction trans_state stmt
      | _ -> assert false (* expected a stmt or at most a compoundstmt *) in
    (* create the label root node into the hashtbl *)
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let root_node' = GotoLabel.find_goto_label trans_state.context label_name sil_loc in
    Procdesc.node_set_succs_exn context.procdesc root_node' res_trans.root_nodes [];
    { empty_res_trans with root_nodes = [root_node']; leaf_nodes = trans_state.succ_nodes }

  and var_deref_trans trans_state stmt_info (decl_ref : Clang_ast_t.decl_ref) =
    let context = trans_state.context in
    let _, _, type_ptr = CAst_utils.get_info_from_decl_ref decl_ref in
    let ast_typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
    let typ =
      match ast_typ with
      | Tstruct _ when decl_ref.dr_kind = `ParmVar ->
          if CGeneral_utils.is_cpp_translation context.translation_unit_context then
            Typ.Tptr (ast_typ, Pk_reference)
          else ast_typ
      | _ -> ast_typ in
    let procname = Procdesc.get_proc_name context.procdesc in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let pvar = CVar_decl.sil_var_of_decl_ref context decl_ref procname in
    CContext.add_block_static_var context procname (pvar, typ);
    let var_exp = Exp.Lvar pvar in
    (* handle references to global const *)
    (* if there's a reference to a global const, add a fake instruction that *)
    (* assigns the global again to its initialization value right before the *)
    (* place where it is used *)
    let trans_result' =
      let is_global_const, init_expr =
        match CAst_utils.get_decl decl_ref.dr_decl_pointer with
        | Some VarDecl (_, _, qual_type, vdi) -> (
            match ast_typ with
            | Tstruct _
              when not (CGeneral_utils.is_cpp_translation context.translation_unit_context) ->
                (* Do not convert a global struct to a local because SIL
                   values do not include structs, they must all be heap-allocated  *)
                (false, None)
            | _ ->
                (vdi.vdi_is_global && (vdi.vdi_is_const_expr || qual_type.qt_is_const),
                 vdi.vdi_init_expr)
          )
        | _ -> false, None in
      if is_global_const then
        init_expr_trans trans_state (var_exp, typ) stmt_info init_expr
      else empty_res_trans in
    let exps = if Self.is_var_self pvar (CContext.is_objc_method context) then
        let class_typename = CContext.get_curr_class_typename context in
        if (CType.is_class typ) then
          raise (Self.SelfClassException class_typename)
        else
          let typ = CType.add_pointer_to_typ (Typ.Tstruct class_typename) in
          [(var_exp, typ)]
      else [(var_exp, typ)] in
    Logging.out_debug "\n\n PVAR ='%s'\n\n" (Pvar.to_string pvar);
    let res_trans = { trans_result' with exps } in
    match typ with
    | Tptr (_, Pk_reference) ->
        (* dereference pvar due to the behavior of reference types in clang's AST *)
        dereference_value_from_result sil_loc res_trans ~strip_pointer:false
    | _ -> res_trans

  and decl_ref_trans trans_state pre_trans_result stmt_info decl_ref ~is_constructor_init =
    Logging.out_debug "  priority node free = '%s'@\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let decl_kind = decl_ref.Clang_ast_t.dr_kind in
    match decl_kind with
    | `EnumConstant -> enum_constant_trans trans_state decl_ref
    | `Function -> function_deref_trans trans_state decl_ref
    | `Var | `ImplicitParam | `ParmVar ->
        var_deref_trans trans_state stmt_info decl_ref
    | `Field | `ObjCIvar ->
        field_deref_trans trans_state stmt_info pre_trans_result decl_ref ~is_constructor_init
    | `CXXMethod | `CXXConversion | `CXXConstructor | `CXXDestructor  ->
        method_deref_trans trans_state pre_trans_result decl_ref stmt_info decl_kind
    | _ ->
        let print_error decl_kind =
          Logging.out
            "Warning: Decl ref expression %s with pointer %d still needs to be translated "
            (Clang_ast_j.string_of_decl_kind decl_kind)
            decl_ref.Clang_ast_t.dr_decl_pointer in
        print_error decl_kind; assert false

  and declRefExpr_trans trans_state stmt_info decl_ref_expr_info _ =
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let decl_ref = match decl_ref_expr_info.Clang_ast_t.drti_decl_ref with
      | Some dr -> dr
      | None -> assert false in
    decl_ref_trans trans_state empty_res_trans stmt_info decl_ref ~is_constructor_init:false

  (* evaluates an enum constant *)
  and enum_const_eval context enum_constant_pointer prev_enum_constant_opt zero =
    match CAst_utils.get_decl enum_constant_pointer with
    | Some Clang_ast_t.EnumConstantDecl (_, _, _, enum_constant_decl_info) ->
        (match enum_constant_decl_info.Clang_ast_t.ecdi_init_expr with
         | Some stmt ->
             expression_trans context stmt
               "WARNING: Expression in Enumeration constant not found\n"
         | None ->
             match prev_enum_constant_opt with
             | Some prev_constant_pointer ->
                 let previous_exp = get_enum_constant_expr context prev_constant_pointer in
                 CArithmetic_trans.sil_const_plus_one previous_exp
             | None -> zero)
    | _ -> zero

  (* get the sil value of the enum constant from the map or by evaluating it *)
  and get_enum_constant_expr context enum_constant_pointer =
    let zero = Exp.Const (Const.Cint IntLit.zero) in
    try
      let (prev_enum_constant_opt, sil_exp_opt) =
        CAst_utils.get_enum_constant_exp enum_constant_pointer in
      match sil_exp_opt with
      | Some exp -> exp
      | None ->
          let exp = enum_const_eval context enum_constant_pointer prev_enum_constant_opt zero in
          CAst_utils.update_enum_map enum_constant_pointer exp;
          exp
    with Not_found -> zero

  and enum_constant_trans trans_state decl_ref =
    let context = trans_state.context in
    let _, _, type_ptr = CAst_utils.get_info_from_decl_ref decl_ref in
    let typ = CType_decl.type_ptr_to_sil_type context.CContext.tenv type_ptr in
    let const_exp = get_enum_constant_expr context decl_ref.Clang_ast_t.dr_decl_pointer in
    { empty_res_trans with exps = [(const_exp, typ)] }

  and arraySubscriptExpr_trans trans_state expr_info stmt_list =
    let context = trans_state.context in
    let typ = CType_decl.get_type_from_expr_info expr_info context.tenv in
    let array_stmt, idx_stmt = (match stmt_list with
        | [a; i] -> a, i  (* Assumption: the statement list contains 2 elements,
                             the first is the array expr and the second the index *)
        | _ -> assert false (* Let's get notified if the assumption is wrong...*) ) in
    let res_trans_a = instruction trans_state array_stmt in
    let res_trans_idx = instruction trans_state idx_stmt in
    let (a_exp, _) = extract_exp_from_list res_trans_a.exps
        "WARNING: In ArraySubscriptExpr there was a problem in translating array exp.\n" in
    let (i_exp, _) = extract_exp_from_list res_trans_idx.exps
        "WARNING: In ArraySubscriptExpr there was a problem in translating index exp.\n" in
    let array_exp = Exp.Lindex (a_exp, i_exp) in

    let root_nodes =
      if res_trans_a.root_nodes <> []
      then res_trans_a.root_nodes
      else res_trans_idx.root_nodes in
    let leaf_nodes =
      if res_trans_idx.leaf_nodes <> []
      then res_trans_idx.leaf_nodes
      else res_trans_a.leaf_nodes in

    if res_trans_idx.root_nodes <> []
    then
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n res_trans_idx.root_nodes [])
        res_trans_a.leaf_nodes;

    (* Note the order of res_trans_idx.ids @ res_trans_a.ids is important. *)
    (* We expect to use only res_trans_idx.ids in construction of other operation. *)
    (* res_trans_a.ids is passed to be Removed.*)
    { empty_res_trans with
      root_nodes;
      leaf_nodes;
      instrs = res_trans_a.instrs @ res_trans_idx.instrs;
      exps = [(array_exp, typ)];
      initd_exps = res_trans_idx.initd_exps @ res_trans_a.initd_exps; }

  and binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list =
    let bok =
      Clang_ast_j.string_of_binary_operator_kind binary_operator_info.Clang_ast_t.boi_kind in
    Logging.out_debug "  BinaryOperator '%s' " bok;
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let nname = "BinaryOperatorStmt: "^ (CArithmetic_trans.bin_op_to_string binary_operator_info) in
    let trans_state' = { trans_state_pri with succ_nodes = [] } in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let typ =
      CType_decl.type_ptr_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_type_ptr in
    match stmt_list with
    | [s1; s2] -> (* Assumption: We expect precisely 2 stmt corresponding to the 2 operands*)
        let rhs_owning_method = CTrans_utils.is_owning_method s2 in
        (* NOTE: we create a node only if required. In that case this node *)
        (* becomes the successor of the nodes that may be created when     *)
        (* translating the operands.                                       *)
        let res_trans_e1 = exec_with_self_exception instruction trans_state' s1 in
        let (var_exp, var_exp_typ)  = extract_exp_from_list res_trans_e1.exps
            "\nWARNING: Missing LHS operand in BinOp. Returning -1. Fix needed...\n" in
        let trans_state'' = { trans_state' with var_exp_typ = Some (var_exp, var_exp_typ)  } in
        let res_trans_e2 =
          (* translation of s2 is done taking care of block special case *)
          exec_with_block_priority_exception (exec_with_self_exception instruction) trans_state''
            s2 stmt_info in
        let (sil_e2, _) = extract_exp_from_list res_trans_e2.exps
            "\nWARNING: Missing RHS operand in BinOp. Returning -1. Fix needed...\n" in
        let binop_res_trans, exp_to_parent =
          if List.exists ~f:(Exp.equal var_exp) res_trans_e2.initd_exps then [], []
          else
            let exp_op, instr_bin =
              CArithmetic_trans.binary_operation_instruction
                binary_operator_info var_exp typ sil_e2 sil_loc rhs_owning_method in

            (* Create a node if the priority if free and there are instructions *)
            let creating_node =
              (PriorityNode.own_priority_node trans_state_pri.priority stmt_info) &&
              (List.length instr_bin >0) in
            let extra_instrs, exp_to_parent =
              if (is_binary_assign_op binary_operator_info)
              (* assignment operator result is lvalue in CPP, rvalue in C, *)
              (* hence the difference *)
              && (not (CGeneral_utils.is_cpp_translation context.translation_unit_context))
              && ((not creating_node) || (is_return_temp trans_state.continuation)) then (
                (* We are in this case when an assignment is inside        *)
                (* another operator that creates a node. Eg. another       *)
                (* assignment.  *)
                (* As no node is created here ids are passed to the parent *)
                let id = Ident.create_fresh Ident.knormal in
                let res_instr = Sil.Load (id, var_exp, var_exp_typ, sil_loc) in
                [res_instr], Exp.Var id
              ) else (
                [], exp_op) in
            let binop_res_trans = { empty_res_trans with
                                    instrs = instr_bin @ extra_instrs
                                  } in
            [binop_res_trans], [(exp_to_parent, var_exp_typ)] in
        let all_res_trans = [res_trans_e1; res_trans_e2] @ binop_res_trans in
        let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
            nname stmt_info all_res_trans in
        { res_trans_to_parent with exps = exp_to_parent }
    | _ -> assert false (* Binary operator should have two operands *)

  and callExpr_trans trans_state si stmt_list expr_info =
    let context = trans_state.context in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    let procname = Procdesc.get_proc_name context.CContext.procdesc in
    let sil_loc = CLocation.get_sil_location si context in
    (* First stmt is the function expr and the rest are params *)
    let fun_exp_stmt, params_stmt = (match stmt_list with
        | fe :: params -> fe, params
        | _ -> assert false) in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    (* claim priority if no ancestors has claimed priority before *)
    let trans_state_callee = { trans_state_pri with succ_nodes = [] } in
    let res_trans_callee = instruction trans_state_callee fun_exp_stmt in
    let (sil_fe, _) = extract_exp_from_list res_trans_callee.exps
        "WARNING: The translation of fun_exp did not return an expression.\
         Returning -1. NEED TO BE FIXED" in
    let callee_pname_opt =
      match sil_fe with
      | Exp.Const (Const.Cfun pn) ->
          Some pn
      | _ -> None (* function pointer *) in
    (* we cannot translate the arguments of __builtin_object_size because preprocessing copies
       them verbatim from a call to a different function, and they might be side-effecting *)
    let should_translate_args =
      not (Option.value_map ~f:CTrans_models.is_builtin_object_size ~default:false callee_pname_opt) in
    let params_stmt = if should_translate_args then params_stmt
      else [] in
    (* As we may have nodes coming from different parameters we need to  *)
    (* call instruction for each parameter and collect the results       *)
    (* afterwards. The 'instructions' function does not do that          *)
    let trans_state_param =
      { trans_state_pri with succ_nodes = []; var_exp_typ = None  } in
    let result_trans_subexprs =
      let instruction' = exec_with_self_exception (exec_with_glvalue_as_reference instruction) in
      let res_trans_p = List.map ~f:(instruction' trans_state_param) params_stmt in
      res_trans_callee :: res_trans_p in
    match Option.bind callee_pname_opt
            (CTrans_utils.builtin_trans
               trans_state_pri sil_loc si function_type result_trans_subexprs) with
    | Some builtin -> builtin
    | None ->
        let is_cf_retain_release =
          Option.value_map
            ~f:CTrans_models.is_cf_retain_release ~default:false callee_pname_opt in
        let act_params =
          let params = List.tl_exn (collect_exprs result_trans_subexprs) in
          if Int.equal (List.length params) (List.length params_stmt) then
            params
          else
            (Logging.out_debug "ERROR: stmt_list and res_trans_par.exps must have same size\n";
             assert false) in
        let act_params = if is_cf_retain_release then
            (Exp.Const (Const.Cint IntLit.one), Typ.Tint Typ.IBool) :: act_params
          else act_params in
        let res_trans_call =
          let cast_trans_fun = cast_trans act_params sil_loc function_type in
          match Option.bind callee_pname_opt cast_trans_fun with
          | Some (instr, cast_exp) ->
              { empty_res_trans with
                instrs = [instr];
                exps = [(cast_exp, function_type)]; }
          | _ ->
              let is_call_to_block = objc_exp_of_type_block fun_exp_stmt in
              let call_flags =
                { CallFlags.default with CallFlags.cf_is_objc_block = is_call_to_block; } in
              create_call_instr trans_state function_type sil_fe act_params sil_loc
                call_flags ~is_objc_method:false in
        let nname = "Call "^(Exp.to_string sil_fe) in
        let all_res_trans = result_trans_subexprs @ [res_trans_call] in
        let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri
            sil_loc nname si all_res_trans in
        let add_cg_edge callee_pname =
          Cg.add_edge context.CContext.cg procname callee_pname
        in
        Option.iter ~f:add_cg_edge callee_pname_opt;
        { res_trans_to_parent with exps = res_trans_call.exps }

  and cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt
      si function_type is_cpp_call_virtual extra_res_trans =
    let open CContext in
    let context = trans_state_pri.context in
    let procname = Procdesc.get_proc_name context.procdesc in
    let sil_loc = CLocation.get_sil_location si context in
    (* first for method address, second for 'this' expression *)
    assert (Int.equal (List.length result_trans_callee.exps) 2);
    let (sil_method, _) = List.hd_exn result_trans_callee.exps in
    let callee_pname =
      match sil_method with
      | Exp.Const (Const.Cfun pn) -> pn
      | _ -> assert false (* method pointer not implemented, this shouldn't happen *) in
    (* As we may have nodes coming from different parameters we need to  *)
    (* call instruction for each parameter and collect the results       *)
    (* afterwards. The 'instructions' function does not do that          *)
    let result_trans_subexprs =
      let trans_state_param =
        { trans_state_pri with succ_nodes = []; var_exp_typ = None } in
      let instruction' = exec_with_self_exception (exec_with_glvalue_as_reference instruction) in
      let res_trans_p = List.map ~f:(instruction' trans_state_param) params_stmt in
      result_trans_callee :: res_trans_p in
    (* first expr is method address, rest are params including 'this' parameter *)
    let actual_params = List.tl_exn (collect_exprs result_trans_subexprs) in
    match cxx_method_builtin_trans trans_state_pri sil_loc result_trans_subexprs callee_pname with
    | Some builtin -> builtin
    | _ ->
        let call_flags = {
          CallFlags.default with
          CallFlags.cf_virtual = is_cpp_call_virtual;
        } in
        let res_trans_call = create_call_instr trans_state_pri function_type sil_method
            actual_params sil_loc call_flags ~is_objc_method:false in
        let nname = "Call " ^ (Exp.to_string sil_method) in
        let all_res_trans = result_trans_subexprs @ [res_trans_call; extra_res_trans] in
        let result_trans_to_parent =
          PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname si all_res_trans in
        Cg.add_edge context.CContext.cg procname callee_pname;
        { result_trans_to_parent with exps = res_trans_call.exps }

  and cxxMemberCallExpr_trans trans_state si stmt_list expr_info =
    let context = trans_state.context in
    (* Structure is the following: *)
    (* CXXMemberCallExpr: first stmt is method+this expr and the rest are normal params *)
    (* CXXOperatorCallExpr: First stmt is method/function deref without this expr and the *)
    (*                      rest are params, possibly including 'this' *)
    let fun_exp_stmt, params_stmt = (match stmt_list with
        | fe :: params -> fe, params
        | _ -> assert false) in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    (* claim priority if no ancestors has claimed priority before *)
    let trans_state_callee = { trans_state_pri with succ_nodes = [] } in
    let result_trans_callee = instruction trans_state_callee fun_exp_stmt in
    let is_cpp_call_virtual = result_trans_callee.is_cpp_call_virtual in
    let fn_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let function_type = add_reference_if_glvalue fn_type_no_ref expr_info in
    cxx_method_construct_call_trans trans_state_pri result_trans_callee params_stmt
      si function_type is_cpp_call_virtual empty_res_trans

  and cxxConstructExpr_trans trans_state si params_stmt ei cxx_constr_info =
    let context = trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let sil_loc = CLocation.get_sil_location si context in
    let decl_ref = cxx_constr_info.Clang_ast_t.xcei_decl_ref in
    let var_exp, class_type = match trans_state.var_exp_typ with
      | Some exp_typ -> exp_typ
      | None ->
          let procdesc = trans_state.context.CContext.procdesc in
          let pvar = Pvar.mk_tmp "__temp_construct_" (Procdesc.get_proc_name procdesc) in
          let class_type = CType_decl.get_type_from_expr_info ei context.CContext.tenv in
          Procdesc.append_locals procdesc [(Pvar.get_name pvar, class_type)];
          Exp.Lvar pvar, class_type in
    let this_type = Typ.Tptr (class_type, Typ.Pk_pointer) in
    let this_res_trans = { empty_res_trans with
                           exps = [(var_exp, this_type)];
                           initd_exps = [var_exp];
                         } in
    let tmp_res_trans = { empty_res_trans with exps = [(var_exp, class_type)] } in
    (* When class type is translated as pointer (std::shared_ptr for example), there needs
       to be extra Load instruction before returning the trans_result of constructorExpr.
       There is no LValueToRvalue cast in the AST afterwards since clang doesn't know
       that class type is translated as pointer type. It gets added here instead. *)
    let extra_res_trans = match class_type with
      | Typ.Tptr _ -> dereference_value_from_result sil_loc tmp_res_trans ~strip_pointer:false
      | _ -> tmp_res_trans in
    let res_trans_callee = decl_ref_trans trans_state this_res_trans si decl_ref
        ~is_constructor_init:false in
    let res_trans = cxx_method_construct_call_trans trans_state_pri res_trans_callee
        params_stmt si Typ.Tvoid false extra_res_trans in
    { res_trans with exps=extra_res_trans.exps }

  and cxx_destructor_call_trans trans_state si this_res_trans class_type_ptr =
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let res_trans_callee = destructor_deref_trans trans_state this_res_trans class_type_ptr si in
    let is_cpp_call_virtual = res_trans_callee.is_cpp_call_virtual in
    if res_trans_callee.exps <> [] then
      cxx_method_construct_call_trans trans_state_pri res_trans_callee [] si Typ.Tvoid
        is_cpp_call_virtual empty_res_trans
    else empty_res_trans

  and objCMessageExpr_trans_special_cases trans_state si obj_c_message_expr_info
      method_type trans_state_pri sil_loc act_params =
    let context = trans_state.context in
    let receiver_kind = obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind in
    let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
    (* class method *)
    if String.equal selector CFrontend_config.class_method && CType.is_class method_type then
      let class_name = CMethod_trans.get_class_name_method_call_from_receiver_kind context
          obj_c_message_expr_info act_params in
      raise (Self.SelfClassException class_name)
      (* alloc or new *)
    else if String.equal selector CFrontend_config.alloc ||
            String.equal selector CFrontend_config.new_str then
      match receiver_kind with
      | `Class type_ptr ->
          let class_opt =
            CMethod_trans.get_class_name_method_call_from_clang
              context.translation_unit_context context.CContext.tenv obj_c_message_expr_info in
          Some (new_or_alloc_trans trans_state_pri sil_loc si type_ptr class_opt selector)
      | _ -> None
      (* assertions *)
    else if CTrans_models.is_handleFailureInMethod selector then
      Some (CTrans_utils.trans_assertion trans_state sil_loc)
    else None


  (* If the first argument of the call is self in a static context, remove it as an argument *)
  (* and change the call from instance to static *)
  and objCMessageExpr_deal_with_static_self trans_state_param stmt_list obj_c_message_expr_info =
    match stmt_list with
    | stmt :: rest ->
        let obj_c_message_expr_info, fst_res_trans =
          try
            let fst_res_trans = instruction trans_state_param stmt in
            obj_c_message_expr_info, fst_res_trans
          with Self.SelfClassException class_typename ->
            let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
            let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
            let obj_c_message_expr_info =
              Ast_expressions.make_obj_c_message_expr_info_class selector class_typename pointer in
            obj_c_message_expr_info, empty_res_trans in
        let instruction' =
          exec_with_self_exception (exec_with_glvalue_as_reference instruction) in
        let l = List.map ~f:(instruction' trans_state_param) rest in
        obj_c_message_expr_info, fst_res_trans :: l
    | [] -> obj_c_message_expr_info, [empty_res_trans]

  and objCMessageExpr_trans trans_state si obj_c_message_expr_info stmt_list expr_info =
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location si context in
    let method_type_no_ref = CType_decl.get_type_from_expr_info expr_info context.CContext.tenv in
    let method_type = add_reference_if_glvalue method_type_no_ref expr_info in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state si in
    let trans_state_param = { trans_state_pri with succ_nodes = []; var_exp_typ = None } in
    let obj_c_message_expr_info, res_trans_subexpr_list =
      objCMessageExpr_deal_with_static_self trans_state_param stmt_list obj_c_message_expr_info in
    let subexpr_exprs = collect_exprs res_trans_subexpr_list in
    match objCMessageExpr_trans_special_cases trans_state si obj_c_message_expr_info
            method_type trans_state_pri sil_loc subexpr_exprs with
    | Some res -> res
    | None ->
        let procname = Procdesc.get_proc_name context.CContext.procdesc in
        let callee_name, method_call_type = get_callee_objc_method context obj_c_message_expr_info
            subexpr_exprs in
        let res_trans_add_self = Self.add_self_parameter_for_super_instance context procname sil_loc
            obj_c_message_expr_info in
        let res_trans_subexpr_list = res_trans_add_self :: res_trans_subexpr_list in
        let subexpr_exprs = collect_exprs res_trans_subexpr_list in
        let is_virtual =
          CMethod_trans.equal_method_call_type method_call_type CMethod_trans.MCVirtual in
        Cg.add_edge context.CContext.cg procname callee_name;

        let param_exps, instr_block_param =
          extract_block_from_tuple procname subexpr_exprs sil_loc in
        let res_trans_block = { empty_res_trans with
                                instrs = instr_block_param;
                              } in
        let call_flags = { CallFlags.default with CallFlags.cf_virtual = is_virtual; } in
        let method_sil = Exp.Const (Const.Cfun callee_name) in
        let res_trans_call = create_call_instr trans_state method_type method_sil param_exps
            sil_loc call_flags ~is_objc_method:true in
        let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
        let nname = "Message Call: "^selector in
        let all_res_trans = res_trans_subexpr_list @ [res_trans_block; res_trans_call] in
        let res_trans_to_parent =
          PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname si all_res_trans in
        { res_trans_to_parent with exps = res_trans_call.exps }


  and dispatch_function_trans trans_state stmt_info stmt_list n =
    Logging.out_debug "\n Call to a dispatch function treated as special case...\n";
    let transformed_stmt = Ast_expressions.translate_dispatch_function stmt_info stmt_list n in
    instruction trans_state transformed_stmt

  and block_enumeration_trans trans_state stmt_info stmt_list ei =
    Logging.out_debug "\n Call to a block enumeration function treated as special case...\n@.";
    let procname = Procdesc.get_proc_name trans_state.context.CContext.procdesc in
    let pvar = CProcname.get_next_block_pvar procname in
    let transformed_stmt, _ =
      Ast_expressions.translate_block_enumerate (Pvar.to_string pvar) stmt_info stmt_list ei in
    instruction trans_state transformed_stmt

  and compoundStmt_trans trans_state stmt_list =
    instructions trans_state stmt_list

  and conditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let procdesc = context.CContext.procdesc in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let do_branch branch stmt var_typ prune_nodes join_node pvar =
      let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
      let trans_state' = { trans_state_pri with succ_nodes = [] } in
      let res_trans_b = instruction trans_state' stmt in
      let (e', _) = extract_exp_from_list res_trans_b.exps
          "\nWARNING: Missing branch expression for Conditional operator. Need to be fixed\n" in
      let set_temp_var = [
        Sil.Store (Exp.Lvar pvar, var_typ, e', sil_loc)
      ] in
      let tmp_var_res_trans = { empty_res_trans with instrs = set_temp_var } in
      let trans_state'' = { trans_state' with succ_nodes = [join_node] } in
      let all_res_trans = [res_trans_b; tmp_var_res_trans] in
      let res_trans = PriorityNode.compute_results_to_parent trans_state'' sil_loc
          "ConditinalStmt Branch" stmt_info all_res_trans in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n res_trans.root_nodes [])
        prune_nodes' in
    (match stmt_list with
     | [cond; exp1; exp2] ->
         let typ =
           CType_decl.type_ptr_to_sil_type
             context.CContext.tenv expr_info.Clang_ast_t.ei_type_ptr in
         let var_typ = add_reference_if_glvalue typ expr_info in
         let join_node = create_node (Procdesc.Node.Join_node) [] sil_loc context in
         Procdesc.node_set_succs_exn context.procdesc join_node succ_nodes [];
         let pvar = mk_temp_sil_var procdesc "SIL_temp_conditional___" in
         Procdesc.append_locals procdesc [(Pvar.get_name pvar, var_typ)];
         let continuation' = mk_cond_continuation trans_state.continuation in
         let trans_state' = { trans_state with continuation = continuation'; succ_nodes = [] } in
         let res_trans_cond = exec_with_priority_exception trans_state' cond cond_trans in
         (* Note: by contruction prune nodes are leafs_nodes_cond *)
         do_branch true exp1 var_typ res_trans_cond.leaf_nodes join_node pvar;
         do_branch false exp2 var_typ res_trans_cond.leaf_nodes join_node pvar;
         let id = Ident.create_fresh Ident.knormal in
         let instrs = [Sil.Load (id, Exp.Lvar pvar, var_typ, sil_loc)] in
         { empty_res_trans with
           root_nodes = res_trans_cond.root_nodes;
           leaf_nodes = [join_node];
           instrs = instrs;
           exps = [(Exp.Var id, typ)];
           initd_exps = []; (* TODO we should get exps from branches+cond *)
         }
     | _ -> assert false)

  (* The GNU extension to the conditional operator which allows the middle operand to be omitted. *)
  and binaryConditionalOperator_trans trans_state stmt_info stmt_list expr_info =
    match stmt_list with
    | [stmt1; ostmt1; ostmt2; stmt2]
      when contains_opaque_value_expr ostmt1 && contains_opaque_value_expr ostmt2 ->
        let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
        let trans_state_pri = PriorityNode.force_claim_priority_node trans_state stmt_info in
        let trans_state_cond = { trans_state_pri with
                                 continuation = mk_cond_continuation trans_state_pri.continuation
                               } in
        (* evaluate stmt1 once. Then, use it as replacement for OpaqueValueExpr*)
        (* when translating ostmt1 and ostmt2 *)
        let init_res_trans = instruction trans_state_cond stmt1 in
        let opaque_exp = extract_exp_from_list init_res_trans.exps "" in
        let trans_state' = { trans_state_pri with opaque_exp = Some opaque_exp } in
        let op_res_trans = conditionalOperator_trans trans_state' stmt_info
            [ostmt1; ostmt2; stmt2] expr_info in
        let trans_state'' = { trans_state_cond with succ_nodes = op_res_trans.root_nodes } in
        let init_res_trans' = PriorityNode.compute_results_to_parent trans_state'' sil_loc
            "BinaryConditinalStmt Init" stmt_info [init_res_trans] in
        let root_nodes = init_res_trans'.root_nodes in
        let root_nodes' = if root_nodes <> [] then root_nodes else op_res_trans.root_nodes in
        { op_res_trans with root_nodes = root_nodes'; }
    | _ -> Logging.out "BinaryConditionalOperator not translated@.";
        assert false

  (* Translate a condition for if/loops statement. It shorts-circuit and/or. *)
  (* The invariant is that the translation of a condition always contains (at least) *)
  (* the prune nodes. Moreover these are always the leaf nodes of the translation. *)
  and cond_trans trans_state cond =
    let context = trans_state.context in
    let si, _ = Clang_ast_proj.get_stmt_tuple cond in
    let sil_loc = CLocation.get_sil_location si context in
    let mk_prune_node b e ins =
      create_prune_node b e ins sil_loc (Sil.Ik_if) context in
    let extract_exp el =
      extract_exp_from_list el
        "\nWARNING: Missing expression for Conditional operator. Need to be fixed" in
    (* this function translate cond without doing shortcircuit *)
    let no_short_circuit_cond () =
      Logging.out_debug " No short-circuit condition\n";
      let res_trans_cond =
        if is_null_stmt cond then {
          empty_res_trans with exps = [(Exp.Const (Const.Cint IntLit.one), (Typ.Tint Typ.IBool))]
        }
        (* Assumption: If it's a null_stmt, it is a loop with no bound, so we set condition to 1 *)
        else
          instruction trans_state cond in
      let e', instrs' =
        define_condition_side_effects res_trans_cond.exps res_trans_cond.instrs sil_loc in
      let prune_t = mk_prune_node true e' instrs' in
      let prune_f = mk_prune_node false e' instrs' in
      List.iter
        ~f:(fun n' -> Procdesc.node_set_succs_exn context.procdesc n' [prune_t; prune_f] [])
        res_trans_cond.leaf_nodes;
      let rnodes = if Int.equal (List.length res_trans_cond.root_nodes) 0 then
          [prune_t; prune_f]
        else res_trans_cond.root_nodes in
      { empty_res_trans with
        root_nodes = rnodes;
        leaf_nodes = [prune_t; prune_f];
        instrs = instrs';
        exps = e';
      } in

    (* This function translate (s1 binop s2) doing shortcircuit for '&&' and '||' *)
    (* At the high level it does cond_trans s1; cond_trans s2; glue_nodes *)
    (* The glue_nodes partitions the prune nodes of s1's translation.*)
    (* Some of them need to go to the statement to be executed after the *)
    (* condition (prune_to_short_c) and others to the root nodes of the *)
    (* translation of s2 (i.e., the case when we need to fully evaluate*)
    (* the condition to decide its truth value). *)
    let short_circuit binop s1 s2 =
      let res_trans_s1 = cond_trans trans_state s1 in
      let prune_nodes_t, prune_nodes_f =
        List.partition_tf ~f:is_true_prune_node res_trans_s1.leaf_nodes in
      let res_trans_s2 = cond_trans trans_state s2 in
      (* prune_to_s2 is the prune node that is connected with the root node of the *)
      (* translation of s2.*)
      (* prune_to_short_c is the prune node that is connected directly with the branch *)
      (* where the control flow goes in case of short circuit *)
      let prune_to_s2, prune_to_short_c = (match binop with
          | Binop.LAnd -> prune_nodes_t, prune_nodes_f
          | Binop.LOr -> prune_nodes_f, prune_nodes_t
          | _ -> assert false) in
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n res_trans_s2.root_nodes [])
        prune_to_s2;
      let root_nodes_to_parent =
        if Int.equal (List.length res_trans_s1.root_nodes) 0
        then res_trans_s1.leaf_nodes
        else res_trans_s1.root_nodes in
      let (exp1, typ1) = extract_exp res_trans_s1.exps in
      let (exp2, _) = extract_exp res_trans_s2.exps in
      let e_cond = Exp.BinOp (binop, exp1, exp2) in
      { empty_res_trans with
        root_nodes = root_nodes_to_parent;
        leaf_nodes = prune_to_short_c@res_trans_s2.leaf_nodes;
        instrs = res_trans_s1.instrs@res_trans_s2.instrs;
        exps = [(e_cond, typ1)];
      } in
    Logging.out_debug "Translating Condition for If-then-else/Loop/Conditional Operator \n";
    let open Clang_ast_t in
    match cond with
    | BinaryOperator(_, [s1; s2], _, boi) ->
        (match boi.Clang_ast_t.boi_kind with
         | `LAnd -> short_circuit (Binop.LAnd) s1 s2
         | `LOr -> short_circuit (Binop.LOr) s1 s2
         | _ -> no_short_circuit_cond ())
    | ParenExpr(_,[s], _) -> (* condition can be wrapped in parenthesys *)
        cond_trans trans_state s
    | _ -> no_short_circuit_cond ()

  and declStmt_in_condition_trans trans_state decl_stmt res_trans_cond =
    match decl_stmt with
    | Clang_ast_t.DeclStmt(stmt_info, _, decl_list) ->
        let trans_state_decl = { trans_state with
                                 succ_nodes = res_trans_cond.root_nodes
                               } in
        declStmt_trans trans_state_decl decl_list stmt_info
    | _ -> res_trans_cond

  and ifStmt_trans trans_state stmt_info stmt_list =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let join_node = create_node (Procdesc.Node.Join_node) [] sil_loc context in
    Procdesc.node_set_succs_exn context.procdesc join_node succ_nodes [];
    let trans_state' = { trans_state with succ_nodes = [join_node] } in
    let do_branch branch stmt_branch prune_nodes =
      (* leaf nodes are ignored here as they will be already attached to join_node *)
      let res_trans_b = instruction trans_state' stmt_branch in
      let nodes_branch =
        (match res_trans_b.root_nodes with
         | [] ->
             [create_node
                (Procdesc.Node.Stmt_node "IfStmt Branch") res_trans_b.instrs sil_loc context]
         | _ ->
             res_trans_b.root_nodes) in
      let prune_nodes_t, prune_nodes_f = List.partition_tf ~f:is_true_prune_node prune_nodes in
      let prune_nodes' = if branch then prune_nodes_t else prune_nodes_f in
      List.iter
        ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n nodes_branch [])
        prune_nodes' in
    (match stmt_list with
     | [_; decl_stmt; cond; stmt1; stmt2] ->
         (* set the flat to inform that we are translating a condition of a if *)
         let continuation' = mk_cond_continuation trans_state.continuation in
         let trans_state'' = { trans_state with
                               continuation = continuation';
                               succ_nodes = []
                             } in
         let res_trans_cond = cond_trans trans_state'' cond in
         let res_trans_decl = declStmt_in_condition_trans trans_state decl_stmt res_trans_cond in
         (* Note: by contruction prune nodes are leafs_nodes_cond *)
         do_branch true stmt1 res_trans_cond.leaf_nodes;
         do_branch false stmt2 res_trans_cond.leaf_nodes;
         { empty_res_trans with
           root_nodes = res_trans_decl.root_nodes;
           leaf_nodes = [join_node];
         }
     | _ -> assert false)

  (* Assumption: the CompoundStmt can be made of different stmts, not just CaseStmts *)
  and switchStmt_trans trans_state stmt_info switch_stmt_list =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let continuation = trans_state.continuation in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let open Clang_ast_t in
    match switch_stmt_list with
    | [_; decl_stmt; cond; CompoundStmt(stmt_info, stmt_list)] ->
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
        let trans_state' ={ trans_state_pri with succ_nodes = []} in
        let res_trans_cond_tmp = instruction trans_state' cond in
        let switch_special_cond_node =
          let node_kind = Procdesc.Node.Stmt_node "Switch_stmt" in
          create_node node_kind res_trans_cond_tmp.instrs sil_loc context in
        List.iter
          ~f:(fun n' ->
              Procdesc.node_set_succs_exn context.procdesc n' [switch_special_cond_node] [])
          res_trans_cond_tmp.leaf_nodes;
        let root_nodes =
          if res_trans_cond_tmp.root_nodes <> [] then res_trans_cond_tmp.root_nodes
          else [switch_special_cond_node] in
        let (switch_e_cond', switch_e_cond'_typ) =
          extract_exp_from_list res_trans_cond_tmp.exps
            "\nWARNING: The condition of the SwitchStmt is not singleton. Need to be fixed\n" in
        let res_trans_cond = { res_trans_cond_tmp with
                               root_nodes = root_nodes;
                               leaf_nodes = [switch_special_cond_node]
                             } in
        let res_trans_decl = declStmt_in_condition_trans trans_state decl_stmt res_trans_cond in
        let trans_state_no_pri =
          if PriorityNode.own_priority_node trans_state_pri.priority stmt_info
          then
            { trans_state_pri with priority = Free }
          else trans_state_pri in
        let switch_exit_point = succ_nodes in
        let continuation' =
          match continuation with
          | Some cont -> Some { cont with break = switch_exit_point }
          | None -> Some { break = switch_exit_point; continue = []; return_temp = false } in
        let trans_state'' = { trans_state_no_pri with continuation = continuation'} in
        let merge_into_cases stmt_list = (* returns list_of_cases * before_any_case_instrs *)
          let rec aux rev_stmt_list acc cases =
            (match rev_stmt_list with
             | CaseStmt (info, a :: b :: CaseStmt x :: c) :: rest -> (* case x: case y: ... *)
                 if c <> []
                 (* empty case with nested case, then followed by some instructions *)
                 then assert false;
                 let rest' = CaseStmt (info, a :: b :: []) :: rest in
                 let rev_stmt_list' = (CaseStmt x) :: rest' in
                 aux rev_stmt_list' acc cases
             | CaseStmt (info, a :: b :: DefaultStmt x :: c) :: rest ->
                 (* case x: default: ... *)
                 if c <> []
                 (* empty case with nested case, then followed by some instructions *)
                 then assert false;
                 let rest' = CaseStmt(info, a :: b :: []) :: rest in
                 let rev_stmt_list' = DefaultStmt x :: rest' in
                 aux rev_stmt_list' acc cases
             | DefaultStmt (info, CaseStmt x :: c) :: rest -> (* default: case x: ... *)
                 if c <> []
                 (* empty case with nested case, then followed by some instructions *)
                 then assert false;
                 let rest' = DefaultStmt (info, []) :: rest in
                 let rev_stmt_list' = CaseStmt x :: rest' in
                 aux rev_stmt_list' acc cases
             | CaseStmt (info, a :: b :: c) :: rest ->
                 aux rest [] (CaseStmt (info, a :: b :: c @ acc) :: cases)
             | DefaultStmt (info, c) :: rest -> (* default is always the last in the list *)
                 aux rest [] (DefaultStmt(info, c @ acc) :: cases)
             | x :: rest ->
                 aux rest (x :: acc) cases
             | [] ->
                 cases, acc) in
          aux (List.rev stmt_list) [] [] in
        let list_of_cases, pre_case_stmts = merge_into_cases stmt_list in
        let rec connected_instruction rev_instr_list successor_nodes =
          (* returns the entry point of the translated set of instr *)
          match rev_instr_list with
          | [] -> successor_nodes
          | instr :: rest ->
              let trans_state''' = { trans_state'' with succ_nodes = successor_nodes } in
              let res_trans_instr = instruction trans_state''' instr in
              let instr_entry_points = res_trans_instr.root_nodes in
              connected_instruction rest instr_entry_points in
        let rec translate_and_connect_cases cases next_nodes next_prune_nodes =
          let create_prune_nodes_for_case case =
            match case with
            | CaseStmt (stmt_info, case_const :: _ :: _) ->
                let trans_state_pri =
                  PriorityNode.try_claim_priority_node trans_state'' stmt_info in
                let res_trans_case_const = instruction trans_state_pri case_const in
                let e_const = res_trans_case_const.exps in
                let e_const' =
                  match e_const with
                  | [(head, _)] -> head
                  | _ -> assert false in
                let sil_eq_cond = Exp.BinOp (Binop.Eq, switch_e_cond', e_const') in
                let sil_loc = CLocation.get_sil_location stmt_info context in
                let true_prune_node =
                  create_prune_node true [(sil_eq_cond, switch_e_cond'_typ)]
                    res_trans_case_const.instrs sil_loc Sil.Ik_switch context in
                let false_prune_node =
                  create_prune_node false [(sil_eq_cond, switch_e_cond'_typ)]
                    res_trans_case_const.instrs sil_loc Sil.Ik_switch context in
                (true_prune_node, false_prune_node)
            | _ -> assert false in
          match cases with (* top-down to handle default cases *)
          | [] -> next_nodes, next_prune_nodes
          | CaseStmt(_, _ :: _ :: case_content) as case :: rest ->
              let last_nodes, last_prune_nodes =
                translate_and_connect_cases rest next_nodes next_prune_nodes in
              let case_entry_point = connected_instruction (List.rev case_content) last_nodes in
              (* connects between cases, then continuation has priority about breaks *)
              let prune_node_t, prune_node_f = create_prune_nodes_for_case case in
              Procdesc.node_set_succs_exn context.procdesc prune_node_t case_entry_point [];
              Procdesc.node_set_succs_exn context.procdesc prune_node_f last_prune_nodes [];
              case_entry_point, [prune_node_t; prune_node_f]
          | DefaultStmt(stmt_info, default_content) :: rest ->
              let sil_loc = CLocation.get_sil_location stmt_info context in
              let placeholder_entry_point =
                create_node
                  (Procdesc.Node.Stmt_node "DefaultStmt_placeholder") [] sil_loc context in
              let last_nodes, last_prune_nodes =
                translate_and_connect_cases rest next_nodes [placeholder_entry_point] in
              let default_entry_point =
                connected_instruction (List.rev default_content) last_nodes in
              Procdesc.node_set_succs_exn
                context.procdesc placeholder_entry_point default_entry_point [];
              default_entry_point, last_prune_nodes
          | _ -> assert false in
        let top_entry_point, top_prune_nodes =
          translate_and_connect_cases list_of_cases succ_nodes succ_nodes in
        let _ = connected_instruction (List.rev pre_case_stmts) top_entry_point in
        Procdesc.node_set_succs_exn
          context.procdesc switch_special_cond_node top_prune_nodes [];
        let top_nodes = res_trans_decl.root_nodes in
        List.iter
          ~f:(fun n' -> Procdesc.Node.append_instrs n' []) succ_nodes;
        (* succ_nodes will remove the temps *)
        { empty_res_trans with root_nodes = top_nodes; leaf_nodes = succ_nodes }
    | _ -> assert false

  and stmtExpr_trans trans_state stmt_list =
    let stmt =
      extract_stmt_from_singleton stmt_list "ERROR: StmtExpr should have only one statement.\n" in
    let res_trans_stmt = instruction trans_state stmt in
    let exps' = List.rev res_trans_stmt.exps in
    match exps' with
    | last_exp :: _ ->
        { res_trans_stmt with exps = [last_exp]; }
    | [] -> res_trans_stmt

  and loop_instruction trans_state loop_kind stmt_info =
    let outer_continuation = trans_state.continuation in
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let join_node = create_node Procdesc.Node.Join_node [] sil_loc context in
    let continuation = Some { break = succ_nodes; continue = [join_node]; return_temp = false } in
    (* set the flat to inform that we are translating a condition of a if *)
    let continuation_cond = mk_cond_continuation outer_continuation in
    let init_incr_nodes =
      match loop_kind with
      | Loops.For (init, _, _, incr, _) ->
          let trans_state' = {
            trans_state with
            succ_nodes = [join_node];
            continuation = continuation;
          } in
          let res_trans_init = instruction trans_state' init in
          let res_trans_incr = instruction trans_state' incr in
          Some (res_trans_init.root_nodes, res_trans_incr.root_nodes)
      | _ -> None in
    let cond_stmt = Loops.get_cond loop_kind in
    let trans_state_cond = {
      trans_state with
      continuation = continuation_cond;
      succ_nodes = [];
    } in
    let res_trans_cond = cond_trans trans_state_cond cond_stmt in
    let decl_stmt_opt = match loop_kind with
      | Loops.For (_, decl_stmt, _, _, _) -> Some decl_stmt
      | Loops.While (decl_stmt_opt, _, _) -> decl_stmt_opt
      | _ -> None in
    let res_trans_decl = match decl_stmt_opt with
      | Some decl_stmt -> declStmt_in_condition_trans trans_state decl_stmt res_trans_cond
      | _ -> res_trans_cond in
    let body_succ_nodes =
      match loop_kind with
      | Loops.For _ -> (match init_incr_nodes with
          | Some (_, nodes_incr) -> nodes_incr
          | None -> assert false)
      | Loops.While _ -> [join_node]
      | Loops.DoWhile _ -> res_trans_cond.root_nodes in
    let body_continuation = match continuation, init_incr_nodes with
      | Some c, Some (_, nodes_incr) ->
          Some { c with continue = nodes_incr }
      | _ -> continuation in
    let res_trans_body =
      let trans_state_body =
        { trans_state with
          succ_nodes = body_succ_nodes;
          continuation = body_continuation } in
      instruction trans_state_body (Loops.get_body loop_kind) in
    let join_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ -> res_trans_decl.root_nodes
      | Loops.DoWhile _ -> res_trans_body.root_nodes in
    (* Note: prune nodes are by contruction the res_trans_cond.leaf_nodes *)
    let prune_nodes_t, prune_nodes_f =
      List.partition_tf ~f:is_true_prune_node res_trans_cond.leaf_nodes in
    let prune_t_succ_nodes =
      match loop_kind with
      | Loops.For _ | Loops.While _ -> res_trans_body.root_nodes
      | Loops.DoWhile _ -> [join_node] in
    Procdesc.node_set_succs_exn context.procdesc join_node join_succ_nodes [];
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n prune_t_succ_nodes [])
      prune_nodes_t;
    List.iter
      ~f:(fun n -> Procdesc.node_set_succs_exn context.procdesc n succ_nodes [])
      prune_nodes_f;
    let root_nodes =
      match loop_kind with
      | Loops.For _ ->
          (match init_incr_nodes with | Some (nodes_init, _) -> nodes_init | None -> assert false)
      | Loops.While _ | Loops.DoWhile _ -> [join_node] in
    { empty_res_trans with root_nodes = root_nodes; leaf_nodes = prune_nodes_f }

  and forStmt_trans trans_state init decl_stmt cond incr body stmt_info =
    let for_kind = Loops.For (init, decl_stmt, cond, incr, body) in
    loop_instruction trans_state for_kind stmt_info

  and whileStmt_trans trans_state decl_stmt cond body stmt_info =
    let while_kind = Loops.While (Some decl_stmt, cond, body) in
    loop_instruction trans_state while_kind stmt_info

  and doStmt_trans trans_state stmt_info cond body =
    let dowhile_kind = Loops.DoWhile (cond, body) in
    loop_instruction trans_state dowhile_kind stmt_info


  (* Iteration over colection

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
    | [iterator_decl; begin_stmt; end_stmt; exit_cond; increment; assign_current_index; loop_body] ->
        let loop_body' = CompoundStmt (stmt_info, [assign_current_index; loop_body]) in
        let null_stmt = NullStmt (stmt_info, []) in
        let beginend_stmt = CompoundStmt (stmt_info, [begin_stmt; end_stmt]) in
        let for_loop =
          ForStmt (stmt_info, [beginend_stmt; null_stmt; exit_cond; increment; loop_body']) in
        instruction trans_state (CompoundStmt (stmt_info, [iterator_decl; for_loop]))
    | _ -> assert false

  (* Fast iteration for colection
     for (type_it i in collection) { body }
     is translate as
     {
      i = type_next_object();
      while(i != nil) { body; i = type_next_object();}
     }
  *)
  and objCForCollectionStmt_trans trans_state item items body stmt_info =
    let _ = instruction trans_state item in
    (* Here we do ast transformation, so we don't need the value of the translation of the *)
    (* variable item but we still need to add the variable to the locals *)
    let assign_next_object, cond = Ast_expressions.make_next_object_exp stmt_info item items in
    let body' = Clang_ast_t.CompoundStmt (stmt_info, [body; assign_next_object]) in
    let null_stmt = Clang_ast_t.NullStmt (stmt_info,[]) in
    let loop = Clang_ast_t.WhileStmt (stmt_info, [null_stmt; cond; body']) in
    instruction trans_state (Clang_ast_t.CompoundStmt (stmt_info, [assign_next_object; loop]))

  and initListExpr_trans trans_state stmt_info expr_info stmts =
    let context = trans_state.context in
    let tenv = context.tenv in
    let is_array typ = match typ with | Typ.Tarray _ -> true | _ -> false in
    let (var_exp, typ) =
      match trans_state.var_exp_typ with
      | Some var_exp_typ -> var_exp_typ
      | None -> create_var_exp_tmp_var trans_state expr_info "SIL_init_list__" in
    let trans_state = { trans_state with var_exp_typ = Some (var_exp, typ) } in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let var_type =
      CType_decl.type_ptr_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_type_ptr in
    let lh = var_or_zero_in_init_list tenv var_exp var_type ~return_zero:false in
    let res_trans_subexpr_list =
      initListExpr_initializers_trans trans_state var_exp 0 stmts typ false stmt_info in
    let rh_exps = collect_exprs res_trans_subexpr_list in
    if Int.equal (List.length rh_exps) 0 then
      let exps =
        match Sil.zero_value_of_numerical_type_option var_type with
        | Some zero_exp -> [(zero_exp, typ)]
        | None -> [] in
      { empty_res_trans with root_nodes = trans_state.succ_nodes; exps = exps; }
    else
      (* For arrays, the size in the type may be an overapproximation of the number *)
      (* of literals the array is initialized with *)
      let lh =
        if is_array var_type && List.length lh > List.length rh_exps then
          List.take lh (List.length rh_exps)
        else lh in
      if Int.equal (List.length rh_exps) (List.length lh) then
        (* Creating new instructions by assigning right hand side to left hand side expressions *)
        let assign_instr (lh_exp, lh_t) (rh_exp, _) = Sil.Store (lh_exp, lh_t, rh_exp, sil_loc) in
        let assign_instrs =
          let initd_exps = collect_initid_exprs res_trans_subexpr_list in
          (* If the variable var_exp is of type array, and some of its indices were initialized *)
          (* by some constructor call, which we can tell by the fact that the index is returned *)
          (* in initd_exps, then we assume that all the indices were initialized and *)
          (* we don't need any assignments. *)
          if List.exists
              ~f:((fun arr index -> Exp.is_array_index_of index arr) var_exp)
              initd_exps
          then []
          else List.map2_exn ~f:assign_instr lh rh_exps in
        let initlist_expr_res =
          { empty_res_trans with
            exps = [(var_exp, var_type)];
            initd_exps = [var_exp];
            instrs = assign_instrs;
          } in
        let all_res_trans = res_trans_subexpr_list @ [initlist_expr_res] in
        let nname = "InitListExp" in
        let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
            nname stmt_info all_res_trans in
        { res_trans_to_parent with exps = initlist_expr_res.exps }
      else  (* If the right hand expressions are not as many as the left hand expressions *)
        (* something's wrong *)
        { empty_res_trans with root_nodes = trans_state.succ_nodes }

  and init_expr_trans trans_state var_exp_typ var_stmt_info init_expr_opt =
    match init_expr_opt with
    | None ->
        (* Nothing to do if no init expression *)
        { empty_res_trans with root_nodes = trans_state.succ_nodes }
    | Some ie -> (*For init expr, translate how to compute it and assign to the var*)
        let var_exp, _ = var_exp_typ in
        let context = trans_state.context in
        let sil_loc = CLocation.get_sil_location var_stmt_info context in
        let trans_state_pri = PriorityNode.try_claim_priority_node trans_state var_stmt_info in
        (* if ie is a block the translation need to be done
           with the block special cases by exec_with_block_priority *)
        let res_trans_ie =
          let trans_state' = { trans_state_pri with
                               succ_nodes = [];
                               var_exp_typ = Some var_exp_typ } in
          let instruction' =
            exec_with_self_exception (exec_with_glvalue_as_reference instruction) in
          exec_with_block_priority_exception instruction' trans_state' ie var_stmt_info in
        let (sil_e1', ie_typ) = extract_exp_from_list res_trans_ie.exps
            "WARNING: In DeclStmt we expect only one expression returned in recursive call\n" in
        let rhs_owning_method = CTrans_utils.is_owning_method ie in
        let _, instrs_assign =
          (* variable might be initialized already - do nothing in that case*)
          if List.exists ~f:(Exp.equal var_exp) res_trans_ie.initd_exps then ([], [])
          else if !Config.arc_mode &&
                  (CTrans_utils.is_method_call ie ||
                   ObjcInterface_decl.is_pointer_to_objc_class ie_typ)
          then
            (* In arc mode, if it's a method call or we are initializing
               with a pointer to objc class *)
            (* we need to add retain/release *)
            let (e, instrs) =
              CArithmetic_trans.assignment_arc_mode
                var_exp ie_typ sil_e1' sil_loc rhs_owning_method true in
            ([(e, ie_typ)], instrs)
          else
            ([], [Sil.Store (var_exp, ie_typ, sil_e1', sil_loc)]) in
        let res_trans_assign = { empty_res_trans with
                                 instrs = instrs_assign } in
        let all_res_trans = [res_trans_ie; res_trans_assign] in
        let res_trans = PriorityNode.compute_results_to_parent trans_state_pri sil_loc "DeclStmt"
            var_stmt_info all_res_trans in
        { res_trans with exps =  [(var_exp, ie_typ)] }

  and collect_all_decl trans_state var_decls next_nodes stmt_info =
    let open Clang_ast_t in
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let procname = Procdesc.get_proc_name procdesc in
    let do_var_dec (di, var_name, qual_type, vdi) next_node =
      let var_decl = VarDecl (di, var_name, qual_type, vdi) in
      let pvar = CVar_decl.sil_var_of_decl context var_decl procname in
      let typ = CType_decl.type_ptr_to_sil_type
          context.CContext.tenv
          qual_type.Clang_ast_t.qt_type_ptr in
      CVar_decl.add_var_to_locals procdesc var_decl typ pvar;
      let trans_state' = { trans_state with succ_nodes = next_node } in
      init_expr_trans trans_state' (Exp.Lvar pvar, typ) stmt_info vdi.Clang_ast_t.vdi_init_expr in

    match var_decls with
    | [] -> { empty_res_trans with root_nodes = next_nodes }
    | VarDecl (di, n, tp, vdi) :: var_decls' ->
        (* Var are defined when procdesc is created, here we only take care of initialization*)
        let res_trans_vd = collect_all_decl trans_state var_decls' next_nodes stmt_info in
        let res_trans_tmp = do_var_dec (di, n, tp, vdi) res_trans_vd.root_nodes in
        { empty_res_trans with
          root_nodes = res_trans_tmp.root_nodes; leaf_nodes = [];
          instrs = res_trans_tmp.instrs @ res_trans_vd.instrs;
          exps = [];
          initd_exps = res_trans_tmp.initd_exps @ res_trans_vd.initd_exps;
        }
    | CXXRecordDecl _ :: var_decls' (*C++/C record decl treated in the same way *)
    | RecordDecl _ :: var_decls' ->
        (* Record declaration is done in the beginning when procdesc is defined.*)
        collect_all_decl trans_state var_decls' next_nodes stmt_info
    | _ -> assert false

  (* stmt_list is ignored because it contains the same instructions as *)
  (* the init expression. We use the latter info.                      *)
  and declStmt_trans trans_state decl_list stmt_info =
    let succ_nodes = trans_state.succ_nodes in
    let res_trans =
      let open Clang_ast_t in
      match decl_list with
      | VarDecl _ :: _ ->  (* Case for simple variable declarations*)
          collect_all_decl trans_state decl_list succ_nodes stmt_info
      | CXXRecordDecl _ :: _ (*C++/C record decl treated in the same way *)
      | RecordDecl _ :: _ -> (* Case for struct *)
          collect_all_decl trans_state decl_list succ_nodes stmt_info
      | _ ->
          Logging.out
            "WARNING: In DeclStmt found an unknown declaration type. \
             RETURNING empty list of declaration. NEED TO BE FIXED";
          empty_res_trans in
    { res_trans with leaf_nodes = [] }

  and objCPropertyRefExpr_trans trans_state stmt_list =
    match stmt_list with
    | [stmt] -> instruction trans_state stmt
    | _ -> assert false

  (* For OpaqueValueExpr we return the translation generated from its source expression*)
  and opaqueValueExpr_trans trans_state opaque_value_expr_info =
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    match trans_state.opaque_exp with
    | Some exp -> { empty_res_trans with exps = [exp] }
    | _ ->
        (match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
         | Some stmt -> instruction trans_state stmt
         | _ -> assert false)

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
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let rec do_semantic_elements el =
      let open Clang_ast_t in
      match el with
      | OpaqueValueExpr _ :: el' -> do_semantic_elements el'
      | stmt :: _ -> instruction trans_state stmt
      | _ -> assert false in
    match stmt_list with
    | _ :: semantic_form ->
        do_semantic_elements semantic_form
    | _ -> assert false

  (* Cast expression are treated the same apart from the cast operation kind*)
  and cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_expr_info =
    let context = trans_state.context in
    Logging.out_debug "  priority node free = '%s'\n@."
      (string_of_bool (PriorityNode.is_priority_free trans_state));
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: In CastExpr There must be only one stmt defining the expression to be cast.\n" in
    let res_trans_stmt = instruction trans_state stmt in
    let typ =
      CType_decl.type_ptr_to_sil_type context.CContext.tenv expr_info.Clang_ast_t.ei_type_ptr in
    let cast_kind = cast_expr_info.Clang_ast_t.cei_cast_kind in
    (* This gives the differnece among cast operations kind*)
    let is_objc_bridged_cast_expr _ stmt =
      match stmt with | Clang_ast_t.ObjCBridgedCastExpr _ -> true | _ -> false in
    let is_objc_bridged = CAst_utils.exists_eventually_st is_objc_bridged_cast_expr () stmt in
    let cast_inst, cast_exp =
      cast_operation trans_state cast_kind res_trans_stmt.exps typ sil_loc is_objc_bridged in
    { res_trans_stmt with
      instrs = res_trans_stmt.instrs @ cast_inst;
      exps = [cast_exp];
    }

  (* function used in the computation for both Member_Expr and ObjCIVarRefExpr *)
  and do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref  =
    let exp_stmt = extract_stmt_from_singleton stmt_list
        "WARNING: in MemberExpr there must be only one stmt defining its expression.\n" in
    (* Don't pass var_exp_typ to child of MemberExpr - this may lead to initializing variable *)
    (* with wrong value. For example, we don't want p to be initialized with X(1) for:*)
    (* int p = X(1).field; *)
    let trans_state' = { trans_state with var_exp_typ = None } in
    let result_trans_exp_stmt = exec_with_glvalue_as_reference instruction trans_state' exp_stmt in
    decl_ref_trans trans_state result_trans_exp_stmt stmt_info decl_ref ~is_constructor_init:false

  and objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info =
    let decl_ref = obj_c_ivar_ref_expr_info.Clang_ast_t.ovrei_decl_ref in
    do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref

  and memberExpr_trans trans_state stmt_info stmt_list member_expr_info =
    let decl_ref = member_expr_info.Clang_ast_t.mei_decl_ref in
    let res_trans = do_memb_ivar_ref_exp trans_state stmt_info stmt_list decl_ref in
    let is_virtual_dispatch = member_expr_info.Clang_ast_t.mei_performs_virtual_dispatch in
    { res_trans with is_cpp_call_virtual = res_trans.is_cpp_call_virtual && is_virtual_dispatch }

  and unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info =
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: We expect only one element in stmt list defining \
         the operand in UnaryOperator. NEED FIXING\n" in
    let trans_state' = { trans_state_pri with succ_nodes = [] } in
    let res_trans_stmt = instruction trans_state' stmt in
    (* Assumption: the operand does not create a cfg node*)
    let (sil_e', _) =
      extract_exp_from_list res_trans_stmt.exps
        "\nWARNING: Missing operand in unary operator. NEED FIXING.\n" in
    let ret_typ =
      CType_decl.type_ptr_to_sil_type
        context.CContext.tenv expr_info.Clang_ast_t.ei_type_ptr in
    let exp_op, instr_op =
      CArithmetic_trans.unary_operation_instruction
        context.translation_unit_context unary_operator_info sil_e' ret_typ sil_loc in
    let unary_op_res_trans = { empty_res_trans with instrs = instr_op } in
    let all_res_trans = [ res_trans_stmt; unary_op_res_trans ] in
    let nname = "UnaryOperator" in
    let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname
        stmt_info all_res_trans in
    { res_trans_to_parent with exps = [(exp_op, ret_typ)] }

  and returnStmt_trans trans_state stmt_info stmt_list =
    let context = trans_state.context in
    let succ_nodes = trans_state.succ_nodes in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let mk_ret_node instrs =
      let ret_node = create_node (Procdesc.Node.Stmt_node "Return Stmt") instrs sil_loc context in
      Procdesc.node_set_succs_exn
        context.procdesc
        ret_node [(Procdesc.get_exit_node context.CContext.procdesc)] [];
      ret_node in
    let trans_result = (match stmt_list with
        | [stmt] -> (* return exp; *)
            let procdesc = context.CContext.procdesc in
            let ret_type = Procdesc.get_ret_type procdesc in
            let ret_exp, ret_typ, var_instrs = match context.CContext.return_param_typ with
              | Some ret_param_typ ->
                  let name = CFrontend_config.return_param in
                  let procname = Procdesc.get_proc_name procdesc in
                  let pvar = Pvar.mk (Mangled.from_string name) procname in
                  let id = Ident.create_fresh Ident.knormal in
                  let instr = Sil.Load (id, Exp.Lvar pvar, ret_param_typ, sil_loc) in
                  let ret_typ = match ret_param_typ with Typ.Tptr (t, _) -> t | _ -> assert false in
                  Exp.Var id, ret_typ, [instr]
              | None ->
                  Exp.Lvar (Procdesc.get_ret_var procdesc), ret_type, [] in
            let trans_state' = { trans_state_pri with
                                 succ_nodes = [];
                                 var_exp_typ = Some (ret_exp, ret_typ) } in
            let res_trans_stmt = exec_with_self_exception instruction trans_state' stmt in
            let (sil_expr, _) = extract_exp_from_list res_trans_stmt.exps
                "WARNING: There should be only one return expression.\n" in

            let ret_instrs = if List.exists ~f:(Exp.equal ret_exp) res_trans_stmt.initd_exps
              then []
              else [Sil.Store (ret_exp, ret_type, sil_expr, sil_loc)] in
            let autorelease_instrs =
              add_autorelease_call context sil_expr ret_type sil_loc in
            let instrs = var_instrs @ res_trans_stmt.instrs @ ret_instrs @ autorelease_instrs in
            let ret_node = mk_ret_node instrs in
            List.iter
              ~f:(fun n -> Procdesc.node_set_succs_exn procdesc n [ret_node] [])
              res_trans_stmt.leaf_nodes;
            let root_nodes_to_parent =
              if List.length res_trans_stmt.root_nodes >0
              then res_trans_stmt.root_nodes
              else [ret_node] in
            { empty_res_trans with root_nodes = root_nodes_to_parent; leaf_nodes = []}
        | [] -> (* return; *)
            let ret_node = mk_ret_node [] in
            { empty_res_trans with root_nodes = [ret_node]; leaf_nodes = []}
        | _ -> Logging.out_debug
                 "\nWARNING: Missing translation of Return Expression. \
                  Return Statement ignored. Need fixing!\n";
            { empty_res_trans with root_nodes = succ_nodes }) in
    (* We expect a return with only one expression *)
    trans_result

  (* We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. *)
  (*  It may be that later on (when we treat ARC) some info can be taken from it. *)
  (* For ParenExpression we translate its body composed by the stmt_list.  *)
  (* In paren expression there should be only one stmt that defines the  expression  *)
  and parenExpr_trans trans_state stmt_list =
    let stmt = extract_stmt_from_singleton stmt_list
        "WARNING: In ParenExpression there should be only one stmt.\n" in
    instruction trans_state stmt

  and objCBoxedExpr_trans trans_state info sel stmt_info stmts =
    let typ =
      CType_decl.class_from_pointer_type
        trans_state.context.CContext.tenv info.Clang_ast_t.ei_type_ptr in
    let obj_c_message_expr_info = Ast_expressions.make_obj_c_message_expr_info_class sel typ None in
    let message_stmt =
      Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  and objCArrayLiteral_trans trans_state info stmt_info stmts =
    let typ =
      CType_decl.class_from_pointer_type
        trans_state.context.CContext.tenv info.Clang_ast_t.ei_type_ptr in
    let meth = CFrontend_config.array_with_objects_count_m in
    let obj_c_mes_expr_info = Ast_expressions.make_obj_c_message_expr_info_class meth typ None in
    let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
    let message_stmt = Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_mes_expr_info) in
    instruction trans_state message_stmt

  and objCDictionaryLiteral_trans trans_state info stmt_info stmts =
    let typ =
      CType_decl.class_from_pointer_type
        trans_state.context.CContext.tenv info.Clang_ast_t.ei_type_ptr in
    let dictionary_literal_pname = BuiltinDecl.__objc_dictionary_literal in
    let dictionary_literal_s = Typ.Procname.get_method dictionary_literal_pname in
    let obj_c_message_expr_info =
      Ast_expressions.make_obj_c_message_expr_info_class dictionary_literal_s typ None in
    let stmts = CGeneral_utils.swap_elements_list stmts in
    let stmts = stmts @ [Ast_expressions.create_nil stmt_info] in
    let message_stmt =
      Clang_ast_t.ObjCMessageExpr
        (stmt_info, stmts, info, obj_c_message_expr_info) in
    instruction trans_state message_stmt

  and objCStringLiteral_trans trans_state stmt_info stmts info =
    let stmts = [Ast_expressions.create_implicit_cast_expr stmt_info stmts
                   Ast_expressions.create_char_star_type `ArrayToPointerDecay] in
    let typ =
      CType_decl.class_from_pointer_type
        trans_state.context.CContext.tenv info.Clang_ast_t.ei_type_ptr in
    let meth = CFrontend_config.string_with_utf8_m in
    let obj_c_mess_expr_info = Ast_expressions.make_obj_c_message_expr_info_class meth typ None in
    let message_stmt = Clang_ast_t.ObjCMessageExpr (stmt_info, stmts, info, obj_c_mess_expr_info) in
    instruction trans_state message_stmt

  (** When objects are autoreleased, they get added a flag AUTORELEASE. All these objects will be
      ignored when checking for memory leaks. When the end of the block autoreleasepool is reached,
      then those objects are released and the autorelease flag is removed. *)
  and objcAutoreleasePool_trans trans_state stmt_info stmts =
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let fname = BuiltinDecl.__objc_release_autorelease_pool in
    let ret_id = Some (Ident.create_fresh Ident.knormal, Typ.Tvoid) in
    (* TODO(jjb): change ret_id to None? *)
    let autorelease_pool_vars = CVar_decl.compute_autorelease_pool_vars context stmts in
    let stmt_call =
      Sil.Call
        (ret_id, (Exp.Const (Const.Cfun fname)),
         autorelease_pool_vars, sil_loc, CallFlags.default) in
    let node_kind = Procdesc.Node.Stmt_node ("Release the autorelease pool") in
    let call_node = create_node node_kind [stmt_call] sil_loc context in
    Procdesc.node_set_succs_exn context.procdesc call_node trans_state.succ_nodes [];
    let trans_state'={ trans_state with continuation = None; succ_nodes =[call_node] } in
    instructions trans_state' stmts

  (* Assumption: stmt_list contains 2 items, the first can be ObjCMessageExpr or ParenExpr *)
  (* We ignore this item since we don't deal with the concurrency problem yet *)
  (* For the same reason we also ignore the stmt_info that
     is related with the ObjCAtSynchronizedStmt construct *)
  (* Finally we recursively work on the CompoundStmt, the second item of stmt_list *)
  and objCAtSynchronizedStmt_trans trans_state stmt_list =
    (match stmt_list with
     | [_; compound_stmt] -> instruction trans_state compound_stmt
     | _ -> assert false)

  and blockExpr_trans trans_state stmt_info expr_info decl =
    let context = trans_state.context in
    let procname = Procdesc.get_proc_name context.CContext.procdesc in
    let loc =
      (match stmt_info.Clang_ast_t.si_source_range with (l1, _) ->
         CLocation.clang_to_sil_location context.CContext.translation_unit_context l1) in
    (* Given a captured var, return the instruction to assign it to a temp *)
    let assign_captured_var (cvar, typ) =
      let id = Ident.create_fresh Ident.knormal in
      let instr = Sil.Load (id, (Exp.Lvar cvar), typ, loc) in
      (id, instr) in
    match decl with
    | Clang_ast_t.BlockDecl (_, block_decl_info) ->
        let open CContext in
        let type_ptr = expr_info.Clang_ast_t.ei_type_ptr in
        let block_pname = CProcname.mk_fresh_block_procname procname in
        let typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
        (* We need to set the explicit dependency between the newly created block and the *)
        (* defining procedure. We add an edge in the call graph.*)
        Cg.add_edge context.cg procname block_pname;
        let captured_block_vars = block_decl_info.Clang_ast_t.bdi_captured_variables in
        let captureds = CVar_decl.captured_vars_from_block_info context captured_block_vars in
        let ids_instrs = List.map ~f:assign_captured_var captureds in
        let ids, instrs = List.unzip ids_instrs in
        let block_data = (context, type_ptr, block_pname, captureds) in
        F.function_decl context.translation_unit_context context.tenv context.cfg context.cg decl
          (Some block_data);
        let captured_vars =
          List.map2_exn ~f:(fun id (pvar, typ) -> (Exp.Var id, pvar, typ)) ids captureds in
        let closure = Exp.Closure { name=block_pname; captured_vars } in
        let block_name = Typ.Procname.to_string block_pname in
        let static_vars = CContext.static_vars_for_block context block_pname in
        let captured_static_vars = captureds @ static_vars in
        let alloc_block_instr =
          allocate_block trans_state block_name captured_static_vars loc in
        { empty_res_trans with
          instrs = alloc_block_instr @ instrs;
          exps = [(closure, typ)];
        }
    | _ -> assert false

  and initListExpr_initializers_trans trans_state var_exp n stmts typ is_dyn_array stmt_info =
    let (var_exp_inside, typ_inside) = match typ with
      | Typ.Tarray (t, _) when Typ.is_array_of_cpp_class typ ->
          Exp.Lindex (var_exp, Exp.Const (Const.Cint (IntLit.of_int n))), t
      | _ when is_dyn_array ->
          Exp.Lindex (var_exp, Exp.Const (Const.Cint (IntLit.of_int n))), typ
      | _ -> var_exp, typ in
    let trans_state' = { trans_state with var_exp_typ = Some (var_exp_inside, typ_inside) } in
    match stmts with
    | [] -> []
    | stmt :: rest ->
        let rest_stmts_res_trans = initListExpr_initializers_trans trans_state var_exp (n + 1) rest
            typ is_dyn_array stmt_info in
        match stmt with
        | Clang_ast_t.InitListExpr (_ , stmts , _) ->
            let inside_stmts_res_trans = initListExpr_initializers_trans trans_state var_exp_inside
                0 stmts typ_inside is_dyn_array stmt_info in
            inside_stmts_res_trans @ rest_stmts_res_trans
        | _ ->
            let stmt_res_trans = if is_dyn_array then
                let init_stmt_info = { stmt_info with
                                       Clang_ast_t.si_pointer = CAst_utils.get_fresh_pointer () } in
                init_expr_trans trans_state' (var_exp_inside, typ_inside) init_stmt_info (Some stmt)
              else instruction trans_state' stmt in
            stmt_res_trans :: rest_stmts_res_trans

  and lambdaExpr_trans trans_state expr_info decl =
    let open CContext in
    let type_ptr = expr_info.Clang_ast_t.ei_type_ptr in
    let context = trans_state.context in
    call_translation context decl;
    let procname = Procdesc.get_proc_name context.procdesc in
    let lambda_pname = CMethod_trans.get_procname_from_cpp_lambda context decl in
    let typ = CType_decl.type_ptr_to_sil_type context.tenv type_ptr in
    (* We need to set the explicit dependency between the newly created lambda and the *)
    (* defining procedure. We add an edge in the call graph.*)
    Cg.add_edge context.cg procname lambda_pname;
    let captured_vars = [] in  (* TODO *)
    let closure = Exp.Closure { name = lambda_pname; captured_vars } in
    { empty_res_trans with exps = [(closure, typ)] }

  and cxxNewExpr_trans trans_state stmt_info expr_info cxx_new_expr_info =
    let context = trans_state.context in
    let typ = CType_decl.get_type_from_expr_info expr_info context.tenv in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let is_dyn_array = cxx_new_expr_info.Clang_ast_t.xnei_is_array in
    let size_exp_opt, res_trans_size =
      if is_dyn_array then
        match CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_array_size_expr with
        | Some stmt ->
            let trans_state_size = { trans_state_pri with succ_nodes = []; } in
            let res_trans_size = instruction trans_state_size stmt in
            (match res_trans_size.exps with
             | [(exp, _)] -> Some exp, res_trans_size
             | _ -> None, empty_res_trans)
        | None -> Some (Exp.Const (Const.Cint (IntLit.minus_one))), empty_res_trans
      else None, empty_res_trans in
    let res_trans_new = cpp_new_trans sil_loc typ size_exp_opt in
    let stmt_opt = CAst_utils.get_stmt_opt cxx_new_expr_info.Clang_ast_t.xnei_initializer_expr in
    let trans_state_init = { trans_state_pri with succ_nodes = []; } in
    let var_exp_typ = match res_trans_new.exps with
      | [var_exp, Typ.Tptr (t, _)] -> (var_exp, t)
      | _ -> assert false in
    (* Need a new stmt_info for the translation of the initializer, so that it can create nodes *)
    (* if it needs to, with the same stmt_info it doesn't work. *)
    let init_stmt_info = { stmt_info with
                           Clang_ast_t.si_pointer = CAst_utils.get_fresh_pointer () } in
    let res_trans_init =
      if is_dyn_array && Typ.is_pointer_to_cpp_class typ then
        let rec create_stmts stmt_opt size_exp_opt =
          match stmt_opt, size_exp_opt with
          | Some stmt, Some (Exp.Const (Const.Cint n)) when not (IntLit.iszero n) ->
              let n_minus_1 = Some ((Exp.Const (Const.Cint (IntLit.sub n IntLit.one)))) in
              stmt :: create_stmts stmt_opt n_minus_1
          | _ -> [] in
        let stmts = create_stmts stmt_opt size_exp_opt in
        let (var_exp, typ) = var_exp_typ in
        let res_trans_init_list = initListExpr_initializers_trans trans_state_init var_exp 0 stmts
            typ is_dyn_array stmt_info in
        CTrans_utils.collect_res_trans context.procdesc res_trans_init_list
      else init_expr_trans trans_state_init var_exp_typ init_stmt_info stmt_opt in
    let all_res_trans = [res_trans_size; res_trans_new; res_trans_init] in
    let nname = "CXXNewExpr" in
    let result_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
        nname stmt_info all_res_trans in
    { result_trans_to_parent with exps = res_trans_new.exps }

  and cxxDeleteExpr_trans trans_state stmt_info stmt_list delete_expr_info =
    let context = trans_state.context in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let is_array = delete_expr_info.Clang_ast_t.xdei_is_array in
    let fname =
      if is_array then BuiltinDecl.__delete_array
      else BuiltinDecl.__delete in
    let param = match stmt_list with [p] -> p | _ -> assert false in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = { trans_state_pri with succ_nodes = [] } in
    let result_trans_param = exec_with_self_exception instruction trans_state_param param in
    let exp = extract_exp_from_list result_trans_param.exps
        "WARNING: There should be one expression to delete. \n" in
    let call_instr =
      Sil.Call (None, Exp.Const (Const.Cfun fname), [exp], sil_loc, CallFlags.default) in
    let call_res_trans = { empty_res_trans with instrs = [call_instr] } in
    let all_res_trans = if false then
        (* FIXME (t10135167): call destructor on deleted pointer if it's not null *)
        (* Right now it's dead code hidden by the 'false' flag *)
        let deleted_type = delete_expr_info.Clang_ast_t.xdei_destroyed_type in
        (* create stmt_info with new pointer so that destructor call doesn't create a node *)
        let destruct_stmt_info = { stmt_info with
                                   Clang_ast_t.si_pointer = CAst_utils.get_fresh_pointer () } in
        (* use empty_res_trans to avoid ending up with same instruction twice *)
        (* otherwise it would happen due to structutre of all_res_trans *)
        let this_res_trans_destruct = { empty_res_trans with exps = result_trans_param.exps } in
        let destruct_res_trans = cxx_destructor_call_trans trans_state_pri destruct_stmt_info
            this_res_trans_destruct deleted_type in
        [ result_trans_param; destruct_res_trans; call_res_trans]
        (* --- END OF DEAD CODE --- *)
      else
        [ result_trans_param; call_res_trans] in

    let res_trans = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
        "Call delete" stmt_info all_res_trans in
    { res_trans with exps = [] }

  and materializeTemporaryExpr_trans trans_state stmt_info stmt_list expr_info =
    let context = trans_state.context in
    let procdesc = context.CContext.procdesc in
    let (pvar, typ) = mk_temp_sil_var_for_expr context.CContext.tenv procdesc
        "SIL_materialize_temp__" expr_info in
    let temp_exp = match stmt_list with [p] -> p | _ -> assert false in
    Procdesc.append_locals procdesc [(Pvar.get_name pvar, typ)];
    let var_exp_typ = (Exp.Lvar pvar, typ) in
    let res_trans = init_expr_trans trans_state var_exp_typ stmt_info (Some temp_exp) in
    { res_trans with exps = [var_exp_typ] }

  and compoundLiteralExpr_trans trans_state stmt_list expr_info =
    let stmt = match stmt_list with [stmt] -> stmt | _ -> assert false in
    let var_exp_typ =
      if Option.is_some trans_state.var_exp_typ then trans_state.var_exp_typ
      else
        Some (create_var_exp_tmp_var trans_state expr_info "SIL_compound_literal__") in
    let trans_state' = { trans_state with var_exp_typ = var_exp_typ } in
    instruction trans_state' stmt

  and cxxDynamicCastExpr_trans trans_state stmt_info stmts cast_type_ptr =
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state' = { trans_state_pri with succ_nodes = [] } in
    let context = trans_state.context in
    let subtypes = Subtype.subtypes_cast in
    let tenv = context.CContext.tenv in
    let sil_loc = CLocation.get_sil_location stmt_info context in
    let cast_type = CType_decl.type_ptr_to_sil_type tenv cast_type_ptr in
    let sizeof_expr = match cast_type with
      | Typ.Tptr (typ, _) -> Exp.Sizeof (typ, None, subtypes)
      | _ -> assert false in
    let builtin = Exp.Const (Const.Cfun BuiltinDecl.__cast) in
    let stmt = match stmts with [stmt] -> stmt | _ -> assert false in
    let res_trans_stmt = exec_with_glvalue_as_reference instruction trans_state' stmt in
    let exp = match res_trans_stmt.exps with | [e] -> e | _ -> assert false in
    let args = [exp; (sizeof_expr, Typ.Tvoid)] in
    let ret_id = Ident.create_fresh Ident.knormal in
    let call = Sil.Call (Some (ret_id, cast_type), builtin, args, sil_loc, CallFlags.default) in
    let res_ex = Exp.Var ret_id in
    let res_trans_dynamic_cast = { empty_res_trans with instrs = [call]; } in
    let all_res_trans = [ res_trans_stmt; res_trans_dynamic_cast ] in
    let nname = "CxxDynamicCast" in
    let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc nname
        stmt_info all_res_trans in
    { res_trans_to_parent with exps = [(res_ex, cast_type)] }

  and cxxDefaultExpr_trans trans_state default_expr_info =
    match default_expr_info.Clang_ast_t.xdaei_init_expr with
    | Some exp -> instruction trans_state exp
    | None -> assert false

  and call_function_with_args instr_name pname trans_state stmt_info stmts =
    let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = { trans_state_pri with succ_nodes = [] } in
    let res_trans_subexpr_list =
      List.map ~f:(exec_with_glvalue_as_reference instruction trans_state_param) stmts in
    let params = collect_exprs res_trans_subexpr_list  in
    let sil_fun = Exp.Const (Const.Cfun pname) in
    let call_instr = Sil.Call (None, sil_fun, params, sil_loc, CallFlags.default) in
    let res_trans_call = { empty_res_trans with
                           instrs = [call_instr];
                           exps = []; } in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
        instr_name stmt_info all_res_trans in
    { res_trans_to_parent with exps = res_trans_call.exps }

  and gccAsmStmt_trans trans_state =
    let pname = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_gcc_asm_stmt in
    call_function_with_args "GCCAsmStmt" pname trans_state
  and objc_cxx_throw_trans trans_state =
    call_function_with_args "ObjCCPPThrow" BuiltinDecl.objc_cpp_throw trans_state

  and cxxPseudoDestructorExpr_trans () =
    let fun_name = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_fun in
    { empty_res_trans with exps = [(Exp.Const (Const.Cfun fun_name), Typ.Tvoid)] }

  and cxxTypeidExpr_trans trans_state stmt_info stmts expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let res_trans_subexpr =
      match stmts with
      | [stmt] ->
          let trans_state_param = { trans_state_pri with succ_nodes = [] } in
          instruction trans_state_param stmt
      | _ -> empty_res_trans in
    let fun_name = BuiltinDecl.__cxx_typeid in
    let sil_fun = Exp.Const (Const.Cfun fun_name) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let type_info_objc = (Exp.Sizeof (typ, None, Subtype.exact), Typ.Tvoid) in
    let field_name_decl = CAst_utils.make_qual_name_decl ["type_info"; "std"] "__type_name" in
    let field_name = CGeneral_utils.mk_class_field_name field_name_decl in
    let ret_exp = Exp.Var ret_id in
    let field_exp = Exp.Lfield (ret_exp, field_name, typ) in
    let args = type_info_objc :: (field_exp, Typ.Tvoid) :: res_trans_subexpr.exps in
    let call_instr = Sil.Call (Some (ret_id, typ), sil_fun, args, sil_loc, CallFlags.default) in
    let res_trans_call = { empty_res_trans with
                           instrs = [call_instr];
                           exps = [(ret_exp, typ)]; } in
    let all_res_trans = [res_trans_subexpr; res_trans_call] in
    let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
        "CXXTypeidExpr" stmt_info all_res_trans in
    { res_trans_to_parent with exps = res_trans_call.exps }

  and cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info =
    let context = trans_state.context in
    let tenv = context.CContext.tenv in
    let sil_loc = CLocation.get_sil_location stmt_info trans_state.context in
    let type_pointer = expr_info.Clang_ast_t.ei_type_ptr in
    let typ = CType_decl.type_ptr_to_sil_type tenv type_pointer in
    let fun_name = Typ.Procname.from_string_c_fun CFrontend_config.infer_skip_fun in
    let trans_state_pri = PriorityNode.try_claim_priority_node trans_state stmt_info in
    let trans_state_param = { trans_state_pri with succ_nodes = [] } in
    let res_trans_subexpr_list = List.map ~f:(instruction trans_state_param) stmts in
    let params = collect_exprs res_trans_subexpr_list  in
    let sil_fun = Exp.Const (Const.Cfun fun_name) in
    let ret_id = Ident.create_fresh Ident.knormal in
    let ret_exp = Exp.Var ret_id in
    let call_instr = Sil.Call (Some (ret_id, typ), sil_fun, params, sil_loc, CallFlags.default) in
    let res_trans_call = { empty_res_trans with
                           instrs = [call_instr];
                           exps = [(ret_exp, typ)]; } in
    let all_res_trans = res_trans_subexpr_list @ [res_trans_call] in
    let res_trans_to_parent = PriorityNode.compute_results_to_parent trans_state_pri sil_loc
        "CXXStdInitializerListExpr" stmt_info all_res_trans in
    { res_trans_to_parent with exps = res_trans_call.exps }

  and objCBridgedCastExpr_trans trans_state stmts expr_info =
    let stmt = extract_stmt_from_singleton stmts "" in
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    let trans_state' = { trans_state with obj_bridged_cast_typ = Some typ } in
    instruction trans_state' stmt

  and binaryOperator_trans_shortc trans_state stmt_info stmt_list expr_info binary_operator_info =
    let open Clang_ast_t in
    match binary_operator_info.boi_kind with
    | `LAnd | `LOr  ->
        (* For LAnd/LOr we compiles a binary expression bo into an semantic equivalent
           conditional operator 'bo ? 1:0'.
           The conditional operator takes care of shortcircuit when/where needed *)
        let bo = BinaryOperator (stmt_info, stmt_list, expr_info, binary_operator_info) in
        let stmt_list' =
          [bo; Ast_expressions.create_integer_literal "1"; Ast_expressions.create_integer_literal "0"] in
        conditionalOperator_trans trans_state stmt_info stmt_list' expr_info
    | _  -> binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list

  and attributedStmt_trans trans_state stmts attrs =
    let open Clang_ast_t in
    match stmts, attrs with
    | [stmt], [attr] ->
        (match stmt, attr with
         | NullStmt _, FallThroughAttr _ -> nullStmt_trans trans_state.succ_nodes
         | _ -> assert false (* More cases to come. With the assert false we can find them *) )
    | _ -> assert false (* Expect that this doesn't happen *)

  and trans_into_undefined_expr trans_state expr_info =
    let tenv = trans_state.context.CContext.tenv in
    let typ = CType_decl.get_type_from_expr_info expr_info tenv in
    { empty_res_trans with exps = [CTrans_utils.undefined_expression (), typ] }

  (* Translates a clang instruction into SIL instructions. It takes a       *)
  (* a trans_state containing current info on the translation and it returns *)
  (* a result_state.*)
  and instruction trans_state instr =
    let stmt_kind = Clang_ast_proj.get_stmt_kind_string instr in
    let stmt_info, _ = Clang_ast_proj.get_stmt_tuple instr in
    let stmt_pointer = stmt_info.Clang_ast_t.si_pointer in
    Logging.out_debug "\nPassing from %s '%d' \n" stmt_kind stmt_pointer;
    let open Clang_ast_t in
    match instr with
    | GotoStmt(stmt_info, _, { Clang_ast_t.gsi_label = label_name; _ }) ->
        gotoStmt_trans trans_state stmt_info label_name

    | LabelStmt(stmt_info, stmt_list, label_name) ->
        labelStmt_trans trans_state stmt_info stmt_list label_name

    | ArraySubscriptExpr(_, stmt_list, expr_info) ->
        arraySubscriptExpr_trans trans_state expr_info stmt_list

    | BinaryOperator (stmt_info, stmt_list, expr_info, binary_operator_info) ->
        binaryOperator_trans_shortc trans_state stmt_info stmt_list expr_info binary_operator_info

    | CallExpr(stmt_info, stmt_list, ei) ->
        (match is_dispatch_function stmt_list with
         | Some block_arg_pos ->
             dispatch_function_trans trans_state stmt_info stmt_list block_arg_pos
         | None ->
             callExpr_trans trans_state stmt_info stmt_list ei)

    | CXXMemberCallExpr(stmt_info, stmt_list, ei) ->
        cxxMemberCallExpr_trans trans_state stmt_info stmt_list ei

    | CXXOperatorCallExpr(stmt_info, stmt_list, ei) ->
        callExpr_trans trans_state stmt_info stmt_list ei

    | CXXConstructExpr (stmt_info, stmt_list, expr_info, cxx_constr_info)
    | CXXTemporaryObjectExpr (stmt_info, stmt_list, expr_info, cxx_constr_info) ->
        cxxConstructExpr_trans trans_state stmt_info stmt_list expr_info cxx_constr_info

    | ObjCMessageExpr(stmt_info, stmt_list, expr_info, obj_c_message_expr_info) ->
        if is_block_enumerate_function obj_c_message_expr_info then
          block_enumeration_trans trans_state stmt_info stmt_list expr_info
        else
          objCMessageExpr_trans trans_state stmt_info obj_c_message_expr_info stmt_list expr_info

    | CompoundStmt (_, stmt_list) ->
        (* No node for this statement. We just collect its statement list*)
        compoundStmt_trans trans_state stmt_list

    | ConditionalOperator(stmt_info, stmt_list, expr_info) ->
        (* Ternary operator "cond ? exp1 : exp2" *)
        conditionalOperator_trans trans_state stmt_info stmt_list expr_info

    | IfStmt(stmt_info, stmt_list) ->
        ifStmt_trans trans_state stmt_info stmt_list

    | SwitchStmt (stmt_info, switch_stmt_list) ->
        switchStmt_trans trans_state stmt_info switch_stmt_list

    | CaseStmt _ ->
        Logging.out_debug
          "FATAL: Passing from CaseStmt outside of SwitchStmt, terminating.\n";
        assert false

    | StmtExpr(_, stmt_list, _) ->
        stmtExpr_trans trans_state stmt_list

    | ForStmt(stmt_info, [init; decl_stmt; cond; incr; body]) ->
        forStmt_trans trans_state init decl_stmt cond incr body stmt_info

    | WhileStmt(stmt_info, [decl_stmt; cond; body]) ->
        whileStmt_trans trans_state decl_stmt cond body stmt_info

    | DoStmt(stmt_info, [body; cond]) ->
        doStmt_trans trans_state stmt_info cond body

    | CXXForRangeStmt (stmt_info, stmt_list) ->
        cxxForRangeStmt_trans trans_state stmt_info stmt_list

    | ObjCForCollectionStmt(stmt_info, [item; items; body]) ->
        objCForCollectionStmt_trans trans_state item items body stmt_info

    | NullStmt _ ->
        nullStmt_trans trans_state.succ_nodes

    | CompoundAssignOperator (stmt_info, stmt_list, expr_info, binary_operator_info, _) ->
        binaryOperator_trans trans_state binary_operator_info stmt_info expr_info stmt_list

    | DeclStmt(stmt_info, _, decl_list) ->
        declStmt_trans trans_state decl_list stmt_info

    | DeclRefExpr(stmt_info, _, _, decl_ref_expr_info) as d ->
        declRefExpr_trans trans_state stmt_info decl_ref_expr_info d

    | ObjCPropertyRefExpr(_, stmt_list, _, _) ->
        objCPropertyRefExpr_trans trans_state stmt_list

    | CXXThisExpr(stmt_info, _, expr_info) -> cxxThisExpr_trans trans_state stmt_info expr_info

    | OpaqueValueExpr(_, _, _, opaque_value_expr_info) ->
        opaqueValueExpr_trans trans_state opaque_value_expr_info

    | PseudoObjectExpr(_, stmt_list, _) ->
        pseudoObjectExpr_trans trans_state stmt_list

    | UnaryExprOrTypeTraitExpr(_, _, expr_info, ei) ->
        unaryExprOrTypeTraitExpr_trans trans_state expr_info ei

    | ObjCBridgedCastExpr(_, stmt_list, expr_info, _, _) ->
        objCBridgedCastExpr_trans trans_state stmt_list expr_info

    | ImplicitCastExpr(stmt_info, stmt_list, expr_info, cast_kind)
    | CStyleCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _)
    | CXXReinterpretCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXConstCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXStaticCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _, _)
    | CXXFunctionalCastExpr(stmt_info, stmt_list, expr_info, cast_kind, _)->
        cast_exprs_trans trans_state stmt_info stmt_list expr_info cast_kind

    | IntegerLiteral(_, _, expr_info, integer_literal_info) ->
        integerLiteral_trans trans_state expr_info integer_literal_info

    | StringLiteral(_, _, expr_info, str) ->
        stringLiteral_trans trans_state expr_info str

    | GNUNullExpr(_, _, expr_info) ->
        gNUNullExpr_trans trans_state expr_info

    | CXXNullPtrLiteralExpr(_, _, expr_info) ->
        nullPtrExpr_trans trans_state expr_info

    | ObjCSelectorExpr(_, _, expr_info, selector) ->
        objCSelectorExpr_trans trans_state expr_info selector

    | ObjCEncodeExpr(_, _, expr_info, objc_encode_expr_info) ->
        objCEncodeExpr_trans trans_state expr_info objc_encode_expr_info

    | ObjCProtocolExpr(_, _, expr_info, decl_ref) ->
        objCProtocolExpr_trans trans_state expr_info decl_ref

    | ObjCIvarRefExpr(stmt_info, stmt_list, _, obj_c_ivar_ref_expr_info) ->
        objCIvarRefExpr_trans trans_state stmt_info stmt_list obj_c_ivar_ref_expr_info

    | MemberExpr(stmt_info, stmt_list, _, member_expr_info) ->
        memberExpr_trans trans_state stmt_info stmt_list member_expr_info

    | UnaryOperator(stmt_info, stmt_list, expr_info, unary_operator_info) ->
        if is_logical_negation_of_int
            trans_state.context.CContext.tenv expr_info unary_operator_info then
          let conditional =
            Ast_expressions.trans_negation_with_conditional stmt_info expr_info stmt_list in
          instruction trans_state conditional
        else
          unaryOperator_trans trans_state stmt_info expr_info stmt_list unary_operator_info

    | ReturnStmt (stmt_info, stmt_list) ->
        returnStmt_trans trans_state stmt_info stmt_list

    (* We analyze the content of the expr. We treat ExprWithCleanups as a wrapper. *)
    (*  It may be that later on (when we treat ARC) some info can be taken from it. *)
    | ExprWithCleanups(_, stmt_list, _, _)
    | ParenExpr(_, stmt_list, _) ->
        parenExpr_trans trans_state stmt_list

    | ObjCBoolLiteralExpr (_, _, expr_info, n)
    | CharacterLiteral (_, _, expr_info, n)
    | CXXBoolLiteralExpr (_, _, expr_info, n) ->
        characterLiteral_trans trans_state expr_info n

    | FloatingLiteral (_, _, expr_info, float_string) ->
        floatingLiteral_trans trans_state expr_info float_string

    | CXXScalarValueInitExpr (_, _, expr_info) ->
        cxxScalarValueInitExpr_trans trans_state expr_info

    | ObjCBoxedExpr (stmt_info, stmts, info, boxed_expr_info) ->
        (match boxed_expr_info.Clang_ast_t.obei_boxing_method with
         | Some sel ->
             objCBoxedExpr_trans trans_state info sel stmt_info stmts
         | None -> assert false)

    | ObjCArrayLiteral (stmt_info, stmts, info) ->
        objCArrayLiteral_trans trans_state info stmt_info stmts

    | ObjCDictionaryLiteral (stmt_info, stmts, info) ->
        objCDictionaryLiteral_trans trans_state info stmt_info stmts

    | ObjCStringLiteral(stmt_info, stmts, info) ->
        objCStringLiteral_trans trans_state stmt_info stmts info

    | BreakStmt _ -> breakStmt_trans trans_state

    | ContinueStmt _ -> continueStmt_trans trans_state

    | ObjCAtSynchronizedStmt(_, stmt_list) ->
        objCAtSynchronizedStmt_trans trans_state stmt_list

    | ObjCIndirectCopyRestoreExpr (_, stmt_list, _) ->
        instructions trans_state stmt_list

    | BlockExpr(stmt_info, _ , expr_info, decl) ->
        blockExpr_trans trans_state stmt_info expr_info decl

    | ObjCAutoreleasePoolStmt (stmt_info, stmts) ->
        objcAutoreleasePool_trans trans_state stmt_info stmts

    | ObjCAtTryStmt (_, stmts) ->
        compoundStmt_trans trans_state stmts
    | CXXTryStmt (_, stmts) ->
        (Logging.out
           "\n!!!!WARNING: found statement %s. \nTranslation need to be improved.... \n"
           (Clang_ast_proj.get_stmt_kind_string instr);
         compoundStmt_trans trans_state stmts)

    | ObjCAtThrowStmt (stmt_info, stmts)
    | CXXThrowExpr (stmt_info, stmts, _) ->
        objc_cxx_throw_trans trans_state stmt_info stmts

    | ObjCAtFinallyStmt (_, stmts) ->
        compoundStmt_trans trans_state stmts

    | ObjCAtCatchStmt _
    | CXXCatchStmt _ ->
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
    | CompoundLiteralExpr (_, stmt_list, expr_info) ->
        compoundLiteralExpr_trans trans_state stmt_list expr_info
    | InitListExpr (stmt_info, stmts, expr_info) ->
        initListExpr_trans trans_state stmt_info expr_info stmts

    | CXXBindTemporaryExpr (_, stmt_list, _, _) ->
        (* right now we ignore this expression and try to translate the child node *)
        parenExpr_trans trans_state stmt_list

    | CXXDynamicCastExpr (stmt_info, stmts, _, _, type_ptr, _) ->
        cxxDynamicCastExpr_trans trans_state stmt_info stmts type_ptr

    | CXXDefaultArgExpr (_, _, _, default_expr_info)
    | CXXDefaultInitExpr (_, _, _, default_expr_info) ->
        cxxDefaultExpr_trans trans_state default_expr_info

    | ImplicitValueInitExpr (_, _, expr_info) ->
        implicitValueInitExpr_trans trans_state expr_info
    | GenericSelectionExpr _ (* to be fixed when we dump the right info in the ast *)
    | SizeOfPackExpr _ ->
        { empty_res_trans with exps = [(Exp.get_undefined false, Typ.Tvoid)] }

    | GCCAsmStmt (stmt_info, stmts) ->
        gccAsmStmt_trans trans_state stmt_info stmts

    | CXXPseudoDestructorExpr _ ->
        cxxPseudoDestructorExpr_trans ()

    | CXXTypeidExpr (stmt_info, stmts, expr_info) ->
        cxxTypeidExpr_trans trans_state stmt_info stmts  expr_info

    | CXXStdInitializerListExpr (stmt_info, stmts, expr_info) ->
        cxxStdInitializerListExpr_trans trans_state stmt_info stmts expr_info

    | LambdaExpr(_, _, expr_info, lambda_expr_info) ->
        let trans_state' = { trans_state with priority = Free } in
        let decl = lambda_expr_info.Clang_ast_t.lei_lambda_decl in
        lambdaExpr_trans trans_state' expr_info decl

    | AttributedStmt (_, stmts, attrs) ->
        attributedStmt_trans trans_state stmts attrs

    | TypeTraitExpr (_, _, expr_info, type_trait_info) ->
        booleanValue_trans trans_state expr_info type_trait_info.Clang_ast_t.xtti_value

    | CXXNoexceptExpr (_, _, expr_info, cxx_noexcept_expr_info) ->
        booleanValue_trans trans_state expr_info cxx_noexcept_expr_info.Clang_ast_t.xnee_value

    | OffsetOfExpr (_, _, expr_info)
    | VAArgExpr (_, _, expr_info) ->
        trans_into_undefined_expr trans_state expr_info


    (* Infer somehow ended up in templated non instantiated code - right now
       it's not supported and failure in those cases is expected. *)
    | SubstNonTypeTemplateParmExpr _
    | SubstNonTypeTemplateParmPackExpr _
    | CXXDependentScopeMemberExpr _ -> raise (CTrans_utils.TemplatedCodeException instr)

    | s -> (Logging.out
              "\n!!!!WARNING: found statement %s. \nACTION REQUIRED: \
               Translation need to be defined. Statement ignored.... \n"
              (Clang_ast_proj.get_stmt_kind_string s);
            assert false)

  (* Function similar to instruction function, but it takes C++ constructor initializer as
     an input parameter. *)
  and cxx_constructor_init_trans ctor_init trans_state =
    let context = trans_state.context in
    let class_ptr = CContext.get_curr_class_decl_ptr context.CContext.curr_class in
    let source_range = ctor_init.Clang_ast_t.xci_source_range in
    let sil_loc = CLocation.get_sil_location_from_range context.CContext.translation_unit_context
        source_range true in
    (* its pointer will be used in PriorityNode *)
    let this_stmt_info = Ast_expressions.dummy_stmt_info () in
    (* this will be used to avoid creating node in init_expr_trans *)
    let child_stmt_info =
      { (Ast_expressions.dummy_stmt_info ()) with Clang_ast_t.si_source_range = source_range } in
    let trans_state' = PriorityNode.try_claim_priority_node trans_state this_stmt_info in
    let class_type_ptr = Ast_expressions.create_pointer_type (`DeclPtr class_ptr) in
    let this_res_trans = this_expr_trans trans_state' sil_loc class_type_ptr in
    let var_res_trans = match ctor_init.Clang_ast_t.xci_subject with
      | `Delegating _ | `BaseClass _ ->
          let this_exp, this_typ = extract_exp_from_list this_res_trans.exps
              "WARNING: There should be one expression for 'this' in constructor. \n" in
          (* Hack: Strip pointer from type here since cxxConstructExpr_trans expects it this way *)
          (* it will add pointer back before making it a parameter to a call *)
          let class_typ = match this_typ with Typ.Tptr (t, _) -> t | _ -> assert false in
          { this_res_trans with exps = [this_exp, class_typ] }
      | `Member (decl_ref) ->
          decl_ref_trans trans_state' this_res_trans child_stmt_info decl_ref
            ~is_constructor_init:true in
    let var_exp_typ = extract_exp_from_list var_res_trans.exps
        "WARNING: There should be one expression to initialize in constructor initializer. \n" in
    let init_expr = ctor_init.Clang_ast_t.xci_init_expr in
    let init_res_trans = init_expr_trans trans_state' var_exp_typ child_stmt_info init_expr in
    PriorityNode.compute_results_to_parent trans_state' sil_loc "Constructor Init"
      this_stmt_info [var_res_trans; init_res_trans]

  (** Given a translation state and list of translation functions it executes translation *)
  and exec_trans_instrs trans_state trans_stmt_fun_list =
    let rec exec_trans_instrs_no_rev trans_state rev_trans_fun_list = match rev_trans_fun_list with
      | [] -> { empty_res_trans with root_nodes = trans_state.succ_nodes }
      | trans_stmt_fun :: trans_stmt_fun_list' ->
          let res_trans_s = trans_stmt_fun trans_state in
          let trans_state' =
            if res_trans_s.root_nodes <> []
            then { trans_state with succ_nodes = res_trans_s.root_nodes }
            else trans_state in
          let res_trans_tail = exec_trans_instrs_no_rev trans_state' trans_stmt_fun_list' in
          { empty_res_trans with
            root_nodes = res_trans_tail.root_nodes;
            leaf_nodes = [];
            instrs = res_trans_tail.instrs @ res_trans_s.instrs;
            exps = res_trans_tail.exps @ res_trans_s.exps;
            initd_exps = res_trans_tail.initd_exps @ res_trans_s.initd_exps;
          } in
    exec_trans_instrs_no_rev trans_state (List.rev trans_stmt_fun_list)

  and get_clang_stmt_trans stmt =
    fun trans_state -> exec_with_node_creation instruction trans_state stmt

  (* TODO write translate function for cxx constructor exprs *)
  and get_custom_stmt_trans stmt = match stmt with
    | `ClangStmt stmt -> get_clang_stmt_trans stmt
    | `CXXConstructorInit instr -> cxx_constructor_init_trans instr

  (** Given a translation state, this function translates a list of clang statements. *)
  and instructions trans_state stmt_list =
    let stmt_trans_fun = List.map ~f:get_clang_stmt_trans stmt_list in
    exec_trans_instrs trans_state stmt_trans_fun

  and expression_trans context stmt warning =
    let trans_state = {
      context = context;
      succ_nodes = [];
      continuation = None;
      priority = Free;
      var_exp_typ = None;
      opaque_exp = None;
      obj_bridged_cast_typ = None;
    } in
    let res_trans_stmt = instruction trans_state stmt in
    fst (CTrans_utils.extract_exp_from_list res_trans_stmt.exps warning)

  let instructions_trans context body extra_instrs exit_node =
    let trans_state = {
      context = context;
      succ_nodes = [exit_node];
      continuation = None;
      priority = Free;
      var_exp_typ = None;
      opaque_exp = None;
      obj_bridged_cast_typ = None
    } in
    let instrs = extra_instrs @ [`ClangStmt body] in
    let instrs_trans = List.map ~f:get_custom_stmt_trans instrs in
    let res_trans = exec_trans_instrs trans_state instrs_trans in
    res_trans.root_nodes

end
