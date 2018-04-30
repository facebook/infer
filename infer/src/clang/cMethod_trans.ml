(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Methods for creating a procdesc from a method or function declaration
    and for resolving a method call and finding the right callee *)

module L = Logging

exception Invalid_declaration

(** When the methoc call is MCStatic, means that it is a class method.  When it is MCVirtual, it
    means that it is an instance method and that the method to be called will be determined at
    runtime. If it is MCNoVirtual it means that it is an instance method but that the method to be
    called will be determined at compile time *)
type method_call_type = MCVirtual | MCNoVirtual | MCStatic [@@deriving compare]

let equal_method_call_type = [%compare.equal : method_call_type]

type function_method_decl_info =
  | Func_decl_info of Clang_ast_t.function_decl_info * Clang_ast_t.qual_type
  | Cpp_Meth_decl_info of
      Clang_ast_t.function_decl_info
      * Clang_ast_t.cxx_method_decl_info
      * Clang_ast_t.pointer
      * Clang_ast_t.qual_type
  | ObjC_Meth_decl_info of Clang_ast_t.obj_c_method_decl_info * Clang_ast_t.pointer
  | Block_decl_info of Clang_ast_t.block_decl_info * Clang_ast_t.qual_type * CContext.t

let get_method_kind function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info _ ->
      ProcAttributes.C_FUNCTION
  | Block_decl_info _ ->
      ProcAttributes.BLOCK
  | Cpp_Meth_decl_info (_, method_decl_info, _, _) ->
      if method_decl_info.Clang_ast_t.xmdi_is_static then ProcAttributes.CPP_CLASS
      else ProcAttributes.CPP_INSTANCE
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      if method_decl_info.Clang_ast_t.omdi_is_instance_method then ProcAttributes.OBJC_INSTANCE
      else ProcAttributes.OBJC_CLASS


let get_original_return_type function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, typ) | Cpp_Meth_decl_info (_, _, _, typ) | Block_decl_info (_, typ, _) ->
      CType.return_type_of_function_type typ
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      method_decl_info.Clang_ast_t.omdi_result_type


let get_class_param function_method_decl_info =
  match get_method_kind function_method_decl_info with
  | ProcAttributes.CPP_INSTANCE | ProcAttributes.OBJC_INSTANCE -> (
    match function_method_decl_info with
    | Cpp_Meth_decl_info (_, _, class_decl_ptr, _) ->
        let class_type = CAst_utils.qual_type_of_decl_ptr class_decl_ptr in
        [(Mangled.from_string CFrontend_config.this, class_type)]
    | ObjC_Meth_decl_info (_, class_decl_ptr) ->
        let class_type = CAst_utils.qual_type_of_decl_ptr class_decl_ptr in
        [(Mangled.from_string CFrontend_config.self, class_type)]
    | _ ->
        [] )
  | _ ->
      []


let should_add_return_param return_type ~is_objc_method =
  match return_type.Typ.desc with Tstruct _ -> not is_objc_method | _ -> false


let is_objc_method function_method_decl_info =
  match function_method_decl_info with ObjC_Meth_decl_info _ -> true | _ -> false


let get_return_param tenv function_method_decl_info =
  let is_objc_method = is_objc_method function_method_decl_info in
  let return_qual_type = get_original_return_type function_method_decl_info in
  let return_typ = CType_decl.qual_type_to_sil_type tenv return_qual_type in
  if should_add_return_param return_typ ~is_objc_method then
    [ ( Mangled.from_string CFrontend_config.return_param
      , Ast_expressions.create_pointer_qual_type return_qual_type ) ]
  else []


let get_param_decls function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (function_decl_info, _) | Cpp_Meth_decl_info (function_decl_info, _, _, _) ->
      function_decl_info.Clang_ast_t.fdi_parameters
  | ObjC_Meth_decl_info (method_decl_info, _) ->
      method_decl_info.Clang_ast_t.omdi_parameters
  | Block_decl_info (block_decl_info, _, _) ->
      block_decl_info.Clang_ast_t.bdi_parameters


let get_language trans_unit_ctx function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (_, _) ->
      trans_unit_ctx.CFrontend_config.lang
  | Cpp_Meth_decl_info _ ->
      CFrontend_config.CPP
  | ObjC_Meth_decl_info _ ->
      CFrontend_config.ObjC
  | Block_decl_info _ ->
      CFrontend_config.ObjC


let is_cpp_virtual function_method_decl_info =
  match function_method_decl_info with
  | Cpp_Meth_decl_info (_, mdi, _, _) ->
      mdi.Clang_ast_t.xmdi_is_virtual
  | _ ->
      false


let is_cpp_nothrow function_method_decl_info =
  match function_method_decl_info with
  | Func_decl_info (fdi, _) | Cpp_Meth_decl_info (fdi, _, _, _) ->
      fdi.Clang_ast_t.fdi_is_no_throw
  | _ ->
      false


(** Returns parameters of a function/method. They will have following order:
    1. self/this parameter (optional, only for methods)
    2. normal parameters
    3. return parameter (optional) *)
let get_parameters trans_unit_ctx tenv decl_info function_method_decl_info =
  let par_to_ms_par par =
    match par with
    | Clang_ast_t.ParmVarDecl (_, name_info, qt, var_decl_info) ->
        let _, mangled = CGeneral_utils.get_var_name_mangled decl_info name_info var_decl_info in
        let param_typ = CType_decl.qual_type_to_sil_type tenv qt in
        let new_qt =
          match param_typ.Typ.desc with
          | Tstruct _ when CGeneral_utils.is_cpp_translation trans_unit_ctx ->
              Ast_expressions.create_reference_qual_type qt
          | _ ->
              qt
        in
        (mangled, new_qt)
    | _ ->
        assert false
  in
  let pars = List.map ~f:par_to_ms_par (get_param_decls function_method_decl_info) in
  get_class_param function_method_decl_info @ pars
  @ get_return_param tenv function_method_decl_info


(** get return type of the function and optionally type of function's return parameter *)
let get_return_val_and_param_types tenv function_method_decl_info =
  let return_qual_type = get_original_return_type function_method_decl_info in
  let return_typ = CType_decl.qual_type_to_sil_type tenv return_qual_type in
  let is_objc_method = is_objc_method function_method_decl_info in
  if should_add_return_param return_typ ~is_objc_method then
    (Ast_expressions.create_void_type, Some (CType.add_pointer_to_typ return_typ))
  else (return_qual_type, None)


let build_method_signature trans_unit_ctx tenv decl_info procname function_method_decl_info
    parent_pointer pointer_to_property_opt =
  let source_range = decl_info.Clang_ast_t.di_source_range in
  let tp, return_param_type_opt = get_return_val_and_param_types tenv function_method_decl_info in
  let method_kind = get_method_kind function_method_decl_info in
  let parameters = get_parameters trans_unit_ctx tenv decl_info function_method_decl_info in
  let attributes = decl_info.Clang_ast_t.di_attributes in
  let lang = get_language trans_unit_ctx function_method_decl_info in
  let is_cpp_virtual = is_cpp_virtual function_method_decl_info in
  let is_cpp_nothrow = is_cpp_nothrow function_method_decl_info in
  let access = decl_info.Clang_ast_t.di_access in
  CMethodSignature.mk procname parameters tp attributes source_range method_kind ~is_cpp_virtual
    ~is_cpp_nothrow lang parent_pointer pointer_to_property_opt return_param_type_opt access


let get_init_list_instrs method_decl_info =
  let create_custom_instr construct_instr = `CXXConstructorInit construct_instr in
  List.map ~f:create_custom_instr method_decl_info.Clang_ast_t.xmdi_cxx_ctor_initializers


let method_signature_of_decl trans_unit_ctx tenv meth_decl block_data_opt =
  let open Clang_ast_t in
  let is_cpp = CGeneral_utils.is_cpp_translation trans_unit_ctx in
  match (meth_decl, block_data_opt) with
  | FunctionDecl (decl_info, _, qt, fdi), _ ->
      let func_decl = Func_decl_info (fdi, qt) in
      let procname = CProcname.from_decl ~is_cpp ~tenv meth_decl in
      let ms = build_method_signature trans_unit_ctx tenv decl_info procname func_decl None None in
      (ms, fdi.Clang_ast_t.fdi_body, [])
  | CXXMethodDecl (decl_info, _, qt, fdi, mdi), _
  | CXXConstructorDecl (decl_info, _, qt, fdi, mdi), _
  | CXXConversionDecl (decl_info, _, qt, fdi, mdi), _
  | CXXDestructorDecl (decl_info, _, qt, fdi, mdi), _ ->
      let procname = CProcname.from_decl ~is_cpp ~tenv meth_decl in
      let parent_ptr = Option.value_exn decl_info.di_parent_pointer in
      let method_decl = Cpp_Meth_decl_info (fdi, mdi, parent_ptr, qt) in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let ms =
        build_method_signature trans_unit_ctx tenv decl_info procname method_decl parent_pointer
          None
      in
      let init_list_instrs = get_init_list_instrs mdi in
      (* it will be empty for methods *)
      (ms, fdi.Clang_ast_t.fdi_body, init_list_instrs)
  | ObjCMethodDecl (decl_info, _, mdi), _ ->
      let procname = CProcname.from_decl ~is_cpp ~tenv meth_decl in
      let parent_ptr = Option.value_exn decl_info.di_parent_pointer in
      let method_decl = ObjC_Meth_decl_info (mdi, parent_ptr) in
      let parent_pointer = decl_info.Clang_ast_t.di_parent_pointer in
      let pointer_to_property_opt =
        match mdi.Clang_ast_t.omdi_property_decl with
        | Some decl_ref ->
            Some decl_ref.Clang_ast_t.dr_decl_pointer
        | None ->
            None
      in
      let ms =
        build_method_signature trans_unit_ctx tenv decl_info procname method_decl parent_pointer
          pointer_to_property_opt
      in
      (ms, mdi.omdi_body, [])
  | BlockDecl (decl_info, bdi), Some (outer_context, tp, procname, _) ->
      let func_decl = Block_decl_info (bdi, tp, outer_context) in
      let ms = build_method_signature trans_unit_ctx tenv decl_info procname func_decl None None in
      (ms, bdi.bdi_body, [])
  | _ ->
      raise Invalid_declaration


let method_signature_of_pointer trans_unit_ctx tenv pointer =
  try
    match CAst_utils.get_decl pointer with
    | Some meth_decl ->
        let ms, _, _ = method_signature_of_decl trans_unit_ctx tenv meth_decl None in
        Some ms
    | None ->
        None
  with Invalid_declaration -> None


let get_method_name_from_clang tenv ms_opt =
  match ms_opt with
  | Some ms -> (
    match CAst_utils.get_decl_opt ms.CMethodSignature.pointer_to_parent with
    | Some decl
      -> (
        ignore (CType_decl.add_types_from_decl_to_tenv tenv decl) ;
        match ObjcCategory_decl.get_base_class_name_from_category decl with
        | Some class_typename ->
            let procname = ms.CMethodSignature.name in
            let new_procname = Typ.Procname.replace_class procname class_typename in
            Some new_procname
        | None ->
            Some ms.CMethodSignature.name )
    | None ->
        Some ms.CMethodSignature.name )
  | None ->
      None


let get_superclass_curr_class_objc context =
  let open Clang_ast_t in
  let decl_ref =
    match CContext.get_curr_class context with
    | CContext.ContextClsDeclPtr ptr -> (
      match CAst_utils.get_decl ptr with
      | Some decl ->
          CAst_utils.get_superclass_curr_class_objc_from_decl decl
      | None ->
          Logging.die InternalError
            "Expected that the current class ptr in the context is a valid pointer to class decl, \
             but didn't find declaration, ptr is %d "
            ptr )
    | CContext.ContextNoCls ->
        Logging.die InternalError
          "This should only be called in the context of a class, but got CContext.ContextNoCls"
  in
  match decl_ref |> Option.value_map ~f:(fun dr -> dr.dr_name) ~default:None with
  | Some name ->
      Typ.Name.Objc.from_qual_name (CAst_utils.get_qualified_name name)
  | None ->
      Logging.die InternalError "Expected to always find a superclass, but found none"


(* Gets the class name from a method signature found by clang, if search is successful *)
let get_class_name_method_call_from_clang trans_unit_ctx tenv obj_c_message_expr_info =
  match obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
  | Some pointer -> (
    match method_signature_of_pointer trans_unit_ctx tenv pointer with
    | Some ms -> (
      match ms.CMethodSignature.name with
      | Typ.Procname.ObjC_Cpp objc_cpp ->
          Some (Typ.Procname.ObjC_Cpp.get_class_type_name objc_cpp)
      | _ ->
          None )
    | None ->
        None )
  | None ->
      None


(* Get class name from a method call accorsing to the info given by the receiver kind  *)
let get_class_name_method_call_from_receiver_kind context obj_c_message_expr_info act_params =
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Class qt ->
      let sil_type = CType_decl.qual_type_to_sil_type context.CContext.tenv qt in
      CType.objc_classname_of_type sil_type
  | `Instance -> (
    match act_params with
    | (_, {Typ.desc= Tptr (t, _)}) :: _ | (_, t) :: _ ->
        CType.objc_classname_of_type t
    | _ ->
        assert false )
  | `SuperInstance ->
      get_superclass_curr_class_objc context
  | `SuperClass ->
      get_superclass_curr_class_objc context


let get_objc_method_data obj_c_message_expr_info =
  let selector = obj_c_message_expr_info.Clang_ast_t.omei_selector in
  let pointer = obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer in
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Instance ->
      (selector, pointer, MCVirtual)
  | `SuperInstance ->
      (selector, pointer, MCNoVirtual)
  | `Class _ | `SuperClass ->
      (selector, pointer, MCStatic)


let get_formal_parameters tenv ms =
  let rec defined_parameters pl =
    match pl with
    | [] ->
        []
    | (mangled, qual_type) :: pl' ->
        let should_add_pointer name ms =
          let is_objc_self =
            String.equal name CFrontend_config.self
            && CFrontend_config.equal_clang_lang ms.CMethodSignature.lang CFrontend_config.ObjC
          in
          let is_cxx_this =
            String.equal name CFrontend_config.this
            && CFrontend_config.equal_clang_lang ms.CMethodSignature.lang CFrontend_config.CPP
          in
          is_objc_self
          && ProcAttributes.equal_clang_method_kind ms.CMethodSignature.method_kind
               ProcAttributes.OBJC_INSTANCE
          || is_cxx_this
        in
        let qt =
          if should_add_pointer (Mangled.to_string mangled) ms then
            Ast_expressions.create_pointer_qual_type qual_type
          else qual_type
        in
        let typ = CType_decl.qual_type_to_sil_type tenv qt in
        (mangled, typ) :: defined_parameters pl'
  in
  defined_parameters ms.CMethodSignature.args


let get_return_type tenv ms =
  let return_type = ms.CMethodSignature.ret_type in
  CType_decl.qual_type_to_sil_type tenv return_type


let sil_func_attributes_of_attributes attrs =
  let rec do_translation acc al =
    match al with
    | [] ->
        List.rev acc
    | Clang_ast_t.SentinelAttr attribute_info :: tl ->
        let sentinel, null_pos =
          match attribute_info.Clang_ast_t.ai_parameters with
          | [a; b] ->
              (int_of_string a, int_of_string b)
          | _ ->
              assert false
        in
        do_translation (PredSymb.FA_sentinel (sentinel, null_pos) :: acc) tl
    | _ :: tl ->
        do_translation acc tl
  in
  do_translation [] attrs


let should_create_procdesc cfg procname defined set_objc_accessor_attr =
  match Typ.Procname.Hash.find cfg procname with
  | previous_procdesc ->
      let is_defined_previous = Procdesc.is_defined previous_procdesc in
      if (defined || set_objc_accessor_attr) && not is_defined_previous then (
        Typ.Procname.Hash.remove cfg procname ;
        true )
      else false
  | exception Caml.Not_found ->
      true


let sil_method_annotation_of_args args method_type : Annot.Method.t =
  let args_types = List.map ~f:snd args in
  let param_annots = List.map ~f:CAst_utils.sil_annot_of_type args_types in
  let retval_annot = CAst_utils.sil_annot_of_type method_type in
  (retval_annot, param_annots)


let is_pointer_to_const {Clang_ast_t.qt_type_ptr} =
  match CAst_utils.get_type qt_type_ptr with
  | Some (PointerType (_, {Clang_ast_t.qt_is_const}))
  | Some (ObjCObjectPointerType (_, {Clang_ast_t.qt_is_const}))
  | Some (RValueReferenceType (_, {Clang_ast_t.qt_is_const}))
  | Some (LValueReferenceType (_, {Clang_ast_t.qt_is_const})) ->
      qt_is_const
  | _ ->
      false


let is_value {Clang_ast_t.qt_type_ptr} =
  match qt_type_ptr with
  | Clang_ast_extend.Builtin _
  (* We rely on the assumption here that Clang_ast_extend.ReferenceOf is only created for pass-by-value structs. *)
  (* TODO: Create a dedicated variant in Clang_ast_extend for pass-by-val params *)
  | Clang_ast_extend.ReferenceOf _ ->
      true
  | Clang_ast_types.TypePtr.Ptr _ ->
      let rec is_value_raw qt_type_ptr =
        match CAst_utils.get_type qt_type_ptr with
        | Some (BuiltinType _)
        | Some (ComplexType _)
        | Some (DependentSizedExtVectorType _)
        | Some (VectorType _)
        | Some (ExtVectorType _)
        | Some (RecordType _)
        | Some (EnumType _)
        | Some (InjectedClassNameType _)
        | Some (ObjCObjectType _)
        | Some (ObjCInterfaceType _) ->
            true
        | Some (AdjustedType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (DecayedType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (ParenType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (DecltypeType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (AtomicType (_, {Clang_ast_t.qt_type_ptr})) ->
            is_value_raw qt_type_ptr
        | Some (TypedefType (_, {Clang_ast_t.tti_child_type})) ->
            is_value_raw tti_child_type.Clang_ast_t.qt_type_ptr
        (* These types could be value types, and we try our best to resolve them *)
        | Some (AttributedType ({Clang_ast_t.ti_desugared_type}, _))
        | Some (TypeOfExprType {Clang_ast_t.ti_desugared_type})
        | Some (TypeOfType {Clang_ast_t.ti_desugared_type})
        | Some (UnaryTransformType {Clang_ast_t.ti_desugared_type})
        | Some (ElaboratedType {Clang_ast_t.ti_desugared_type})
        | Some (AutoType {Clang_ast_t.ti_desugared_type})
        | Some (DependentNameType {Clang_ast_t.ti_desugared_type})
        | Some (DeducedTemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (TemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (DependentTemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (TemplateTypeParmType {Clang_ast_t.ti_desugared_type})
        | Some (SubstTemplateTypeParmType {Clang_ast_t.ti_desugared_type})
        | Some (SubstTemplateTypeParmPackType {Clang_ast_t.ti_desugared_type})
        | Some (PackExpansionType {Clang_ast_t.ti_desugared_type})
        | Some (UnresolvedUsingType {Clang_ast_t.ti_desugared_type}) -> (
          match ti_desugared_type with Some ptr -> is_value_raw ptr | None -> false )
        (* These types are known to be non-value types *)
        | Some (PointerType _)
        | Some (BlockPointerType _)
        | Some (LValueReferenceType _)
        | Some (RValueReferenceType _)
        | Some (MemberPointerType _)
        | Some (ConstantArrayType _)
        | Some (IncompleteArrayType _)
        | Some (VariableArrayType _)
        | Some (DependentSizedArrayType _)
        | Some (FunctionProtoType _)
        | Some (FunctionNoProtoType _)
        | Some (ObjCObjectPointerType _)
        | Some (NoneType _)
        | Some (DependentAddressSpaceType _)
        (* These types I don't know what they are. Be conservative and treat them as non value types *)
        | Some (ObjCTypeParamType _)
        | Some (PipeType _)
        | None ->
            false
      in
      is_value_raw qt_type_ptr
  | _ ->
      false


(** Returns a list of the indices of expressions in [args] which point to const-typed values. Each
    index is offset by [shift]. *)
let get_const_args_indices ~shift args =
  let i = ref shift in
  let rec aux result = function
    | [] ->
        List.rev result
    | (_, qual_type) :: tl ->
        incr i ;
        if is_pointer_to_const qual_type then aux (!i - 1 :: result) tl else aux result tl
  in
  aux [] args


let get_byval_args_indices ~shift args =
  List.filter_mapi args ~f:(fun index (_, qual_type) ->
      let index' = index + shift in
      Option.some_if (is_value qual_type) index' )


let get_objc_property_accessor tenv ms =
  let open Clang_ast_t in
  match CAst_utils.get_decl_opt ms.CMethodSignature.pointer_to_property_opt with
  | Some (ObjCPropertyDecl (_, _, obj_c_property_decl_info))
    -> (
      let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
      match CAst_utils.get_decl_opt_with_decl_ref ivar_decl_ref with
      | Some (ObjCIvarDecl (_, name_decl_info, _, _, _))
        -> (
          let class_tname =
            Typ.Name.Objc.from_qual_name
              (QualifiedCppName.from_field_qualified_name
                 (QualifiedCppName.of_rev_list name_decl_info.ni_qual_name))
          in
          let field_name = CGeneral_utils.mk_class_field_name class_tname name_decl_info.ni_name in
          match Tenv.lookup tenv class_tname with
          | Some {fields}
            -> (
              let field_opt =
                List.find ~f:(fun (name, _, _) -> Typ.Fieldname.equal name field_name) fields
              in
              match field_opt with
              | Some field when CMethodSignature.is_getter ms ->
                  Some (ProcAttributes.Objc_getter field)
              | Some field when CMethodSignature.is_setter ms ->
                  Some (ProcAttributes.Objc_setter field)
              | _ ->
                  None )
          | None ->
              None )
      | _ ->
          None )
  | _ ->
      None


(** Creates a procedure description. *)
let create_local_procdesc ?(set_objc_accessor_attr= false) trans_unit_ctx cfg tenv ms fbody
    captured =
  let defined = not (Int.equal (List.length fbody) 0) in
  let proc_name = ms.CMethodSignature.name in
  let pname = Typ.Procname.to_string proc_name in
  let attributes = sil_func_attributes_of_attributes ms.CMethodSignature.attributes in
  let method_ret_type = ms.CMethodSignature.ret_type in
  let method_annotation = sil_method_annotation_of_args ms.CMethodSignature.args method_ret_type in
  let clang_method_kind = ms.CMethodSignature.method_kind in
  let is_cpp_nothrow = ms.CMethodSignature.is_cpp_nothrow in
  let access =
    match ms.CMethodSignature.access with
    | `None ->
        PredSymb.Default
    | `Private ->
        PredSymb.Private
    | `Protected ->
        PredSymb.Protected
    | `Public ->
        PredSymb.Protected
  in
  let create_new_procdesc () =
    let formals = get_formal_parameters tenv ms in
    let captured_mangled = List.map ~f:(fun (var, t) -> (Pvar.get_name var, t)) captured in
    (* Captured variables for blocks are treated as parameters *)
    let formals = captured_mangled @ formals in
    let const_formals =
      get_const_args_indices ~shift:(List.length captured_mangled) ms.CMethodSignature.args
    in
    let by_vals =
      get_byval_args_indices ~shift:(List.length captured_mangled) ms.CMethodSignature.args
    in
    let source_range = ms.CMethodSignature.loc in
    L.(debug Capture Verbose) "@\nCreating a new procdesc for function: '%s'@\n@." pname ;
    L.(debug Capture Verbose) "@\nms = %a@\n@." CMethodSignature.pp ms ;
    L.(debug Capture Verbose)
      "@\nbyvals = [ %s ]@\n@."
      (String.concat ~sep:", " (List.map by_vals ~f:string_of_int)) ;
    let loc_start = CLocation.get_sil_location_from_range trans_unit_ctx source_range true in
    let loc_exit = CLocation.get_sil_location_from_range trans_unit_ctx source_range false in
    let ret_type = get_return_type tenv ms in
    let objc_property_accessor =
      if set_objc_accessor_attr then get_objc_property_accessor tenv ms else None
    in
    let procdesc =
      let proc_attributes =
        { (ProcAttributes.default proc_name) with
          ProcAttributes.captured= captured_mangled
        ; formals
        ; const_formals
        ; by_vals
        ; access
        ; func_attributes= attributes
        ; is_defined= defined
        ; is_cpp_noexcept_method= is_cpp_nothrow
        ; is_model= Config.models_mode
        ; loc= loc_start
        ; clang_method_kind
        ; objc_accessor= objc_property_accessor
        ; translation_unit= Some trans_unit_ctx.CFrontend_config.source_file
        ; method_annotation
        ; ret_type }
      in
      Cfg.create_proc_desc cfg proc_attributes
    in
    if defined then (
      let start_kind = Procdesc.Node.Start_node proc_name in
      let start_node = Procdesc.create_node procdesc loc_start start_kind [] in
      let exit_kind = Procdesc.Node.Exit_node proc_name in
      let exit_node = Procdesc.create_node procdesc loc_exit exit_kind [] in
      Procdesc.set_start_node procdesc start_node ;
      Procdesc.set_exit_node procdesc exit_node )
  in
  if should_create_procdesc cfg proc_name defined set_objc_accessor_attr then (
    create_new_procdesc () ; true )
  else false


(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc cfg proc_name clang_method_kind type_opt =
  if not (Typ.Procname.Hash.mem cfg proc_name) then
    let ret_type, formals =
      match type_opt with
      | Some (ret_type, arg_types) ->
          (ret_type, List.map ~f:(fun typ -> (Mangled.from_string "x", typ)) arg_types)
      | None ->
          (Typ.mk Typ.Tvoid, [])
    in
    let proc_attributes =
      {(ProcAttributes.default proc_name) with ProcAttributes.formals; clang_method_kind; ret_type}
    in
    ignore (Cfg.create_proc_desc cfg proc_attributes)


let create_procdesc_with_pointer context pointer class_name_opt name =
  let open CContext in
  match method_signature_of_pointer context.translation_unit_context context.tenv pointer with
  | Some callee_ms ->
      ignore
        (create_local_procdesc context.translation_unit_context context.cfg context.tenv callee_ms
           [] []) ;
      callee_ms.CMethodSignature.name
  | None ->
      let callee_name, method_kind =
        match class_name_opt with
        | Some class_name ->
            ( CProcname.NoAstDecl.cpp_method_of_string context.tenv class_name name
            , ProcAttributes.CPP_INSTANCE )
        | None ->
            let is_cpp = CGeneral_utils.is_cpp_translation context.translation_unit_context in
            ( CProcname.NoAstDecl.c_function_of_string ~is_cpp context.tenv name
            , ProcAttributes.C_FUNCTION )
      in
      create_external_procdesc context.cfg callee_name method_kind None ;
      callee_name


let get_procname_from_cpp_lambda context dec =
  match dec with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rdi) -> (
    match cxx_rdi.xrdi_lambda_call_operator with
    | Some dr ->
        let name_info, decl_ptr, _ = CAst_utils.get_info_from_decl_ref dr in
        create_procdesc_with_pointer context decl_ptr None name_info.ni_name
    | _ ->
        assert false )
  | _ ->
      assert false


let get_captures_from_cpp_lambda dec =
  match dec with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rdi) ->
      cxx_rdi.xrdi_lambda_captures
  | _ ->
      assert false
