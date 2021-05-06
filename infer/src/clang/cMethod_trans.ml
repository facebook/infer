(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Methods for creating a procdesc from a method or function declaration and for resolving a method
    call and finding the right callee *)

module L = Logging

(** When the methoc call is MCStatic, means that it is a class method. When it is MCVirtual, it
    means that it is an instance method and that the method to be called will be determined at
    runtime. If it is MCNoVirtual it means that it is an instance method but that the method to be
    called will be determined at compile time *)
type method_call_type = MCVirtual | MCNoVirtual | MCStatic [@@deriving compare]

let equal_method_call_type = [%compare.equal: method_call_type]

let method_signature_of_pointer tenv pointer =
  try
    match CAst_utils.get_decl pointer with
    | Some meth_decl ->
        let procname = CType_decl.CProcname.from_decl ~tenv meth_decl in
        let ms = CType_decl.method_signature_of_decl tenv meth_decl procname in
        Some ms
    | None ->
        None
  with CFrontend_errors.Invalid_declaration -> None


let get_method_name_from_clang tenv ms_opt =
  match ms_opt with
  | Some ms -> (
    match CAst_utils.get_decl_opt ms.CMethodSignature.pointer_to_parent with
    | Some decl -> (
        ignore (CType_decl.add_types_from_decl_to_tenv tenv decl) ;
        match ObjcCategory_decl.get_base_class_name_from_category decl with
        | Some class_typename ->
            let procname = ms.CMethodSignature.name in
            let new_procname = Procname.replace_class procname class_typename in
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
let get_class_name_method_call_from_clang tenv obj_c_message_expr_info =
  match obj_c_message_expr_info.Clang_ast_t.omei_decl_pointer with
  | Some pointer -> (
    match method_signature_of_pointer tenv pointer with
    | Some ms -> (
      match ms.CMethodSignature.name with
      | Procname.ObjC_Cpp objc_cpp ->
          Some (Procname.ObjC_Cpp.get_class_type_name objc_cpp)
      | _ ->
          None )
    | None ->
        None )
  | None ->
      None


(* Get class name from a method call according to the info given by the receiver kind  *)
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
  match obj_c_message_expr_info.Clang_ast_t.omei_receiver_kind with
  | `Instance ->
      (selector, MCVirtual)
  | `SuperInstance ->
      (selector, MCNoVirtual)
  | `Class _ | `SuperClass ->
      (selector, MCStatic)


let should_create_procdesc cfg procname ~defined ~set_objc_accessor_attr =
  match Procname.Hash.find cfg procname with
  | previous_procdesc ->
      let is_defined_previous = Procdesc.is_defined previous_procdesc in
      if (defined || set_objc_accessor_attr) && not is_defined_previous then (
        Procname.Hash.remove cfg procname ;
        true )
      else false
  | exception Caml.Not_found ->
      true


(** Returns a list of the indices of expressions in [args] which point to const-typed values. Each
    index is offset by [shift]. *)
let get_const_params_indices ~shift params =
  let i = ref shift in
  let rec aux result = function
    | [] ->
        List.rev result
    | ({is_pointer_to_const} : CMethodSignature.param_type) :: tl ->
        incr i ;
        if is_pointer_to_const then aux ((!i - 1) :: result) tl else aux result tl
  in
  aux [] params


let get_objc_property_accessor tenv ms =
  let open Clang_ast_t in
  match CAst_utils.get_decl_opt ms.CMethodSignature.pointer_to_property_opt with
  | Some (ObjCPropertyDecl (_, _, obj_c_property_decl_info)) -> (
      let ivar_decl_ref = obj_c_property_decl_info.Clang_ast_t.opdi_ivar_decl in
      match CAst_utils.get_decl_opt_with_decl_ref_opt ivar_decl_ref with
      | Some (ObjCIvarDecl (_, name_decl_info, _, _, _)) -> (
          let class_tname =
            Typ.Name.Objc.from_qual_name
              (QualifiedCppName.from_field_qualified_name
                 (QualifiedCppName.of_rev_list name_decl_info.ni_qual_name))
          in
          let field_name = CGeneral_utils.mk_class_field_name class_tname name_decl_info.ni_name in
          match Tenv.lookup tenv class_tname with
          | Some {fields} -> (
              let field_opt =
                List.find ~f:(fun (name, _, _) -> Fieldname.equal name field_name) fields
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


let find_sentinel_attribute attrs =
  List.find_map attrs ~f:(function
    | `SentinelAttr (_attr_info, {Clang_ast_t.sai_sentinel= sentinel; sai_null_pos= null_pos}) ->
        Some (sentinel, null_pos)
    | _ ->
        None )


(** Creates a procedure description. *)
let create_local_procdesc ?(set_objc_accessor_attr = false) ?(record_lambda_captured = false)
    ?(is_cpp_lambda_call_operator = false) trans_unit_ctx cfg tenv ms fbody captured =
  let defined = not (List.is_empty fbody) in
  let proc_name = ms.CMethodSignature.name in
  let clang_method_kind = ms.CMethodSignature.method_kind in
  let access : ProcAttributes.access =
    match ms.CMethodSignature.access with
    | `None ->
        Default
    | `Private ->
        Private
    | `Protected ->
        Protected
    | `Public ->
        Public
  in
  let captured_mangled =
    List.map
      ~f:(fun (var, typ, capture_mode) -> {CapturedVar.name= Pvar.get_name var; typ; capture_mode})
      captured
  in
  (* Retrieve captured variables from procdesc created when translating captured variables in lambda expression *)
  (* We want to do this before `should_create_procdesc` is called as it can remove previous procdesc *)
  let captured_mangled_not_formals =
    if is_cpp_lambda_call_operator then
      match Procname.Hash.find cfg proc_name with
      | procdesc_prev ->
          Procdesc.get_captured procdesc_prev
      | exception Caml.Not_found ->
          captured_mangled
    else captured_mangled
  in
  let create_new_procdesc () =
    let all_params = Option.to_list ms.CMethodSignature.class_param @ ms.CMethodSignature.params in
    let has_added_return_param = ms.CMethodSignature.has_added_return_param in
    let method_annotation =
      let return = snd ms.CMethodSignature.ret_type in
      let params = List.map ~f:(fun ({annot} : CMethodSignature.param_type) -> annot) all_params in
      Annot.Method.{return; params}
    in
    let formals =
      List.map ~f:(fun ({name; typ} : CMethodSignature.param_type) -> (name, typ)) all_params
    in
    (* Captured variables for blocks are treated as parameters, but not for cpp lambdas *)
    let captured_as_formals =
      if is_cpp_lambda_call_operator then []
      else List.map ~f:(fun {CapturedVar.name; typ} -> (name, typ)) captured_mangled
    in
    let formals = captured_as_formals @ formals in
    let const_formals =
      get_const_params_indices ~shift:(List.length captured_as_formals) all_params
    in
    let source_range = ms.CMethodSignature.loc in
    L.(debug Capture Verbose)
      "@\nCreating a new procdesc for function: '%a'@\n@." Procname.pp proc_name ;
    L.(debug Capture Verbose) "@\nms = %a@\n@." CMethodSignature.pp ms ;
    let loc_start =
      CLocation.location_of_source_range trans_unit_ctx.CFrontend_config.source_file source_range
    in
    let loc_exit =
      CLocation.location_of_source_range ~pick_location:`End
        trans_unit_ctx.CFrontend_config.source_file source_range
    in
    let ret_type = fst ms.CMethodSignature.ret_type in
    let objc_property_accessor =
      if set_objc_accessor_attr then get_objc_property_accessor tenv ms else None
    in
    let translation_unit = trans_unit_ctx.CFrontend_config.source_file in
    let procdesc =
      let proc_attributes =
        { (ProcAttributes.default translation_unit proc_name) with
          ProcAttributes.captured= captured_mangled_not_formals
        ; formals
        ; const_formals
        ; has_added_return_param
        ; is_ret_type_pod= ms.CMethodSignature.is_ret_type_pod
        ; is_ret_constexpr= ms.CMethodSignature.is_ret_constexpr
        ; access
        ; is_defined= defined
        ; is_biabduction_model= Config.biabduction_models_mode
        ; passed_as_noescape_block_to= ms.CMethodSignature.passed_as_noescape_block_to
        ; is_no_return= ms.CMethodSignature.is_no_return
        ; is_objc_arc_on= trans_unit_ctx.CFrontend_config.is_objc_arc_on
        ; is_variadic= ms.CMethodSignature.is_variadic
        ; sentinel_attr= find_sentinel_attribute ms.CMethodSignature.attributes
        ; loc= loc_start
        ; clang_method_kind
        ; objc_accessor= objc_property_accessor
        ; method_annotation
        ; ret_type }
      in
      Cfg.create_proc_desc cfg proc_attributes
    in
    if defined then (
      let start_node = Procdesc.create_node procdesc loc_start Procdesc.Node.Start_node [] in
      let exit_node = Procdesc.create_node procdesc loc_exit Procdesc.Node.Exit_node [] in
      Procdesc.set_start_node procdesc start_node ;
      Procdesc.set_exit_node procdesc exit_node )
  in
  if should_create_procdesc cfg proc_name ~defined ~set_objc_accessor_attr then (
    create_new_procdesc () ;
    true )
  else (
    (* If we do not create procdesc when captured variables are translated,
       then we want to record captured variables in the previously created procdesc *)
    ignore
      ( if record_lambda_captured then
        match Procname.Hash.find cfg proc_name with
        | procdesc_prev ->
            let new_attributes =
              {(Procdesc.get_attributes procdesc_prev) with captured= captured_mangled}
            in
            Procdesc.set_attributes procdesc_prev new_attributes
        | exception Caml.Not_found ->
            () ) ;
    false )


(** Create a procdesc for objc methods whose signature cannot be found. *)
let create_external_procdesc trans_unit_ctx cfg proc_name clang_method_kind type_opt =
  if not (Procname.Hash.mem cfg proc_name) then
    let ret_type, formals =
      match type_opt with
      | Some (ret_type, arg_types) ->
          (ret_type, List.map ~f:(fun typ -> (Mangled.from_string "x", typ)) arg_types)
      | None ->
          (StdTyp.void, [])
    in
    let proc_attributes =
      { (ProcAttributes.default trans_unit_ctx.CFrontend_config.source_file proc_name) with
        ProcAttributes.formals
      ; clang_method_kind
      ; ret_type }
    in
    ignore (Cfg.create_proc_desc cfg proc_attributes)


let create_procdesc_with_pointer ?(captured_vars = []) context pointer class_name_opt name =
  let open CContext in
  match method_signature_of_pointer context.tenv pointer with
  | Some callee_ms ->
      ignore
        (create_local_procdesc context.translation_unit_context context.cfg context.tenv callee_ms
           [] captured_vars
           ~record_lambda_captured:(not (List.is_empty captured_vars))) ;
      callee_ms.CMethodSignature.name
  | None ->
      let callee_name, method_kind =
        match class_name_opt with
        | Some class_name ->
            ( CType_decl.CProcname.NoAstDecl.cpp_method_of_string context.tenv class_name name
            , ClangMethodKind.CPP_INSTANCE )
        | None ->
            ( CType_decl.CProcname.NoAstDecl.c_function_of_string context.tenv name
            , ClangMethodKind.C_FUNCTION )
      in
      create_external_procdesc context.translation_unit_context context.cfg callee_name method_kind
        None ;
      callee_name


let get_procname_from_cpp_lambda context dec captured_vars =
  match dec with
  | Clang_ast_t.CXXRecordDecl (_, _, _, _, _, _, _, cxx_rdi) -> (
    match cxx_rdi.xrdi_lambda_call_operator with
    | Some dr ->
        let name_info, decl_ptr, _ = CAst_utils.get_info_from_decl_ref dr in
        create_procdesc_with_pointer context decl_ptr None name_info.ni_name ~captured_vars
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
