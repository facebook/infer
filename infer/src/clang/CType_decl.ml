(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Processes types and record declarations by adding them to the tenv *)

module BuildMethodSignature = struct
  let get_class_parameter_name method_kind =
    match method_kind with
    | ClangMethodKind.CPP_INSTANCE ->
        Some (Mangled.from_string CFrontend_config.this)
    | ClangMethodKind.OBJC_INSTANCE ->
        Some (Mangled.from_string CFrontend_config.self)
    | _ ->
        None


  let param_type_of_qual_type ?(is_cpp_const_member_function = false) qual_type_to_sil_type tenv
      name qual_type =
    let typ = qual_type_to_sil_type tenv qual_type in
    let is_pointer_to_const = CType.is_pointer_to_const qual_type in
    (* non-static member functions declared as const cannot modify their this*
       object parameter, hence should have pointer to const type
       https://en.cppreference.com/w/cpp/language/member_functions *)
    let typ =
      if Mangled.is_this name && is_cpp_const_member_function then Typ.set_ptr_to_const typ else typ
    in
    let annot = CAst_utils.sil_annot_of_type qual_type in
    CMethodSignature.mk_param_type ~is_pointer_to_const ~annot name typ


  let get_class_param qual_type_to_sil_type tenv method_decl =
    let aux ~is_cpp_const_member_function parent_pointer =
      let method_kind = CMethodProperties.get_method_kind method_decl in
      match method_kind with
      | ClangMethodKind.CPP_INSTANCE | ClangMethodKind.OBJC_INSTANCE -> (
        match (get_class_parameter_name method_kind, parent_pointer) with
        | Some name, Some parent_pointer ->
            let qual_type = CAst_utils.qual_type_of_decl_ptr parent_pointer in
            let pointer_qual_type = Ast_expressions.create_pointer_qual_type qual_type in
            Some
              (param_type_of_qual_type ~is_cpp_const_member_function qual_type_to_sil_type tenv name
                 pointer_qual_type )
        | _ ->
            None )
      | _ ->
          None
    in
    let open Clang_ast_t in
    match method_decl with
    | FunctionDecl _ | BlockDecl _ ->
        None
    | CXXMethodDecl (decl_info, _, _, _, cmdi) ->
        aux ~is_cpp_const_member_function:cmdi.Clang_ast_t.xmdi_is_const decl_info.di_parent_pointer
    | CXXConstructorDecl (decl_info, _, _, _, _)
    | CXXConversionDecl (decl_info, _, _, _, _)
    | CXXDestructorDecl (decl_info, _, _, _, _)
    | ObjCMethodDecl (decl_info, _, _) ->
        aux ~is_cpp_const_member_function:false decl_info.di_parent_pointer
    | _ ->
        raise CFrontend_errors.Invalid_declaration


  let should_add_return_param return_type =
    match return_type.Typ.desc with Tstruct _ -> true | _ -> false


  let get_return_type_and_param_type qual_type_to_sil_type tenv ~block_return_type method_decl =
    let return_qual_type =
      match block_return_type with
      | Some return_type ->
          CType.return_type_of_function_type return_type
      | None ->
          CMethodProperties.get_return_type method_decl
    in
    let return_typ = qual_type_to_sil_type tenv return_qual_type in
    let return_param =
      if should_add_return_param return_typ then
        let name = Mangled.from_string CFrontend_config.return_param in
        let return_qual_type = Ast_expressions.create_pointer_qual_type return_qual_type in
        Some (param_type_of_qual_type qual_type_to_sil_type tenv name return_qual_type)
      else None
    in
    (return_typ, return_qual_type, return_param)


  (** Returns parameters of a function/method. They will have following order:

      + normal parameters
      + return parameter (optional) *)
  let get_parameters qual_type_to_sil_type tenv ~block_return_type method_decl =
    let open Clang_ast_t in
    let par_to_ms_par par =
      match par with
      | ParmVarDecl (_, name_info, qt, var_decl_info) ->
          let method_decl_info = Clang_ast_proj.get_decl_tuple method_decl in
          let _, name =
            CGeneral_utils.get_var_name_mangled method_decl_info name_info var_decl_info
          in
          let typ =
            ( match CAst_utils.get_decl_from_typ_ptr qt.qt_type_ptr with
            | Some (CXXRecordDecl _) | Some (ClassTemplateSpecializationDecl _) ->
                Ast_expressions.create_reference_qual_type qt
            | _ ->
                qt )
            |> qual_type_to_sil_type tenv
          in
          let is_pointer_to_const = CType.is_pointer_to_const qt in
          let is_reference = CType.is_reference_type qt in
          let is_no_escape_block_arg = CAst_utils.is_no_escape_block_arg par in
          let annot = CAst_utils.sil_annot_of_type qt in
          CMethodSignature.mk_param_type name typ ~is_pointer_to_const ~is_reference ~annot
            ~is_no_escape_block_arg
      | _ ->
          raise CFrontend_errors.Invalid_declaration
    in
    let params = List.map ~f:par_to_ms_par (CMethodProperties.get_param_decls method_decl) in
    let return_param =
      Option.to_list
        ( get_return_type_and_param_type qual_type_to_sil_type tenv ~block_return_type method_decl
        |> trd3 )
    in
    params @ return_param


  let type_of_captured_var qual_type_to_sil_type tenv ~is_block_inside_objc_class_method decl_ref =
    match decl_ref with
    | {Clang_ast_t.dr_name= Some {Clang_ast_t.ni_name}} ->
        (* In Objective-C class methods, self is not the standard self instance, since in this
           context we don't have an instance. Instead it is used to get the class of the method.
           We translate this variables in a different way than normal, we don't treat them as
           variables in Sil, instead we remove them and get the class directly in the frontend.
           For that reason, we shouldn't add them as captured variables of blocks, since they
           don't appear anywhere else in the translation. *)
        if is_block_inside_objc_class_method && String.equal ni_name CFrontend_config.self then None
        else Some (Option.value_exn decl_ref.Clang_ast_t.dr_qual_type |> qual_type_to_sil_type tenv)
    | _ ->
        assert false


  let types_of_captured_vars qual_type_to_sil_type tenv meth_decl =
    let captured_vars = CMethodProperties.get_block_captured_variables meth_decl in
    let is_block =
      ClangMethodKind.equal (CMethodProperties.get_method_kind meth_decl) ClangMethodKind.BLOCK
    in
    let is_block_inside_objc_class_method =
      is_block && CMethodProperties.is_inside_objc_class_method meth_decl
    in
    List.map ~f:(fun cv -> Option.value_exn cv.Clang_ast_t.bcv_variable) captured_vars
    |> List.filter_map
         ~f:(type_of_captured_var qual_type_to_sil_type tenv ~is_block_inside_objc_class_method)


  (** get return type of the function and optionally type of function's return parameter *)
  let get_return_val_and_param_types qual_type_to_sil_type tenv ~block_return_type method_decl =
    let return_typ, return_qual_type, return_param_type =
      get_return_type_and_param_type qual_type_to_sil_type tenv ~block_return_type method_decl
    in
    let is_ret_typ_pod = CGeneral_utils.is_type_pod return_qual_type in
    match return_param_type with
    | Some _ ->
        ( StdTyp.void
        , Some (CType.add_pointer_to_typ return_typ)
        , Annot.Item.empty
        , true
        , is_ret_typ_pod )
    | None ->
        let return_typ_annot = CAst_utils.sil_annot_of_type return_qual_type in
        (return_typ, None, return_typ_annot, false, is_ret_typ_pod)


  let method_signature_of_decl qual_type_to_sil_type tenv method_decl ?block_return_type
      ?(block_as_arg_attributes = None) procname =
    let decl_info = Clang_ast_proj.get_decl_tuple method_decl in
    let loc = decl_info.Clang_ast_t.di_source_range in
    let ret_type, return_param_typ, ret_typ_annot, has_added_return_param, is_ret_type_pod =
      get_return_val_and_param_types qual_type_to_sil_type tenv ~block_return_type method_decl
    in
    let is_ret_constexpr = CMethodProperties.is_constexpr method_decl in
    let method_kind = CMethodProperties.get_method_kind method_decl in
    let pointer_to_parent = decl_info.di_parent_pointer in
    let class_param = get_class_param qual_type_to_sil_type tenv method_decl in
    let params = get_parameters qual_type_to_sil_type tenv ~block_return_type method_decl in
    let attributes = decl_info.Clang_ast_t.di_attributes in
    let is_cpp_const_member_fun = CMethodProperties.is_cpp_const_member_fun method_decl in
    let is_cpp_virtual = CMethodProperties.is_cpp_virtual method_decl in
    let is_cpp_copy_assignment = CMethodProperties.is_cpp_copy_assignment method_decl in
    let is_cpp_copy_ctor = CMethodProperties.is_cpp_copy_ctor method_decl in
    let is_cpp_move_ctor = CMethodProperties.is_cpp_move_ctor method_decl in
    let is_cpp_deleted = CMethodProperties.is_cpp_deleted method_decl in
    let is_cpp_implicit = CAst_utils.is_cpp_implicit_decl method_decl in
    let is_no_return = CMethodProperties.is_no_return method_decl in
    let is_variadic = CMethodProperties.is_variadic method_decl in
    let access = decl_info.Clang_ast_t.di_access in
    let pointer_to_property_opt = CMethodProperties.get_pointer_to_property method_decl in
    { CMethodSignature.name= procname
    ; access
    ; class_param
    ; params
    ; is_cpp_const_member_fun
    ; is_cpp_copy_assignment
    ; is_cpp_copy_ctor
    ; is_cpp_move_ctor
    ; is_cpp_deleted
    ; is_cpp_implicit
    ; ret_type= (ret_type, ret_typ_annot)
    ; is_ret_type_pod
    ; is_ret_constexpr
    ; has_added_return_param
    ; attributes
    ; loc
    ; method_kind
    ; is_cpp_virtual
    ; block_as_arg_attributes
    ; is_no_return
    ; is_variadic
    ; pointer_to_parent
    ; pointer_to_property_opt
    ; return_param_typ }


  let method_signature_body_of_decl qual_type_to_sil_type tenv method_decl ?block_return_type
      ?block_as_arg_attributes procname =
    let body = CMethodProperties.get_method_body method_decl in
    let init_list_instrs = CMethodProperties.get_init_list_instrs method_decl in
    let ms =
      method_signature_of_decl qual_type_to_sil_type tenv method_decl ?block_return_type
        ?block_as_arg_attributes procname
    in
    (ms, body, init_list_instrs)
end

let get_struct_decls decl =
  Clang_ast_proj.get_decl_context_tuple decl |> Option.value_map ~f:fst ~default:[]


let add_predefined_objc_types tenv =
  ignore (Tenv.mk_struct tenv (CType_to_sil_type.get_builtin_objc_typename `ObjCClass)) ;
  ignore (Tenv.mk_struct tenv (CType_to_sil_type.get_builtin_objc_typename `ObjCId))


let add_predefined_types tenv = add_predefined_objc_types tenv

let create_c_record_typename (tag_kind : Clang_ast_t.tag_kind) =
  match tag_kind with
  | `TTK_Struct | `TTK_Interface | `TTK_Enum ->
      Typ.Name.C.from_qual_name
  | `TTK_Union ->
      Typ.Name.C.union_from_qual_name
  | `TTK_Class ->
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate ~is_union:false


let get_class_template_name = function
  | Clang_ast_t.ClassTemplateDecl (_, name_info, _) ->
      CAst_utils.get_qualified_name name_info
  | _ ->
      assert false


let get_superclass_decls decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_rec_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_rec_info, _, _, _) ->
      (* there is no concept of virtual inheritance in the backend right now *)
      let base_ptr = cxx_rec_info.Clang_ast_t.xrdi_bases @ cxx_rec_info.Clang_ast_t.xrdi_vbases in
      let get_decl_or_fail typ_ptr =
        match CAst_utils.get_decl_from_typ_ptr typ_ptr with
        | Some decl ->
            decl
        | None ->
            assert false
      in
      List.map ~f:get_decl_or_fail base_ptr
  | _ ->
      []


let translate_as_type_ptr_matcher =
  QualifiedCppName.Match.of_fuzzy_qual_names ["infer_traits::TranslateAsType"]


let get_translate_as_friend_decl decl_list =
  let is_translate_as_friend_name (_, name_info) =
    let qual_name = CAst_utils.get_qualified_name name_info in
    QualifiedCppName.Match.match_qualifiers translate_as_type_ptr_matcher qual_name
  in
  let get_friend_decl_opt (decl : Clang_ast_t.decl) =
    match decl with
    | FriendDecl (_, `Type type_ptr) ->
        CAst_utils.get_decl_from_typ_ptr type_ptr
    | _ ->
        None
  in
  let is_translate_as_friend_decl decl =
    match get_friend_decl_opt decl with
    | Some decl ->
        let named_decl_tuple_opt = Clang_ast_proj.get_named_decl_tuple decl in
        Option.exists ~f:is_translate_as_friend_name named_decl_tuple_opt
    | None ->
        false
  in
  match get_friend_decl_opt (List.find_exn ~f:is_translate_as_friend_decl decl_list) with
  | Some
      (Clang_ast_t.ClassTemplateSpecializationDecl
        (_, _, _, _, _, _, _, _, _, _, {tsi_specialization_args= [`Type t_ptr]}) ) ->
      Some t_ptr
  | _ ->
      None
  | exception (Not_found_s _ | Caml.Not_found) ->
      None


let get_record_definition decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl
      (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr}, _, _, _, _)
  | CXXRecordDecl (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr}, _)
  | RecordDecl (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr})
    when (not rdi_is_complete_definition) && rdi_definition_ptr <> 0 ->
      CAst_utils.get_decl rdi_definition_ptr |> Option.value ~default:decl
  | _ ->
      decl


let mk_objc_method class_typename method_name method_kind parameters =
  Procname.ObjC_Cpp
    (Procname.ObjC_Cpp.make class_typename method_name method_kind Typ.NoTemplate parameters)


let rec get_mangled_method_name function_decl_info method_decl_info =
  (* For virtual methods return mangled name of the method from most base class
     Go recursively until there is no method in any parent class. All names
     of the same method need to be the same, otherwise dynamic dispatch won't
     work. *)
  let open Clang_ast_t in
  match method_decl_info.xmdi_overriden_methods with
  | [] ->
      function_decl_info.fdi_mangled_name
  | base1_dr :: _ -> (
      let base1 =
        match CAst_utils.get_decl base1_dr.dr_decl_pointer with Some b -> b | _ -> assert false
      in
      match base1 with
      | CXXMethodDecl (_, _, _, fdi, mdi)
      | CXXConstructorDecl (_, _, _, fdi, mdi)
      | CXXConversionDecl (_, _, _, fdi, mdi)
      | CXXDestructorDecl (_, _, _, fdi, mdi) ->
          get_mangled_method_name fdi mdi
      | _ ->
          assert false )


let rec get_struct_fields tenv ?cxx_record_decl_info decl =
  let open Clang_ast_t in
  let decl_list = get_struct_decls decl in
  let class_tname = get_record_typename ~tenv decl in
  let do_one_decl decl =
    match decl with
    | FieldDecl (_, {ni_name}, qt, _) ->
        let typ = qual_type_to_sil_type tenv qt in
        let annotation_items = CAst_utils.sil_annot_of_type qt in
        let name = Fieldname.make class_tname ni_name in
        [Struct.mk_field name typ ~annot:annotation_items]
    | _ ->
        []
  in
  let base_decls = get_superclass_decls decl in
  let base_class_fields = List.map ~f:(get_struct_fields tenv ?cxx_record_decl_info) base_decls in
  let fields = List.map ~f:do_one_decl decl_list in
  List.concat (base_class_fields @ fields)


(** For a record declaration it returns/constructs the type *)
and get_record_declaration_type tenv decl =
  let definition_decl = get_record_definition decl in
  match get_record_friend_decl_type tenv definition_decl with
  | Some t ->
      t.Typ.desc
  | None ->
      get_record_struct_type tenv definition_decl


and get_record_friend_decl_type tenv definition_decl =
  let open Clang_ast_t in
  match definition_decl with
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _, _)
  | CXXRecordDecl (_, _, _, decl_list, _, _, _, _) ->
      Option.map ~f:(qual_type_to_sil_type tenv) (get_translate_as_friend_decl decl_list)
  | _ ->
      None


(* We need to take the name out of the type as the struct can be anonymous
   If tenv is not passed, then template instantiaion information may be incorrect,
   as it defaults to Typ.NoTemplate *)
and get_record_typename ?tenv decl =
  let open Clang_ast_t in
  let linters_mode = match tenv with Some _ -> false | None -> true in
  let is_union_tag tag_kind = match tag_kind with `TTK_Union -> true | _ -> false in
  match (decl, tenv) with
  | RecordDecl (_, name_info, _, _, _, tag_kind, _), _ ->
      CAst_utils.get_qualified_name ~linters_mode name_info |> create_c_record_typename tag_kind
  | ( ClassTemplateSpecializationDecl (_, _, _, _, _, tag_kind, _, _, mangling, _, spec_info)
    , Some tenv ) ->
      let tname =
        match CAst_utils.get_decl spec_info.tsi_template_decl with
        | Some dec ->
            get_class_template_name dec
        | None ->
            assert false
      in
      let args = get_template_args tenv spec_info.tsi_specialization_args in
      let mangled = if String.equal "" mangling then None else Some mangling in
      Typ.Name.Cpp.from_qual_name
        (Typ.Template {mangled; args})
        ~is_union:(is_union_tag tag_kind) tname
  | CXXRecordDecl (_, name_info, _, _, _, `TTK_Union, _, _), _ ->
      Typ.CUnion (CAst_utils.get_qualified_name ~linters_mode name_info)
  | CXXRecordDecl (_, name_info, _, _, _, tag_kind, _, _), _
  | ClassTemplatePartialSpecializationDecl (_, name_info, _, _, _, tag_kind, _, _, _, _, _), _
  | ClassTemplateSpecializationDecl (_, name_info, _, _, _, tag_kind, _, _, _, _, _), _ ->
      (* we use Typ.CppClass for C++ because we expect Typ.CppClass from *)
      (* types that have methods. And in C++ struct/class/union can have methods *)
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate ~is_union:(is_union_tag tag_kind)
        (CAst_utils.get_qualified_name ~linters_mode name_info)
  | ObjCInterfaceDecl (_, name_info, _, _, _), _ | ObjCImplementationDecl (_, name_info, _, _, _), _
    ->
      CAst_utils.get_qualified_name name_info |> Typ.Name.Objc.from_qual_name
  | ObjCProtocolDecl (_, name_info, _, _, _), _ ->
      CAst_utils.get_qualified_name name_info |> Typ.Name.Objc.protocol_from_qual_name
  | ObjCCategoryDecl (_, _, _, _, {odi_class_interface= Some {dr_name}}), _
  | ObjCCategoryImplDecl (_, _, _, _, {ocidi_class_interface= Some {dr_name}}), _ -> (
    match dr_name with
    | Some name_info ->
        CAst_utils.get_qualified_name name_info |> Typ.Name.Objc.from_qual_name
    | None ->
        assert false )
  | _ ->
      Logging.die InternalError "Unhandled decl: %s." (Clang_ast_proj.get_decl_kind_string decl)


(** fetches list of superclasses for C++ classes *)
and get_superclass_list_cpp tenv decl =
  let base_decls = get_superclass_decls decl in
  let get_super_field super_decl = get_record_typename ~tenv super_decl in
  List.map ~f:get_super_field base_decls


and add_types_from_decl_to_tenv tenv decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl _ | CXXRecordDecl _ | RecordDecl _ ->
      get_record_declaration_type tenv decl
  | ObjCInterfaceDecl _ ->
      ObjcInterface_decl.interface_declaration qual_type_to_sil_type procname_from_decl tenv decl
  | ObjCImplementationDecl _ ->
      ObjcInterface_decl.interface_impl_declaration qual_type_to_sil_type procname_from_decl tenv
        decl
  | ObjCProtocolDecl _ ->
      ObjcProtocol_decl.protocol_decl qual_type_to_sil_type tenv decl
  | ObjCCategoryDecl _ ->
      ObjcCategory_decl.category_decl qual_type_to_sil_type procname_from_decl tenv decl
  | ObjCCategoryImplDecl _ ->
      ObjcCategory_decl.category_impl_decl qual_type_to_sil_type procname_from_decl tenv decl
  | EnumDecl _ ->
      CEnum_decl.enum_decl decl
  | _ ->
      assert false


and get_template_args tenv (template_args : Clang_ast_t.template_instantiation_arg_info list) =
  let rec aux = function
    | `Type qual_type ->
        [Typ.TType (qual_type_to_sil_type tenv qual_type)]
    | `Expression | `TemplateExpansion | `Template | `Declaration _ ->
        [Typ.TOpaque]
    | `Integral i -> (
      match Int64.of_string i with x -> [Typ.TInt x] | exception Failure _ -> [Typ.TOpaque] )
    | `Null ->
        [Typ.TNull]
    | `NullPtr ->
        [Typ.TNullPtr]
    | `Pack p ->
        List.concat_map ~f:aux p
  in
  List.concat_map ~f:aux template_args


and qual_type_to_sil_type tenv qual_type =
  CType_to_sil_type.qual_type_to_sil_type add_types_from_decl_to_tenv tenv qual_type


and get_template_info tenv (fdi : Clang_ast_t.function_decl_info) =
  match fdi.fdi_template_specialization with
  | Some {tsi_specialization_args} ->
      Typ.Template
        {mangled= fdi.fdi_mangled_name; args= get_template_args tenv tsi_specialization_args}
  | None ->
      Typ.NoTemplate


and mk_c_function ?tenv name function_decl_info_opt =
  let file =
    match function_decl_info_opt with
    (* when we model static functions, we cannot take the file into account to
       create a mangled name because the file of the model is different to the real file,
       thus the model won't work *)
    | Some (decl_info, function_decl_info)
      when function_decl_info.Clang_ast_t.fdi_is_static
           && not (CTrans_models.is_modelled_static_function (QualifiedCppName.to_qual_string name))
      ->
        let file_opt =
          (fst decl_info.Clang_ast_t.di_source_range).Clang_ast_t.sl_file
          |> Option.map ~f:SourceFile.from_abs_path
        in
        let file_to_hex src = SourceFile.to_string src |> Utils.string_crc_hex32 in
        Option.value_map ~f:file_to_hex ~default:"" file_opt
    | _ ->
        ""
  in
  let mangled_opt, is_cpp =
    match function_decl_info_opt with
    | Some (_, function_decl_info) ->
        (function_decl_info.Clang_ast_t.fdi_mangled_name, function_decl_info.Clang_ast_t.fdi_is_cpp)
    | _ ->
        (None, false)
  in
  let mangled_name = match mangled_opt with Some m when is_cpp -> m | _ -> "" in
  let template_info =
    match (function_decl_info_opt, tenv) with
    | Some (_, function_decl_info), Some t ->
        get_template_info t function_decl_info
    | _ ->
        Typ.NoTemplate
  in
  let mangled = file ^ mangled_name in
  if String.is_empty mangled then Procname.from_string_c_fun (QualifiedCppName.to_qual_string name)
  else Procname.C (Procname.C.c name ~mangled template_info)


and mk_cpp_method ?tenv class_name method_name ?meth_decl mangled parameters =
  let open Clang_ast_t in
  let method_kind =
    match meth_decl with
    | Some (Clang_ast_t.CXXConstructorDecl (_, _, _, _, _)) ->
        Procname.ObjC_Cpp.CPPConstructor mangled
    | Some (Clang_ast_t.CXXDestructorDecl _) ->
        Procname.ObjC_Cpp.CPPDestructor mangled
    | _ ->
        Procname.ObjC_Cpp.CPPMethod mangled
  in
  let template_info =
    match meth_decl with
    | Some
        ( CXXMethodDecl (_, _, _, fdi, _)
        | CXXConstructorDecl (_, _, _, fdi, _)
        | CXXConversionDecl (_, _, _, fdi, _)
        | CXXDestructorDecl (_, _, _, fdi, _) ) -> (
      match tenv with Some t -> get_template_info t fdi | None -> Typ.NoTemplate )
    | _ ->
        Typ.NoTemplate
  in
  Procname.ObjC_Cpp
    (Procname.ObjC_Cpp.make class_name method_name method_kind template_info parameters)


and get_class_typename ?tenv method_decl_info =
  let class_ptr = Option.value_exn method_decl_info.Clang_ast_t.di_parent_pointer in
  match CAst_utils.get_decl class_ptr with
  | Some class_decl ->
      get_record_typename ?tenv class_decl
  | None ->
      CFrontend_errors.incorrect_assumption __POS__ method_decl_info.Clang_ast_t.di_source_range
        "Expecting class declaration when getting the class typename"


and objc_method_procname ?tenv decl_info method_name parameters mdi =
  let class_typename = get_class_typename ?tenv decl_info in
  let is_instance = mdi.Clang_ast_t.omdi_is_instance_method in
  let method_kind = Procname.ObjC_Cpp.objc_method_kind_of_bool is_instance in
  mk_objc_method class_typename method_name method_kind parameters


and procname_from_decl ?tenv ?block_return_type ?outer_proc meth_decl =
  let open Clang_ast_t in
  let parameters =
    match tenv with
    | Some tenv ->
        let parameters =
          BuildMethodSignature.get_parameters qual_type_to_sil_type ~block_return_type tenv
            meth_decl
        in
        let parameter_types =
          List.map ~f:(fun ({typ} : CMethodSignature.param_type) -> typ) parameters
        in
        let captured_vars_types =
          BuildMethodSignature.types_of_captured_vars qual_type_to_sil_type tenv meth_decl
        in
        List.map ~f:Procname.Parameter.of_typ (captured_vars_types @ parameter_types)
    | None ->
        []
  in
  let mk_cpp_method decl_info name_info fdi mdi =
    let mangled = get_mangled_method_name fdi mdi in
    let method_name = CAst_utils.get_unqualified_name name_info in
    let class_typename = get_class_typename ?tenv decl_info in
    mk_cpp_method ?tenv class_typename method_name ~meth_decl mangled parameters
  in
  match meth_decl with
  | FunctionDecl (decl_info, name_info, _, fdi) ->
      let name = CAst_utils.get_qualified_name name_info in
      mk_c_function ?tenv name (Some (decl_info, fdi))
  | CXXConstructorDecl (decl_info, {ni_name= ""; ni_qual_name= "" :: qual_names}, _, fdi, mdi) ->
      (* For some constructors of non-class objects in C++, the clang frontend gives empty method
         name, e.g. struct, lambda, and union.  For better readability, we replace them to a
         constant non-empty name. *)
      let name_info =
        { ni_name= CFrontend_config.cxx_constructor
        ; ni_qual_name= CFrontend_config.cxx_constructor :: qual_names }
      in
      mk_cpp_method decl_info name_info fdi mdi
  | CXXMethodDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConstructorDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConversionDecl (decl_info, name_info, _, fdi, mdi)
  | CXXDestructorDecl (decl_info, name_info, _, fdi, mdi) ->
      mk_cpp_method decl_info name_info fdi mdi
  | ObjCMethodDecl (decl_info, name_info, mdi) ->
      objc_method_procname ?tenv decl_info name_info.Clang_ast_t.ni_name parameters mdi
  | BlockDecl (decl_info, block_decl_info) ->
      let outer_proc_class_name =
        Option.value_map ~default:None ~f:Procname.get_class_type_name outer_proc
      in
      let name, mangled = CAst_utils.create_objc_block_name decl_info block_decl_info in
      Procname.Block {Typ.class_name= outer_proc_class_name; name; mangled}
  | _ ->
      Logging.die InternalError "Expected method decl, but got %s."
        (Clang_ast_proj.get_decl_kind_string meth_decl)


and get_struct_methods struct_decl tenv =
  let open Clang_ast_t in
  List.filter_map (get_struct_decls struct_decl) ~f:(fun decl ->
      match decl with
      | FunctionDecl _
      | CXXMethodDecl _
      | CXXConstructorDecl _
      | CXXConversionDecl _
      | CXXDestructorDecl _
      | ObjCMethodDecl _ ->
          Some (procname_from_decl ~tenv decl)
      | _ ->
          None )


and add_record tenv decl_info definition_decl record_decl_info ?cxx_record_decl_info type_ptr =
  let sil_typename = get_record_typename ~tenv definition_decl in
  let sil_desc = Typ.Tstruct sil_typename in
  match Tenv.lookup tenv sil_typename with
  | Some _ ->
      sil_desc (* just reuse what is already in tenv *)
  | None ->
      let is_translatable_definition =
        let open Clang_ast_t in
        record_decl_info.rdi_is_complete_definition && not record_decl_info.rdi_is_dependent_type
      in
      if is_translatable_definition then (
        CAst_utils.update_sil_types_map type_ptr sil_desc ;
        let fields = get_struct_fields tenv ?cxx_record_decl_info definition_decl in
        (* Note: We treat static field same as global variables *)
        let statics = [] in
        let methods = get_struct_methods definition_decl tenv in
        let supers =
          get_superclass_list_cpp tenv definition_decl
          (* Mitigation: Sometimes the list of super classes includes the root type. *)
          |> List.filter ~f:(fun super ->
                 let is_sil_typename = Typ.Name.equal sil_typename super in
                 if is_sil_typename then
                   Logging.internal_error "The type %a has a super class of itself.@\n" Typ.Name.pp
                     sil_typename ;
                 not is_sil_typename )
        in
        let annots =
          if Typ.Name.Cpp.is_class sil_typename then Annot.Class.cpp
          else (* No annotations for structs *) Annot.Item.empty
        in
        let class_info =
          let is_trivially_copyable =
            Option.exists cxx_record_decl_info ~f:(fun {Clang_ast_t.xrdi_is_trivially_copyable} ->
                xrdi_is_trivially_copyable )
          in
          Struct.ClassInfo.CppClassInfo {is_trivially_copyable}
        in
        let source_file =
          (fst decl_info.Clang_ast_t.di_source_range).sl_file
          |> Option.map ~f:SourceFile.from_abs_path
        in
        Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots ~class_info ?source_file
          sil_typename
        |> ignore ;
        CAst_utils.update_sil_types_map type_ptr sil_desc ;
        sil_desc )
      else (
        (* There is no definition for that struct in whole translation unit.
           Put empty struct into tenv to prevent backend problems *)
        ignore (Tenv.mk_struct tenv ~dummy:true sil_typename) ;
        CAst_utils.update_sil_types_map type_ptr sil_desc ;
        sil_desc )


and get_record_struct_type tenv definition_decl : Typ.desc =
  let open Clang_ast_t in
  match definition_decl with
  | RecordDecl (decl_info, _, type_ptr, _, _, _, record_decl_info) ->
      add_record tenv decl_info definition_decl record_decl_info type_ptr
  | CXXRecordDecl (decl_info, _, type_ptr, _, _, _, record_decl_info, cxx_record_decl_info)
  | ClassTemplateSpecializationDecl
      (decl_info, _, type_ptr, _, _, _, record_decl_info, cxx_record_decl_info, _, _, _) ->
      add_record tenv decl_info definition_decl record_decl_info ~cxx_record_decl_info type_ptr
  | _ ->
      assert false


let method_signature_body_of_decl =
  BuildMethodSignature.method_signature_body_of_decl qual_type_to_sil_type


let method_signature_of_decl = BuildMethodSignature.method_signature_of_decl qual_type_to_sil_type

let should_add_return_param = BuildMethodSignature.should_add_return_param

let type_of_captured_var = BuildMethodSignature.type_of_captured_var qual_type_to_sil_type

module CProcname = struct
  let from_decl = procname_from_decl

  module NoAstDecl = struct
    let c_function_of_string tenv name =
      let qual_name = QualifiedCppName.of_qual_string name in
      mk_c_function ~tenv qual_name None


    let cpp_method_of_string tenv class_name method_name =
      mk_cpp_method ~tenv class_name method_name None []


    let objc_method_of_string_kind class_name method_name method_kind parameters =
      mk_objc_method class_name method_name method_kind parameters
  end
end

let get_type_from_expr_info ei tenv =
  let qt = ei.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt


let class_from_pointer_type tenv qual_type =
  match (qual_type_to_sil_type tenv qual_type).Typ.desc with
  | Tptr ({desc= Tstruct typename}, _) ->
      typename
  | _ ->
      assert false
