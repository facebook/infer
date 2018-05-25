(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Processes types and record declarations by adding them to the tenv *)

let get_struct_decls decl =
  let open Clang_ast_t in
  match decl with
  | CapturedDecl (_, decl_list, _)
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
  | ClassTemplatePartialSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
  | CXXRecordDecl (_, _, _, decl_list, _, _, _, _)
  | EnumDecl (_, _, _, decl_list, _, _, _)
  | LinkageSpecDecl (_, decl_list, _)
  | NamespaceDecl (_, _, decl_list, _, _)
  | ObjCCategoryDecl (_, _, decl_list, _, _)
  | ObjCCategoryImplDecl (_, _, decl_list, _, _)
  | ObjCImplementationDecl (_, _, decl_list, _, _)
  | ObjCInterfaceDecl (_, _, decl_list, _, _)
  | ObjCProtocolDecl (_, _, decl_list, _, _)
  | RecordDecl (_, _, _, decl_list, _, _, _)
  | TranslationUnitDecl (_, decl_list, _, _) ->
      decl_list
  | AccessSpecDecl _
  | BlockDecl _
  | ClassScopeFunctionSpecializationDecl _
  | EmptyDecl _
  | ExportDecl _
  | ExternCContextDecl _
  | FileScopeAsmDecl _
  | FriendDecl _
  | FriendTemplateDecl _
  | ImportDecl _
  | LabelDecl _
  | NamespaceAliasDecl _
  | ObjCCompatibleAliasDecl _
  | ObjCMethodDecl _
  | ObjCPropertyDecl _
  | BuiltinTemplateDecl _
  | ClassTemplateDecl _
  | FunctionTemplateDecl _
  | TypeAliasTemplateDecl _
  | VarTemplateDecl _
  | TemplateTemplateParmDecl _
  | TemplateTypeParmDecl _
  | ObjCTypeParamDecl _
  | TypeAliasDecl _
  | TypedefDecl _
  | UnresolvedUsingTypenameDecl _
  | UsingDecl _
  | UsingDirectiveDecl _
  | UsingPackDecl _
  | UsingShadowDecl _
  | ConstructorUsingShadowDecl _
  | BindingDecl _
  | FieldDecl _
  | ObjCAtDefsFieldDecl _
  | ObjCIvarDecl _
  | FunctionDecl _
  | CXXDeductionGuideDecl _
  | CXXMethodDecl _
  | CXXConstructorDecl _
  | CXXConversionDecl _
  | CXXDestructorDecl _
  | MSPropertyDecl _
  | NonTypeTemplateParmDecl _
  | VarDecl _
  | DecompositionDecl _
  | ImplicitParamDecl _
  | OMPCapturedExprDecl _
  | ParmVarDecl _
  | VarTemplateSpecializationDecl _
  | VarTemplatePartialSpecializationDecl _
  | EnumConstantDecl _
  | IndirectFieldDecl _
  | OMPDeclareReductionDecl _
  | UnresolvedUsingValueDecl _
  | OMPThreadPrivateDecl _
  | ObjCPropertyImplDecl _
  | PragmaCommentDecl _
  | PragmaDetectMismatchDecl _
  | StaticAssertDecl _ ->
      []


let add_predefined_objc_types tenv =
  ignore (Tenv.mk_struct tenv (CType_to_sil_type.get_builtin_objc_typename `ObjCClass)) ;
  ignore (Tenv.mk_struct tenv (CType_to_sil_type.get_builtin_objc_typename `ObjCId))


let add_predefined_types tenv = add_predefined_objc_types tenv

let create_c_record_typename (tag_kind: Clang_ast_t.tag_kind) =
  match tag_kind with
  | `TTK_Struct | `TTK_Interface | `TTK_Enum ->
      Typ.Name.C.from_qual_name
  | `TTK_Union ->
      Typ.Name.C.union_from_qual_name
  | `TTK_Class ->
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate


let get_class_template_name = function
  | Clang_ast_t.ClassTemplateDecl (_, name_info, _) ->
      CAst_utils.get_qualified_name name_info
  | _ ->
      assert false


let get_superclass_decls decl =
  let open Clang_ast_t in
  match decl with
  | CXXRecordDecl (_, _, _, _, _, _, _, cxx_rec_info)
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, cxx_rec_info, _, _) ->
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
  let get_friend_decl_opt (decl: Clang_ast_t.decl) =
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
        Option.value_map ~f:is_translate_as_friend_name ~default:false named_decl_tuple_opt
    | None ->
        false
  in
  match get_friend_decl_opt (List.find_exn ~f:is_translate_as_friend_decl decl_list) with
  | Some
      (Clang_ast_t.ClassTemplateSpecializationDecl
        (_, _, _, _, _, _, _, _, _, {tsi_specialization_args= [`Type t_ptr]})) ->
      Some t_ptr
  | _ ->
      None
  | exception Caml.Not_found ->
      None


let get_record_definition decl =
  let open Clang_ast_t in
  match decl with
  | ClassTemplateSpecializationDecl
      (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr}, _, _, _)
  | CXXRecordDecl (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr}, _)
  | RecordDecl (_, _, _, _, _, _, {rdi_is_complete_definition; rdi_definition_ptr})
    when not rdi_is_complete_definition && rdi_definition_ptr <> 0 ->
      CAst_utils.get_decl rdi_definition_ptr |> Option.value ~default:decl
  | _ ->
      decl


let mk_objc_method class_typename method_name method_kind =
  Typ.Procname.ObjC_Cpp
    (Typ.Procname.ObjC_Cpp.make class_typename method_name method_kind Typ.NoTemplate)


let rec get_mangled_method_name function_decl_info method_decl_info =
  (* For virtual methods return mangled name of the method from most base class
     Go recursively until there is no method in any parent class. All names
     of the same method need to be the same, otherwise dynamic dispatch won't
     work. *)
  let open Clang_ast_t in
  match method_decl_info.xmdi_overriden_methods with
  | [] ->
      function_decl_info.fdi_mangled_name
  | base1_dr :: _ ->
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
          assert false


let rec get_struct_fields tenv decl =
  let open Clang_ast_t in
  let decl_list = get_struct_decls decl in
  let class_tname = get_record_typename ~tenv decl in
  let do_one_decl decl =
    match decl with
    | FieldDecl (_, {ni_name}, qt, _) ->
        let id = CGeneral_utils.mk_class_field_name class_tname ni_name in
        let typ = qual_type_to_sil_type tenv qt in
        let annotation_items = CAst_utils.sil_annot_of_type qt in
        [(id, typ, annotation_items)]
    | _ ->
        []
  in
  let base_decls = get_superclass_decls decl in
  let base_class_fields = List.map ~f:(get_struct_fields tenv) base_decls in
  List.concat (base_class_fields @ List.map ~f:do_one_decl decl_list)


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
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
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
  match (decl, tenv) with
  | RecordDecl (_, name_info, _, _, _, tag_kind, _), _ ->
      CAst_utils.get_qualified_name ~linters_mode name_info |> create_c_record_typename tag_kind
  | ClassTemplateSpecializationDecl (_, _, _, _, _, _, _, _, mangling, spec_info), Some tenv ->
      let tname =
        match CAst_utils.get_decl spec_info.tsi_template_decl with
        | Some dec ->
            get_class_template_name dec
        | None ->
            assert false
      in
      let args = get_template_args tenv spec_info in
      let mangled = if String.equal "" mangling then None else Some mangling in
      Typ.Name.Cpp.from_qual_name (Typ.Template {mangled; args}) tname
  | CXXRecordDecl (_, name_info, _, _, _, _, _, _), _
  | ClassTemplateSpecializationDecl (_, name_info, _, _, _, _, _, _, _, _), _ ->
      (* we use Typ.CppClass for C++ because we expect Typ.CppClass from *)
      (* types that have methods. And in C++ struct/class/union can have methods *)
      Typ.Name.Cpp.from_qual_name Typ.NoTemplate
        (CAst_utils.get_qualified_name ~linters_mode name_info)
  | ObjCInterfaceDecl (_, name_info, _, _, _), _
  | ObjCImplementationDecl (_, name_info, _, _, _), _ ->
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
      assert false


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
      ObjcInterface_decl.interface_declaration qual_type_to_sil_type tenv decl
  | ObjCImplementationDecl _ ->
      ObjcInterface_decl.interface_impl_declaration qual_type_to_sil_type tenv decl
  | ObjCProtocolDecl _ ->
      ObjcProtocol_decl.protocol_decl qual_type_to_sil_type tenv decl
  | ObjCCategoryDecl _ ->
      ObjcCategory_decl.category_decl qual_type_to_sil_type tenv decl
  | ObjCCategoryImplDecl _ ->
      ObjcCategory_decl.category_impl_decl qual_type_to_sil_type tenv decl
  | EnumDecl _ ->
      CEnum_decl.enum_decl decl
  | _ ->
      assert false


and get_template_args tenv (tsi: Clang_ast_t.template_specialization_info) =
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
  List.concat_map ~f:aux tsi.tsi_specialization_args


and qual_type_to_sil_type tenv qual_type =
  CType_to_sil_type.qual_type_to_sil_type add_types_from_decl_to_tenv tenv qual_type


and get_template_info tenv (fdi: Clang_ast_t.function_decl_info) =
  match fdi.fdi_template_specialization with
  | Some spec_info ->
      Typ.Template {mangled= fdi.fdi_mangled_name; args= get_template_args tenv spec_info}
  | None ->
      Typ.NoTemplate


and mk_c_function ?tenv ~is_cpp name function_decl_info_opt =
  let file =
    match function_decl_info_opt with
    | Some (decl_info, function_decl_info) -> (
      match function_decl_info.Clang_ast_t.fdi_storage_class with
      | Some "static"
      (* when we model static functions, we cannot take the file into account to
       create a mangled name because the file of the model is different to the real file,
       thus the model won't work *)
        when not (CTrans_models.is_modelled_static_function (QualifiedCppName.to_qual_string name)) ->
          let file_opt =
            (fst decl_info.Clang_ast_t.di_source_range).Clang_ast_t.sl_file
            |> Option.map ~f:SourceFile.from_abs_path
          in
          let file_to_hex src = SourceFile.to_string src |> Utils.string_crc_hex32 in
          Option.value_map ~f:file_to_hex ~default:"" file_opt
      | _ ->
          "" )
    | None ->
        ""
  in
  let mangled_opt =
    match function_decl_info_opt with
    | Some (_, function_decl_info) ->
        function_decl_info.Clang_ast_t.fdi_mangled_name
    | _ ->
        None
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
  if String.is_empty mangled then
    Typ.Procname.from_string_c_fun (QualifiedCppName.to_qual_string name)
  else Typ.Procname.C (Typ.Procname.c name mangled template_info)


and mk_cpp_method ?tenv class_name method_name ?meth_decl mangled =
  let open Clang_ast_t in
  let method_kind =
    match meth_decl with
    | Some (Clang_ast_t.CXXConstructorDecl (_, _, _, _, {xmdi_is_constexpr})) ->
        Typ.Procname.ObjC_Cpp.CPPConstructor {mangled; is_constexpr= xmdi_is_constexpr}
    | Some (Clang_ast_t.CXXDestructorDecl _) ->
        Typ.Procname.ObjC_Cpp.CPPDestructor {mangled}
    | _ ->
        Typ.Procname.ObjC_Cpp.CPPMethod {mangled}
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
  Typ.Procname.ObjC_Cpp
    (Typ.Procname.ObjC_Cpp.make class_name method_name method_kind template_info)


and get_class_typename ?tenv method_decl_info =
  let class_ptr = Option.value_exn method_decl_info.Clang_ast_t.di_parent_pointer in
  match CAst_utils.get_decl class_ptr with
  | Some class_decl ->
      get_record_typename ?tenv class_decl
  | None ->
      CFrontend_config.incorrect_assumption __POS__ method_decl_info.Clang_ast_t.di_source_range
        "Expecting class declaration when getting the class typename"


and objc_method_procname ?tenv decl_info method_name mdi =
  let class_typename = get_class_typename ?tenv decl_info in
  let is_instance = mdi.Clang_ast_t.omdi_is_instance_method in
  let method_kind = Typ.Procname.ObjC_Cpp.objc_method_kind_of_bool is_instance in
  mk_objc_method class_typename method_name method_kind


and objc_block_procname outer_proc_opt =
  let outer_proc_string = Option.value_map ~f:Typ.Procname.to_string outer_proc_opt ~default:"" in
  let block_procname_with_index i =
    Printf.sprintf "%s%s%s%d" Config.anonymous_block_prefix outer_proc_string
      Config.anonymous_block_num_sep i
  in
  let name = block_procname_with_index (CFrontend_config.get_fresh_block_index ()) in
  Typ.Procname.mangled_objc_block name


and from_decl ?tenv ~is_cpp ?outer_proc meth_decl =
  let open Clang_ast_t in
  match meth_decl with
  | FunctionDecl (decl_info, name_info, _, fdi) ->
      let name = CAst_utils.get_qualified_name name_info in
      let function_info = Some (decl_info, fdi) in
      mk_c_function ~is_cpp ?tenv name function_info
  | CXXMethodDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConstructorDecl (decl_info, name_info, _, fdi, mdi)
  | CXXConversionDecl (decl_info, name_info, _, fdi, mdi)
  | CXXDestructorDecl (decl_info, name_info, _, fdi, mdi) ->
      let mangled = get_mangled_method_name fdi mdi in
      let method_name = CAst_utils.get_unqualified_name name_info in
      let class_typename = get_class_typename ?tenv decl_info in
      mk_cpp_method ?tenv class_typename method_name ~meth_decl mangled
  | ObjCMethodDecl (decl_info, name_info, mdi) ->
      objc_method_procname ?tenv decl_info name_info.Clang_ast_t.ni_name mdi
  | BlockDecl _ ->
      objc_block_procname outer_proc
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
      | ObjCMethodDecl _
      | BlockDecl _ ->
          Some (from_decl ~is_cpp:true ~tenv decl)
      | _ ->
          None )


and get_record_struct_type tenv definition_decl : Typ.desc =
  let open Clang_ast_t in
  match definition_decl with
  | ClassTemplateSpecializationDecl (_, _, type_ptr, _, _, _, record_decl_info, _, _, _)
  | CXXRecordDecl (_, _, type_ptr, _, _, _, record_decl_info, _)
  | RecordDecl (_, _, type_ptr, _, _, _, record_decl_info)
    -> (
      let sil_typename = get_record_typename ~tenv definition_decl in
      let sil_desc = Typ.Tstruct sil_typename in
      match Tenv.lookup tenv sil_typename with
      | Some _ ->
          sil_desc (* just reuse what is already in tenv *)
      | None ->
          let is_translatable_definition =
            let open Clang_ast_t in
            record_decl_info.rdi_is_complete_definition
            && not record_decl_info.rdi_is_dependent_type
          in
          if is_translatable_definition then (
            CAst_utils.update_sil_types_map type_ptr sil_desc ;
            let fields = get_struct_fields tenv definition_decl in
            (* Note: We treat static field same as global variables *)
            let statics = [] in
            let methods = get_struct_methods definition_decl tenv in
            let supers = get_superclass_list_cpp tenv definition_decl in
            let annots =
              if Typ.Name.Cpp.is_class sil_typename then Annot.Class.cpp
              else (* No annotations for structs *) Annot.Item.empty
            in
            Tenv.mk_struct tenv ~fields ~statics ~methods ~supers ~annots sil_typename |> ignore ;
            CAst_utils.update_sil_types_map type_ptr sil_desc ;
            sil_desc )
          else (
            (* There is no definition for that struct in whole translation unit.
                Put empty struct into tenv to prevent backend problems *)
            ignore (Tenv.mk_struct tenv ~fields:[] sil_typename) ;
            CAst_utils.update_sil_types_map type_ptr sil_desc ;
            sil_desc ) )
  | _ ->
      assert false


module CProcname = struct
  let from_decl = from_decl

  module NoAstDecl = struct
    let c_function_of_string ~is_cpp tenv name =
      let qual_name = QualifiedCppName.of_qual_string name in
      mk_c_function ~is_cpp ~tenv qual_name None


    let cpp_method_of_string tenv class_name method_name =
      mk_cpp_method ~tenv class_name method_name None


    let objc_method_of_string_kind class_name method_name method_kind =
      mk_objc_method class_name method_name method_kind
  end

  let from_decl_for_linters ~is_cpp method_decl =
    let open Clang_ast_t in
    match method_decl with
    | ObjCMethodDecl (decl_info, name_info, mdi) ->
        let method_name =
          match String.split ~on:':' name_info.Clang_ast_t.ni_name with
          | hd :: _ ->
              hd
          | _ ->
              name_info.Clang_ast_t.ni_name
        in
        objc_method_procname decl_info method_name mdi
    | _ ->
        from_decl ~is_cpp method_decl
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
