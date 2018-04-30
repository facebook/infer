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


let rec get_struct_fields tenv decl =
  let open Clang_ast_t in
  let decl_list =
    match decl with
    | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
    | CXXRecordDecl (_, _, _, decl_list, _, _, _, _)
    | RecordDecl (_, _, _, decl_list, _, _, _) ->
        decl_list
    | _ ->
        []
  in
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
  match get_record_custom_type tenv definition_decl with
  | Some t ->
      t.Typ.desc
  | None ->
      get_record_struct_type tenv definition_decl


and get_record_custom_type tenv definition_decl =
  let result = get_record_friend_decl_type tenv definition_decl in
  let result = if Option.is_none result then get_record_as_typevar definition_decl else result in
  result


and get_record_friend_decl_type tenv definition_decl =
  let open Clang_ast_t in
  match definition_decl with
  | ClassTemplateSpecializationDecl (_, _, _, decl_list, _, _, _, _, _, _)
  | CXXRecordDecl (_, _, _, decl_list, _, _, _, _) ->
      Option.map ~f:(qual_type_to_sil_type tenv) (get_translate_as_friend_decl decl_list)
  | _ ->
      None


and get_record_as_typevar (definition_decl: Clang_ast_t.decl) =
  let open Clang_ast_t in
  match definition_decl with
  | CXXRecordDecl (decl_info, name_info, _, _, _, _, _, _) ->
      let is_infer_typevar = function
        | AnnotateAttr {ai_parameters= [_; name; _]} when String.equal name "__infer_type_var" ->
            true
        | _ ->
            false
      in
      if List.exists ~f:is_infer_typevar decl_info.di_attributes then
        let tname = CAst_utils.get_qualified_name name_info |> QualifiedCppName.to_qual_string in
        Some (Typ.mk (TVar tname))
      else None
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
  | ObjCImplementationDecl (_, name_info, _, _, _), _
  | ObjCProtocolDecl (_, name_info, _, _, _), _ ->
      CAst_utils.get_qualified_name name_info |> Typ.Name.Objc.from_qual_name
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
            let statics = [] in
            (* Note: We treat static field same as global variables *)
            let methods = [] in
            (* C++ methods are not put into tenv (info isn't used) *)
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


let get_type_from_expr_info ei tenv =
  let qt = ei.Clang_ast_t.ei_qual_type in
  qual_type_to_sil_type tenv qt


let class_from_pointer_type tenv qual_type =
  match (qual_type_to_sil_type tenv qual_type).Typ.desc with
  | Tptr ({desc= Tstruct typename}, _) ->
      typename
  | _ ->
      assert false
