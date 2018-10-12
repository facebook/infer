(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let get_builtin_objc_typename builtin_type =
  match builtin_type with
  | `ObjCId ->
      Typ.Name.C.from_string CFrontend_config.objc_object
  | `ObjCClass ->
      Typ.Name.C.from_string CFrontend_config.objc_class


let get_builtin_objc_type builtin_type =
  let typ = Typ.mk (Tstruct (get_builtin_objc_typename builtin_type)) in
  match builtin_type with `ObjCId -> typ.Typ.desc | `ObjCClass -> Typ.Tptr (typ, Typ.Pk_pointer)


let type_desc_of_builtin_type_kind builtin_type_kind =
  match builtin_type_kind with
  | `Void ->
      Typ.Tvoid
  | `Bool ->
      Typ.Tint IBool
  | `Char_U ->
      Typ.Tint IUChar
  | `UChar ->
      Typ.Tint IUChar
  | `WChar_U ->
      Typ.Tint IUChar
  | `Char_S ->
      Typ.Tint IChar
  | `SChar ->
      Typ.Tint ISChar
  | `WChar_S | `Char16 | `Char32 ->
      Typ.Tint IChar
  | `UShort ->
      Typ.Tint IUShort
  | `Short ->
      Typ.Tint IShort
  | `UInt | `UInt128 ->
      Typ.Tint IUInt
  | `ULong ->
      Typ.Tint IULong
  | `ULongLong ->
      Typ.Tint IULongLong
  | `Int | `Int128 ->
      Typ.Tint IInt
  | `Long ->
      Typ.Tint ILong
  | `LongLong ->
      Typ.Tint ILongLong
  | `Half ->
      Typ.Tint IShort (*?*)
  | `Float ->
      Typ.Tfloat FFloat
  | `Double ->
      Typ.Tfloat FDouble
  | `LongDouble ->
      Typ.Tfloat FLongDouble
  | `NullPtr ->
      Typ.Tint IInt
  | `ObjCId ->
      get_builtin_objc_type `ObjCId
  | `ObjCClass ->
      get_builtin_objc_type `ObjCClass
  | _ ->
      Typ.Tvoid


let type_of_builtin_type_kind ?(is_const = false) builtin_type_kind =
  let desc = type_desc_of_builtin_type_kind builtin_type_kind in
  let quals = Typ.mk_type_quals ~is_const () in
  Typ.mk ~quals desc


let pointer_attribute_of_objc_attribute attr_info =
  match attr_info.Clang_ast_t.ati_lifetime with
  | `OCL_None | `OCL_Strong ->
      Typ.Pk_pointer
  | `OCL_ExplicitNone ->
      Typ.Pk_objc_unsafe_unretained
  | `OCL_Weak ->
      Typ.Pk_objc_weak
  | `OCL_Autoreleasing ->
      Typ.Pk_objc_autoreleasing


let rec build_array_type translate_decl tenv (qual_type : Clang_ast_t.qual_type) length_opt
    stride_opt =
  let array_type = qual_type_to_sil_type translate_decl tenv qual_type in
  let length = Option.map ~f:IntLit.of_int length_opt in
  let stride = Option.map ~f:IntLit.of_int stride_opt in
  Typ.Tarray {elt= array_type; length; stride}


and type_desc_of_attr_type translate_decl tenv type_info attr_info =
  match type_info.Clang_ast_t.ti_desugared_type with
  | Some type_ptr -> (
    match CAst_utils.get_type type_ptr with
    | Some (Clang_ast_t.ObjCObjectPointerType (_, qual_type)) ->
        let typ = qual_type_to_sil_type translate_decl tenv qual_type in
        Typ.Tptr (typ, pointer_attribute_of_objc_attribute attr_info)
    | _ ->
        type_ptr_to_type_desc translate_decl tenv type_ptr )
  | None ->
      Typ.Tvoid


and type_desc_of_c_type translate_decl tenv c_type : Typ.desc =
  let open Clang_ast_t in
  match c_type with
  | NoneType _ ->
      Tvoid
  | BuiltinType (_, builtin_type_kind) ->
      type_desc_of_builtin_type_kind builtin_type_kind
  | PointerType (_, qual_type) | ObjCObjectPointerType (_, qual_type) ->
      let typ = qual_type_to_sil_type translate_decl tenv qual_type in
      let desc = typ.Typ.desc in
      if Typ.equal_desc desc (get_builtin_objc_type `ObjCClass) then desc
      else Typ.Tptr (typ, Typ.Pk_pointer)
  | ObjCObjectType (_, objc_object_type_info) ->
      type_ptr_to_type_desc translate_decl tenv objc_object_type_info.Clang_ast_t.ooti_base_type
  | BlockPointerType (_, qual_type) ->
      let typ = qual_type_to_sil_type translate_decl tenv qual_type in
      Typ.Tptr (typ, Typ.Pk_pointer)
  | IncompleteArrayType (_, {arti_element_type; arti_stride})
  | DependentSizedArrayType (_, {arti_element_type; arti_stride}) ->
      build_array_type translate_decl tenv arti_element_type None arti_stride
  | VariableArrayType (_, {arti_element_type; arti_stride}, _) ->
      build_array_type translate_decl tenv arti_element_type None arti_stride
  | ConstantArrayType (_, {arti_element_type; arti_stride}, n) ->
      build_array_type translate_decl tenv arti_element_type (Some n) arti_stride
  | FunctionProtoType _ | FunctionNoProtoType _ ->
      Typ.Tfun {no_return= false}
  | ParenType (_, qual_type) ->
      (qual_type_to_sil_type translate_decl tenv qual_type).Typ.desc
  | DecayedType (_, qual_type) ->
      (qual_type_to_sil_type translate_decl tenv qual_type).Typ.desc
  | RecordType (_, pointer) | EnumType (_, pointer) ->
      decl_ptr_to_type_desc translate_decl tenv pointer
  | ElaboratedType type_info -> (
    match type_info.Clang_ast_t.ti_desugared_type with
    (* TODO desugar to qualtype *)
    | Some type_ptr ->
        type_ptr_to_type_desc translate_decl tenv type_ptr
    | None ->
        Typ.Tvoid )
  | ObjCInterfaceType (_, pointer) ->
      decl_ptr_to_type_desc translate_decl tenv pointer
  | RValueReferenceType (_, qual_type) | LValueReferenceType (_, qual_type) ->
      let typ = qual_type_to_sil_type translate_decl tenv qual_type in
      Typ.Tptr (typ, Typ.Pk_reference)
  | AttributedType (type_info, attr_info) ->
      (* TODO desugar to qualtyp *)
      type_desc_of_attr_type translate_decl tenv type_info attr_info
  | _ -> (
      (* TypedefType, etc *)
      let type_info = Clang_ast_proj.get_type_tuple c_type in
      match type_info.Clang_ast_t.ti_desugared_type with
      (* TODO desugar typedeftype to qualtype *)
      | Some typ ->
          type_ptr_to_type_desc translate_decl tenv typ
      | None ->
          Typ.Tvoid )


and decl_ptr_to_type_desc translate_decl tenv decl_ptr : Typ.desc =
  let open Clang_ast_t in
  let typ = Clang_ast_extend.DeclPtr decl_ptr in
  try Clang_ast_extend.TypePointerMap.find typ !CFrontend_config.sil_types_map
  with Caml.Not_found -> (
    match CAst_utils.get_decl decl_ptr with
    | Some (CXXRecordDecl _ as d)
    | Some (RecordDecl _ as d)
    | Some (ClassTemplateSpecializationDecl _ as d)
    | Some (ObjCInterfaceDecl _ as d)
    | Some (ObjCImplementationDecl _ as d)
    | Some (ObjCProtocolDecl _ as d)
    | Some (ObjCCategoryDecl _ as d)
    | Some (ObjCCategoryImplDecl _ as d)
    | Some (EnumDecl _ as d) ->
        translate_decl tenv d
    | Some _ ->
        L.(debug Capture Verbose)
          "Warning: Wrong decl found for  pointer %s "
          (Clang_ast_j.string_of_pointer decl_ptr) ;
        Typ.Tvoid
    | None ->
        L.(debug Capture Verbose)
          "Warning: Decl pointer %s not found."
          (Clang_ast_j.string_of_pointer decl_ptr) ;
        Typ.Tvoid )


and clang_type_ptr_to_type_desc translate_decl tenv type_ptr =
  try Clang_ast_extend.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Caml.Not_found -> (
    match CAst_utils.get_type type_ptr with
    | Some c_type ->
        let type_desc = type_desc_of_c_type translate_decl tenv c_type in
        CAst_utils.update_sil_types_map type_ptr type_desc ;
        type_desc
    | _ ->
        Typ.Tvoid )


and type_ptr_to_type_desc translate_decl tenv type_ptr : Typ.desc =
  match type_ptr with
  | Clang_ast_types.TypePtr.Ptr _ ->
      clang_type_ptr_to_type_desc translate_decl tenv type_ptr
  | Clang_ast_extend.Builtin kind ->
      type_desc_of_builtin_type_kind kind
  | Clang_ast_extend.PointerOf typ ->
      let sil_typ = qual_type_to_sil_type translate_decl tenv typ in
      Typ.Tptr (sil_typ, Pk_pointer)
  | Clang_ast_extend.ReferenceOf typ ->
      let sil_typ = qual_type_to_sil_type translate_decl tenv typ in
      Typ.Tptr (sil_typ, Pk_reference)
  | Clang_ast_extend.ClassType typename ->
      Typ.Tstruct typename
  | Clang_ast_extend.DeclPtr ptr ->
      decl_ptr_to_type_desc translate_decl tenv ptr
  | Clang_ast_extend.ErrorType ->
      Typ.Tvoid
  | _ ->
      L.(die InternalError) "unknown variant for type_ptr"


and qual_type_to_sil_type translate_decl tenv qual_type =
  let desc = type_ptr_to_type_desc translate_decl tenv qual_type.Clang_ast_t.qt_type_ptr in
  let quals = Typ.mk_type_quals ~is_const:qual_type.Clang_ast_t.qt_is_const () in
  Typ.mk ~quals desc
