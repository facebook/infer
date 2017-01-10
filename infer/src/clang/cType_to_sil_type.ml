(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

let get_builtin_objc_typename builtin_type =
  match builtin_type with
  | `ObjCId -> Typename.TN_csu (Csu.Struct, (Mangled.from_string CFrontend_config.objc_object))
  | `ObjCClass -> Typename.TN_csu (Csu.Struct, (Mangled.from_string CFrontend_config.objc_class))

let get_builtin_objc_type builtin_type =
  let typ = Typ.Tstruct (get_builtin_objc_typename builtin_type) in
  match builtin_type with
  | `ObjCId -> typ
  | `ObjCClass -> Typ.Tptr (typ, Typ.Pk_pointer)


let sil_type_of_builtin_type_kind builtin_type_kind =
  match builtin_type_kind with
  | `Void -> Typ.Tvoid
  | `Bool -> Typ.Tint Typ.IBool
  | `Char_U -> Typ.Tint Typ.IUChar
  | `UChar -> Typ.Tint Typ.IUChar
  | `WChar_U -> Typ.Tint Typ.IUChar
  | `Char_S -> Typ.Tint Typ.IChar
  | `SChar -> Typ.Tint Typ.ISChar
  | `WChar_S
  | `Char16
  | `Char32 -> Typ.Tint Typ.IChar
  | `UShort
  | `Short -> Typ.Tint Typ.IShort
  | `UInt
  | `UInt128 -> Typ.Tint Typ.IUInt
  | `ULong -> Typ.Tint Typ.IULong
  | `ULongLong -> Typ.Tint Typ.IULongLong
  | `Int
  | `Int128 -> Typ.Tint Typ.IInt
  | `Long -> Typ.Tint Typ.ILong
  | `LongLong -> Typ.Tint Typ.ILongLong
  | `Half -> Typ.Tint Typ.IShort (*?*)
  | `Float -> Typ.Tfloat Typ.FFloat
  | `Double -> Typ.Tfloat Typ.FDouble
  | `LongDouble -> Typ.Tfloat Typ.FLongDouble
  | `NullPtr -> Typ.Tint Typ.IInt
  | `ObjCId -> get_builtin_objc_type `ObjCId
  | `ObjCClass -> get_builtin_objc_type `ObjCClass
  | _ -> Typ.Tvoid

let pointer_attribute_of_objc_attribute attr_info =
  match attr_info.Clang_ast_t.ati_lifetime with
  | `OCL_None | `OCL_Strong -> Typ.Pk_pointer
  | `OCL_ExplicitNone -> Typ.Pk_objc_unsafe_unretained
  | `OCL_Weak -> Typ.Pk_objc_weak
  | `OCL_Autoreleasing -> Typ.Pk_objc_autoreleasing

let rec build_array_type translate_decl tenv type_ptr n_opt =
  let array_type = type_ptr_to_sil_type translate_decl tenv type_ptr in
  let len = Option.map ~f:(fun n -> IntLit.of_int64 (Int64.of_int n)) n_opt in
  Typ.Tarray (array_type, len)

and sil_type_of_attr_type translate_decl tenv type_info attr_info =
  match type_info.Clang_ast_t.ti_desugared_type with
  | Some type_ptr ->
      (match CAst_utils.get_type type_ptr with
       | Some Clang_ast_t.ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
           let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
           Typ.Tptr (typ, pointer_attribute_of_objc_attribute attr_info)
       | _ -> type_ptr_to_sil_type translate_decl tenv type_ptr)
  | None -> Typ.Tvoid

and sil_type_of_c_type translate_decl tenv c_type =
  let open Clang_ast_t in
  match c_type with
  | NoneType _ -> Typ.Tvoid
  | BuiltinType (_, builtin_type_kind) ->
      sil_type_of_builtin_type_kind builtin_type_kind
  | PointerType (_, {Clang_ast_t.qt_type_ptr})
  | ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
      let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
      if Typ.equal typ (get_builtin_objc_type `ObjCClass) then
        typ
      else Typ.Tptr (typ, Typ.Pk_pointer)
  | ObjCObjectType (_, objc_object_type_info) ->
      type_ptr_to_sil_type translate_decl tenv objc_object_type_info.Clang_ast_t.base_type
  | BlockPointerType (_, type_ptr) ->
      let typ = type_ptr_to_sil_type translate_decl tenv type_ptr in
      Typ.Tptr (typ, Typ.Pk_pointer)
  | IncompleteArrayType (_, type_ptr)
  | DependentSizedArrayType (_, type_ptr)
  | VariableArrayType (_, type_ptr) ->
      build_array_type translate_decl tenv type_ptr None
  | ConstantArrayType (_, type_ptr, n) ->
      build_array_type translate_decl tenv type_ptr (Some n)
  | FunctionProtoType _
  | FunctionNoProtoType _ ->
      Typ.Tfun false
  | ParenType (_, type_ptr) ->
      type_ptr_to_sil_type translate_decl tenv type_ptr
  | DecayedType (_, type_ptr) ->
      type_ptr_to_sil_type translate_decl tenv type_ptr
  | RecordType (_, pointer)
  | EnumType (_, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | ElaboratedType (type_info) ->
      (match type_info.Clang_ast_t.ti_desugared_type with
         Some type_ptr -> type_ptr_to_sil_type translate_decl tenv type_ptr
       | None -> Typ.Tvoid)
  | ObjCInterfaceType (_, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | RValueReferenceType (_, {Clang_ast_t.qt_type_ptr})
  | LValueReferenceType (_, {Clang_ast_t.qt_type_ptr}) ->
      let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
      Typ.Tptr (typ, Typ.Pk_reference)
  | AttributedType (type_info, attr_info) ->
      sil_type_of_attr_type translate_decl tenv type_info attr_info
  | _ -> (* TypedefType, etc *)
      let type_info = Clang_ast_proj.get_type_tuple c_type in
      match type_info.Clang_ast_t.ti_desugared_type with
      | Some typ -> type_ptr_to_sil_type translate_decl tenv typ
      | None -> Typ.Tvoid

and decl_ptr_to_sil_type translate_decl tenv decl_ptr =
  let open Clang_ast_t in
  let typ = `DeclPtr decl_ptr in
  try Clang_ast_types.TypePointerMap.find typ !CFrontend_config.sil_types_map
  with Not_found ->
  match CAst_utils.get_decl decl_ptr with
  | Some (CXXRecordDecl _ as d)
  | Some (RecordDecl _ as d)
  | Some (ClassTemplateSpecializationDecl _ as d)
  | Some (ObjCInterfaceDecl _ as d)
  | Some (ObjCImplementationDecl _ as d)
  | Some (ObjCProtocolDecl _ as d)
  | Some (ObjCCategoryDecl _ as d)
  | Some (ObjCCategoryImplDecl _ as d)
  | Some (EnumDecl _ as d) -> translate_decl tenv d
  | Some _ ->
      Logging.err_debug "Warning: Wrong decl found for  pointer %s "
        (Clang_ast_j.string_of_pointer decl_ptr);
      Typ.Tvoid
  | None ->
      Logging.err_debug "Warning: Decl pointer %s not found."
        (Clang_ast_j.string_of_pointer decl_ptr);
      Typ.Tvoid

and clang_type_ptr_to_sil_type translate_decl tenv type_ptr =
  try
    Clang_ast_types.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    (match CAst_utils.get_type type_ptr with
     | Some c_type ->
         let sil_type = sil_type_of_c_type translate_decl tenv c_type in
         CAst_utils.update_sil_types_map type_ptr sil_type;
         sil_type
     | _ -> Typ.Tvoid)

and prebuilt_type_to_sil_type type_ptr =
  try
    Clang_ast_types.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    Logging.out "Prebuilt type %s not found\n"
      (Clang_ast_types.type_ptr_to_string type_ptr);
    assert false

and type_ptr_to_sil_type translate_decl tenv type_ptr =
  match type_ptr with
  | `TPtr _ -> clang_type_ptr_to_sil_type translate_decl tenv type_ptr
  | `Prebuilt _ -> prebuilt_type_to_sil_type type_ptr
  | `PointerOf typ ->
      let sil_typ = type_ptr_to_sil_type translate_decl tenv typ in
      Typ.Tptr (sil_typ, Typ.Pk_pointer)
  | `ReferenceOf typ ->
      let sil_typ = type_ptr_to_sil_type translate_decl tenv typ in
      Typ.Tptr (sil_typ, Typ.Pk_reference)
  | `ClassType (name, lang) ->
      let kind = match lang with `OBJC -> Csu.Objc | `CPP -> Csu.CPP in
      Typ.Tstruct (CType.mk_classname name kind)
  | `StructType name -> Typ.Tstruct (CType.mk_structname name)
  | `DeclPtr ptr -> decl_ptr_to_sil_type translate_decl tenv ptr
  | `ErrorType -> Typ.Tvoid
