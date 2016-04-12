(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open CFrontend_utils

let get_builtin_objc_typename builtin_type =
  match builtin_type with
  | `ObjCId -> Typename.TN_csu (Csu.Struct, (Mangled.from_string CFrontend_config.objc_object))
  | `ObjCClass -> Typename.TN_csu (Csu.Struct, (Mangled.from_string CFrontend_config.objc_class))

let get_builtin_objc_type builtin_type =
  let typ = Sil.Tvar (get_builtin_objc_typename builtin_type) in
  match builtin_type with
  | `ObjCId -> typ
  | `ObjCClass -> Sil.Tptr (typ, Sil.Pk_pointer)


let sil_type_of_builtin_type_kind builtin_type_kind =
  match builtin_type_kind with
  | `Void -> Sil.Tvoid
  | `Bool -> Sil.Tint Sil.IBool
  | `Char_U -> Sil.Tint Sil.IUChar
  | `UChar -> Sil.Tint Sil.IUChar
  | `WChar_U -> Sil.Tint Sil.IUChar
  | `Char_S -> Sil.Tint Sil.IChar
  | `SChar -> Sil.Tint Sil.ISChar
  | `WChar_S
  | `Char16
  | `Char32 -> Sil.Tint Sil.IChar
  | `UShort
  | `Short -> Sil.Tint Sil.IShort
  | `UInt
  | `UInt128 -> Sil.Tint Sil.IUInt
  | `ULong -> Sil.Tint Sil.IULong
  | `ULongLong -> Sil.Tint Sil.IULongLong
  | `Int
  | `Int128 -> Sil.Tint Sil.IInt
  | `Long -> Sil.Tint Sil.ILong
  | `LongLong -> Sil.Tint Sil.ILongLong
  | `Half -> Sil.Tint Sil.IShort (*?*)
  | `Float -> Sil.Tfloat Sil.FFloat
  | `Double -> Sil.Tfloat Sil.FDouble
  | `LongDouble -> Sil.Tfloat Sil.FLongDouble
  | `NullPtr -> Sil.Tint Sil.IInt
  | `ObjCId -> get_builtin_objc_type `ObjCId
  | `ObjCClass -> get_builtin_objc_type `ObjCClass
  | _ -> Sil.Tvoid

let pointer_attribute_of_objc_attribute attr_info =
  match attr_info.Clang_ast_t.ati_lifetime with
  | `OCL_None | `OCL_Strong -> Sil.Pk_pointer
  | `OCL_ExplicitNone -> Sil.Pk_objc_unsafe_unretained
  | `OCL_Weak -> Sil.Pk_objc_weak
  | `OCL_Autoreleasing -> Sil.Pk_objc_autoreleasing

let rec build_array_type translate_decl tenv type_ptr n =
  let array_type = type_ptr_to_sil_type translate_decl tenv type_ptr in
  let exp = Sil.exp_int (Sil.Int.of_int64 (Int64.of_int n)) in
  Sil.Tarray (array_type, exp)

and sil_type_of_attr_type translate_decl tenv type_info attr_info =
  match type_info.Clang_ast_t.ti_desugared_type with
  | Some type_ptr ->
      (match Ast_utils.get_type type_ptr with
       | Some Clang_ast_t.ObjCObjectPointerType (_, type_ptr') ->
           let typ = type_ptr_to_sil_type translate_decl tenv type_ptr' in
           Sil.Tptr (typ, pointer_attribute_of_objc_attribute attr_info)
       | _ -> type_ptr_to_sil_type translate_decl tenv type_ptr)
  | None -> Sil.Tvoid

and sil_type_of_c_type translate_decl tenv c_type =
  let open Clang_ast_t in
  match c_type with
  | NoneType _ -> Sil.Tvoid
  | BuiltinType (_, builtin_type_kind) ->
      sil_type_of_builtin_type_kind builtin_type_kind
  | PointerType (_, type_ptr)
  | ObjCObjectPointerType (_, type_ptr) ->
      let typ = type_ptr_to_sil_type translate_decl tenv type_ptr in
      if Sil.typ_equal typ (get_builtin_objc_type `ObjCClass) then
        typ
      else Sil.Tptr (typ, Sil.Pk_pointer)
  | ObjCObjectType (_, objc_object_type_info) ->
      type_ptr_to_sil_type translate_decl tenv objc_object_type_info.Clang_ast_t.base_type
  | BlockPointerType (_, type_ptr) ->
      let typ = type_ptr_to_sil_type translate_decl tenv type_ptr in
      Sil.Tptr (typ, Sil.Pk_pointer)
  | IncompleteArrayType (_, type_ptr)
  | DependentSizedArrayType (_, type_ptr)
  | VariableArrayType (_, type_ptr) ->
      build_array_type translate_decl tenv type_ptr (-1)
  | ConstantArrayType (_, type_ptr, n) ->
      build_array_type translate_decl tenv type_ptr n
  | FunctionProtoType _
  | FunctionNoProtoType _ ->
      Sil.Tfun false
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
       | None -> Sil.Tvoid)
  | ObjCInterfaceType (_, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | RValueReferenceType (_, type_ptr)
  | LValueReferenceType (_, type_ptr) ->
      let typ = type_ptr_to_sil_type translate_decl tenv type_ptr in
      Sil.Tptr (typ, Sil.Pk_reference)
  | AttributedType (type_info, attr_info) ->
      sil_type_of_attr_type translate_decl tenv type_info attr_info
  | _ -> (* TypedefType, etc *)
      let type_info = Clang_ast_proj.get_type_tuple c_type in
      match type_info.Clang_ast_t.ti_desugared_type with
      | Some typ -> type_ptr_to_sil_type translate_decl tenv typ
      | None -> Sil.Tvoid

and decl_ptr_to_sil_type translate_decl tenv decl_ptr =
  let open Clang_ast_t in
  let typ = `DeclPtr decl_ptr in
  try Clang_ast_types.TypePointerMap.find typ !CFrontend_config.sil_types_map
  with Not_found ->
    match Ast_utils.get_decl decl_ptr with
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
        Printing.log_err "Warning: Wrong decl found for  pointer %s "
          (Clang_ast_j.string_of_pointer decl_ptr);
        Sil.Tvoid
    | None ->
        Printing.log_err "Warning: Decl pointer %s not found."
          (Clang_ast_j.string_of_pointer decl_ptr);
        Sil.Tvoid

and clang_type_ptr_to_sil_type translate_decl tenv type_ptr =
  try
    Clang_ast_types.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    (match Ast_utils.get_type type_ptr with
     | Some c_type ->
         let sil_type = sil_type_of_c_type translate_decl tenv c_type in
         Ast_utils.update_sil_types_map type_ptr sil_type;
         sil_type
     | _ -> Sil.Tvoid)

and prebuilt_type_to_sil_type type_ptr =
  try
    Clang_ast_types.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    Printing.log_stats "Prebuilt type %s not found\n"
      (Clang_ast_types.type_ptr_to_string type_ptr);
    assert false

and type_ptr_to_sil_type translate_decl tenv type_ptr =
  match type_ptr with
  | `TPtr _ -> clang_type_ptr_to_sil_type translate_decl tenv type_ptr
  | `Prebuilt _ -> prebuilt_type_to_sil_type type_ptr
  | `PointerOf typ ->
      let sil_typ = type_ptr_to_sil_type translate_decl tenv typ in
      Sil.Tptr (sil_typ, Sil.Pk_pointer)
  | `ClassType (name, lang) ->
      let kind = match lang with `OBJC -> Csu.Objc | `CPP -> Csu.CPP in
      Sil.Tvar (CTypes.mk_classname name kind)
  | `StructType name -> Sil.Tvar (CTypes.mk_structname name)
  | `DeclPtr ptr -> decl_ptr_to_sil_type translate_decl tenv ptr
  | `ErrorType -> Sil.Tvoid
