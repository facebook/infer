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
  | `ObjCId -> Typ.Name.C.from_string CFrontend_config.objc_object
  | `ObjCClass -> Typ.Name.C.from_string CFrontend_config.objc_class

let get_builtin_objc_type builtin_type =
  let typ = Typ.mk (Tstruct (get_builtin_objc_typename builtin_type)) in
  match builtin_type with
  | `ObjCId -> typ
  | `ObjCClass -> Typ.mk (Tptr (typ, Typ.Pk_pointer))


let sil_type_of_builtin_type_kind builtin_type_kind =
  match builtin_type_kind with
  | `Void -> Typ.mk Tvoid
  | `Bool -> Typ.mk (Tint IBool)
  | `Char_U -> Typ.mk (Tint IUChar)
  | `UChar -> Typ.mk (Tint IUChar)
  | `WChar_U -> Typ.mk (Tint IUChar)
  | `Char_S -> Typ.mk (Tint IChar)
  | `SChar -> Typ.mk (Tint ISChar)
  | `WChar_S
  | `Char16
  | `Char32 -> Typ.mk (Tint IChar)
  | `UShort
  | `Short -> Typ.mk (Tint IShort)
  | `UInt
  | `UInt128 -> Typ.mk (Tint IUInt)
  | `ULong -> Typ.mk (Tint IULong)
  | `ULongLong -> Typ.mk (Tint IULongLong)
  | `Int
  | `Int128 -> Typ.mk (Tint IInt)
  | `Long -> Typ.mk (Tint ILong)
  | `LongLong -> Typ.mk (Tint ILongLong)
  | `Half -> Typ.mk (Tint IShort) (*?*)
  | `Float -> Typ.mk (Tfloat FFloat)
  | `Double -> Typ.mk (Tfloat FDouble)
  | `LongDouble -> Typ.mk (Tfloat FLongDouble)
  | `NullPtr -> Typ.mk (Tint IInt)
  | `ObjCId -> get_builtin_objc_type `ObjCId
  | `ObjCClass -> get_builtin_objc_type `ObjCClass
  | _ -> Typ.mk Tvoid

let pointer_attribute_of_objc_attribute attr_info =
  match attr_info.Clang_ast_t.ati_lifetime with
  | `OCL_None | `OCL_Strong -> Typ.Pk_pointer
  | `OCL_ExplicitNone -> Typ.Pk_objc_unsafe_unretained
  | `OCL_Weak -> Typ.Pk_objc_weak
  | `OCL_Autoreleasing -> Typ.Pk_objc_autoreleasing

let rec build_array_type translate_decl tenv type_ptr n_opt =
  let array_type = type_ptr_to_sil_type translate_decl tenv type_ptr in
  let len = Option.map ~f:(fun n -> IntLit.of_int64 (Int64.of_int n)) n_opt in
  Typ.mk (Tarray (array_type, len))

and sil_type_of_attr_type translate_decl tenv type_info attr_info =
  match type_info.Clang_ast_t.ti_desugared_type with
  | Some type_ptr ->
      (match CAst_utils.get_type type_ptr with
       | Some Clang_ast_t.ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
           let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
           Typ.mk (Tptr (typ, pointer_attribute_of_objc_attribute attr_info))
       | _ -> type_ptr_to_sil_type translate_decl tenv type_ptr)
  | None -> Typ.mk Tvoid

and sil_type_of_c_type translate_decl tenv c_type : Typ.t =
  let open Clang_ast_t in
  match c_type with
  | NoneType _ -> Typ.mk Tvoid
  | BuiltinType (_, builtin_type_kind) ->
      sil_type_of_builtin_type_kind builtin_type_kind
  | PointerType (_, {Clang_ast_t.qt_type_ptr})
  | ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr}) ->
      let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
      if Typ.equal typ (get_builtin_objc_type `ObjCClass) then
        typ
      else Typ.mk (Tptr (typ, Typ.Pk_pointer))
  | ObjCObjectType (_, objc_object_type_info) ->
      type_ptr_to_sil_type translate_decl tenv objc_object_type_info.Clang_ast_t.base_type
  | BlockPointerType (_, type_ptr) ->
      let typ = type_ptr_to_sil_type translate_decl tenv type_ptr in
      Typ.mk (Tptr (typ, Typ.Pk_pointer))
  | IncompleteArrayType (_, type_ptr)
  | DependentSizedArrayType (_, type_ptr)
  | VariableArrayType (_, type_ptr) ->
      build_array_type translate_decl tenv type_ptr None
  | ConstantArrayType (_, type_ptr, n) ->
      build_array_type translate_decl tenv type_ptr (Some n)
  | FunctionProtoType _
  | FunctionNoProtoType _ ->
      Typ.mk (Tfun false)
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
       | None -> Typ.mk Tvoid)
  | ObjCInterfaceType (_, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | RValueReferenceType (_, {Clang_ast_t.qt_type_ptr})
  | LValueReferenceType (_, {Clang_ast_t.qt_type_ptr}) ->
      let typ = type_ptr_to_sil_type translate_decl tenv qt_type_ptr in
      Typ.mk (Tptr (typ, Typ.Pk_reference))
  | AttributedType (type_info, attr_info) ->
      sil_type_of_attr_type translate_decl tenv type_info attr_info
  | _ -> (* TypedefType, etc *)
      let type_info = Clang_ast_proj.get_type_tuple c_type in
      match type_info.Clang_ast_t.ti_desugared_type with
      | Some typ -> type_ptr_to_sil_type translate_decl tenv typ
      | None -> Typ.mk Tvoid

and decl_ptr_to_sil_type translate_decl tenv decl_ptr =
  let open Clang_ast_t in
  let typ = Clang_ast_extend.DeclPtr decl_ptr in
  try Clang_ast_extend.TypePointerMap.find typ !CFrontend_config.sil_types_map
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
      Typ.mk Tvoid
  | None ->
      Logging.err_debug "Warning: Decl pointer %s not found."
        (Clang_ast_j.string_of_pointer decl_ptr);
      Typ.mk Tvoid

and clang_type_ptr_to_sil_type translate_decl tenv type_ptr =
  try
    Clang_ast_extend.TypePointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    (match CAst_utils.get_type type_ptr with
     | Some c_type ->
         let sil_type = sil_type_of_c_type translate_decl tenv c_type in
         CAst_utils.update_sil_types_map type_ptr sil_type;
         sil_type
     | _ -> Typ.mk Tvoid)

and type_ptr_to_sil_type translate_decl tenv type_ptr =
  match type_ptr with
  | Clang_ast_types.TypePtr.Ptr _ -> clang_type_ptr_to_sil_type translate_decl tenv type_ptr
  | Clang_ast_extend.Builtin kind -> sil_type_of_builtin_type_kind kind
  | Clang_ast_extend.PointerOf typ ->
      let sil_typ = type_ptr_to_sil_type translate_decl tenv typ in
      Typ.mk (Tptr (sil_typ, Pk_pointer))
  | Clang_ast_extend.ReferenceOf typ ->
      let sil_typ = type_ptr_to_sil_type translate_decl tenv typ in
      Typ.mk (Tptr (sil_typ, Pk_reference))
  | Clang_ast_extend.ClassType typename ->
      Typ.mk (Tstruct typename)
  | Clang_ast_extend.DeclPtr ptr -> decl_ptr_to_sil_type translate_decl tenv ptr
  | Clang_ast_extend.ErrorType -> Typ.mk Tvoid
  | _ -> raise (invalid_arg "unknown variant for type_ptr")
