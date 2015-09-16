(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open CFrontend_utils

let custom_qual_type_to_sil_type type_pointer =
  if Utils.string_is_prefix "custom" type_pointer then
    let typ =
      (match CTypes.get_name_from_type_pointer type_pointer with
       | "custom_class_name", class_name -> Sil.Tvar (CTypes.mk_classname class_name)
       | "custom_pointer_custom_class_name", class_name ->
           Sil.Tptr (Sil.Tvar (CTypes.mk_classname class_name), Sil.Pk_pointer)
       | "custom_struct_name", struct_name -> Sil.Tvar (CTypes.mk_structname struct_name)
       | "custom_pointer_custom_struct_name", struct_name ->
           Sil.Tptr (Sil.Tvar (CTypes.mk_structname struct_name), Sil.Pk_pointer)
       | _ -> assert false) in
    Some typ
  else None

let get_builtin_objc_typename builtin_type =
  match builtin_type with
  | `ObjCId -> Sil.TN_csu (Sil.Struct, (Mangled.from_string CFrontend_config.objc_object))
  | `ObjCClass -> Sil.TN_csu (Sil.Struct, (Mangled.from_string CFrontend_config.objc_class))

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

let rec build_array_type translate_decl tenv type_ptr n =
  let array_type = qual_type_ptr_to_sil_type translate_decl tenv type_ptr in
  let exp = Sil.exp_int (Sil.Int.of_int64 (Int64.of_int n)) in
  Sil.Tarray (array_type, exp)

and sil_type_of_c_type translate_decl tenv c_type =
  let open Clang_ast_t in
  match c_type with
  | NoneType (type_info) -> Sil.Tvoid
  | BuiltinType (type_info, builtin_type_kind) ->
      sil_type_of_builtin_type_kind builtin_type_kind
  | PointerType (type_info, type_ptr)
  | ObjCObjectPointerType (type_info, type_ptr) ->
      let typ = qual_type_ptr_to_sil_type translate_decl tenv type_ptr in
      if Sil.typ_equal typ (get_builtin_objc_type `ObjCClass) then
        typ
      else Sil.Tptr (typ, Sil.Pk_pointer)
  | ObjCObjectType (type_info, objc_object_type_info) ->
      qual_type_ptr_to_sil_type translate_decl tenv objc_object_type_info.Clang_ast_t.base_type
  | BlockPointerType (type_info, type_ptr) ->
      let typ = qual_type_ptr_to_sil_type translate_decl tenv type_ptr in
      Sil.Tptr (typ, Sil.Pk_pointer)
  | IncompleteArrayType (type_info, type_ptr)
  | DependentSizedArrayType (type_info, type_ptr)
  | VariableArrayType (type_info, type_ptr) ->
      build_array_type translate_decl tenv type_ptr (-1)
  | ConstantArrayType (type_info, type_ptr, n) ->
      build_array_type translate_decl tenv type_ptr n
  | FunctionProtoType (type_info, function_type_info, _)
  | FunctionNoProtoType (type_info, function_type_info) ->
      Sil.Tfun false
  | ParenType (type_info, type_ptr) ->
      qual_type_ptr_to_sil_type translate_decl tenv type_ptr
  | DecayedType (type_info, type_ptr) ->
      qual_type_ptr_to_sil_type translate_decl tenv type_ptr
  | RecordType (type_info, pointer)
  | EnumType (type_info, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | ElaboratedType (type_info) ->
      (match type_info.Clang_ast_t.ti_desugared_type with
         Some type_ptr -> qual_type_ptr_to_sil_type translate_decl tenv type_ptr
       | None -> Sil.Tvoid)
  | ObjCInterfaceType (type_info, pointer) ->
      decl_ptr_to_sil_type translate_decl tenv pointer
  | LValueReferenceType (type_info, type_ptr) ->
      let typ = qual_type_ptr_to_sil_type translate_decl tenv type_ptr in
      Sil.Tptr (typ, Sil.Pk_reference)
  | AttributedType type_info ->
      (match type_info.Clang_ast_t.ti_desugared_type with
       | Some type_ptr ->
           (match Ast_utils.get_type type_ptr  with
            | Some ObjCObjectPointerType (type_info', type_ptr') ->
                let typ = qual_type_ptr_to_sil_type translate_decl tenv type_ptr' in
                CTypes.sil_type_of_attr_pointer_type typ type_info.Clang_ast_t.ti_raw
            | _ -> qual_type_ptr_to_sil_type translate_decl tenv type_ptr)
       | None -> Sil.Tvoid)
  | _ -> (* TypedefType, etc *)
      let type_info = Clang_ast_proj.get_type_tuple c_type in
      match type_info.Clang_ast_t.ti_desugared_type with
      | Some typ -> qual_type_ptr_to_sil_type translate_decl tenv typ
      | None -> Sil.Tvoid

and decl_ptr_to_sil_type translate_decl tenv decl_ptr =
  let open Clang_ast_t in
  try Clang_ast_main.PointerMap.find decl_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    match Ast_utils.get_decl decl_ptr with
    | Some (ObjCInterfaceDecl(decl_info, name_info, decl_list, decl_context_info, oidi)) ->
        Sil.Tvar (CTypes.mk_classname name_info.Clang_ast_t.ni_name)
    | Some (CXXRecordDecl _ as d)
    | Some (RecordDecl _ as d) -> translate_decl tenv None d
    | Some (EnumDecl(_, name_info, _, _, _, _, _) ) ->
        Sil.Tvar (CTypes.mk_enumname name_info.Clang_ast_t.ni_name)
    | Some _ ->
        Printing.log_err "Warning: Wrong decl found for  pointer %s "
          (Clang_ast_j.string_of_pointer decl_ptr);
        Sil.Tvoid
    | None ->
        Printing.log_err "Warning: Decl pointer %s not found."
          (Clang_ast_j.string_of_pointer decl_ptr);
        Sil.Tvoid

and qual_type_ptr_to_sil_type translate_decl tenv type_ptr =
  try
    Clang_ast_main.PointerMap.find type_ptr !CFrontend_config.sil_types_map
  with Not_found ->
    match Ast_utils.get_type type_ptr with
    | Some c_type ->
        let sil_type = sil_type_of_c_type translate_decl tenv c_type in
        Ast_utils.update_sil_types_map type_ptr sil_type;
        sil_type
    | _ -> Sil.Tvoid

and qual_type_to_sil_type translate_decl tenv qt =
  let type_ptr = qt.Clang_ast_t.qt_type_ptr in
  match custom_qual_type_to_sil_type type_ptr with
  | Some typ -> typ
  | None -> qual_type_ptr_to_sil_type translate_decl tenv type_ptr
