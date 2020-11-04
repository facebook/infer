(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module for retrieving types *)

module L = Logging

let add_pointer_to_typ typ = Typ.mk (Tptr (typ, Typ.Pk_pointer))

let objc_classname_of_desc desc =
  match (desc : Typ.desc) with
  | Tstruct name ->
      name
  | Tfun ->
      Typ.Name.Objc.from_string CFrontend_config.objc_object
  | _ ->
      L.(debug Capture Verbose)
        "Classname of type cannot be extracted in %s" (Typ.desc_to_string desc) ;
      Typ.Name.Objc.from_string "undefined"


let objc_classname_of_type Typ.{desc} = objc_classname_of_desc desc

let is_class typ =
  match typ.Typ.desc with
  | Typ.Tptr ({desc= Tstruct name}, _) ->
      String.equal (Typ.Name.name name) CFrontend_config.objc_class
  | _ ->
      false


let rec return_type_of_function_qual_type (qual_type : Clang_ast_t.qual_type) =
  let open Clang_ast_t in
  match CAst_utils.get_type qual_type.qt_type_ptr with
  | Some (FunctionProtoType (_, function_type_info, _))
  | Some (FunctionNoProtoType (_, function_type_info)) ->
      function_type_info.Clang_ast_t.fti_return_type
  | Some (BlockPointerType (_, in_qual)) ->
      return_type_of_function_qual_type in_qual
  | Some _ ->
      L.(debug Capture Verbose)
        "Warning: Type pointer %s is not a function type."
        (Clang_ast_extend.type_ptr_to_string qual_type.qt_type_ptr) ;
      {qual_type with qt_type_ptr= Clang_ast_extend.ErrorType}
  | None ->
      L.(debug Capture Verbose)
        "Warning: Type pointer %s not found."
        (Clang_ast_extend.type_ptr_to_string qual_type.qt_type_ptr) ;
      {qual_type with qt_type_ptr= Clang_ast_extend.ErrorType}


let return_type_of_function_type qual_type = return_type_of_function_qual_type qual_type

let is_block_type {Clang_ast_t.qt_type_ptr} =
  let open Clang_ast_t in
  match CAst_utils.get_desugared_type qt_type_ptr with
  | Some (BlockPointerType _) ->
      true
  | _ ->
      false


let is_reference_type {Clang_ast_t.qt_type_ptr} =
  match CAst_utils.get_desugared_type qt_type_ptr with
  | Some (Clang_ast_t.LValueReferenceType _) ->
      true
  | Some (Clang_ast_t.RValueReferenceType _) ->
      true
  | _ ->
      false


let is_pointer_to_const {Clang_ast_t.qt_type_ptr} =
  match CAst_utils.get_type qt_type_ptr with
  | Some (PointerType (_, {Clang_ast_t.qt_is_const}))
  | Some (ObjCObjectPointerType (_, {Clang_ast_t.qt_is_const}))
  | Some (RValueReferenceType (_, {Clang_ast_t.qt_is_const}))
  | Some (LValueReferenceType (_, {Clang_ast_t.qt_is_const})) ->
      qt_is_const
  | _ ->
      false
