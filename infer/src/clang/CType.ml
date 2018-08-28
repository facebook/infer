(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module for retrieving types *)

module L = Logging

let add_pointer_to_typ typ = Typ.mk (Tptr (typ, Typ.Pk_pointer))

let objc_classname_of_type typ =
  match typ.Typ.desc with
  | Typ.Tstruct name ->
      name
  | Typ.Tfun _ ->
      Typ.Name.Objc.from_string CFrontend_config.objc_object
  | _ ->
      L.(debug Capture Verbose)
        "Classname of type cannot be extracted in type %s" (Typ.to_string typ) ;
      Typ.Name.Objc.from_string "undefined"


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


let is_value {Clang_ast_t.qt_type_ptr} =
  match qt_type_ptr with
  | Clang_ast_extend.Builtin _
  (* We rely on the assumption here that Clang_ast_extend.ReferenceOf is only created for pass-by-value structs. *)
  (* TODO: Create a dedicated variant in Clang_ast_extend for pass-by-val params *)
  | Clang_ast_extend.ReferenceOf _ ->
      true
  | Clang_ast_types.TypePtr.Ptr _ ->
      let rec is_value_raw qt_type_ptr =
        match CAst_utils.get_type qt_type_ptr with
        | Some (BuiltinType _)
        | Some (ComplexType _)
        | Some (DependentSizedExtVectorType _)
        | Some (VectorType _)
        | Some (ExtVectorType _)
        | Some (RecordType _)
        | Some (EnumType _)
        | Some (InjectedClassNameType _)
        | Some (ObjCObjectType _)
        | Some (ObjCInterfaceType _) ->
            true
        | Some (AdjustedType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (DecayedType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (ParenType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (DecltypeType (_, {Clang_ast_t.qt_type_ptr}))
        | Some (AtomicType (_, {Clang_ast_t.qt_type_ptr})) ->
            is_value_raw qt_type_ptr
        | Some (TypedefType (_, {Clang_ast_t.tti_child_type})) ->
            is_value_raw tti_child_type.Clang_ast_t.qt_type_ptr
        (* These types could be value types, and we try our best to resolve them *)
        | Some (AttributedType ({Clang_ast_t.ti_desugared_type}, _))
        | Some (TypeOfExprType {Clang_ast_t.ti_desugared_type})
        | Some (TypeOfType {Clang_ast_t.ti_desugared_type})
        | Some (UnaryTransformType {Clang_ast_t.ti_desugared_type})
        | Some (ElaboratedType {Clang_ast_t.ti_desugared_type})
        | Some (AutoType {Clang_ast_t.ti_desugared_type})
        | Some (DependentNameType {Clang_ast_t.ti_desugared_type})
        | Some (DeducedTemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (TemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (DependentTemplateSpecializationType {Clang_ast_t.ti_desugared_type})
        | Some (TemplateTypeParmType {Clang_ast_t.ti_desugared_type})
        | Some (SubstTemplateTypeParmType {Clang_ast_t.ti_desugared_type})
        | Some (SubstTemplateTypeParmPackType {Clang_ast_t.ti_desugared_type})
        | Some (PackExpansionType {Clang_ast_t.ti_desugared_type})
        | Some (UnresolvedUsingType {Clang_ast_t.ti_desugared_type}) -> (
          match ti_desugared_type with Some ptr -> is_value_raw ptr | None -> false )
        (* These types are known to be non-value types *)
        | Some (PointerType _)
        | Some (BlockPointerType _)
        | Some (LValueReferenceType _)
        | Some (RValueReferenceType _)
        | Some (MemberPointerType _)
        | Some (ConstantArrayType _)
        | Some (IncompleteArrayType _)
        | Some (VariableArrayType _)
        | Some (DependentSizedArrayType _)
        | Some (FunctionProtoType _)
        | Some (FunctionNoProtoType _)
        | Some (ObjCObjectPointerType _)
        | Some (NoneType _)
        | Some (DependentAddressSpaceType _)
        (* These types I don't know what they are. Be conservative and treat them as non value types *)
        | Some (ObjCTypeParamType _)
        | Some (PipeType _)
        | None ->
            false
      in
      is_value_raw qt_type_ptr
  | _ ->
      false
