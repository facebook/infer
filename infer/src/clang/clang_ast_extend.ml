(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* This module adds more variants to some types in AST The implementation extends default one from
   the facebook-clang-plugins repository *)

module L = Logging

(* Type pointers *)
type Clang_ast_types.TypePtr.t +=
  | Builtin of Clang_ast_t.builtin_type_kind
  | PointerOf of Clang_ast_t.qual_type
  | ReferenceOf of Clang_ast_t.qual_type
  | ClassType of Typ.Name.t
  | DeclPtr of int
  | ErrorType

let rec type_ptr_to_string = function
  | Clang_ast_types.TypePtr.Ptr raw ->
      "clang_ptr_" ^ string_of_int raw
  | Builtin t ->
      "sil_" ^ Clang_ast_j.string_of_builtin_type_kind t
  | PointerOf typ ->
      "pointer_of_" ^ type_ptr_to_string typ.Clang_ast_t.qt_type_ptr
  | ReferenceOf typ ->
      "reference_of_" ^ type_ptr_to_string typ.Clang_ast_t.qt_type_ptr
  | ClassType name ->
      "class_name_" ^ Typ.Name.name name
  | DeclPtr raw ->
      "decl_ptr_" ^ string_of_int raw
  | ErrorType ->
      "error_type"
  | _ ->
      "unknown"


module TypePointerOrd = struct
  type t = Clang_ast_types.TypePtr.t

  let rec compare a1 a2 =
    match (a1, a2) with
    | _ when phys_equal a1 a2 ->
        0
    | Clang_ast_types.TypePtr.Ptr a, Clang_ast_types.TypePtr.Ptr b ->
        Int.compare a b
    | Clang_ast_types.TypePtr.Ptr _, _ ->
        1
    | _, Clang_ast_types.TypePtr.Ptr _ ->
        -1
    | Builtin a, Builtin b ->
        Poly.compare a b
    | Builtin _, _ ->
        1
    | _, Builtin _ ->
        -1
    | PointerOf a, PointerOf b ->
        compare_qual_type a b
    | PointerOf _, _ ->
        1
    | _, PointerOf _ ->
        -1
    | ReferenceOf a, ReferenceOf b ->
        compare_qual_type a b
    | ReferenceOf _, _ ->
        1
    | _, ReferenceOf _ ->
        -1
    | ClassType a, ClassType b ->
        Typ.Name.compare a b
    | ClassType _, _ ->
        1
    | _, ClassType _ ->
        -1
    | DeclPtr a, DeclPtr b ->
        Int.compare a b
    | DeclPtr _, _ ->
        1
    | _, DeclPtr _ ->
        -1
    | ErrorType, ErrorType ->
        0
    | t1, t2 ->
        L.(die InternalError)
          "unexpected type_ptr variants: %s, %s" (type_ptr_to_string t1) (type_ptr_to_string t2)


  and compare_qual_type (qt1 : Clang_ast_t.qual_type) (qt2 : Clang_ast_t.qual_type) =
    if phys_equal qt1 qt2 then 0
    else
      (* enable warning here to warn and update comparison funtion when new field is added *)
      let[@warning "+missing-record-field-pattern"] { Clang_ast_t.qt_type_ptr= t1
                                                    ; qt_is_const= c1
                                                    ; qt_is_restrict= r1
                                                    ; qt_is_volatile= v1 } =
        qt1
      in
      let[@warning "+missing-record-field-pattern"] { Clang_ast_t.qt_type_ptr= t2
                                                    ; qt_is_const= c2
                                                    ; qt_is_restrict= r2
                                                    ; qt_is_volatile= v2 } =
        qt2
      in
      let qt_cmp = compare t1 t2 in
      if qt_cmp <> 0 then qt_cmp
      else
        let const_cmp = Bool.compare c1 c2 in
        if const_cmp <> 0 then const_cmp
        else
          let restrict_cmp = Bool.compare r1 r2 in
          if restrict_cmp <> 0 then restrict_cmp else Bool.compare v1 v2
end

module TypePointerMap = Caml.Map.Make (TypePointerOrd)
