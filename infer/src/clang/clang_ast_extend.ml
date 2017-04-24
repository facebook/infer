(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* This module adds more variants to some types in AST *)
(* The implementation extends default one from *)
(* facebook-clang-plugins repository *)


(* Type pointers *)
type Clang_ast_types.TypePtr.t +=
  | Builtin of Clang_ast_t.builtin_type_kind
  | PointerOf of Clang_ast_types.TypePtr.t
  | ReferenceOf of Clang_ast_types.TypePtr.t
  | ClassType of Typ.Name.t
  | DeclPtr of int
  | ErrorType

module TypePointerOrd = struct
  type t = Clang_ast_types.TypePtr.t
  let rec compare a1 a2 = match a1, a2 with
    | _ when phys_equal a1 a2 -> 0
    | Clang_ast_types.TypePtr.Ptr a, Clang_ast_types.TypePtr.Ptr b -> Int.compare a b
    | Clang_ast_types.TypePtr.Ptr _, _ -> 1
    | _, Clang_ast_types.TypePtr.Ptr _ -> -1
    | Builtin a, Builtin b -> Polymorphic_compare.compare a b
    | Builtin _, _ -> 1
    | _, Builtin _ -> -1
    | PointerOf a, PointerOf b -> compare a b
    | PointerOf _, _ -> 1
    | _, PointerOf _ -> -1
    | ReferenceOf a, ReferenceOf b -> compare a b
    | ReferenceOf _, _ -> 1
    | _, ReferenceOf _ -> -1
    | ClassType a, ClassType b -> Typ.Name.compare a b
    | ClassType _, _ -> 1
    | _, ClassType _ -> -1
    | DeclPtr a, DeclPtr b -> Int.compare a b
    | DeclPtr _, _ -> 1
    | _, DeclPtr _ -> -1
    | ErrorType, ErrorType -> 0
    | _ -> raise (invalid_arg ("unexpected type_ptr variants: "))
end

module TypePointerMap = Caml.Map.Make(TypePointerOrd)


let rec type_ptr_to_string = function
  | Clang_ast_types.TypePtr.Ptr raw -> "clang_ptr_" ^ (string_of_int raw)
  | Builtin t -> "sil_" ^ (Clang_ast_j.string_of_builtin_type_kind t)
  | PointerOf typ ->  "pointer_of_" ^ type_ptr_to_string typ
  | ReferenceOf typ -> "reference_of_" ^ type_ptr_to_string typ
  | ClassType name -> "class_name_" ^ Typ.Name.name name
  | DeclPtr raw -> "decl_ptr_" ^ (string_of_int raw)
  | ErrorType -> "error_type"
  | _ -> "unknown"
