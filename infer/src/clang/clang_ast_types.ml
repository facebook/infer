(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* This module adds more structure to some fields in AST *)
(* The implementation is replacement of default one from *)
(* facebook-clang-plugins repository *)


(* Type pointers *)

exception Not_Clang_Pointer

type t_ptr = [
  | `TPtr of int
  | `Prebuilt of int
  | `PointerOf of t_ptr
  | `ReferenceOf of t_ptr
  | `ClassType of Typ.Name.t
  | `StructType of Typ.Name.t
  | `DeclPtr of int
  | `ErrorType
] [@@deriving compare]

module TypePointerOrd = struct
  type t = t_ptr [@@deriving compare]
end

module TypePointerMap = Caml.Map.Make(TypePointerOrd)

let rec type_ptr_to_string type_ptr = match type_ptr with
  | `TPtr raw -> "clang_ptr_" ^ (string_of_int raw)
  | `Prebuilt raw -> "prebuilt_" ^ (string_of_int raw)
  | `PointerOf typ -> "pointer_of_" ^ type_ptr_to_string typ
  | `ReferenceOf typ -> "reference_of_" ^ type_ptr_to_string typ
  | `ClassType name -> "class_name_" ^ Typ.Name.name name
  | `StructType name -> "struct_name_" ^ Typ.Name.name name
  | `DeclPtr raw -> "decl_ptr_" ^ (string_of_int raw)
  | `ErrorType -> "error_type"

let type_ptr_to_clang_pointer type_ptr = match type_ptr with
  | `TPtr raw -> raw
  | _ -> raise Not_Clang_Pointer

let pointer_to_type_ptr raw = `TPtr raw

let type_ptr_to_pointer type_ptr = match type_ptr with
  | `TPtr raw -> raw
  | _ -> 0 (* invalid pointer *)


(* Source files *)

type src_file = SourceFile.t

let source_file_of_string = SourceFile.from_abs_path

let string_of_source_file = SourceFile.to_abs_path
