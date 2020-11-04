(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module for retrieving types *)

val add_pointer_to_typ : Typ.t -> Typ.t

val objc_classname_of_type : Typ.t -> Typ.Name.t

val objc_classname_of_desc : Typ.desc -> Typ.Name.t

val is_class : Typ.t -> bool

val return_type_of_function_type : Clang_ast_t.qual_type -> Clang_ast_t.qual_type

val is_block_type : Clang_ast_t.qual_type -> bool

val is_reference_type : Clang_ast_t.qual_type -> bool

val is_pointer_to_const : Clang_ast_t.qual_type -> bool
