(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module for retrieving types *)

val add_pointer_to_typ : Typ.t -> Typ.t

val objc_classname_of_type : Typ.t -> Typ.Name.t

val remove_pointer_to_typ : Typ.t -> Typ.t

val is_class : Typ.t -> bool

val return_type_of_function_type : Clang_ast_t.type_ptr -> Clang_ast_t.type_ptr

val is_block_type : Clang_ast_t.type_ptr -> bool

val is_reference_type : Clang_ast_t.type_ptr -> bool

val get_name_from_type_pointer : string -> string * string
