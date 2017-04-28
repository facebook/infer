(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Processes types and record declarations by adding them to the tenv *)

val get_record_typename : ?tenv:Tenv.t -> Clang_ast_t.decl -> Typ.Name.t

val add_types_from_decl_to_tenv : Tenv.t -> Clang_ast_t.decl -> Typ.desc

(* Adds the predefined types objc_class which is a struct, *)
(* and Class, which is a pointer to objc_class. *)
val add_predefined_types : Tenv.t -> unit

val qual_type_to_sil_type : Tenv.t -> Clang_ast_t.qual_type -> Typ.t

val class_from_pointer_type : Tenv.t -> Clang_ast_t.qual_type -> Typ.Name.t

val get_class_type_np : Tenv.t -> Clang_ast_t.expr_info ->
  Clang_ast_t.obj_c_message_expr_info -> Typ.t

val get_type_from_expr_info : Clang_ast_t.expr_info -> Tenv.t -> Typ.t
