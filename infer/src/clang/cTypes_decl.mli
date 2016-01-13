(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Processes types and record declarations by adding them to the tenv *)

val add_struct_to_tenv : Sil.tenv -> Sil.typ -> unit

val get_record_name : Clang_ast_t.decl -> string

val add_types_from_decl_to_tenv : Sil.tenv -> Clang_ast_t.decl -> Sil.typ

(* Adds the predefined types objc_class which is a struct, *)
(* and Class, which is a pointer to objc_class. *)
val add_predefined_types : Sil.tenv -> unit

val type_ptr_to_sil_type : Sil.tenv -> Clang_ast_t.type_ptr -> Sil.typ

val class_from_pointer_type : Sil.tenv -> Clang_ast_t.type_ptr -> string

val get_class_type_np : Sil.tenv -> Clang_ast_t.expr_info ->
  Clang_ast_t.obj_c_message_expr_info -> Sil.typ

val get_type_curr_class : Sil.tenv -> CContext.curr_class -> Sil.typ

val get_type_from_expr_info : Clang_ast_t.expr_info -> Sil.tenv -> Sil.typ

val type_name_to_sil_type : Sil.tenv -> string -> Sil.typ
