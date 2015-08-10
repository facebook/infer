(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Processes types and record declarations by adding them to the tenv *)

val get_declaration_type : Sil.tenv -> string option -> Clang_ast_t.decl_info -> string ->
  Clang_ast_t.opt_type -> Clang_ast_t.decl list -> Clang_ast_t.decl_context_info ->
  Clang_ast_t.record_decl_info -> Sil.typ

val add_struct_to_tenv : Sil.tenv -> Sil.typ -> unit

val get_record_name : Clang_ast_t.opt_type -> string

val get_method_decls : Clang_ast_t.decl -> Clang_ast_t.decl list -> (Clang_ast_t.decl * Clang_ast_t.decl) list

val do_typedef_declaration : Sil.tenv -> string option -> Clang_ast_t.decl_info -> string ->
  Clang_ast_t.opt_type -> Clang_ast_t.typedef_decl_info -> unit

val do_record_declaration : Sil.tenv -> string option -> Clang_ast_t.decl_info -> string ->
  Clang_ast_t.opt_type -> Clang_ast_t.decl list -> Clang_ast_t.decl_context_info ->
  Clang_ast_t.record_decl_info -> unit

val parse_func_type : string -> string -> (Sil.typ * Sil.typ list) option

(* Adds the predefined types objc_class which is a struct, *)
(* and Class, which is a pointer to objc_class. *)
val add_predefined_types : Sil.tenv -> unit

val qual_type_to_sil_type : Sil.tenv -> Clang_ast_t.qual_type -> Sil.typ

val qual_type_to_sil_type_np : Sil.tenv -> Clang_ast_t.qual_type -> Sil.typ

val class_from_pointer_type : Sil.tenv -> Clang_ast_t.qual_type -> string

val get_class_type_np : Sil.tenv -> Clang_ast_t.expr_info ->
  Clang_ast_t.obj_c_message_expr_info -> Sil.typ

val extract_sil_type_from_stmt : Sil.tenv -> Clang_ast_t.stmt -> Sil.typ

val get_type_curr_class : Sil.tenv -> CContext.curr_class -> Sil.typ

val expand_structured_type : Sil.tenv -> Sil.typ -> Sil.typ

val get_type_from_expr_info : Clang_ast_t.expr_info -> Sil.tenv -> Sil.typ

val type_name_to_sil_type : Sil.tenv -> string -> Sil.typ
