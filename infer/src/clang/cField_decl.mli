(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Utility module to retrieve fields of structs of classes *)
open CFrontend_utils

val fields_superclass : Sil.tenv -> Clang_ast_t.obj_c_interface_decl_info ->
  (Ident.fieldname * Sil.typ * Sil.item_annotation) list

type field_type = Ident.fieldname * Sil.typ * (Sil.annotation * bool) list

val get_fields : Ast_utils.type_ptr_to_sil_type -> Sil.tenv -> CContext.curr_class ->
  Clang_ast_t.decl list -> field_type list

val fields_superclass : Sil.tenv -> Clang_ast_t.obj_c_interface_decl_info -> field_type list

val build_sil_field : Ast_utils.type_ptr_to_sil_type -> Sil.tenv -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.type_ptr -> Clang_ast_t.property_attribute list -> field_type

val add_missing_fields : Sil.tenv -> string -> field_type list -> unit

val is_ivar_atomic : Ident.fieldname -> Sil.struct_fields -> bool

val get_property_corresponding_ivar : Sil.tenv -> Ast_utils.type_ptr_to_sil_type -> string ->
  Clang_ast_t.decl -> Ident.fieldname
