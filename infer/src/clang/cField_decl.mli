(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module to retrieve fields of structs of classes *)

type field_type = Ident.fieldname * Typ.t * (Annot.t * bool) list

val get_fields : CAst_utils.type_ptr_to_sil_type -> Tenv.t -> CContext.curr_class ->
  Clang_ast_t.decl list -> field_type list

val fields_superclass :
  Tenv.t -> Clang_ast_t.obj_c_interface_decl_info -> Csu.class_kind -> field_type list

val build_sil_field : CAst_utils.type_ptr_to_sil_type -> Tenv.t -> Clang_ast_t.named_decl_info ->
  Clang_ast_t.type_ptr -> Clang_ast_t.property_attribute list -> field_type

val add_missing_fields : Tenv.t -> string -> Csu.class_kind -> field_type list -> unit

val modelled_field : Clang_ast_t.named_decl_info -> field_type list
