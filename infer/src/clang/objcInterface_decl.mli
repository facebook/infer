(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** In this module an ObjC interface declaration is processed. The class  *)
(** is saved in the tenv as a struct with the corresponding fields, potential superclass and *)
(** list of defined methods *)

val interface_declaration : Sil.tenv -> Clang_ast_t.decl_info -> string -> Clang_ast_t.decl list ->
  Clang_ast_t.obj_c_interface_decl_info -> CContext.curr_class

val find_field : Sil.tenv -> string -> Sil.typ option -> bool ->
  (Ident.fieldname * Sil.typ * Sil.item_annotation) option

val interface_impl_declaration : Sil.tenv -> string -> Clang_ast_t.decl list ->
  Clang_ast_t.obj_c_implementation_decl_info -> CContext.curr_class

val is_pointer_to_objc_class : Sil.tenv -> Sil.typ -> bool
