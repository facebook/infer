(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

(** Utility module to retrieve fields of structs of classes *)

val get_fields : Sil.tenv -> CContext.curr_class -> Clang_ast_t.decl list ->
(Ident.fieldname * Sil.typ * Sil.item_annotation) list

val fields_superclass : Sil.tenv -> Clang_ast_t.obj_c_interface_decl_info ->
(Ident.fieldname * Sil.typ * Sil.item_annotation) list

val mk_class_field_name : string -> string -> Ident.fieldname