(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Utility module for retrieving types *)

val lookup_var_type : CContext.t -> Sil.pvar ->  Sil.typ

val add_pointer_to_typ : Sil.typ -> Sil.typ

val get_raw_qual_type_decl_ref_exp_info : Clang_ast_t.decl_ref_expr_info -> string option

val extract_type_from_stmt : Clang_ast_t.stmt -> Clang_ast_t.qual_type

val get_type : Clang_ast_t.qual_type -> string

val search_enum_type_by_name : Sil.tenv -> string -> Sil.const option

val classname_of_type : Sil.typ -> string

val mk_classname : string -> Sil.typename

val mk_structname : string -> Sil.typename

val mk_enumname : string -> Sil.typename

val get_name_from_struct: Sil.typ -> Mangled.t

val remove_pointer_to_typ : Sil.typ -> Sil.typ

val is_class : Sil.typ -> bool

val sil_type_of_attr_pointer_type : Sil.typ -> string -> Sil.typ

val return_type_of_function_type : Clang_ast_t.qual_type -> Clang_ast_t.pointer

val expand_structured_type : Sil.tenv -> Sil.typ -> Sil.typ

val get_name_from_type_pointer : string -> string * string
