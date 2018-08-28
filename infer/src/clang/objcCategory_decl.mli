(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In this module an ObjC category declaration or implementation is processed. The category    *)

(** is saved in the tenv as a struct with the corresponding fields and methods , and the class it belongs to *)

val category_decl :
     CAst_utils.qual_type_to_sil_type
  -> CAst_utils.procname_from_decl
  -> Tenv.t
  -> Clang_ast_t.decl
  -> Typ.desc

val category_impl_decl :
     CAst_utils.qual_type_to_sil_type
  -> CAst_utils.procname_from_decl
  -> Tenv.t
  -> Clang_ast_t.decl
  -> Typ.desc

val get_base_class_name_from_category : Clang_ast_t.decl -> Typ.Name.t option
