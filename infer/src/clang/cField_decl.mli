(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module to retrieve fields of structs of classes *)

val get_fields :
     CAst_utils.qual_type_to_sil_type
  -> Tenv.t
  -> Typ.Name.t
  -> Clang_ast_t.decl list
  -> Struct.field list

val fields_superclass : Tenv.t -> Clang_ast_t.decl_ref option -> Struct.field list

val modelled_field : Clang_ast_t.named_decl_info -> Struct.field list
