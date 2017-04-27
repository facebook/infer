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

type field_type = Fieldname.t * Typ.t * (Annot.t * bool) list

val get_fields : CAst_utils.qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl list ->
  field_type list

val fields_superclass : Tenv.t -> Clang_ast_t.obj_c_interface_decl_info -> field_type list

val add_missing_fields : Tenv.t -> QualifiedCppName.t -> field_type list -> unit

val modelled_field : Clang_ast_t.named_decl_info -> field_type list
