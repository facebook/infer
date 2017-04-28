(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** In this module an ObjC interface declaration is processed. The class is saved in the tenv as a
    struct with the corresponding fields, potential superclass and list of defined methods *)

val interface_declaration : CAst_utils.qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl ->
  Typ.desc

val interface_impl_declaration : CAst_utils.qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl ->
  Typ.desc

val is_pointer_to_objc_class : Typ.t -> bool
