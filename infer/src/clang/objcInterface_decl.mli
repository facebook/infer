(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In this module an ObjC interface declaration is processed. The class is saved in the tenv as a
    struct with the corresponding fields, potential superclass and list of defined methods *)

val interface_declaration :
     CAst_utils.qual_type_to_sil_type
  -> CAst_utils.procname_from_decl
  -> Tenv.t
  -> Clang_ast_t.decl
  -> Typ.desc

val interface_impl_declaration :
     CAst_utils.qual_type_to_sil_type
  -> CAst_utils.procname_from_decl
  -> Tenv.t
  -> Clang_ast_t.decl
  -> Typ.desc
