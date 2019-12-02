(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** In this module an ObjC protocol declaration or implementation is processed. The protocol is
    saved in the tenv as a struct with the corresponding methods *)

val protocol_decl : CAst_utils.qual_type_to_sil_type -> Tenv.t -> Clang_ast_t.decl -> Typ.desc
