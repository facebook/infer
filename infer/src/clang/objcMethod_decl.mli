(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_methods :
  CAst_utils.procname_from_decl -> Tenv.t -> Clang_ast_t.decl list -> Typ.Procname.t list

val add_missing_methods : Tenv.t -> Typ.name -> Typ.Procname.t list -> unit
