(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val get_methods : Typ.name -> Clang_ast_t.decl list -> Typ.Procname.t list

val add_missing_methods : Tenv.t -> Typ.name -> Typ.Procname.t list -> unit
