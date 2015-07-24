(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Translate an enumeration declaration by adding it to the tenv and *)
(** translating the code and adding it to a fake procdesc *)

val enum_decl : string -> Sil.tenv -> Cfg.cfg -> Cg.t -> string option ->
  Clang_ast_t.decl list -> Clang_ast_t.opt_type -> unit
