(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Process variable declarations by saving them as local or global variables.  *)
(** Computes the local variables of a function or method to be added to the procdesc *)

val sil_var_of_decl : CContext.t -> Clang_ast_t.decl -> Typ.Procname.t -> Pvar.t

val sil_var_of_decl_ref : CContext.t -> Clang_ast_t.decl_ref -> Typ.Procname.t -> Pvar.t

val add_var_to_locals : Procdesc.t -> Clang_ast_t.decl -> Typ.t -> Pvar.t -> unit

val compute_autorelease_pool_vars : CContext.t -> Clang_ast_t.stmt list -> (Exp.t * Typ.t) list

val captured_vars_from_block_info : CContext.t -> Clang_ast_t.block_captured_variable list ->
  (Pvar.t * Typ.t) list
