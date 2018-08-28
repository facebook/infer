(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Process variable declarations by saving them as local or global variables.  *)

(** Computes the local variables of a function or method to be added to the procdesc *)

val sil_var_of_decl : CContext.t -> Clang_ast_t.decl -> Typ.Procname.t -> Pvar.t

val sil_var_of_decl_ref :
  CContext.t -> Clang_ast_t.source_range -> Clang_ast_t.decl_ref -> Typ.Procname.t -> Pvar.t

val add_var_to_locals : Procdesc.t -> Clang_ast_t.decl -> Typ.t -> Pvar.t -> unit

val sil_var_of_captured_var :
     CContext.t
  -> Clang_ast_t.source_range
  -> Typ.Procname.t
  -> Clang_ast_t.decl_ref
  -> (Pvar.t * Typ.typ) option

val captured_vars_from_block_info :
     CContext.t
  -> Clang_ast_t.source_range
  -> Clang_ast_t.block_captured_variable list
  -> (Pvar.t * Typ.t) list
