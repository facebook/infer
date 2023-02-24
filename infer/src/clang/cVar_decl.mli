(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Process variable declarations by saving them as local or global variables. *)

val sil_var_of_decl : CContext.t -> Clang_ast_t.decl -> Procname.t -> Pvar.t
(** Computes the local variables of a function or method to be added to the procdesc *)

val sil_var_of_decl_ref :
  CContext.t -> Clang_ast_t.source_range -> Clang_ast_t.decl_ref -> Procname.t -> Pvar.t

val add_var_to_locals : Procdesc.t -> Clang_ast_t.decl -> Typ.t -> Pvar.t -> unit

val sil_var_of_captured_var :
     CContext.t
  -> Clang_ast_t.source_range
  -> Procname.t
  -> Clang_ast_t.decl_ref
  -> (Pvar.t * Typ.t * bool) option

val captured_vars_from_block_info :
     CContext.t
  -> Clang_ast_t.source_range
  -> Clang_ast_t.block_captured_variable list
  -> (Pvar.t * Typ.t * bool) list

val mk_temp_sil_var : Procdesc.t -> name:string -> Pvar.t

val mk_temp_sil_var_for_expr :
  CContext.t -> name:string -> clang_pointer:int -> Clang_ast_t.expr_info -> Pvar.t * Typ.t

val materialize_cpp_temporary :
  CContext.t -> Clang_ast_t.stmt_info -> Clang_ast_t.expr_info -> Pvar.t * Typ.t

val mk_sil_global_var :
     Tenv.t
  -> CFrontend_config.translation_unit_context
  -> ?mk_name:(string -> Mangled.t -> Mangled.t)
  -> Clang_ast_t.decl_info
  -> Clang_ast_t.named_decl_info
  -> Clang_ast_t.var_decl_info
  -> Clang_ast_t.template_instantiation_arg_info list option
  -> Clang_ast_t.qual_type
  -> Pvar.t
