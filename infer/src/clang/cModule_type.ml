(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type block_data = CContext.t * Clang_ast_t.type_ptr * Procname.t * (Pvar.t * Sil.typ) list

type instr_type = [
  | `ClangStmt of Clang_ast_t.stmt
  | `CXXConstructorInit of Clang_ast_t.cxx_ctor_initializer
]

module type CTranslation =
sig
  val instructions_trans : CContext.t -> Clang_ast_t.stmt -> instr_type list ->
    Cfg.Node.t -> Cfg.Node.t list
end

module type CFrontend = sig
  val function_decl : Tenv.t -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    block_data option -> unit

  val translate_one_declaration : Tenv.t -> Cg.t -> Cfg.cfg ->
    Clang_ast_t.decl -> Clang_ast_t.decl -> unit
end
