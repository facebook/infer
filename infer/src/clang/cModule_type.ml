(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type block_data = CContext.t * Clang_ast_t.type_ptr * Typ.Procname.t * (Pvar.t * Typ.t) list

type instr_type = [
  | `ClangStmt of Clang_ast_t.stmt
  | `CXXConstructorInit of Clang_ast_t.cxx_ctor_initializer
]

type decl_trans_context = [ `DeclTraversal | `Translation ]

module type CTranslation =
sig
  (** Translates instructions: (statements and expressions) from the ast into sil *)

  (** It receives the context, a list of statements from clang ast, list of custom statments to be
      added before clang statements and the exit node and it returns a list of cfg nodes that
      represent the translation of the stmts into sil. *)
  val instructions_trans : CContext.t -> Clang_ast_t.stmt -> instr_type list ->
    Procdesc.Node.t -> Procdesc.Node.t list
end

module type CFrontend = sig
  val function_decl : CFrontend_config.translation_unit_context -> Tenv.t -> Cfg.cfg -> Cg.t ->
    Clang_ast_t.decl -> block_data option -> unit

  val translate_one_declaration : CFrontend_config.translation_unit_context -> Tenv.t -> Cg.t ->
    Cfg.cfg -> decl_trans_context -> Clang_ast_t.decl -> unit
end
