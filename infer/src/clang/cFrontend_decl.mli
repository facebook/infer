(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Translate declarations **)

module type CFrontend_decl = sig
  val function_decl : Tenv.t -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit

  val translate_one_declaration : Tenv.t -> Cg.t -> Cfg.cfg ->
    Clang_ast_t.decl -> Clang_ast_t.decl -> unit
end

module CFrontend_decl_funct(T: CModule_type.CTranslation) : CFrontend_decl
