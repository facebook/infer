(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Process methods or functions declarations by adding them to the cfg. *)

module CMethod_decl_funct(T: CModule_type.CTranslation) : sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class ->
    Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit

end

module type CMethod_decl = sig
  val process_methods : Sil.tenv -> Cg.t -> Cfg.cfg -> CContext.curr_class ->
    Clang_ast_t.decl list -> unit

  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> Clang_ast_t.decl ->
    CModule_type.block_data option -> unit
end
