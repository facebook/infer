(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module type CTranslation =
sig
  val instructions_trans : CContext.t -> Clang_ast_t.stmt list -> Cfg.Node.t -> Cfg.Node.t list
end

module type CMethod_declaration =
sig
  val function_decl : Sil.tenv -> Cfg.cfg -> Cg.t -> string option -> bool -> Clang_ast_t.decl_info ->
  string -> Clang_ast_t.qual_type -> Clang_ast_t.function_decl_info -> (Mangled.t * Sil.typ * bool) list -> Procname.t option -> CContext.curr_class -> unit

  val process_getter_setter : CContext.t ->  Procname.t -> bool
end
