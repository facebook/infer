(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module type CTrans = sig
  (** Translates instructions: (statements and expressions) from the ast into sil *)

  (** It receives the context, a list of statements from clang ast, list of custom statments *)
  (** to be added before clang statements and the exit node and it returns a list of cfg nodes *)
  (** that reporesent the translation of the stmts into sil. *)
  val instructions_trans : CContext.t -> Clang_ast_t.stmt -> CModule_type.instr_type list ->
    Cfg.Node.t -> Cfg.Node.t list

end


module CTrans_funct(F: CModule_type.CFrontend) : CTrans

