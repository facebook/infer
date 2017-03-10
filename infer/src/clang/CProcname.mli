(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Given decl, return its procname. This function should be used for all procedures
    present in original AST *)
val from_decl :
  CFrontend_config.translation_unit_context -> ?tenv:Tenv.t -> Clang_ast_t.decl -> Typ.Procname.t

(** WARNING: functions from this module should not be used if full decl is available in AST *)
module NoAstDecl : sig
  val c_function_of_string :
    CFrontend_config.translation_unit_context -> Tenv.t -> string -> Typ.Procname.t

  val cpp_method_of_string : Tenv.t -> Typename.t -> string -> Typ.Procname.t

  val objc_method_of_string_kind : Typename.t -> string -> Typ.Procname.objc_cpp_method_kind ->
    Typ.Procname.t

end

(** Makes a fresh name for a block defined inside the defining procedure.
    It updates the global block_counter *)
val mk_fresh_block_procname : Typ.Procname.t -> Typ.Procname.t

(** Returns the next fresh name for a block defined inside the defining procedure
    It does not update the global block_counter *)
val get_next_block_pvar : Typ.Procname.t -> Pvar.t

val reset_block_counter : unit -> unit
