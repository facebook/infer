(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


(* Module for warnings detected at translation time by the frontend *)


(* Checks for warnings on properties of class c *)
val check_for_property_errors : Cfg.cfg -> Cg.t -> Sil.tenv -> string -> Clang_ast_t.decl list ->
  unit

(* Call checkers on a specific access of an ivar *)
val check_for_ivar_errors :
  CContext.t -> Clang_ast_t.stmt_info -> Clang_ast_t.obj_c_ivar_ref_expr_info -> unit

val check_for_captured_vars : CContext.t -> Clang_ast_t.stmt_info ->
  (Sil.pvar * Sil.typ) list -> unit
