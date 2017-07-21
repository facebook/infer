(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Utility module for translating unary and binary operations and compound assignments *)

val bin_op_to_string : Clang_ast_t.binary_operator_info -> string

val binary_operation_instruction :
  Clang_ast_t.binary_operator_info -> Exp.t -> Typ.t -> Exp.t -> Location.t -> bool
  -> Exp.t * Sil.instr list

val unary_operation_instruction :
  CFrontend_config.translation_unit_context -> Clang_ast_t.unary_operator_info -> Exp.t -> Typ.t
  -> Location.t -> Exp.t * Sil.instr list

val assignment_arc_mode :
  Exp.t -> Typ.t -> Exp.t -> Location.t -> bool -> bool -> Exp.t * Sil.instr list

val sil_const_plus_one : Exp.t -> Exp.t
