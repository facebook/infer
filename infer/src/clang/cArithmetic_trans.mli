(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Utility module for translating unary and binary operations and compound assignments *)

val bin_op_to_string : Clang_ast_t.binary_operator_info -> string

val binary_operation_instruction :
     Clang_ast_t.source_range
  -> Clang_ast_t.binary_operator_info
  -> Exp.t * Typ.t
  -> Typ.t
  -> Exp.t * Typ.t
  -> Location.t
  -> Exp.t * Sil.instr list
(** Returns a pair ([binary_expression], instructions). "binary_expression" is returned when we are
   calculating an expression "instructions" is not empty when the binary operator is actually a
   statement like an assignment. *)

val unary_operation_instruction :
     CFrontend_config.translation_unit_context
  -> Clang_ast_t.unary_operator_info
  -> Exp.t
  -> Typ.t
  -> Location.t
  -> Exp.t * Sil.instr list

val sil_const_plus_one : Exp.t -> Exp.t
