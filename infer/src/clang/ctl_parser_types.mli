(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Types used by the ctl parser *)

(** the kind of AST nodes where formulas are evaluated *)
type ast_node =
  | Stmt of Clang_ast_t.stmt
  | Decl of Clang_ast_t.decl

val ast_node_name : ast_node -> string

val ast_node_type : ast_node -> string

exception ALParsingException of string

val infer_prefix : string
val report_when_const : string
val message_const : string
val suggestion_const : string
val severity_const : string
val mode_const : string

type abs_ctype

val tmp_c_type_equal : ?name_c_type : string -> Clang_ast_t.c_type -> abs_ctype -> bool
