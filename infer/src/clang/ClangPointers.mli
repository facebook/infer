(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** pointers produced by the AST exporter to represent sharing in the AST *)
type t = Clang_ast_t.pointer

module Map : module type of Map.Make (Int)

val ivar_to_property_table : Clang_ast_t.decl Int.Table.t
(** maps ivar decl pointer to its decl record *)

val pointer_decl_table : Clang_ast_t.decl Int.Table.t
(** maps decl pointer to its decl record *)

val pointer_stmt_table : Clang_ast_t.stmt Int.Table.t
(** maps stmt pointer to its stmt record *)

val pointer_type_table : Clang_ast_t.c_type Int.Table.t
(** map pointer to its type *)

val populate_all_tables : Clang_ast_t.decl -> unit
(** discover what pointers should point to in the tables above; should be run once for the current
    toplevel decl *)
