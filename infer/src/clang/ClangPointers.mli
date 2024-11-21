(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Map : Stdlib.Map.S with type key = int

val pointer_decl_table : Clang_ast_t.decl IInt.Hash.t
(** maps decl pointer to its decl record *)

val pointer_stmt_table : Clang_ast_t.stmt IInt.Hash.t
(** maps stmt pointer to its stmt record *)

val pointer_type_table : Clang_ast_t.c_type IInt.Hash.t
(** map pointer to its type *)

val populate_all_tables : Clang_ast_t.decl -> unit
(** discover what pointers should point to in the tables above; should be run once for the current
    toplevel decl *)
