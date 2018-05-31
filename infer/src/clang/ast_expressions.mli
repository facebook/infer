(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Clang_ast_t

(** This module creates extra ast constructs that are needed for the translation *)

val create_pointer_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_reference_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_char_star_type : ?quals:Typ.type_quals -> unit -> qual_type

val make_next_object_exp :
  stmt_info -> stmt -> Clang_ast_t.stmt -> Clang_ast_t.stmt * Clang_ast_t.stmt

val create_nil : stmt_info -> stmt

val create_implicit_cast_expr : stmt_info -> stmt list -> qual_type -> cast_kind -> stmt

val make_obj_c_message_expr_info_class :
  string -> Typ.Name.t -> pointer option -> obj_c_message_expr_info

val trans_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
(** We translate an expression with a conditional
    x <=> x?1:0 *)

val trans_negation_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
(** We translate the logical negation of an expression with a conditional
    !x <=> x?0:1 *)
