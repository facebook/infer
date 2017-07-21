(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open Clang_ast_t

(** This module creates extra ast constructs that are needed for the translation *)

val dummy_stmt : unit -> stmt

val dummy_source_range : unit -> source_range

val dummy_stmt_info : unit -> stmt_info

val create_class_qual_type : ?quals:Typ.type_quals -> Typ.Name.t -> qual_type

val create_pointer_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_reference_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_char_star_type : ?quals:Typ.type_quals -> unit -> qual_type

val create_id_type : qual_type

val create_void_type : qual_type

val create_int_type : qual_type

val create_BOOL_type : qual_type

val create_integer_literal : string -> stmt

val make_stmt_info : decl_info -> stmt_info

val make_decl_ref_expr_info : decl_ref -> decl_ref_expr_info

val make_next_object_exp :
  stmt_info -> stmt -> Clang_ast_t.stmt -> Clang_ast_t.stmt * Clang_ast_t.stmt

val create_nil : stmt_info -> stmt

val create_implicit_cast_expr : stmt_info -> stmt list -> qual_type -> cast_kind -> stmt

val make_binary_stmt : stmt -> stmt -> stmt_info -> expr_info -> binary_operator_info -> stmt

val make_obj_c_message_expr_info_class :
  string -> Typ.Name.t -> pointer option -> obj_c_message_expr_info

val make_obj_c_message_expr_info_instance : string -> obj_c_message_expr_info

val translate_dispatch_function : stmt_info -> stmt list -> int -> stmt

val translate_block_enumerate :
  string -> stmt_info -> stmt list -> expr_info
  -> stmt * (string * Clang_ast_t.pointer * qual_type) list

(* We translate an expression with a conditional*)
(* x <=> x?1:0 *)

val trans_with_conditional : stmt_info -> expr_info -> stmt list -> stmt

(* We translate the logical negation of an expression with a conditional*)
(* !x <=> x?0:1 *)

val trans_negation_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
