(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Clang_ast_t

(** This module creates extra ast constructs that are needed for the translation *)

val create_pointer_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_reference_qual_type : ?quals:Typ.type_quals -> qual_type -> qual_type

val create_void_type : qual_type

val create_char_star_type : ?quals:Typ.type_quals -> unit -> qual_type

val create_class_pointer_qual_type : ?quals:Typ.type_quals -> Typ.Name.t -> qual_type

val create_nil : stmt_info -> stmt

val create_implicit_cast_expr : stmt_info -> stmt list -> qual_type -> cast_kind -> stmt

val create_decl_info : stmt_info -> pointer -> decl_info

val default_var_decl_info : var_decl_info

val create_named_decl_info : string -> named_decl_info

val create_decl_ref_expr : stmt_info -> pointer -> named_decl_info -> qual_type -> stmt

val create_obj_c_message_expr : stmt_info -> qual_type -> selector -> stmt list -> stmt

val make_obj_c_message_expr_info_class :
  string -> Typ.Name.t -> pointer option -> obj_c_message_expr_info

val trans_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
(** We translate an expression with a conditional x <=> x?1:0 *)

val trans_negation_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
(** We translate the logical negation of an expression with a conditional !x <=> x?0:1 *)
