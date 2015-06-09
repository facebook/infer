(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

open Clang_ast_t

(** This module creates extra ast constructs that are needed for the translation *)

val dummy_stmt : unit -> stmt

val dummy_decl_info : decl_info -> decl_info

val dummy_source_range : unit -> source_range

val dummy_stmt_info : unit -> stmt_info

val create_qual_type : string -> qual_type

val create_pointer_type : string -> qual_type

val make_objc_ivar_decl : decl_info -> qual_type option -> obj_c_property_impl_decl_info -> decl

val make_deref_self_field : string -> decl_info -> qual_type -> string -> stmt

val make_stmt_info : decl_info -> stmt_info

val make_method_decl_info : obj_c_method_decl_info -> stmt -> obj_c_method_decl_info

val make_general_decl_ref : decl_kind -> string -> bool -> qual_type -> decl_ref

val make_decl_ref_expr_info : decl_ref -> decl_ref_expr_info

val make_expr_info : qual_type -> expr_info

val make_cast_expr : qual_type -> decl_info -> decl_ref_expr_info -> object_kind -> stmt

val make_self_field : string -> decl_info -> qual_type -> string -> stmt

val make_nondet_exp : stmt_info -> stmt

val make_obj_c_message_expr_info : string -> string -> obj_c_message_expr_info

val create_nil : stmt_info -> stmt

val create_implicit_cast_expr : stmt_info -> stmt list -> qual_type -> cast_kind -> stmt

val create_char_type : unit -> qual_type

val make_message_expr : qual_type -> string -> stmt -> stmt_info -> stmt

val make_compound_stmt : stmt list -> stmt_info -> stmt

val make_decl_ref_exp_var : string * qual_type -> decl_kind -> stmt_info -> stmt

val make_binary_stmt : stmt -> stmt -> stmt_info -> expr_info -> binary_operator_info -> stmt

val make_obj_c_message_expr_info_class : string -> qual_type -> obj_c_message_expr_info

val translate_dispatch_function : string -> stmt_info -> stmt list -> expr_info -> int -> stmt * qual_type

(* We translate the logical negation of an integer with a conditional*)
(* !x <=> x?0:1 *)
val trans_negation_with_conditional : stmt_info -> expr_info -> stmt list -> stmt
