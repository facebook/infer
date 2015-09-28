(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Clang_ast_t

(** This module creates extra ast constructs that are needed for the translation *)

val dummy_stmt : unit -> stmt

val dummy_decl_info : decl_info -> decl_info

val dummy_decl_info_in_curr_file : decl_info -> decl_info

val dummy_source_range : unit -> source_range

val dummy_stmt_info : unit -> stmt_info

val create_char_star_type : qual_type

val create_id_type : qual_type

val create_nsarray_star_type : qual_type

val create_void_type : qual_type

val create_int_type : qual_type

val create_void_star_type : qual_type

val create_BOOL_type : qual_type

val create_unsigned_long_type : qual_type

val create_void_unsigned_long_type : qual_type

val create_void_void_type : qual_type

val create_class_type : string -> qual_type

val create_struct_type : string -> qual_type

val create_pointer_type : qual_type -> qual_type

val create_qual_type_with_just_pointer : Clang_ast_t.pointer -> qual_type

val make_objc_ivar_decl : decl_info -> qual_type -> obj_c_property_impl_decl_info -> string -> decl

val make_deref_self_field : string -> decl_info -> qual_type -> string -> stmt

val make_stmt_info : decl_info -> stmt_info

val make_method_decl_info : obj_c_method_decl_info -> stmt -> obj_c_method_decl_info

val make_decl_ref_qt : decl_kind -> pointer -> string -> bool -> qual_type -> decl_ref

val make_decl_ref_expr_info : decl_ref -> decl_ref_expr_info

val make_general_expr_info : qual_type -> value_kind -> object_kind -> expr_info

val make_expr_info : qual_type -> expr_info

val make_cast_expr : qual_type -> decl_info -> decl_ref_expr_info -> object_kind -> stmt

val make_self_field : string -> decl_info -> qual_type -> string -> stmt

val make_next_object_exp : stmt_info -> stmt -> Clang_ast_t.stmt -> Clang_ast_t.stmt

val create_nil : stmt_info -> stmt

val create_implicit_cast_expr : stmt_info -> stmt list -> qual_type -> cast_kind -> stmt

val make_message_expr : qual_type -> string -> stmt -> stmt_info -> bool -> stmt

val make_compound_stmt : stmt list -> stmt_info -> stmt

val make_decl_ref_exp_var : string * qual_type * pointer -> decl_kind -> stmt_info -> stmt

val make_binary_stmt : stmt -> stmt -> stmt_info -> expr_info -> binary_operator_info -> stmt

val make_obj_c_message_expr_info_class : string -> string -> obj_c_message_expr_info

val make_obj_c_message_expr_info_instance : string -> obj_c_message_expr_info

val translate_dispatch_function : string -> stmt_info -> stmt list -> expr_info -> int -> stmt * qual_type

val translate_block_enumerate : string ->  stmt_info  -> stmt list -> expr_info -> stmt * (string * string* qual_type) list

(* We translate the logical negation of an integer with a conditional*)
(* !x <=> x?0:1 *)
val trans_negation_with_conditional : stmt_info -> expr_info -> stmt list -> stmt

val create_assume_not_null_call : decl_info -> string -> qual_type -> stmt
