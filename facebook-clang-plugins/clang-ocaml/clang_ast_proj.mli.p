(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Clang_ast_t

val get_cast_kind : stmt -> cast_kind option
val get_decl_context_tuple : decl -> (decl_context_tuple) option
val get_decl_kind_string : decl -> string
val get_decl_tuple : decl -> (decl_tuple)
val get_expr_tuple : stmt -> (expr_tuple) option
val get_cxx_construct_expr_tuple : stmt -> (cxx_construct_expr_tuple) option
val get_function_decl_tuple : decl -> (function_decl_tuple) option
val get_named_decl_tuple : decl -> (named_decl_tuple) option
val get_stmt_kind_string : stmt -> string
val get_stmt_tuple : stmt -> (stmt_tuple)
val get_tag_decl_tuple : decl -> (tag_decl_tuple) option
val get_type_decl_tuple : decl -> (type_decl_tuple) option
val get_type_tuple : c_type -> (type_tuple)
val get_var_decl_tuple : decl -> (var_decl_tuple) option

val is_valid_astnode_kind : string -> bool
val is_valid_binop_kind_name : string -> bool
val is_valid_unop_kind_name : string -> bool

val string_of_binop_kind : binary_operator_kind -> string
val string_of_cast_kind : cast_kind -> string
val string_of_unop_kind : unary_operator_kind -> string

val update_cxx_construct_expr_tuple : ((cxx_construct_expr_tuple) -> (cxx_construct_expr_tuple)) -> stmt -> stmt
val update_decl_context_tuple : ((decl_context_tuple) -> (decl_context_tuple)) -> decl -> decl
val update_decl_tuple : ((decl_tuple) -> (decl_tuple)) -> decl -> decl
val update_expr_tuple : ((expr_tuple) -> (expr_tuple)) -> stmt -> stmt
val update_named_decl_tuple : ((named_decl_tuple) -> (named_decl_tuple)) -> decl -> decl
val update_stmt_tuple : ((stmt_tuple) -> (stmt_tuple)) -> stmt -> stmt
val update_tag_decl_tuple : ((tag_decl_tuple) -> (tag_decl_tuple)) -> decl -> decl
val update_type_decl_tuple : ((type_decl_tuple) -> (type_decl_tuple)) -> decl -> decl
val update_var_decl_tuple : ((var_decl_tuple) -> (var_decl_tuple)) -> decl -> decl
