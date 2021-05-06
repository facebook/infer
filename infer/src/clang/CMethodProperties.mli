(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for extracting properties of functions or method declarations *)

val get_method_kind : Clang_ast_t.decl -> ClangMethodKind.t

val is_inside_objc_class_method : Clang_ast_t.decl -> bool

val get_return_type : Clang_ast_t.decl -> Clang_ast_t.qual_type

val get_param_decls : Clang_ast_t.decl -> Clang_ast_t.decl list

val get_method_body : Clang_ast_t.decl -> Clang_ast_t.stmt option

val is_cpp_lambda_call_operator : Clang_ast_t.decl -> bool

val is_cpp_virtual : Clang_ast_t.decl -> bool

val is_constexpr : Clang_ast_t.decl -> bool

val get_init_list_instrs : Clang_ast_t.decl -> CFrontend_config.instr_type list

val get_pointer_to_property : Clang_ast_t.decl -> Clang_ast_t.pointer option

val is_no_return : Clang_ast_t.decl -> bool

val is_variadic : Clang_ast_t.decl -> bool

val get_block_captured_variables : Clang_ast_t.decl -> Clang_ast_t.block_captured_variable list
