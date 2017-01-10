(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = string * string list (* (name, [param1,...,paramK]) *)

val captured_variables_cxx_ref : Clang_ast_t.decl -> Clang_ast_t.named_decl_info list

val call_method : string -> Clang_ast_t.stmt -> bool

val property_name_contains_word : string -> Clang_ast_t.decl -> bool

val is_objc_extension : CLintersContext.context -> bool

val is_syntactically_global_var : Clang_ast_t.decl -> bool

val is_const_expr_var : Clang_ast_t.decl -> bool

val call_function_named : string list -> Clang_ast_t.stmt -> bool

val is_strong_property : Clang_ast_t.decl -> bool

val is_assign_property : Clang_ast_t.decl -> bool

val is_property_pointer_type : Clang_ast_t.decl -> bool

val context_in_synchronized_block : CLintersContext.context -> bool

val is_ivar_atomic : Clang_ast_t.stmt -> bool

val is_method_property_accessor_of_ivar : Clang_ast_t.stmt -> CLintersContext.context -> bool

val is_objc_constructor : CLintersContext.context -> bool

val is_objc_dealloc : CLintersContext.context -> bool

val captures_cxx_references : Clang_ast_t.decl -> bool

val is_binop_with_kind : string -> Clang_ast_t.stmt -> bool

val is_unop_with_kind : string -> Clang_ast_t.stmt -> bool

val isa : string -> Clang_ast_t.stmt -> bool

val is_stmt : string -> Clang_ast_t.stmt -> bool

val is_decl : string -> Clang_ast_t.decl -> bool

val pp_predicate : Format.formatter -> t -> unit

val decl_unavailable_in_supported_ios_sdk : Clang_ast_t.decl -> bool

val get_available_attr_ios_sdk : Clang_ast_t.decl -> string option
