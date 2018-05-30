(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Functions for extracting properties of functions or method declarations *)

val get_method_kind : Clang_ast_t.decl -> ProcAttributes.clang_method_kind

val get_return_type : Clang_ast_t.decl -> Clang_ast_t.qual_type

val get_param_decls : Clang_ast_t.decl -> Clang_ast_t.decl list

val get_method_body : Clang_ast_t.decl -> Clang_ast_t.stmt option

val is_cpp_virtual : Clang_ast_t.decl -> bool

val is_cpp_nothrow : Clang_ast_t.decl -> bool

val get_init_list_instrs :
  Clang_ast_t.decl -> [> `CXXConstructorInit of Clang_ast_t.cxx_ctor_initializer] list

val get_pointer_to_property : Clang_ast_t.decl -> Clang_ast_t.pointer option

val is_objc_method : Clang_ast_t.decl -> bool
