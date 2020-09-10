(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type visit_decl_t = Atdgen_runtime.Util.Validation.path -> Clang_ast_t.decl -> unit

type visit_stmt_t = Atdgen_runtime.Util.Validation.path -> Clang_ast_t.stmt -> unit

type visit_type_t = Atdgen_runtime.Util.Validation.path -> Clang_ast_t.c_type -> unit

type visit_src_loc_t = Atdgen_runtime.Util.Validation.path -> Clang_ast_t.source_location -> unit

let empty_visitor _path _decl = ()

let decl_visitor = ref (empty_visitor : visit_decl_t)

let stmt_visitor = ref (empty_visitor : visit_stmt_t)

let type_visitor = ref (empty_visitor : visit_type_t)

let source_location_visitor = ref (empty_visitor : visit_src_loc_t)

let visit_decl path decl =
  !decl_visitor path decl ;
  (* return None to pass atd validation *)
  None


let visit_stmt path stmt =
  !stmt_visitor path stmt ;
  (* return None to pass atd validation *)
  None


let visit_type path c_type =
  !type_visitor path c_type ;
  (* return None to pass atd validation *)
  None


let visit_source_loc path src_loc =
  !source_location_visitor path src_loc ;
  (* return None to pass atd validation *)
  None
