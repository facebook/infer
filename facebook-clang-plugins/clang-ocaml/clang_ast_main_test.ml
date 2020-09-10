(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception PointerMismatch

let validate_ptr key_ptr node =
  let node_ptr = Clang_ast_main.get_ptr_from_node node in
  if node_ptr != key_ptr then raise PointerMismatch


let validate_decl_ptr key_ptr decl = validate_ptr key_ptr (`DeclNode decl)

let validate_stmt_ptr key_ptr stmt = validate_ptr key_ptr (`StmtNode stmt)

let validate_type_ptr key_ptr c_type = validate_ptr key_ptr (`TypeNode c_type)

let print_node path kind_str =
  let indent = String.make (List.length path) ' ' in
  prerr_string indent ;
  prerr_string kind_str ;
  prerr_newline ()


let sloc_to_str sloc =
  let opt_to_str to_str_f v = match v with Some x -> to_str_f x | None -> "None" in
  let file_str = opt_to_str (fun f -> f) sloc.Clang_ast_t.sl_file in
  let line_str = opt_to_str string_of_int sloc.Clang_ast_t.sl_line in
  let column_str = opt_to_str string_of_int sloc.Clang_ast_t.sl_column in
  file_str ^ ":" ^ line_str ^ ":" ^ column_str


let src_range_to_str range =
  let first, last = range in
  sloc_to_str first ^ " " ^ sloc_to_str last


let print_decl path decl =
  let kind_str = Clang_ast_proj.get_decl_kind_string decl in
  let decl_info = Clang_ast_proj.get_decl_tuple decl in
  let src_str = src_range_to_str decl_info.Clang_ast_t.di_source_range in
  print_node path (kind_str ^ " " ^ src_str)


let print_stmt path stmt = print_node path (Clang_ast_proj.get_stmt_kind_string stmt)

let print_map_size map =
  let s = Clang_ast_main.PointerMap.cardinal map in
  prerr_string (string_of_int s ^ " ")


let check_decl_cache_from_file fname =
  let ast = Atdgen_runtime.Util.Json.from_file Clang_ast_j.read_decl fname in
  let decl_cache, stmt_cache, type_cache, _ = Clang_ast_main.index_node_pointers ast in
  print_map_size decl_cache ;
  print_map_size stmt_cache ;
  print_map_size type_cache ;
  prerr_newline () ;
  Clang_ast_main.PointerMap.iter validate_decl_ptr decl_cache ;
  Clang_ast_main.PointerMap.iter validate_stmt_ptr stmt_cache ;
  Clang_ast_main.PointerMap.iter validate_type_ptr type_cache ;
  Clang_ast_main.visit_ast ~visit_decl:print_decl ~visit_stmt:print_stmt ast


let main =
  let v = Sys.argv in
  try
    for i = 1 to Array.length v - 1 do
      check_decl_cache_from_file v.(i)
    done
  with
  | PointerMismatch ->
      prerr_string "Pointer in cache doesn't match" ;
      exit 1
  | Yojson.Json_error s | Atdgen_runtime.Oj_run.Error s ->
      prerr_string s ;
      prerr_newline () ;
      exit 1
