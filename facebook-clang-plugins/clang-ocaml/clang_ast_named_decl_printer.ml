(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module P = Printf

let rec visit_named_decls f decl =
  let () =
    match Clang_ast_proj.get_named_decl_tuple decl with Some (x, y) -> f x y | None -> ()
  in
  match Clang_ast_proj.get_decl_context_tuple decl with
  | Some (l, _) ->
      List.iter (visit_named_decls f) l
  | None ->
      ()


let print_named_decl_from_file fname =
  let ast = Atdgen_runtime.Util.Json.from_file Clang_ast_j.read_decl fname in
  let getname name_info = name_info.Clang_ast_t.ni_name in
  visit_named_decls
    (fun _ info ->
      print_string (getname info) ;
      print_newline () )
    ast


let main =
  try Array.iteri (fun i arg -> if i <> 0 then print_named_decl_from_file arg) Sys.argv
  with Yojson.Json_error s | Atdgen_runtime.Oj_run.Error s ->
    prerr_string s ;
    prerr_newline () ;
    exit 1
