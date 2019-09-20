(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let process_ast ast default_source_file =
  let open Clang_ast_t in
  let rec extract_location ast_range decl =
    match decl with
    | ObjCMethodDecl (di, _, _)
    | CXXConversionDecl (di, _, _, _, _)
    | FunctionDecl (di, _, _, _)
    | CXXMethodDecl (di, _, _, _, _)
    | CXXConstructorDecl (di, _, _, _, _)
    | CXXDestructorDecl (di, _, _, _, _) ->
        let range = CLocation.location_of_decl_info default_source_file di in
        Typ.Procname.Map.add (CType_decl.CProcname.from_decl decl) range ast_range
    | _ -> (
      match Clang_ast_proj.get_decl_context_tuple decl with
      | Some (decls, _) ->
          List.fold decls ~f:extract_location ~init:ast_range
      | None ->
          ast_range )
  in
  match ast with
  | TranslationUnitDecl (_, decl_list, _, _) ->
      List.fold decl_list ~init:Typ.Procname.Map.empty ~f:(fun map decl ->
          let info = Clang_ast_proj.get_decl_tuple decl in
          let source_range = info.di_source_range in
          if
            CLocation.should_translate_lib default_source_file source_range `DeclTraversal
              ~translate_when_used:true
          then extract_location map decl
          else map )
  | _ ->
      assert false
