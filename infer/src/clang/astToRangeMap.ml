(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let process_ast trans_unit_ctx ast tenv default_source_file f =
  let open Clang_ast_t in
  let mk_key decl = CType_decl.CProcname.from_decl ~tenv decl in
  let call_f decl range =
    CFrontend_errors.protect trans_unit_ctx
      ~recover:(fun () -> ())
      ~pp_context:(fun f () ->
        F.fprintf f "%a: processing %s" SourceFile.pp default_source_file
          (Clang_ast_j.string_of_decl decl) )
      ~f:(fun () -> f (mk_key decl) range)
  in
  let rec extract_location decl =
    match decl with
    | ObjCMethodDecl (di, _, _)
    | CXXConversionDecl (di, _, _, _, _)
    | FunctionDecl (di, _, _, _)
    | CXXMethodDecl (di, _, _, _, _)
    | CXXConstructorDecl (di, _, _, _, _)
    | CXXDestructorDecl (di, _, _, _, _) ->
        let range = CLocation.location_of_decl_info default_source_file di in
        call_f decl range
    | _ -> (
      match Clang_ast_proj.get_decl_context_tuple decl with
      | Some (decls, _) ->
          List.iter decls ~f:extract_location
      | None ->
          L.(debug Capture Verbose)
            "@\n          Found %s.., skipping"
            (Clang_ast_proj.get_decl_kind_string decl) )
  in
  match ast with
  | TranslationUnitDecl (_, decl_list, _, _) ->
      CFrontend_config.global_translation_unit_decls := decl_list ;
      L.(debug Capture Verbose) "@\n Processing AST...@\n" ;
      List.iter decl_list ~f:(fun d ->
          let info = Clang_ast_proj.get_decl_tuple d in
          let source_range = info.di_source_range in
          if
            CLocation.should_translate_lib default_source_file source_range `DeclTraversal
              ~translate_when_used:true
          then extract_location d ) ;
      L.(debug Capture Verbose) "@\n Finished processing AST.@\n"
  | _ ->
      assert false
