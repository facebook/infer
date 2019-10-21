(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(* Builds a clang procedure, following the format required to match with profiler samples: 
C Functions: name, mangled name optional 
ObjC Methods: mangled_name  
ObjC Blocks: mangled_name 
C++ methods: mangled_name (For us mangled name is optional, but if it is not there then we can't match the method) *)
let clang_proc_of_decl decl =
  let open Clang_ast_t in
  match decl with
  | ObjCMethodDecl (_, _, omdi) ->
      Some (ClangProc.ObjcMethod {mangled_name= omdi.Clang_ast_t.omdi_mangled_name})
  | BlockDecl (_, bdi) ->
      Some (ClangProc.ObjcBlock {mangled_name= bdi.Clang_ast_t.bdi_mangled_name})
  | FunctionDecl (_, named_decl_info, _, fdi) ->
      Some
        (ClangProc.CFunction
           { name= named_decl_info.Clang_ast_t.ni_name
           ; mangled_name= fdi.Clang_ast_t.fdi_mangled_name })
  | CXXConversionDecl (_, _, _, fdi, _)
  | CXXMethodDecl (_, _, _, fdi, _)
  | CXXConstructorDecl (_, _, _, fdi, _)
  | CXXDestructorDecl (_, _, _, fdi, _) -> (
    match fdi.Clang_ast_t.fdi_mangled_name with
    | Some mangled_name ->
        Some (ClangProc.CppMethod {mangled_name})
    | None ->
        None )
  | _ ->
      None


let rec process_block_decls_in_stmts default_source_file ast_range stmts =
  let open Clang_ast_t in
  let process_decls_in_stmt ast_range stmt =
    let _, stmts = Clang_ast_proj.get_stmt_tuple stmt in
    let ast_range' = process_block_decls_in_stmts default_source_file ast_range stmts in
    match stmt with
    | BlockExpr (_, _, _, decl) ->
        process_proc_decl default_source_file ast_range' decl
    | _ ->
        ast_range'
  in
  List.fold ~f:process_decls_in_stmt ~init:ast_range stmts


and process_proc_decl default_source_file ast_range decl =
  let open Clang_ast_t in
  match decl with
  | ObjCMethodDecl (di, _, _)
  | CXXConversionDecl (di, _, _, _, _)
  | FunctionDecl (di, _, _, _)
  | CXXMethodDecl (di, _, _, _, _)
  | CXXConstructorDecl (di, _, _, _, _)
  | CXXDestructorDecl (di, _, _, _, _)
  | BlockDecl (di, _) -> (
      let range = CLocation.location_of_decl_info default_source_file di in
      let procname = CType_decl.CProcname.from_decl decl in
      let clang_proc = clang_proc_of_decl decl in
      let ast_range' = Typ.Procname.Map.add procname (range, clang_proc) ast_range in
      match CAst_utils.get_method_body_opt decl with
      | Some stmt ->
          process_block_decls_in_stmts default_source_file ast_range' [stmt]
      | None ->
          ast_range' )
  | _ -> (
    match Clang_ast_proj.get_decl_context_tuple decl with
    | Some (decls, _) ->
        List.fold ~f:(process_proc_decl default_source_file) ~init:ast_range decls
    | None ->
        ast_range )


let process_ast ast default_source_file =
  let open Clang_ast_t in
  match ast with
  | TranslationUnitDecl (_, decl_list, _, _) ->
      List.fold decl_list ~init:Typ.Procname.Map.empty ~f:(fun map decl ->
          let info = Clang_ast_proj.get_decl_tuple decl in
          let source_range = info.di_source_range in
          if
            CLocation.should_translate_lib default_source_file source_range `DeclTraversal
              ~translate_when_used:true
          then process_proc_decl default_source_file map decl
          else map )
  | _ ->
      assert false
