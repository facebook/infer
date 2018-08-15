(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Functions for extracting properties of functions or method declarations *)

let get_method_kind meth_decl =
  let open Clang_ast_t in
  match meth_decl with
  | FunctionDecl _ ->
      ClangMethodKind.C_FUNCTION
  | CXXMethodDecl (_, _, _, _, method_decl_info)
  | CXXConstructorDecl (_, _, _, _, method_decl_info)
  | CXXConversionDecl (_, _, _, _, method_decl_info)
  | CXXDestructorDecl (_, _, _, _, method_decl_info) ->
      if method_decl_info.Clang_ast_t.xmdi_is_static then ClangMethodKind.CPP_CLASS
      else ClangMethodKind.CPP_INSTANCE
  | ObjCMethodDecl (_, _, method_decl_info) ->
      if method_decl_info.Clang_ast_t.omdi_is_instance_method then ClangMethodKind.OBJC_INSTANCE
      else ClangMethodKind.OBJC_CLASS
  | BlockDecl _ ->
      ClangMethodKind.BLOCK
  | _ ->
      raise CFrontend_config.Invalid_declaration


let rec is_inside_objc_class_method meth_decl =
  let open Clang_ast_t in
  match meth_decl with
  | ObjCMethodDecl _ ->
      ClangMethodKind.equal (get_method_kind meth_decl) ClangMethodKind.OBJC_CLASS
  | BlockDecl (di, _) -> (
    match CAst_utils.get_decl_opt di.di_parent_pointer with
    | Some decl ->
        is_inside_objc_class_method decl
    | None ->
        false )
  | _ ->
      false


let get_return_type method_decl =
  let open Clang_ast_t in
  match method_decl with
  | FunctionDecl (_, _, qt, _)
  | CXXMethodDecl (_, _, qt, _, _)
  | CXXConstructorDecl (_, _, qt, _, _)
  | CXXConversionDecl (_, _, qt, _, _)
  | CXXDestructorDecl (_, _, qt, _, _) ->
      CType.return_type_of_function_type qt
  | ObjCMethodDecl (_, _, omdi) ->
      omdi.omdi_result_type
  | _ ->
      raise CFrontend_config.Invalid_declaration


let get_param_decls method_decl =
  let open Clang_ast_t in
  match method_decl with
  | FunctionDecl (_, _, _, function_decl_info)
  | CXXMethodDecl (_, _, _, function_decl_info, _)
  | CXXConstructorDecl (_, _, _, function_decl_info, _)
  | CXXConversionDecl (_, _, _, function_decl_info, _)
  | CXXDestructorDecl (_, _, _, function_decl_info, _) ->
      function_decl_info.fdi_parameters
  | ObjCMethodDecl (_, _, method_decl_info) ->
      method_decl_info.omdi_parameters
  | BlockDecl (_, block_decl_info) ->
      block_decl_info.bdi_parameters
  | _ ->
      raise CFrontend_config.Invalid_declaration


let get_method_body method_decl =
  let open Clang_ast_t in
  match method_decl with
  | FunctionDecl (_, _, _, function_decl_info)
  | CXXMethodDecl (_, _, _, function_decl_info, _)
  | CXXConstructorDecl (_, _, _, function_decl_info, _)
  | CXXConversionDecl (_, _, _, function_decl_info, _)
  | CXXDestructorDecl (_, _, _, function_decl_info, _) ->
      function_decl_info.fdi_body
  | ObjCMethodDecl (_, _, method_decl_info) ->
      method_decl_info.omdi_body
  | BlockDecl (_, block_decl_info) ->
      block_decl_info.bdi_body
  | _ ->
      raise CFrontend_config.Invalid_declaration


let is_cpp_virtual method_decl =
  let open Clang_ast_t in
  match method_decl with
  | CXXMethodDecl (_, _, _, _, mdi)
  | CXXConstructorDecl (_, _, _, _, mdi)
  | CXXConversionDecl (_, _, _, _, mdi)
  | CXXDestructorDecl (_, _, _, _, mdi) ->
      mdi.xmdi_is_virtual
  | _ ->
      false


let is_cpp_nothrow method_decl =
  let open Clang_ast_t in
  match method_decl with
  | FunctionDecl (_, _, _, fdi)
  | CXXMethodDecl (_, _, _, fdi, _)
  | CXXConstructorDecl (_, _, _, fdi, _)
  | CXXConversionDecl (_, _, _, fdi, _)
  | CXXDestructorDecl (_, _, _, fdi, _) ->
      fdi.fdi_is_no_throw
  | _ ->
      false


let get_init_list_instrs method_decl =
  let open Clang_ast_t in
  match method_decl with
  | CXXMethodDecl (_, _, _, _, mdi)
  | CXXConstructorDecl (_, _, _, _, mdi)
  | CXXConversionDecl (_, _, _, _, mdi)
  | CXXDestructorDecl (_, _, _, _, mdi) ->
      let create_custom_instr construct_instr = `CXXConstructorInit construct_instr in
      List.map ~f:create_custom_instr mdi.xmdi_cxx_ctor_initializers
  | _ ->
      []


let get_pointer_to_property method_decl =
  let open Clang_ast_t in
  match method_decl with
  | ObjCMethodDecl (_, _, mdi) -> (
    match mdi.Clang_ast_t.omdi_property_decl with
    | Some decl_ref ->
        Some decl_ref.Clang_ast_t.dr_decl_pointer
    | None ->
        None )
  | _ ->
      None


let is_objc_method method_decl =
  match method_decl with Clang_ast_t.ObjCMethodDecl _ -> true | _ -> false


let is_variadic method_decl =
  let open Clang_ast_t in
  match method_decl with
  | FunctionDecl (_, _, _, function_decl_info)
  | CXXMethodDecl (_, _, _, function_decl_info, _)
  | CXXConstructorDecl (_, _, _, function_decl_info, _)
  | CXXConversionDecl (_, _, _, function_decl_info, _)
  | CXXDestructorDecl (_, _, _, function_decl_info, _) ->
      function_decl_info.fdi_is_variadic
  | ObjCMethodDecl (_, _, method_decl_info) ->
      method_decl_info.Clang_ast_t.omdi_is_variadic
  | BlockDecl (_, block_decl_info) ->
      block_decl_info.bdi_is_variadic
  | _ ->
      raise CFrontend_config.Invalid_declaration


let get_block_captured_variables method_decl =
  let open Clang_ast_t in
  match method_decl with
  | BlockDecl (_, block_decl_info) ->
      block_decl_info.bdi_captured_variables
  | _ ->
      []
