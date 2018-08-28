(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(* Helper functions *)
let location_from_stmt lctx stmt =
  let info, _ = Clang_ast_proj.get_stmt_tuple stmt in
  CLocation.location_of_source_range lctx.CLintersContext.translation_unit_context.source_file
    info.Clang_ast_t.si_source_range


let location_from_dinfo lctx info =
  CLocation.location_of_source_range lctx.CLintersContext.translation_unit_context.source_file
    info.Clang_ast_t.di_source_range


let location_from_decl lctx dec =
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.location_of_source_range lctx.CLintersContext.translation_unit_context.source_file
    info.Clang_ast_t.di_source_range


let location_from_an lcxt an =
  match an with
  | Ctl_parser_types.Stmt st ->
      location_from_stmt lcxt st
  | Ctl_parser_types.Decl d ->
      location_from_decl lcxt d


let tag_name_of_node an =
  match an with
  | Ctl_parser_types.Stmt stmt ->
      Clang_ast_proj.get_stmt_kind_string stmt
  | Ctl_parser_types.Decl decl ->
      Clang_ast_proj.get_decl_kind_string decl


let decl_ref_or_selector_name an =
  match CTL.next_state_via_transition an CTL.PointerToDecl with
  | [(Ctl_parser_types.Decl (ObjCMethodDecl _) as decl_an)] ->
      "The selector " ^ Ctl_parser_types.ast_node_name decl_an
  | [(Ctl_parser_types.Decl _ as decl_an)] ->
      "The reference " ^ Ctl_parser_types.ast_node_name decl_an
  | _ ->
      L.(die ExternalError)
        "decl_ref_or_selector_name must be called with a DeclRefExpr or an ObjCMessageExpr, but \
         got %s"
        (tag_name_of_node an)


let iphoneos_target_sdk_version context _ =
  match CPredicates.iphoneos_target_sdk_version_by_path context with Some f -> f | None -> "0"


let available_ios_sdk an =
  let open Ctl_parser_types in
  match CTL.next_state_via_transition an CTL.PointerToDecl with
  | [Decl decl] -> (
    match CPredicates.get_available_attr_ios_sdk (Decl decl) with
    | Some version ->
        version
    | None ->
        "" )
  | _ ->
      L.(die ExternalError)
        "available_ios_sdk must be called with a DeclRefExpr or an ObjCMessageExpr, but got %s"
        (tag_name_of_node an)


let class_available_ios_sdk an =
  match CPredicates.receiver_method_call an with
  | Some decl -> (
    match CPredicates.get_available_attr_ios_sdk (Decl decl) with
    | Some version ->
        version
    | None ->
        "" )
  | None ->
      L.(die ExternalError)
        "class_available_ios_sdk must be called with ObjCMessageExpr, but got %s"
        (tag_name_of_node an)


let receiver_method_call an =
  match CPredicates.receiver_method_call an with
  | Some decl ->
      Ctl_parser_types.ast_node_name (Ctl_parser_types.Decl decl)
  | _ ->
      L.(die ExternalError)
        "receiver_method_call must be called with ObjCMessageExpr, but got %s"
        (tag_name_of_node an)


let ivar_name an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Stmt (ObjCIvarRefExpr (_, _, _, rei)) -> (
      let dr_ref = rei.ovrei_decl_ref in
      let ivar_pointer = dr_ref.dr_decl_pointer in
      match CAst_utils.get_decl ivar_pointer with
      | Some (ObjCIvarDecl (_, named_decl_info, _, _, _)) ->
          named_decl_info.Clang_ast_t.ni_name
      | _ ->
          "" )
  | _ ->
      ""


let cxx_ref_captured_in_block an =
  let open Ctl_parser_types in
  let open Clang_ast_t in
  let capt_refs =
    match an with
    | Decl _ ->
        CPredicates.captured_variables_cxx_ref an
    | Stmt (BlockExpr (_, _, _, d)) ->
        CPredicates.captured_variables_cxx_ref (Decl d)
    | _ ->
        []
  in
  let var_desc vars var_named_decl_info = vars ^ "'" ^ var_named_decl_info.ni_name ^ "'" in
  List.fold ~f:var_desc ~init:"" capt_refs


let class_name node =
  let open Clang_ast_t in
  let class_name_of_interface_type typ =
    match typ with
    | Some (ObjCInterfaceType (_, ptr)) -> (
      match CAst_utils.get_decl ptr with
      | Some (ObjCInterfaceDecl (_, ndi, _, _, _)) ->
          ndi.ni_name
      | _ ->
          "" )
    | _ ->
        ""
  in
  match CPredicates.get_ast_node_type_ptr node with
  | Some type_ptr -> (
      let typ = CAst_utils.get_desugared_type type_ptr in
      match typ with
      | Some (ObjCObjectPointerType (_, {Clang_ast_t.qt_type_ptr})) ->
          class_name_of_interface_type (CAst_utils.get_desugared_type qt_type_ptr)
      | Some (ObjCInterfaceType _) ->
          class_name_of_interface_type typ
      | _ ->
          "" )
  | _ ->
      ""
