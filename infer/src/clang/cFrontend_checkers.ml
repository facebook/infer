(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(* To create a new checker you should: *)
(* 1. Define a checker function, say my_checker, in this module. *)
(* my_checker should define: *)
(*   -a) a condition that determine if the checker fires *)
(*   -b) a issue_desc that describes the warning (see warning_desc definition) *)
(* 2. Add your checker to the CFrontend_checkers interface *)
(* 3. Decide in which element of the AST my_checker should be evaluated. *)
(*    - If it is a statement then you need to invoke my_checker from *)
(*    run_frontend_checkers_on_stmt in CFrontend_error module.*)
(*    - If it is a declaration invoke it from run_frontend_checkers_on_decl *)

(* Helper functions *)
let location_from_stmt lctx stmt =
  let info, _ = Clang_ast_proj.get_stmt_tuple stmt in
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.si_source_range true

let location_from_dinfo lctx info =
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.di_source_range true

let location_from_decl lctx dec =
  let info = Clang_ast_proj.get_decl_tuple dec in
  CLocation.get_sil_location_from_range lctx.CLintersContext.translation_unit_context
    info.Clang_ast_t.di_source_range true

let location_from_an lcxt an =
  match an with
  | Ctl_parser_types.Stmt st -> location_from_stmt lcxt st
  | Ctl_parser_types.Decl d -> location_from_decl lcxt d

let decl_name an =
  match an with
  | Ctl_parser_types.Decl dec ->
      (match Clang_ast_proj.get_named_decl_tuple dec with
       | Some (_, n) -> n.Clang_ast_t.ni_name
       | None -> "")
  | _ -> ""

let tag_name_of_node an =
  match an with
  | Ctl_parser_types.Stmt stmt -> Clang_ast_proj.get_stmt_kind_string stmt
  | Ctl_parser_types.Decl decl -> Clang_ast_proj.get_decl_kind_string decl

let decl_ref_or_selector_name an =
  match CTL.next_state_via_transition an (Some CTL.PointerToDecl) with
  | Some (Ctl_parser_types.Decl ObjCMethodDecl _ as decl_an) ->
      "The selector " ^ (decl_name decl_an)
  | Some (Ctl_parser_types.Decl _ as decl_an) ->
      "The reference " ^ (decl_name decl_an)
  | _ -> failwith("decl_ref_or_selector_name must be called with a DeclRefExpr \
                   or an ObjCMessageExpr, but got " ^ (tag_name_of_node an))

let iphoneos_target_sdk_version _ =
  match Config.iphoneos_target_sdk_version with
  | Some f -> f
  | None -> "0"

let available_ios_sdk an =
  let open Ctl_parser_types in
  match CTL.next_state_via_transition an (Some CTL.PointerToDecl) with
  | Some Decl decl ->
      (match CPredicates.get_available_attr_ios_sdk (Decl decl) with
       | Some version -> version
       | None -> "")
  | _ -> failwith("available_ios_sdk must be called with a DeclRefExpr \
                   or an ObjCMessageExpr, but got " ^ (tag_name_of_node an))

let ivar_name an =
  let open Clang_ast_t in
  match an with
  | Ctl_parser_types.Stmt (ObjCIvarRefExpr (_, _, _, rei)) ->
      let dr_ref = rei.ovrei_decl_ref in
      let ivar_pointer = dr_ref.dr_decl_pointer in
      (match CAst_utils.get_decl ivar_pointer with
       | Some (ObjCIvarDecl (_, named_decl_info, _, _, _)) ->
           named_decl_info.Clang_ast_t.ni_name
       | _ -> "")
  | _ -> ""

let cxx_ref_captured_in_block an =
  let open Ctl_parser_types in
  let open Clang_ast_t in
  let capt_refs = match an with
    | Decl _ -> CPredicates.captured_variables_cxx_ref an
    | Stmt (BlockExpr(_, _, _, d)) ->
        CPredicates.captured_variables_cxx_ref (Decl d)
    | _ -> [] in
  let var_desc vars var_named_decl_info =
    vars ^ "'" ^ var_named_decl_info.ni_name ^ "'" in
  List.fold ~f:var_desc ~init:"" capt_refs
