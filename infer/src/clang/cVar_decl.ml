(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Process variable declarations by saving them as local or global variables.  *)

(** Computes the local variables of a function or method to be added to the procdesc *)

module L = Logging

let is_custom_var_pointer pointer = pointer <= 0

let sil_var_of_decl context var_decl procname =
  let outer_procname = CContext.get_outer_procname context in
  let trans_unit_ctx = context.CContext.translation_unit_context in
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let shoud_be_mangled = not (is_custom_var_pointer decl_info.Clang_ast_t.di_pointer) in
      let var_decl_details = Some (decl_info, qual_type, var_decl_info, shoud_be_mangled) in
      CGeneral_utils.mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
  | ParmVarDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let var_decl_details = Some (decl_info, qual_type, var_decl_info, false) in
      CGeneral_utils.mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
  | _ ->
      assert false


let sil_var_of_decl_ref context source_range decl_ref procname =
  let name =
    match decl_ref.Clang_ast_t.dr_name with Some name_info -> name_info | None -> assert false
  in
  match decl_ref.Clang_ast_t.dr_kind with
  | `ImplicitParam ->
      let outer_procname = CContext.get_outer_procname context in
      let trans_unit_ctx = context.CContext.translation_unit_context in
      CGeneral_utils.mk_sil_var trans_unit_ctx name None procname outer_procname
  | _ ->
      let pointer = decl_ref.Clang_ast_t.dr_decl_pointer in
      if is_custom_var_pointer pointer then
        Pvar.mk (Mangled.from_string name.Clang_ast_t.ni_name) procname
      else
        match CAst_utils.get_decl pointer with
        | Some var_decl ->
            sil_var_of_decl context var_decl procname
        | None ->
            (* FIXME(t21762295) *)
            CFrontend_config.incorrect_assumption __POS__ source_range
              "pointer '%d' for var decl not found. The var decl was: %a" pointer
              (Pp.to_string ~f:Clang_ast_j.string_of_decl_ref)
              decl_ref


let get_var_attribute decl_info =
  let open Clang_ast_t in
  let has_block_attribute =
    List.exists decl_info.Clang_ast_t.di_attributes ~f:(fun attr ->
        match attr with BlocksAttr _ -> true | _ -> false )
  in
  if has_block_attribute then [ProcAttributes.Modify_in_block] else []


let add_var_to_locals procdesc var_decl typ pvar =
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, _, _, vdi) ->
      if not vdi.Clang_ast_t.vdi_is_global then
        let attributes = get_var_attribute decl_info in
        let var_data : ProcAttributes.var_data = {name= Pvar.get_name pvar; typ; attributes} in
        Procdesc.append_locals procdesc [var_data]
  | _ ->
      assert false


let sil_var_of_captured_var decl_ref context source_range procname =
  match decl_ref with
  | {Clang_ast_t.dr_qual_type= Some qual_type} ->
      ( sil_var_of_decl_ref context source_range decl_ref procname
      , CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type )
  | _ ->
      assert false


(* Returns a list of captured variables as sil variables. *)
let captured_vars_from_block_info context source_range cvl =
  let procname = Procdesc.get_proc_name context.CContext.procdesc in
  let sil_var_of_captured_var {Clang_ast_t.bcv_variable} vars_acc =
    match bcv_variable with
    | Some ({Clang_ast_t.dr_name= Some {Clang_ast_t.ni_name}} as decl_ref) ->
        if String.equal ni_name CFrontend_config.self && not (CContext.is_objc_instance context)
        then vars_acc
        else sil_var_of_captured_var decl_ref context source_range procname :: vars_acc
    | _ ->
        assert false
  in
  List.fold_right ~f:sil_var_of_captured_var cvl ~init:[]
