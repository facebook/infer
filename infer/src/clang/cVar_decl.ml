(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Process variable declarations by saving them as local or global variables.  *)
(** Computes the local variables of a function or method to be added to the procdesc *)

module L = Logging

let is_custom_var_pointer pointer =
  pointer <= 0

let sil_var_of_decl context var_decl procname =
  let outer_procname = CContext.get_outer_procname context in
  let trans_unit_ctx = context.CContext.translation_unit_context in
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let shoud_be_mangled =
        not (is_custom_var_pointer decl_info.Clang_ast_t.di_pointer) in
      let var_decl_details = Some
          (decl_info, qual_type, var_decl_info, shoud_be_mangled) in
      CGeneral_utils.mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
  | ParmVarDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let var_decl_details = Some
          (decl_info, qual_type, var_decl_info, false) in
      CGeneral_utils.mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
  | _ -> assert false

let sil_var_of_decl_ref context decl_ref procname =
  let name =
    match decl_ref.Clang_ast_t.dr_name with
    | Some name_info -> name_info
    | None -> assert false in
  let pointer = decl_ref.Clang_ast_t.dr_decl_pointer in
  match decl_ref.Clang_ast_t.dr_kind with
  | `ImplicitParam ->
      let outer_procname = CContext.get_outer_procname context in
      let trans_unit_ctx = context.CContext.translation_unit_context in
      CGeneral_utils.mk_sil_var trans_unit_ctx name None procname outer_procname
  | _ ->
      if is_custom_var_pointer pointer then
        Pvar.mk (Mangled.from_string name.Clang_ast_t.ni_name) procname
      else match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
        | Some var_decl -> sil_var_of_decl context var_decl procname
        | None -> assert false

let add_var_to_locals procdesc var_decl sil_typ pvar =
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (_, _, _, vdi) ->
      if not vdi.Clang_ast_t.vdi_is_global then
        Procdesc.append_locals procdesc [(Pvar.get_name pvar, sil_typ)]
  | _ -> assert false

let compute_autorelease_pool_vars context stmts =
  let rec do_stmts map = function
    | [] ->
        map
    | Clang_ast_t.DeclRefExpr (_, _, _, drei):: stmts' ->
        let map1 = match drei.Clang_ast_t.drti_decl_ref with
          | Some decl_ref ->
              (match decl_ref.Clang_ast_t.dr_qual_type with
               | Some qual_type when decl_ref.Clang_ast_t.dr_kind = `Var ->
                   let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
                   let procname = Procdesc.get_proc_name context.CContext.procdesc in
                   let pvar = sil_var_of_decl_ref context decl_ref procname in
                   if Pvar.is_local pvar then
                     Exp.Map.add (Exp.Lvar pvar) typ map
                   else map
               | _ ->
                   map)
          | None ->
              map in
        do_stmts map1 stmts'
    | s :: stmts' ->
        let sl = snd (Clang_ast_proj.get_stmt_tuple s) in
        let map1 = do_stmts map sl in
        do_stmts map1 stmts' in
  Exp.Map.bindings (do_stmts Exp.Map.empty stmts)

(* Returns a list of captured variables as sil variables. *)
let captured_vars_from_block_info context cvl =
  let procname = Procdesc.get_proc_name context.CContext.procdesc in
  let sil_var_of_captured_var cv vars =
    match cv.Clang_ast_t.bcv_variable with
    | Some dr ->
        (match dr.Clang_ast_t.dr_name, dr.Clang_ast_t.dr_qual_type with
         | Some name_info, Some qual_type ->
             let n = name_info.Clang_ast_t.ni_name in
             if String.equal n CFrontend_config.self &&
                not (CContext.is_objc_instance context) then
               vars
             else
               let pvar = sil_var_of_decl_ref context dr procname in
               let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
               (pvar, typ) :: vars
         | _ -> assert false)
    | _ -> assert false in
  List.fold_right ~f:sil_var_of_captured_var cvl ~init:[]
