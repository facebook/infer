(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Process variable declarations by saving them as local or global variables.  *)
(** Computes the local variables of a function or method to be added to the procdesc *)

open CFrontend_utils

module L = Logging

let is_custom_var_pointer pointer =
  pointer <= 0

let is_captured procdesc vname =
  IList.exists
    (fun (name, _) -> (Mangled.to_string name) = vname)
    (Cfg.Procdesc.get_captured procdesc)

let sil_var_of_decl context var_decl procname =
  let outer_procname = CContext.get_outer_procname context in
  let procdesc = context.CContext.procdesc in
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, name_info, type_ptr, var_decl_info) ->
      let shoud_be_mangled =
        not (is_custom_var_pointer decl_info.Clang_ast_t.di_pointer) &&
        not (is_captured procdesc name_info.Clang_ast_t.ni_name) in
      let var_decl_details = Some (decl_info, type_ptr, var_decl_info, shoud_be_mangled) in
      General_utils.mk_sil_var name_info var_decl_details procname outer_procname
  | ParmVarDecl (decl_info, name_info, type_ptr, var_decl_info) ->
      let var_decl_details = Some (decl_info, type_ptr, var_decl_info, false) in
      General_utils.mk_sil_var name_info var_decl_details procname outer_procname
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
      General_utils.mk_sil_var name None procname outer_procname
  | _ ->
      if is_custom_var_pointer pointer then
        Pvar.mk (Mangled.from_string name.Clang_ast_t.ni_name) procname
      else match Ast_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
        | Some var_decl -> sil_var_of_decl context var_decl procname
        | None -> assert false

let add_var_to_locals procdesc var_decl sil_typ pvar =
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (_, _, _, vdi) ->
      if not vdi.Clang_ast_t.vdi_is_global then
        Cfg.Procdesc.append_locals procdesc [(Pvar.get_name pvar, sil_typ)]
  | _ -> assert false

let rec compute_autorelease_pool_vars context stmts =
  let procname = Cfg.Procdesc.get_proc_name context.CContext.procdesc in
  match stmts with
  | [] -> []
  | Clang_ast_t.DeclRefExpr (_, _, _, drei):: stmts' ->
      (let res = compute_autorelease_pool_vars context stmts' in
       match drei.Clang_ast_t.drti_decl_ref with
       | Some decl_ref ->
           (match decl_ref.Clang_ast_t.dr_type_ptr with
            | Some type_ptr when decl_ref.Clang_ast_t.dr_kind = `Var ->
                let typ = CTypes_decl.type_ptr_to_sil_type context.CContext.tenv type_ptr in
                let pvar = sil_var_of_decl_ref context decl_ref procname in
                if Pvar.is_local pvar then
                  General_utils.append_no_duplicateds [(Sil.Lvar pvar, typ)] res
                else res
            | _ -> res)
       | _ -> res)
  | s :: stmts' ->
      let sl = snd (Clang_ast_proj.get_stmt_tuple s) in
      compute_autorelease_pool_vars context (sl @ stmts')


(* Returns a list of captured variables as sil variables. *)
let captured_vars_from_block_info context cvl =
  let procname = Cfg.Procdesc.get_proc_name context.CContext.procdesc in
  let sil_var_of_captured_var cv vars =
    match cv.Clang_ast_t.bcv_variable with
    | Some dr ->
        (match dr.Clang_ast_t.dr_name, dr.Clang_ast_t.dr_type_ptr with
         | Some name_info, Some type_ptr ->
             let n = name_info.Clang_ast_t.ni_name in
             if n = CFrontend_config.self && not (CContext.is_objc_instance context) then
               vars
             else
               let pvar = sil_var_of_decl_ref context dr procname in
               let typ = CTypes_decl.type_ptr_to_sil_type context.CContext.tenv type_ptr in
               (pvar, typ) :: vars
         | _ -> assert false)
    | _ -> assert false in
  IList.fold_right sil_var_of_captured_var cvl []
