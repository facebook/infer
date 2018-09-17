(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Process variable declarations by saving them as local or global variables.  *)

(** Computes the local variables of a function or method to be added to the procdesc *)

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
  | _ -> (
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
              decl_ref )


let has_block_attribute decl_info =
  let open Clang_ast_t in
  List.exists decl_info.di_attributes ~f:(fun attr ->
      match attr with BlocksAttr _ -> true | _ -> false )


let add_var_to_locals procdesc var_decl typ pvar =
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, _, _, vdi) ->
      if not vdi.Clang_ast_t.vdi_is_global then
        let modify_in_block = has_block_attribute decl_info in
        let is_constexpr =
          vdi.Clang_ast_t.vdi_is_const_expr
          || (Typ.is_const typ.Typ.quals && vdi.Clang_ast_t.vdi_is_init_expr_cxx11_constant)
        in
        let var_data : ProcAttributes.var_data =
          {name= Pvar.get_name pvar; typ; modify_in_block; is_constexpr}
        in
        Procdesc.append_locals procdesc [var_data]
  | _ ->
      assert false


(* The context here is of the method that contains the block *)
let sil_var_of_captured_var context source_range procname decl_ref =
  let is_block_inside_objc_class_method = CContext.is_objc_class_method context in
  let var_opt =
    match decl_ref with
    | {Clang_ast_t.dr_name= Some {Clang_ast_t.ni_name}} ->
        (* In Objective-C class methods, self is not the standard self instance, since in this
        context we don't have an instance. Instead it is used to get the class of the method.
        We translate this variables in a different way than normal, we don't treat them as
        variables in Sil, instead we remove them and get the class directly in the frontend.
        For that reason, we shouldn't add them as captured variables of blocks, since they
        don't appear anywhere else in the translation. *)
        if is_block_inside_objc_class_method && String.equal ni_name CFrontend_config.self then
          None
        else Some (sil_var_of_decl_ref context source_range decl_ref procname)
    | _ ->
        assert false
  in
  let typ_opt =
    CType_decl.type_of_captured_var context.CContext.tenv ~is_block_inside_objc_class_method
      decl_ref
  in
  match (var_opt, typ_opt) with
  | Some var, Some typ ->
      Some (var, typ)
  | None, None ->
      None
  | _ ->
      Logging.die InternalError
        "Not possible case, captured variable and its type should both be available or not at %s"
        (Clang_ast_j.string_of_source_range source_range)


(* Returns a list of captured variables as sil variables. *)
let captured_vars_from_block_info context source_range captured_vars =
  let procname = Procdesc.get_proc_name context.CContext.procdesc in
  let cv_decl_ref_list =
    List.map ~f:(fun cv -> Option.value_exn cv.Clang_ast_t.bcv_variable) captured_vars
  in
  List.filter_map ~f:(sil_var_of_captured_var context source_range procname) cv_decl_ref_list
