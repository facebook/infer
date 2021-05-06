(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let is_custom_var_pointer pointer = pointer <= 0

let mk_sil_global_var {CFrontend_config.source_file} ?(mk_name = fun _ x -> x) decl_info
    named_decl_info var_decl_info qt =
  let name_string, simple_name =
    CGeneral_utils.get_var_name_mangled decl_info named_decl_info var_decl_info
  in
  let translation_unit =
    match Clang_ast_t.(var_decl_info.vdi_is_extern, var_decl_info.vdi_init_expr) with
    | true, None ->
        None
    | _, None when var_decl_info.Clang_ast_t.vdi_is_static_data_member ->
        (* non-const static data member get extern scope unless they are defined out of line here
           (in which case vdi_init_expr will not be None) *)
        None
    | true, Some _
    (* "extern" variables with initialisation code are not extern at all, but compilers accept this
    *)
    | false, _ ->
        Some source_file
  in
  let is_constexpr = var_decl_info.Clang_ast_t.vdi_is_constexpr in
  let is_ice = var_decl_info.Clang_ast_t.vdi_is_init_ice in
  let is_pod = CGeneral_utils.is_type_pod qt in
  let is_static_global =
    var_decl_info.Clang_ast_t.vdi_is_global
    (* only top-level declarations really have file scope, static field members have a global scope
       *)
    && (not var_decl_info.Clang_ast_t.vdi_is_static_data_member)
    && var_decl_info.Clang_ast_t.vdi_is_static
  in
  let is_const = qt.Clang_ast_t.qt_is_const in
  let desugared_type = CAst_utils.get_desugared_type qt.Clang_ast_t.qt_type_ptr in
  let is_constant_array =
    Option.exists desugared_type ~f:(function Clang_ast_t.ConstantArrayType _ -> true | _ -> false)
  in
  Pvar.mk_global ~is_constexpr ~is_ice ~is_pod
    ~is_static_local:var_decl_info.Clang_ast_t.vdi_is_static_local ~is_static_global
    ~is_constant_array ~is_const ?translation_unit (mk_name name_string simple_name)


let mk_sil_var trans_unit_ctx named_decl_info decl_info_qual_type_opt procname outer_procname =
  match decl_info_qual_type_opt with
  | Some (decl_info, qt, var_decl_info, should_be_mangled) ->
      let name_string, simple_name =
        CGeneral_utils.get_var_name_mangled decl_info named_decl_info var_decl_info
      in
      if var_decl_info.Clang_ast_t.vdi_is_global then
        let mk_name =
          if var_decl_info.Clang_ast_t.vdi_is_static_local then
            Some
              (fun name_string _ ->
                Mangled.from_string (F.asprintf "%a.%s" Procname.pp outer_procname name_string) )
          else None
        in
        mk_sil_global_var trans_unit_ctx ?mk_name decl_info named_decl_info var_decl_info qt
      else if not should_be_mangled then Pvar.mk simple_name procname
      else
        let start_location = fst decl_info.Clang_ast_t.di_source_range in
        let line_opt = start_location.Clang_ast_t.sl_line in
        let line_str = match line_opt with Some line -> string_of_int line | None -> "" in
        let mangled = Utils.string_crc_hex32 line_str in
        let mangled_name = Mangled.mangled name_string mangled in
        Pvar.mk mangled_name procname
  | None ->
      let name_string =
        CAst_utils.get_qualified_name named_decl_info |> QualifiedCppName.to_qual_string
      in
      Pvar.mk (Mangled.from_string name_string) procname


let sil_var_of_decl context var_decl procname =
  let outer_procname = CContext.get_outer_procname context in
  let trans_unit_ctx = context.CContext.translation_unit_context in
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, name_info, qual_type, var_decl_info)
  | VarTemplateSpecializationDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let shoud_be_mangled = not (is_custom_var_pointer decl_info.Clang_ast_t.di_pointer) in
      let var_decl_details = Some (decl_info, qual_type, var_decl_info, shoud_be_mangled) in
      mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
  | ParmVarDecl (decl_info, name_info, qual_type, var_decl_info) ->
      let var_decl_details = Some (decl_info, qual_type, var_decl_info, false) in
      mk_sil_var trans_unit_ctx name_info var_decl_details procname outer_procname
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
      mk_sil_var trans_unit_ctx name None procname outer_procname
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
            CFrontend_errors.incorrect_assumption __POS__ source_range
              "pointer '%d' for var decl not found. The var decl was: %a" pointer
              (Pp.of_string ~f:Clang_ast_j.string_of_decl_ref)
              decl_ref )


let has_block_attribute decl_info =
  let open Clang_ast_t in
  List.exists decl_info.di_attributes ~f:(fun attr ->
      match attr with `BlocksAttr _ -> true | _ -> false )


let add_var_to_locals procdesc var_decl typ pvar =
  let open Clang_ast_t in
  match var_decl with
  | VarDecl (decl_info, _, _, vdi) ->
      if not vdi.Clang_ast_t.vdi_is_global then
        let modify_in_block = has_block_attribute decl_info in
        let is_constexpr =
          vdi.Clang_ast_t.vdi_is_constexpr
          || (Typ.is_const typ.Typ.quals && vdi.Clang_ast_t.vdi_is_init_expr_cxx11_constant)
        in
        let is_declared_unused =
          List.exists decl_info.di_attributes ~f:(function `UnusedAttr _ -> true | _ -> false)
        in
        let var_data : ProcAttributes.var_data =
          {name= Pvar.get_name pvar; typ; modify_in_block; is_constexpr; is_declared_unused}
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
        if is_block_inside_objc_class_method && String.equal ni_name CFrontend_config.self then None
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
      let modify_in_block =
        match CAst_utils.get_decl decl_ref.Clang_ast_t.dr_decl_pointer with
        | Some (VarDecl (decl_info, _, _, _)) ->
            has_block_attribute decl_info
        | _ ->
            false
      in
      Some (var, typ, modify_in_block)
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


let mk_temp_sil_var procdesc ~name =
  let procname = Procdesc.get_proc_name procdesc in
  Pvar.mk_tmp name procname


let mk_temp_sil_var_for_expr context ~name ~clang_pointer expr_info =
  match Caml.Hashtbl.find_opt context.CContext.temporary_names clang_pointer with
  | Some pvar_typ ->
      pvar_typ
  | None ->
      let qual_type = expr_info.Clang_ast_t.ei_qual_type in
      let typ = CType_decl.qual_type_to_sil_type context.CContext.tenv qual_type in
      let pvar_typ = (mk_temp_sil_var context.CContext.procdesc ~name, typ) in
      Caml.Hashtbl.add context.CContext.temporary_names clang_pointer pvar_typ ;
      pvar_typ


let materialize_cpp_temporary context stmt_info expr_info =
  (* the type we get here is a 'best effort' type - the translation may decide to use different type
     later *)
  mk_temp_sil_var_for_expr context ~name:Pvar.materialized_cpp_temporary
    ~clang_pointer:stmt_info.Clang_ast_t.si_pointer expr_info
