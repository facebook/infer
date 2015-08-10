(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module to preprocess location information in the AST.
    The original location information is incremental, each location is a delta
    w.r.t. the previous one. This module processes the AST and makes locations explicit. *)

open Utils
open Clang_ast_j

module L = Logging
module F = Format


(** Get the sub-declarations of the current declaration. *)
let decl_get_sub_decls decl = match decl with
  | CXXRecordDecl (_, _, _, _, decl_list, _, _, _)
  | RecordDecl (_, _, _, _, decl_list, _, _)
  | ObjCInterfaceDecl (_, _, decl_list, _, _)
  | ObjCProtocolDecl (_, _, decl_list, _, _)
  | ObjCCategoryDecl (_, _, decl_list, _, _)
  | ObjCCategoryImplDecl (_, _, decl_list, _, _)
  | ObjCImplementationDecl (_, _, decl_list, _, _)
  | EnumDecl (_, _, _, _, decl_list, _, _)
  | LinkageSpecDecl (_, decl_list, _)
  | NamespaceDecl (_, _, decl_list, _, _) ->
      decl_list
  | _ ->
      []


(** Set the sub-declarations of the current declaration. *)
let decl_set_sub_decls decl decl_list' = match decl with
  | CXXRecordDecl (decl_info, name, opt_type, type_ptr, decl_list, decl_context_info, record_decl_info, cxx_record_info) ->
      CXXRecordDecl (decl_info, name, opt_type, type_ptr, decl_list', decl_context_info, record_decl_info, cxx_record_info)
  | RecordDecl (decl_info, name, opt_type, type_ptr, decl_list, decl_context_info, record_decl_info) ->
      RecordDecl (decl_info, name, opt_type, type_ptr, decl_list', decl_context_info, record_decl_info)
  | ObjCInterfaceDecl (decl_info, name, decl_list, decl_context_info, obj_c_interface_decl_info) ->
      ObjCInterfaceDecl (decl_info, name, decl_list', decl_context_info, obj_c_interface_decl_info)
  | ObjCProtocolDecl (decl_info, name, decl_list, decl_context_info, obj_c_protocol_decl_info) ->
      ObjCProtocolDecl (decl_info, name, decl_list', decl_context_info, obj_c_protocol_decl_info)
  | ObjCCategoryDecl (decl_info, name, decl_list, decl_context_info, category_decl_info) ->
      ObjCCategoryDecl (decl_info, name, decl_list', decl_context_info, category_decl_info)
  | ObjCCategoryImplDecl (decl_info, name, decl_list, decl_context_info, category_impl_info) ->
      ObjCCategoryImplDecl (decl_info, name, decl_list', decl_context_info, category_impl_info)
  | ObjCImplementationDecl (decl_info, class_name, decl_list, decl_context_info, idi) ->
      ObjCImplementationDecl (decl_info, class_name, decl_list', decl_context_info, idi)
  | EnumDecl (decl_info, name, opt_type, type_ptr, decl_list, decl_context_info, enum_decl_info) ->
      EnumDecl (decl_info, name, opt_type, type_ptr, decl_list', decl_context_info, enum_decl_info)
  | LinkageSpecDecl (decl_info, decl_list, decl_context_info) ->
      LinkageSpecDecl (decl_info, decl_list', decl_context_info)
  | NamespaceDecl (decl_info, name, decl_list, decl_context_info, namespace_decl_info) ->
      NamespaceDecl (decl_info, name, decl_list', decl_context_info, namespace_decl_info)
  | _ ->
      decl


(** Pretty print a source location. *)
let pp_source_loc fmt source_loc =
  let file = match source_loc.sl_file with
    | Some file -> file
    | None -> "None" in
  let line = match source_loc.sl_line with
    | Some n -> string_of_int n
    | None -> "None" in
  let column = match source_loc.sl_column with
    | Some n -> string_of_int n
    | None -> "None" in
  if file = "None" && line = "None" && column = "None"
  then F.fprintf fmt "_"
  else F.fprintf fmt "%s:%s:%s" file line column


(** Pretty print a source range. *)
let pp_source_range fmt (sloc1, sloc2) =
  F.fprintf fmt "<%a, %a>" pp_source_loc sloc1 pp_source_loc sloc2


(** Pretty print an AST. *)
let pp_ast_decl fmt ast_decl =
  let rec dump_stmt prefix stmt =
    let prefix1 = prefix ^ "  " in
    let stmt_str = Clang_ast_proj.get_stmt_kind_string stmt in
    let stmt_info, stmt_list = Clang_ast_proj.get_stmt_tuple stmt in
    let decl_list = match stmt with
      | DeclStmt (_, _, decl_list) -> decl_list
      | _ -> [] in
    F.fprintf fmt "%s%s %a@\n"
      prefix
      stmt_str
      pp_source_range stmt_info.si_source_range;
    list_iter (dump_stmt prefix1) stmt_list;
    list_iter (dump_decl prefix1) decl_list
  and dump_decl prefix decl =
    let prefix1 = prefix ^ "  " in
    match decl with
    | FunctionDecl (decl_info, name, qt, fdecl_info) ->
        F.fprintf fmt "%sFunctionDecl %s %a@\n"
          prefix
          name.Clang_ast_t.ni_name
          pp_source_range decl_info.di_source_range;
        list_iter (dump_decl prefix1) fdecl_info.fdi_decls_in_prototype_scope;
        list_iter (dump_decl prefix1) fdecl_info.fdi_parameters;
        Option.may (dump_stmt prefix1) fdecl_info.fdi_body
    | ObjCMethodDecl (decl_info, name, obj_c_method_decl_info) ->
        F.fprintf fmt "%sObjCMethodDecl %s %a@\n"
          prefix
          name.Clang_ast_t.ni_name
          pp_source_range decl_info.di_source_range;
        Option.may (dump_stmt prefix1) obj_c_method_decl_info.omdi_body
    | VarDecl (decl_info, name, qual_type, var_decl_info) ->
        F.fprintf fmt "%sVarDecl %a@\n"
          prefix
          pp_source_range decl_info.di_source_range;
        Option.may (dump_stmt prefix1) var_decl_info.vdi_init_expr
    | _ ->
        let decl_kind_str = Clang_ast_proj.get_decl_kind_string decl in
        let decl_info = Clang_ast_proj.get_decl_tuple decl in
        let decl_list = decl_get_sub_decls decl in
        F.fprintf fmt "%s%s %a@\n"
          prefix
          decl_kind_str
          pp_source_range decl_info.di_source_range;
        list_iter (dump_decl prefix1) decl_list in

  let decl_str = Clang_ast_proj.get_decl_kind_string ast_decl in
  match ast_decl with
  | TranslationUnitDecl (_, decl_list, _, _) ->
      F.fprintf fmt "%s (%d declarations)@\n" decl_str (list_length decl_list);
      list_iter (dump_decl "") decl_list
  | _ ->
      assert false


(** Compose incremental location information and make locations explicit. *)
module LocComposer : sig
  (** Status of the composer. *)
  type status

  (** Create a new composer with the initial status. *)
  val create : unit -> status

  (** Compose a new source_range to the current one. *)
  val compose : status -> source_range -> source_range

  (** Set the current file if specified in the source_range.
      The composer will not descend into file included from the current one.
      For locations in included files, it will return instead the last known
      location of the current file. *)
  val set_current_file : status -> source_range -> unit
end = struct
  type status =
    { mutable curr_file: string option;
      mutable curr_source_range: source_range;
      mutable in_curr_file : bool }

  let empty_sloc = { Clang_ast_t.sl_file = None; sl_line = None; sl_column = None }

  let create () =
    { curr_file = None;
      curr_source_range = (empty_sloc, empty_sloc);
      in_curr_file = true; }

  let set_current_file st (sloc1, sloc2) =
    match sloc1.sl_file, sloc2.sl_file with
    | _, Some fname
    | Some fname, None ->
        st.curr_file <- Some fname;
        st.in_curr_file <- true
    | _ ->
        ()

  let sloc_is_current_file st sloc = match st.curr_file, sloc.sl_file with
    | Some curr_f, Some f ->
        Some (f = curr_f)
    | None, _ -> None
    | _, None -> None

  let update_sloc st old_sloc new_sloc =
    match sloc_is_current_file st new_sloc with
    | Some true ->
        st.in_curr_file <- true;
        new_sloc
    | Some false ->
        st.in_curr_file <- false;
        old_sloc
    | None ->
        if st.in_curr_file
        then
          let update x_opt y_opt =
            if y_opt <> None then y_opt else x_opt in
          { sl_file = update old_sloc.sl_file new_sloc.sl_file;
            sl_line = update old_sloc.sl_line new_sloc.sl_line;
            sl_column = update old_sloc.sl_column new_sloc.sl_column }
        else
          old_sloc

  let update_status st (sloc1, sloc2) =
    if sloc1 = empty_sloc && sloc2 = empty_sloc
    then
      ()
    else
      let _, old_sloc2 = st.curr_source_range in
      let new_sloc1 = update_sloc st old_sloc2 sloc1 in
      let new_sloc2 = update_sloc st new_sloc1 sloc2 in
      st.curr_source_range <- (new_sloc1, new_sloc2)

  let compose st source_range =
    update_status st source_range;
    st.curr_source_range
end


(** Apply a location composer to the locations in a statement. *)
let rec stmt_process_locs loc_composer stmt =
  let update (stmt_info, stmt_list) =
    let stmt_info' =
      { stmt_info with
        si_source_range = LocComposer.compose loc_composer stmt_info.si_source_range } in
    let stmt_list' = list_map (stmt_process_locs loc_composer) stmt_list in
    (stmt_info', stmt_list') in
  match Clang_ast_proj.update_stmt_tuple update stmt with
  | DeclStmt (stmt_info, stmt_list, decl_list) ->
      let decl_list' = list_map (decl_process_locs loc_composer) decl_list in
      DeclStmt (stmt_info, stmt_list, decl_list')
  | stmt' ->
      stmt'

(** Apply a location composer to the locations in a declaration. *)
and decl_process_locs loc_composer decl =
  let decl' =
    let update decl_info =
      { decl_info with
        di_source_range = LocComposer.compose loc_composer decl_info.di_source_range } in
    let decl_list = decl_get_sub_decls decl in
    let decl1 = Clang_ast_proj.update_decl_tuple update decl in
    let decl_list' = list_map (decl_process_locs loc_composer) decl_list in
    decl_set_sub_decls decl1 decl_list' in
  let get_updated_fun_decl (decl_info', name, qt, fdecl_info) =
    let fdi_decls_in_prototype_scope' =
      list_map (decl_process_locs loc_composer) fdecl_info.fdi_decls_in_prototype_scope in
    let fdi_parameters' =
      list_map (decl_process_locs loc_composer) fdecl_info.fdi_parameters in
    let body' = Option.map (stmt_process_locs loc_composer) fdecl_info.fdi_body in
    let fdecl_info' =
      { fdecl_info with
        fdi_body = body';
        fdi_parameters = fdi_parameters';
        fdi_decls_in_prototype_scope = fdi_decls_in_prototype_scope'; } in
    (decl_info', name, qt, fdecl_info') in
  match decl' with
  | FunctionDecl fun_info -> FunctionDecl (get_updated_fun_decl fun_info)
  | CXXMethodDecl fun_info -> CXXMethodDecl (get_updated_fun_decl fun_info)
  | ObjCMethodDecl (decl_info', name, obj_c_method_decl_info) ->
      let body' =
        Option.map (stmt_process_locs loc_composer) obj_c_method_decl_info.omdi_body in
      let obj_c_method_decl_info' = { obj_c_method_decl_info with omdi_body = body' } in
      ObjCMethodDecl (decl_info', name, obj_c_method_decl_info')
  | VarDecl (decl_info, string, qual_type, var_decl_info) ->
      let vdi_init_expr' =
        Option.map (stmt_process_locs loc_composer) var_decl_info.vdi_init_expr in
      let var_decl_info' =
        { var_decl_info with vdi_init_expr = vdi_init_expr' } in
      VarDecl (decl_info, string, qual_type, var_decl_info')
  | _ ->
      decl'


(** Process locations in the AST by making them explicit.
    Each toplevel declaration determines the current file,
    and once diving into the details of the declaration, location
    information about other (include) files is ignored. *)
let ast_decl_process_locs loc_composer ast_decl =

  let toplevel_decl_process_locs decl =
    let decl_info = Clang_ast_proj.get_decl_tuple decl in
    LocComposer.set_current_file loc_composer decl_info.di_source_range;
    decl_process_locs loc_composer decl in

  match ast_decl with
  | TranslationUnitDecl (decl_info, decl_list, decl_context_info, type_list) ->
      let decl_list' = list_map toplevel_decl_process_locs decl_list in
      TranslationUnitDecl (decl_info, decl_list', decl_context_info, type_list)
  | _ ->
      assert false


let preprocess_ast_decl ast_decl =
  let loc_composer = LocComposer.create () in
  ast_decl_process_locs loc_composer ast_decl
