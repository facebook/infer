(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hashtbl = Caml.Hashtbl
module StmtMap = ClangPointers.Map

type cxx_temporary =
  {pvar: Pvar.t; typ: Typ.t; qual_type: Clang_ast_t.qual_type; marker: (Pvar.t * Sil.if_kind) option}

type var_to_destroy =
  | VarDecl of
      ( Clang_ast_t.decl_info
      * Clang_ast_t.named_decl_info
      * Clang_ast_t.qual_type
      * Clang_ast_t.var_decl_info )
  | CXXTemporary of cxx_temporary

let pp_var_to_destroy fmt = function
  | VarDecl (_, {ni_name}, _, _) ->
      F.pp_print_string fmt ni_name
  | CXXTemporary {pvar} ->
      Pvar.pp Pp.text fmt pvar


type pointer = (* = Clang_ast_t.pointer *) int [@@deriving compare]

type curr_class = ContextClsDeclPtr of pointer | ContextNoCls [@@deriving compare]

type str_node_map = (string, Procdesc.Node.t) Hashtbl.t

type t =
  { translation_unit_context: CFrontend_config.translation_unit_context
  ; tenv: Tenv.t
  ; cfg: Cfg.t
  ; procdesc: Procdesc.t
  ; immediate_curr_class: curr_class
  ; return_param_typ: Typ.t option
  ; outer_context: t option
  ; mutable blocks_static_vars: (Pvar.t * Typ.t) list Procname.Map.t
  ; label_map: str_node_map
  ; vars_to_destroy: var_to_destroy list StmtMap.t
  ; temporary_names: (Clang_ast_t.pointer, Pvar.t * Typ.t) Hashtbl.t
  ; temporaries_constructor_markers: (Pvar.t * Typ.t) Pvar.Map.t }

let create_context translation_unit_context tenv cfg procdesc immediate_curr_class return_param_typ
    outer_context =
  { translation_unit_context
  ; tenv
  ; cfg
  ; procdesc
  ; immediate_curr_class
  ; return_param_typ
  ; outer_context
  ; blocks_static_vars= Procname.Map.empty
  ; label_map= Hashtbl.create 17
  ; vars_to_destroy= StmtMap.empty
  ; temporary_names= Hashtbl.create 0
  ; temporaries_constructor_markers= Pvar.Map.empty }


let rec is_objc_method context =
  match context.outer_context with
  | Some outer_context ->
      is_objc_method outer_context
  | None ->
      context.procdesc |> Procdesc.get_proc_name |> Procname.is_objc_method


let rec is_objc_class_method context =
  match context.outer_context with
  | Some outer_context ->
      is_objc_class_method outer_context
  | None ->
      let attrs = Procdesc.get_attributes context.procdesc in
      ClangMethodKind.equal attrs.ProcAttributes.clang_method_kind ClangMethodKind.OBJC_CLASS


let rec get_curr_class context =
  match (context.immediate_curr_class, context.outer_context) with
  | ContextNoCls, Some outer_context ->
      get_curr_class outer_context
  | _ ->
      context.immediate_curr_class


let get_curr_class_decl_ptr stmt_info curr_class =
  match curr_class with
  | ContextClsDeclPtr ptr ->
      ptr
  | _ ->
      CFrontend_errors.incorrect_assumption __POS__ stmt_info.Clang_ast_t.si_source_range
        "current class is not ContextClsDeclPtr"


let get_curr_class_ptr stmt_info curr_class =
  let decl_ptr = get_curr_class_decl_ptr stmt_info curr_class in
  let get_ptr_from_decl_ref = function
    | Some dr ->
        dr.Clang_ast_t.dr_decl_pointer
    | None ->
        assert false
  in
  (* Resolve categories to their class names *)
  match CAst_utils.get_decl decl_ptr with
  | Some (ObjCCategoryDecl (_, _, _, _, ocdi)) ->
      get_ptr_from_decl_ref ocdi.odi_class_interface
  | Some (ObjCCategoryImplDecl (_, _, _, _, ocidi)) ->
      get_ptr_from_decl_ref ocidi.ocidi_class_interface
  | _ ->
      decl_ptr


let get_curr_class_typename stmt_info context =
  let tenv = context.tenv in
  let curr_class = get_curr_class context in
  match get_curr_class_ptr stmt_info curr_class |> CAst_utils.get_decl with
  | Some decl ->
      CType_decl.get_record_typename ~tenv decl
  | None ->
      assert false


let add_block_static_var context block_name static_var_typ =
  match (context.outer_context, static_var_typ) with
  | Some outer_context, (static_var, _) when Pvar.is_global static_var ->
      let new_static_vars, duplicate =
        try
          let static_vars = Procname.Map.find block_name outer_context.blocks_static_vars in
          if
            List.mem
              ~equal:(fun (var1, _) (var2, _) -> Pvar.equal var1 var2)
              static_vars static_var_typ
          then (static_vars, true)
          else (static_var_typ :: static_vars, false)
        with Caml.Not_found -> ([static_var_typ], false)
      in
      if not duplicate then
        let blocks_static_vars =
          Procname.Map.add block_name new_static_vars outer_context.blocks_static_vars
        in
        outer_context.blocks_static_vars <- blocks_static_vars
  | _ ->
      ()


let rec get_outer_procname context =
  match context.outer_context with
  | Some outer_context ->
      get_outer_procname outer_context
  | None ->
      Procdesc.get_proc_name context.procdesc


module CXXTemporarySet = PrettyPrintable.MakePPSet (struct
  type t = cxx_temporary

  let pp f {pvar; marker} =
    F.fprintf f "(%a,%a)" (Pvar.pp Pp.text_break) pvar
      (Pp.option (Pp.pair ~fst:(Pvar.pp Pp.text_break) ~snd:Sil.pp_if_kind))
      marker


  let compare {pvar= pvar1} {pvar= pvar2} = Pvar.compare pvar1 pvar2
end)
