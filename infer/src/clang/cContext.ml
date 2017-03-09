(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl

(** Contains current class and current method to be translated as well as local variables, *)
(** and the cg, cfg, and tenv corresponding to the current file. *)

module L = Logging

type pointer (* = Clang_ast_t.pointer *) = int [@@deriving compare]

type curr_class =
  | ContextClsDeclPtr of pointer
  | ContextNoCls
[@@deriving compare]

let equal_curr_class = [%compare.equal : curr_class]

type str_node_map = (string, Procdesc.Node.t) Hashtbl.t

type t =
  {
    translation_unit_context : CFrontend_config.translation_unit_context;
    tenv : Tenv.t;
    cg : Cg.t;
    cfg : Cfg.cfg;
    procdesc : Procdesc.t;
    is_objc_method : bool;
    curr_class: curr_class;
    return_param_typ : Typ.t option;
    outer_context : t option; (** in case of objc blocks, the context of the method containing the
                                  block *)
    mutable blocks_static_vars : ((Pvar.t * Typ.t) list) Typ.Procname.Map.t;
    label_map : str_node_map;
  }

let create_context translation_unit_context tenv cg cfg procdesc curr_class return_param_typ
    is_objc_method outer_context =
  { translation_unit_context; tenv; cg; cfg; procdesc; curr_class; return_param_typ;
    is_objc_method; outer_context;
    blocks_static_vars = Typ.Procname.Map.empty;
    label_map = Hashtbl.create 17;
  }

let get_cfg context = context.cfg

let get_cg context = context.cg

let get_tenv context = context.tenv

let get_procdesc context = context.procdesc

let rec is_objc_method context =
  match context.outer_context with
  | Some outer_context -> is_objc_method outer_context
  | None -> context.is_objc_method

let rec is_objc_instance context =
  match context.outer_context with
  | Some outer_context -> is_objc_instance outer_context
  | None ->
      let attrs = Procdesc.get_attributes context.procdesc in
      attrs.ProcAttributes.is_objc_instance_method

let rec get_curr_class context =
  match context.curr_class, context.outer_context with
  | ContextNoCls, Some outer_context ->
      get_curr_class outer_context
  |  _ -> context.curr_class

let get_curr_class_decl_ptr curr_class =
  match curr_class with
  | ContextClsDeclPtr ptr -> ptr
  | _ -> assert false

let get_curr_class_ptr curr_class =
  let decl_ptr = get_curr_class_decl_ptr curr_class in
  let get_ptr_from_decl_ref = function
    | Some dr -> dr.Clang_ast_t.dr_decl_pointer
    | None -> assert false in
  (* Resolve categories to their class names *)
  match CAst_utils.get_decl decl_ptr with
  | Some ObjCCategoryDecl (_, _, _, _, ocdi) ->
      get_ptr_from_decl_ref ocdi.odi_class_interface
  | Some ObjCCategoryImplDecl (_, _, _, _, ocidi) ->
      get_ptr_from_decl_ref ocidi.ocidi_class_interface
  | _ -> decl_ptr

let get_curr_class_name curr_class =
  let class_decl_ptr = get_curr_class_ptr curr_class in
  let _, name_info = match Option.bind
                             (CAst_utils.get_decl class_decl_ptr)
                             Clang_ast_proj.get_named_decl_tuple with
  | Some result -> result
  | None -> assert false in
  CAst_utils.get_qualified_name name_info

let get_curr_class_typename curr_class =
  match get_curr_class_ptr curr_class |> CAst_utils.get_decl with
  | Some decl -> CType_decl.get_record_typename decl
  | None -> assert false

let curr_class_to_string curr_class =
  match curr_class with
  | ContextClsDeclPtr ptr -> ("decl_ptr: " ^ string_of_int ptr)
  | ContextNoCls -> "no class"

let add_block_static_var context block_name static_var_typ =
  match context.outer_context, static_var_typ with
  | Some outer_context, (static_var, _) when Pvar.is_global static_var ->
      (let new_static_vars, duplicate =
         try
           let static_vars = Typ.Procname.Map.find block_name outer_context.blocks_static_vars in
           if List.mem ~equal:(
               fun (var1, _) (var2, _) -> Pvar.equal var1 var2
             ) static_vars static_var_typ  then
             static_vars, true
           else
             static_var_typ :: static_vars, false
         with Not_found -> [static_var_typ], false in
       if not duplicate then
         let blocks_static_vars =
           Typ.Procname.Map.add block_name new_static_vars outer_context.blocks_static_vars in
         outer_context.blocks_static_vars <- blocks_static_vars)
  | _ -> ()

let static_vars_for_block context block_name =
  try Typ.Procname.Map.find block_name context.blocks_static_vars
  with Not_found -> []

let rec get_outer_procname context =
  match context.outer_context with
  | Some outer_context -> get_outer_procname outer_context
  | None -> Procdesc.get_proc_name context.procdesc
