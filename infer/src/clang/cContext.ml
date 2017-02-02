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

type _super = string option
let compare__super _ _ = 0

type _protos = string list
let compare__protos _ _ = 0

type curr_class =
  | ContextCls of string * _super * _protos
  (*class name and name of (optional) super class , and a list of protocols *)
  | ContextCategory of string * string (* category name and corresponding class *)
  | ContextProtocol of string  (* category name and corresponding class *)
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
    mutable blocks_static_vars : ((Pvar.t * Typ.t) list) Procname.Map.t;
    label_map : str_node_map;
  }

let create_context translation_unit_context tenv cg cfg procdesc curr_class return_param_typ
    is_objc_method outer_context =
  { translation_unit_context; tenv; cg; cfg; procdesc; curr_class; return_param_typ;
    is_objc_method; outer_context;
    blocks_static_vars = Procname.Map.empty;
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

let get_curr_class_name curr_class =
  match curr_class with
  | ContextCls (name, _, _) -> name
  | ContextCategory (_, cls) -> cls
  | ContextProtocol name -> name
  | ContextClsDeclPtr _ -> assert false
  | ContextNoCls -> assert false

let get_curr_class_decl_ptr curr_class =
  match curr_class with
  | ContextClsDeclPtr ptr -> ptr
  | _ -> assert false

let curr_class_to_string curr_class =
  match curr_class with
  | ContextCls (name, superclass, protocols) ->
      ("class " ^ name ^ ", superclass: " ^ (Option.value ~default:"" superclass) ^
       ",  protocols: " ^ (IList.to_string (fun x -> x) protocols))
  | ContextCategory (name, cls) -> ("category " ^ name ^ " of class " ^ cls)
  | ContextProtocol name -> ("protocol " ^ name)
  | ContextClsDeclPtr ptr -> ("decl_ptr: " ^ string_of_int ptr)
  | ContextNoCls -> "no class"

let create_curr_class tenv class_name ck =
  let class_tn_name = Typename.TN_csu (Csu.Class ck, (Mangled.from_string class_name)) in
  match Tenv.lookup tenv class_tn_name with
  | Some { supers } ->
      (let supers_names = IList.map Typename.name supers in
       match supers_names with
       | superclass:: protocols ->
           ContextCls (class_name, Some superclass, protocols)
       | [] -> ContextCls (class_name, None, []))
  | _ -> assert false

let add_block_static_var context block_name static_var_typ =
  match context.outer_context, static_var_typ with
  | Some outer_context, (static_var, _) when Pvar.is_global static_var ->
      (let new_static_vars, duplicate =
         try
           let static_vars = Procname.Map.find block_name outer_context.blocks_static_vars in
           if List.mem ~equal:(
               fun (var1, _) (var2, _) -> Pvar.equal var1 var2
             ) static_vars static_var_typ  then
             static_vars, true
           else
             static_var_typ :: static_vars, false
         with Not_found -> [static_var_typ], false in
       if not duplicate then
         let blocks_static_vars =
           Procname.Map.add block_name new_static_vars outer_context.blocks_static_vars in
         outer_context.blocks_static_vars <- blocks_static_vars)
  | _ -> ()

let static_vars_for_block context block_name =
  try Procname.Map.find block_name context.blocks_static_vars
  with Not_found -> []

let rec get_outer_procname context =
  match context.outer_context with
  | Some outer_context -> get_outer_procname outer_context
  | None -> Procdesc.get_proc_name context.procdesc
