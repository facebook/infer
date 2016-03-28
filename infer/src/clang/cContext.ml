(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Contains current class and current method to be translated as well as local variables, *)
(** and the cg, cfg, and tenv corresponding to the current file. *)

module L = Logging

type curr_class =
  | ContextCls of string * string option * string list
  (*class name and name of (optional) super class , and a list of protocols *)
  | ContextCategory of string * string (* category name and corresponding class *)
  | ContextProtocol of string  (* category name and corresponding class *)
  | ContextNoCls

type t =
  {
    tenv : Tenv.t;
    cg : Cg.t;
    cfg : Cfg.cfg;
    procdesc : Cfg.Procdesc.t;
    is_objc_method : bool;
    curr_class: curr_class;
    return_param_typ : Sil.typ option;
    is_callee_expression : bool;
    outer_context : t option; (* in case of objc blocks, the context of the method containing the block *)
    mutable blocks_static_vars : ((Pvar.t * Sil.typ) list) Procname.Map.t;
  }

let create_context tenv cg cfg procdesc curr_class return_param_typ is_objc_method context_opt =
  { tenv = tenv;
    cg = cg;
    cfg = cfg;
    procdesc = procdesc;
    curr_class = curr_class;
    return_param_typ = return_param_typ;
    is_callee_expression = false;
    is_objc_method = is_objc_method;
    outer_context = context_opt;
    blocks_static_vars = Procname.Map.empty
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
      let attrs = Cfg.Procdesc.get_attributes context.procdesc in
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
  | ContextNoCls -> assert false

let curr_class_to_string curr_class =
  match curr_class with
  | ContextCls (name, superclass, protocols) ->
      ("class " ^ name ^ ", superclass: " ^ (Option.default "" superclass) ^
       ",  protocols: " ^ (IList.to_string (fun x -> x) protocols))
  | ContextCategory (name, cls) -> ("category " ^ name ^ " of class " ^ cls)
  | ContextProtocol name -> ("protocol " ^ name)
  | ContextNoCls -> "no class"

let curr_class_compare curr_class1 curr_class2 =
  match curr_class1, curr_class2 with
  | ContextCls (name1, _, _), ContextCls (name2, _, _) ->
      String.compare name1 name2
  | ContextCls (_, _, _), _ -> -1
  | _, ContextCls (_, _, _) -> 1
  | ContextCategory (name1, cls1), ContextCategory (name2, cls2) ->
      pair_compare String.compare String.compare (name1, cls1) (name2, cls2)
  | ContextCategory (_, _), _ -> -1
  | _, ContextCategory (_, _) -> 1
  | ContextProtocol name1, ContextProtocol name2 ->
      String.compare name1 name2
  | ContextProtocol _, _ -> -1
  | _, ContextProtocol _ -> 1
  | ContextNoCls, ContextNoCls -> 0

let curr_class_equal curr_class1 curr_class2 =
  curr_class_compare curr_class1 curr_class2 == 0

let curr_class_hash curr_class =
  match curr_class with
  | ContextCls (name, _, _) -> Hashtbl.hash name
  | ContextCategory (name, cls) -> Hashtbl.hash (name, cls)
  | ContextProtocol name -> Hashtbl.hash name
  | ContextNoCls -> Hashtbl.hash "no class"

let create_curr_class tenv class_name ck =
  let class_tn_name = Typename.TN_csu (Csu.Class ck, (Mangled.from_string class_name)) in
  match Tenv.lookup tenv class_tn_name with
  | Some { Sil.superclasses } ->
      (let superclasses_names = IList.map Typename.name superclasses in
       match superclasses_names with
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
           if IList.mem (
               fun (var1, _) (var2, _) -> Pvar.equal var1 var2
             ) static_var_typ static_vars then
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
  | None -> Cfg.Procdesc.get_proc_name context.procdesc

let is_curr_proc_objc_getter context field_name =
  let attrs = Cfg.Procdesc.get_attributes context.procdesc in
  match attrs.ProcAttributes.objc_accessor with
  | Some ProcAttributes.Objc_getter field ->
      Ident.fieldname_equal field field_name
  | _ -> false

let is_curr_proc_objc_setter context field_name =
  let attrs = Cfg.Procdesc.get_attributes context.procdesc in
  match attrs.ProcAttributes.objc_accessor with
  | Some ProcAttributes.Objc_setter field ->
      Ident.fieldname_equal field field_name
  | _ -> false
