(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Contains current class and current method to be translated as well as local variables, *)
(** and the cg, cfg, and tenv corresponding to the current file. *)

type curr_class =
  | ContextCls of string * string option * string list
  (*class name and name of (optional) super class , and a list of protocols *)
  | ContextCategory of string * string (* category name and corresponding class *)
  | ContextProtocol of string  (* category name and corresponding class *)
  | ContextClsDeclPtr of Clang_ast_t.pointer
  | ContextNoCls

type str_node_map = (string, Cfg.Node.t) Hashtbl.t

type t =
  {
    translation_unit_context : CFrontend_config.translation_unit_context;
    tenv : Tenv.t;
    cg : Cg.t;
    cfg : Cfg.cfg;
    procdesc : Cfg.Procdesc.t;
    is_objc_method : bool;
    curr_class: curr_class;
    return_param_typ : Typ.t option;
    outer_context : t option; (** in case of objc blocks, the context of the method containing the
                                  block *)
    mutable blocks_static_vars : ((Pvar.t * Typ.t) list) Procname.Map.t;
    label_map : str_node_map;
  }

val get_procdesc : t -> Cfg.Procdesc.t

val get_cfg : t -> Cfg.cfg

val get_cg : t -> Cg.t

val get_curr_class : t -> curr_class

val get_curr_class_name : curr_class -> string

val get_curr_class_decl_ptr : curr_class -> Clang_ast_t.pointer

val curr_class_to_string : curr_class -> string

val curr_class_compare : curr_class -> curr_class -> int

val curr_class_equal : curr_class -> curr_class -> bool

val is_objc_method : t -> bool

val get_tenv : t -> Tenv.t

val create_context : CFrontend_config.translation_unit_context -> Tenv.t -> Cg.t -> Cfg.cfg ->
  Cfg.Procdesc.t -> curr_class -> Typ.t option -> bool -> t option -> t

val create_curr_class : Tenv.t -> string -> Csu.class_kind -> curr_class

val add_block_static_var : t -> Procname.t -> (Pvar.t * Typ.t) -> unit

val static_vars_for_block : t -> Procname.t -> (Pvar.t * Typ.t) list

val is_objc_instance : t -> bool

val get_outer_procname : t -> Procname.t
