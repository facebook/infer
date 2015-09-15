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

type varMap
type pointerVarMap

type curr_class =
  | ContextCls of string * string option * string list
  (*class name and name of (optional) super class , and a list of protocols *)
  | ContextCategory of string * string (* category name and corresponding class *)
  | ContextProtocol of string  (* category name and corresponding class *)
  | ContextNoCls

type t =
  {
    tenv : Sil.tenv;
    cg : Cg.t;
    cfg : Cfg.cfg;
    procdesc : Cfg.Procdesc.t;
    is_objc_method : bool;
    is_instance : bool;
    curr_class: curr_class;
    is_callee_expression : bool;
    namespace: string option; (* contains the name of the namespace if we are in the scope of one*)
    mutable local_vars : (Mangled.t * Sil.typ * bool) list; (* (name, type, is_static flag) *)
    mutable captured_vars : (Mangled.t * Sil.typ * bool) list; (* (name, type, is_static flag) *)
    mutable local_vars_stack : varMap;
    mutable local_vars_pointer : pointerVarMap
  }

module LocalVars :
sig
  val find_var_with_pointer : t -> string -> Sil.pvar
  val lookup_var: t -> string -> string -> Clang_ast_t.decl_kind -> Sil.pvar option
  val add_pointer_var : string -> Sil.pvar -> t -> unit
  val enter_and_leave_scope : t -> (t -> 'a -> 'b) -> 'a -> unit
  val add_local_var : t -> string -> Sil.typ -> string -> bool -> unit
  val reset_block : unit -> unit
  val add_pointer_var : string -> Sil.pvar -> t -> unit
end

val get_procdesc : t -> Cfg.Procdesc.t

val get_cfg : t -> Cfg.cfg

val get_cg : t -> Cg.t

val get_curr_class : t -> curr_class

val get_curr_class_name : curr_class -> string

val curr_class_to_string : curr_class -> string

val curr_class_compare : curr_class -> curr_class -> int

val curr_class_equal : curr_class -> curr_class -> bool

val curr_class_hash : curr_class -> int

val is_objc_method : t -> bool

val get_tenv : t -> Sil.tenv

val create_context : Sil.tenv -> Cg.t -> Cfg.cfg -> Cfg.Procdesc.t ->
  string option -> curr_class -> bool -> (Mangled.t * Sil.typ * bool) list -> bool -> t

val create_curr_class : Sil.tenv -> string -> curr_class
