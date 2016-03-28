(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Javalib_pack
open Sawja_pack

(** data structure for representing whether an instruction is a goto, a return or a standard instruction.   *)
type jump_kind =
  | Next
  | Jump of int
  | Exit


(** data structure for identifying whether a method is the initialiser of a class - that initialises the static fields- or a standard method *)
type meth_kind =
  | Normal
  | Init

(** Hastable for storing nodes that correspond to if-instructions. These are
    used when adding the edges in the contrl flow graph. *)
module NodeTbl : Hashtbl.S with type key = Cfg.Node.t


(** data structure for saving the three structures tht contain the intermediate
    representation of a file: the type environment, the control graph and the control
    flow graph *)
type icfg = {
  tenv : Tenv.t;
  cg : Cg.t;
  cfg : Cfg.cfg;
}

(** data structure for storing the context elements.  *)
type t


(** cretes a context for a given method.   *)
val create_context :
  Inferconfig.NeverReturnNull.matcher ->
  icfg ->
  Cfg.Procdesc.t ->
  JBir.t ->
  JBasics.class_name ->
  meth_kind ->
  JCode.jcode Javalib.interface_or_class ->
  JClasspath.program ->
  t

(** returns the intermediate representation of the Java file from the context.  *)
val get_icfg : t -> icfg

(** returns the code of the method from the context *)
val get_impl : t -> JBir.t

(** returns the class where the method is defined from the context *)
val get_cn : t -> JBasics.class_name

(** returns the type environment that corresponds to the current file. *)
val get_tenv : t -> Tenv.t

(** returns the control graph that corresponds to the current file.  *)
val get_cg : t -> Cg.t

(**  returns the control flow graph that corresponds to the current file. *)
val get_cfg : t -> Cfg.cfg

(** returns the procedure description in the intermediate language for the
    current method. *)
val get_procdesc : t -> Cfg.Procdesc.t

(** returns the method kind of the current method: standard or initialiser of
    static fields. *)
val get_meth_kind : t -> meth_kind

(** adds to the context the line that an if-node will jump to *)
val add_if_jump : t -> Cfg.Node.t -> int -> unit

(** returns whether the given node corresponds to an if-instruction *)
val get_if_jump : t -> Cfg.Node.t -> int option

(** adds to the context the line that the node in the given line will jump to. *)
val add_goto_jump : t -> int -> jump_kind -> unit

(** if the given line corresponds to a goto instruction, then returns the
    line where it jumps to, otherwise returns the next line. *)
val get_goto_jump : t -> int -> jump_kind

(** returns whether the given line corresponds to a goto instruction.  *)
val is_goto_jump : t -> int -> bool

(** returns the Java program *)
val get_program : t -> JClasspath.program

(** returns the current node *)
val get_node : t -> JCode.jcode Javalib.interface_or_class

(** returns a match function for procedures that are never returning null
    according to .inferfonfig *)
val get_never_null_matcher : t -> Inferconfig.NeverReturnNull.matcher

(** [set_pvar context var type] adds a variable with a type to the context  *)
val set_pvar : t -> JBir.var -> Sil.typ -> Pvar.t

(** [get_var_type context var] returns the type of the variable, if the variable is in the context *)
val get_var_type : t -> JBir.var -> Sil.typ option

(** resets the dynamic type of the variables in the context. *)
val reset_pvar_type : t -> unit

(** resets the hashtable mapping methods to their exception nodes  *)
val reset_exn_node_table : unit -> unit

(** adds the exception node for a given method *)
val add_exn_node : Procname.t -> Cfg.Node.t -> unit

(** returns the exception node of a given method *)
val get_exn_node : Cfg.Procdesc.t -> Cfg.Node.t option
