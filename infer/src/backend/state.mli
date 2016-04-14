(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** State of symbolic execution *)

(**  Internal state *)
type t

(** Add diverging states *)
val add_diverging_states : Paths.PathSet.t -> unit

type const_map = Cfg.Node.t -> Sil.exp -> Sil.const option

(** Get the constant map for the current procedure. *)
val get_const_map : unit -> const_map

(** Get the diverging states for the node *)
val get_diverging_states_node : unit -> Paths.PathSet.t

(** Get the diverging states for the procedure *)
val get_diverging_states_proc : unit -> Paths.PathSet.t

(** Get update instrumentation for the current loc *)
val get_inst_update : Sil.path_pos -> Sil.inst

(** Get last instruction seen in symbolic execution *)
val get_instr : unit -> Sil.instr option

(** Get last location seen in symbolic execution *)
val get_loc : unit -> Location.t

(** Get the location trace of the last path seen in symbolic execution *)
val get_loc_trace : unit -> Errlog.loc_trace

(** Get last node seen in symbolic execution *)
val get_node : unit -> Cfg.Node.t

(** Get id of last node seen in symbolic execution *)
val get_node_id : unit -> Cfg.Node.id

(** Get id and key of last node seen in symbolic execution *)
val get_node_id_key : unit -> Cfg.Node.id * int

(** return the normalized precondition extracted form the last prop seen, if any
    the abstraction function is a parameter to get around module dependencies *)
val get_normalized_pre :
  (Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t) -> Prop.normal Prop.t option

(** Get last path seen in symbolic execution *)
val get_path : unit -> Paths.Path.t * (Sil.path_pos option)

(** Get the last path position seen in symbolic execution *)
val get_path_pos : unit -> Sil.path_pos

(** Get last last prop,tenv,pdesc seen in symbolic execution *)
val get_prop_tenv_pdesc : unit -> (Prop.normal Prop.t * Tenv.t * Cfg.Procdesc.t) option

(** Get last session seen in symbolic execution *)
val get_session : unit -> int

(** Mark the end of symbolic execution of a node *)
val mark_execution_end : Cfg.Node.t -> unit

(** Mark the start of symbolic execution of a node *)
val mark_execution_start : Cfg.Node.t -> unit

(** Mark that the execution of the current instruction failed *)
val mark_instr_fail : (Prop.normal Prop.t) option -> exn -> unit

(** Mark that the execution of the current instruction was OK *)
val mark_instr_ok : unit -> unit

(** Create a function to find duplicate nodes.
    A node is a duplicate of another one if they have the same kind and location
    and normalized (w.r.t. renaming of let - bound ids) list of instructions. *)
val mk_find_duplicate_nodes: Cfg.Procdesc.t -> (Cfg.Node.t -> Cfg.NodeSet.t)

type log_issue =
  Procname.t ->
  ?loc: Location.t option ->
  ?node_id: (int * int) option ->
  ?session: int option ->
  ?ltr: Errlog.loc_trace option ->
  ?pre: Prop.normal Prop.t option ->
  exn ->
  unit

(** Process the failures during symbolic execution of a procedure *)
val process_execution_failures : log_issue -> Procname.t -> unit

(** Reset all the global data. *)
val reset : unit -> unit

(** Reset the diverging states information for the node *)
val reset_diverging_states_node : unit -> unit

(** Restore the old state. *)
val restore_state : t -> unit

(** Return the old state, and revert the current state to the initial one. *)
val save_state : unit -> t

(** Set the constant map for the current procedure. *)
val set_const_map : const_map -> unit

(** Set last instruction seen in symbolic execution *)
val set_instr : Sil.instr -> unit

(** Set last node seen in symbolic execution *)
val set_node : Cfg.node -> unit

(** Get last path seen in symbolic execution *)
val set_path : Paths.Path.t -> Sil.path_pos option -> unit

(** Set last prop,tenv,pdesc seen in symbolic execution *)
val set_prop_tenv_pdesc : Prop.normal Prop.t -> Tenv.t -> Cfg.Procdesc.t -> unit

(** Set last session seen in symbolic execution *)
val set_session : int -> unit
