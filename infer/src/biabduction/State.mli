(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** State of symbolic execution *)

val add_diverging_states : Paths.PathSet.t -> unit
(** Add diverging states *)

val get_diverging_states_node : unit -> Paths.PathSet.t
(** Get the diverging states for the node *)

val get_diverging_states_proc : unit -> Paths.PathSet.t
(** Get the diverging states for the procedure *)

val get_inst_update : PredSymb.path_pos -> Predicates.inst
(** Get update instrumentation for the current loc *)

val get_loc_trace : unit -> Errlog.loc_trace
(** Get the location trace of the last path seen in symbolic execution *)

val get_normalized_pre :
  (Tenv.t -> Prop.normal Prop.t -> Prop.normal Prop.t) -> Prop.normal Prop.t option
(** return the normalized precondition extracted form the last prop seen, if any the abstraction
    function is a parameter to get around module dependencies *)

val get_path : unit -> Paths.Path.t * PredSymb.path_pos option
(** Get last path seen in symbolic execution *)

val get_path_pos : unit -> PredSymb.path_pos
(** Get the last path position seen in symbolic execution *)

val get_prop_tenv_pdesc : unit -> (Prop.normal Prop.t * Tenv.t * Procdesc.t) option
(** Get last last prop,tenv,pdesc seen in symbolic execution *)

val mark_execution_end : Procdesc.Node.t -> unit
(** Mark the end of symbolic execution of a node *)

val mark_execution_start : Procdesc.Node.t -> unit
(** Mark the start of symbolic execution of a node *)

val mark_instr_fail : exn -> unit
(** Mark that the execution of the current instruction failed *)

val mark_instr_ok : unit -> unit
(** Mark that the execution of the current instruction was OK *)

type log_issue = ?node:Procdesc.Node.t -> ?loc:Location.t -> ?ltr:Errlog.loc_trace -> exn -> unit

val process_execution_failures : log_issue -> unit
(** Process the failures during symbolic execution of a procedure *)

val reset : unit -> unit
(** Reset all the global data. *)

val reset_diverging_states_node : unit -> unit
(** Reset the diverging states information for the node *)

val set_path : Paths.Path.t -> PredSymb.path_pos option -> unit
(** Get last path seen in symbolic execution *)

val set_prop_tenv_pdesc : Prop.normal Prop.t -> Tenv.t -> Procdesc.t -> unit
(** Set last prop,tenv,pdesc seen in symbolic execution *)
