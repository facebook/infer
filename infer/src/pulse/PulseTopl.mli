(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

type value = AbstractValue.t

type event =
  | ArrayWrite of {aw_array: value; aw_index: value}
  | Call of {return: value option; arguments: value list; procname: Procname.t}

type state [@@deriving compare, equal]

val start : unit -> state
(** Return the initial state of [Topl.automaton ()]. *)

val small_step :
     Location.t
  -> keep:AbstractValue.Set.t
  -> get_dynamic_type:(value -> Typ.t option)
  -> path_condition:PathCondition.t
  -> event
  -> state
  -> state

val large_step :
     call_location:Location.t
  -> callee_proc_name:Procname.t
  -> substitution:(value * ValueHistory.t) AbstractValue.Map.t
  -> keep:AbstractValue.Set.t
  -> get_dynamic_type:(value -> Typ.t option)
  -> path_condition:PathCondition.t
  -> callee_prepost:state
  -> state
  -> state
(** [large_step ~substitution ~keep ~condition state ~callee_prepost] updates [state] according to
    [callee_prepost]. The abstract values in [condition] and [state] are in one scope, and those in
    [callee_prepost] in another scope: the [substitution] maps from the callee scope to the
    condition&state scope. *)

val filter_for_summary :
  get_dynamic_type:(value -> Typ.t option) -> PathCondition.t -> state -> state
(** Remove from state those parts that are inconsistent with the path condition. (We do a cheap
    check to not introduce inconsistent Topl states, but they may become inconsistent because the
    program path condition is updated later.) *)

val simplify :
     keep:AbstractValue.Set.t
  -> get_dynamic_type:(value -> Typ.t option)
  -> path_condition:PathCondition.t
  -> state
  -> state
(** Keep only a subset of abstract values. This is used for extracting summaries. *)

val report_errors : Procdesc.t -> Errlog.t -> state -> unit
(** Calls [Reporting.log_issue] with error traces, if any. *)

val pp_state : Format.formatter -> state -> unit

module Debug : sig
  val dropped_disjuncts_count : int ref
end
