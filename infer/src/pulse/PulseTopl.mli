(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type value = PulseAbstractValue.t

type event =
  | ArrayWrite of {aw_array: value; aw_index: value}
  | Call of {return: value option; arguments: value list; procname: Procname.t}

type state [@@deriving compare, equal]

val start : unit -> state
(** Return the initial state of [Topl.automaton ()]. *)

val small_step : Location.t -> PulsePathCondition.t -> event -> state -> state

val large_step :
     call_location:Location.t
  -> callee_proc_name:Procname.t
  -> substitution:(value * PulseValueHistory.t) PulseAbstractValue.Map.t
  -> condition:PulsePathCondition.t
  -> callee_prepost:state
  -> state
  -> state
(** [large_step ~substitution ~condition state ~callee_prepost] updates [state] according to
    [callee_prepost]. The abstract values in [condition] and [state] are in one scope, and those in
    [callee_prepost] in another scope: the [substitution] maps from the callee scope to the
    condition&state scope. *)

val filter_for_summary : PulsePathCondition.t -> state -> state
(** Remove from state those parts that are inconsistent with the path condition. (We do a cheap
    check to not introduce inconsistent Topl states, but they mey become inconsistent because the
    program path condition is updated later.) *)

val simplify : keep:PulseAbstractValue.Set.t -> state -> state
(** Keep only a subset of abstract values. This is used for extracting summaries. *)

val report_errors : Procdesc.t -> Errlog.t -> state -> unit
(** Calls [Reporting.log_issue] with error traces, if any. *)

val pp_state : Format.formatter -> state -> unit
