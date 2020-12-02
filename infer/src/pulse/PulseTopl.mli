(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type value = PulseAbstractValue.t

type event = Call of {return: value option; arguments: value list; procname: Procname.t}

type state

val start : unit -> state
(** Return the initial state of [Topl.automaton ()]. *)

val small_step : PulsePathCondition.t -> event -> state -> state

val large_step :
     substitution:(PulseAbstractValue.t * PulseValueHistory.t) PulseAbstractValue.Map.t
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

val pp_state : Format.formatter -> state -> unit
