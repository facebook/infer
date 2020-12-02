(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type value = PulseAbstractValue.t

type event = Call of {return: value option; arguments: value list}

type state

val start : unit -> state
(** Return the initial state of [Topl.automaton ()]. *)

val small_step : PulsePathCondition.t -> event -> state -> state

val pp_state : Format.formatter -> state -> unit
