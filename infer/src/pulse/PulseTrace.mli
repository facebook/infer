(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module ValueHistory = PulseValueHistory

type 'a t =
  | Immediate of {imm: 'a; location: Location.t; history: ValueHistory.t}
  | ViaCall of
      { f: CallEvent.t
      ; location: Location.t  (** location of the call event *)
      ; history: ValueHistory.t  (** the call involves a value with this prior history *)
      ; in_call: 'a t  (** last step of the trace is in a call to [f] made at [location] *) }
[@@deriving compare]

val pp : (F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit

val get_outer_location : 'a t -> Location.t
(** skip histories and go straight to the where the action is: either the action itself or the
      call that leads to the action *)

val get_start_location : 'a t -> Location.t
(** initial step in the history if not empty, or else same as {!get_outer_location} *)

val get_immediate : 'a t -> 'a

val add_to_errlog :
     nesting:int
  -> (F.formatter -> 'a -> unit)
  -> 'a t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list
