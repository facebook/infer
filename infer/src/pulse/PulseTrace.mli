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

type t =
  | Immediate of {location: Location.t; history: ValueHistory.t}
  | ViaCall of
      { f: CallEvent.t
      ; location: Location.t  (** location of the call event *)
      ; history: ValueHistory.t  (** the call involves a value with this prior history *)
      ; in_call: t  (** last step of the trace is in a call to [f] made at [location] *) }
[@@deriving compare, equal]

val pp : pp_immediate:(F.formatter -> unit) -> F.formatter -> t -> unit

val get_outer_location : t -> Location.t
(** skip histories and go straight to the where the action is: either the action itself or the call
    that leads to the action *)

val get_start_location : t -> Location.t
(** initial step in the history if not empty, or else same as {!get_outer_location} *)

val add_to_errlog :
     ?include_value_history:bool
  -> nesting:int
  -> pp_immediate:(F.formatter -> unit)
  -> t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list

val synchronous_add_to_errlog :
     nesting:int
  -> pp_immediate:(F.formatter -> unit)
  -> t list
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list
(** [synchronous_add_to_errlog] adds a list of the traces to the errlog in the given order while
    grouping traces that take place at the same location to reduce deduplication and ensure events
    happening together are identifiable as such. E.g. if two traces start with a [ViaCall] and they
    happen at the same location, then that call will only appear once and the rest of those traces
    will then be added synchronously as well *)

val find_map : t -> f:(ValueHistory.event -> 'a option) -> 'a option
(** [find_map] applied to history events; does not look into contextual events *)

val has_invalidation : t -> bool
(** whether the main part of the trace contains an invalidation event *)
