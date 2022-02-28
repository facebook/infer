(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Timestamp = PulseTimestamp

type event =
  | Allocation of {f: CallEvent.t; location: Location.t; timestamp: Timestamp.t}
  | Assignment of Location.t * Timestamp.t
  | Call of {f: CallEvent.t; location: Location.t; in_call: t; timestamp: Timestamp.t}
  | Capture of
      { captured_as: Pvar.t
      ; mode: CapturedVar.capture_mode
      ; location: Location.t
      ; timestamp: Timestamp.t }
  | ConditionPassed of
      {if_kind: Sil.if_kind; is_then_branch: bool; location: Location.t; timestamp: Timestamp.t}
  | CppTemporaryCreated of Location.t * Timestamp.t
  | FormalDeclared of Pvar.t * Location.t * Timestamp.t
  | Invalidated of PulseInvalidation.t * Location.t * Timestamp.t
  | NilMessaging of Location.t * Timestamp.t
  | Returned of Location.t * Timestamp.t
  | StructFieldAddressCreated of Fieldname.t RevList.t * Location.t * Timestamp.t
  | VariableAccessed of Pvar.t * Location.t * Timestamp.t
  | VariableDeclared of Pvar.t * Location.t * Timestamp.t

and t = private
  | Epoch  (** start of time *)
  | Sequence of event * t
      (** [Sequence \[event, hist\]] represents an event [event] occurring *after* [hist].
          Invariant: the timestamp of [event] is greater than all the (local, i.e. not inside
          function calls) timestamps in [hist]. *)
  | InContext of
      { main: t  (** trace of the "main" value being traced *)
      ; context: t list  (** contextual traces, eg conditionals that the path is under *) }
  | BinaryOp of Binop.t * t * t  (** branch history due to a binop *)
[@@deriving compare, equal, yojson_of]

val epoch : t

val sequence : ?context:t list -> event -> t -> t

val in_context : t list -> t -> t

val binary_op : Binop.t -> t -> t -> t

val pp : F.formatter -> t -> unit

val pp_fields : F.formatter -> Fieldname.t RevList.t -> unit

val singleton : event -> t

type iter_event =
  | EnterCall of CallEvent.t * Location.t
  | ReturnFromCall of CallEvent.t * Location.t
  | Event of event

val iter_main : t -> f:(iter_event -> unit) -> unit
(** iterate on all events in reverse timestamp order, ignoring events in contexts and recursing into
    the histories inside call events. Timestamp order is the lexicographic order induced by
    projecting events onto their timestamps and appending timestamps within calls, e.g. the
    timestamp of the inner assignement in

    {[
      Call {timestamp=10; in_call=[..., Call{timestamp=4;
      in_call=[..., Assignement (...,timestamp=3) ] } ] }
    ]}

    can be written [10.4.3] and the order is such that, e.g., [10.4.3 < 10.5], [10.5] being the
    timestamp of the event following the inner [Call] event in the example above. *)

val location_of_event : event -> Location.t

val add_to_errlog : nesting:int -> t -> Errlog.loc_trace_elem list -> Errlog.loc_trace_elem list

val get_first_main_event : t -> event option
