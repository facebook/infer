(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module TaintItem = PulseTaintItem
module Timestamp = PulseTimestamp

(** Used to identify which cells (places in the memory) in the current precondition are mentioned in
    a value history in the current abstract state, i.e. were used to compute that value. *)
module CellId : sig
  type t = private int [@@deriving compare, equal, yojson_of]

  val pp : F.formatter -> t -> unit

  val next : unit -> t

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t
end

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
  | TaintSource of TaintItem.t * Location.t * Timestamp.t
  | TaintPropagated of Location.t * Timestamp.t
  | VariableAccessed of Pvar.t * Location.t * Timestamp.t
  | VariableDeclared of Pvar.t * Location.t * Timestamp.t

and t = private
  | Epoch  (** start of time *)
  | Sequence of event * t
      (** [Sequence [event, hist]] represents an event [event] occurring *after* [hist]. Invariant:
          the timestamp of [event] is greater than all the (local, i.e. not inside function calls)
          timestamps in [hist]. *)
  | InContext of
      { main: t  (** trace of the "main" value being traced *)
      ; context: t list  (** contextual traces, eg conditionals that the path is under *) }
  | BinaryOp of Binop.t * t * t  (** branch history due to a binop *)
  | FromCellIds of CellId.Set.t * t
      (** the set of cells that this were used in this history; used in particular in summary
          application to know which caller value histories should be pre-pended to a callee value
          history *)
  | Multiplex of t list  (** interlace multiple histories together *)
  | UnknownCall of {f: CallEvent.t; actuals: t list; location: Location.t; timestamp: Timestamp.t}
[@@deriving compare, equal, yojson_of]

val epoch : t

val sequence : ?context:t list -> event -> t -> t

val in_context : t list -> t -> t

val binary_op : Binop.t -> t -> t -> t

val from_cell_id : CellId.t -> t -> t

val unknown_call : CallEvent.t -> t list -> Location.t -> Timestamp.t -> t

val pp : F.formatter -> t -> unit

val pp_fields : F.formatter -> Fieldname.t RevList.t -> unit

val singleton : event -> t

val get_cell_ids : t -> CellId.Set.t option

val get_cell_id_exn : t -> CellId.t option
(** same as [get_cell_ids] but assumes the resulting set is a singleton *)

val of_cell_ids_in_map : t CellId.Map.t -> CellId.Set.t -> t option
(** multiplex of the histories corresponding to the given cell ids according to the cell id map
    provided; [None] if there are no such histories to multiplex together *)

type iter_event =
  | EnterCall of CallEvent.t * Location.t
  | ReturnFromCall of CallEvent.t * Location.t
  | Event of event

val rev_iter_main : t -> f:(iter_event -> unit) -> unit
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

val iter : main_only:bool -> t -> f:(iter_event -> unit) -> unit
[@@warning "-unused-value-declaration"]
(** like [rev_iter_main] but iterates in order (by reversing the order iteration) and iterates on
    only main events like [rev_iter_main] if [main_only] is [true], otherwise iterates on *all*
    events including contexts if [main_only] is [false] *)
(* used in unit tests *)

val location_of_event : event -> Location.t

val timestamp_of_event : event -> Timestamp.t [@@warning "-unused-value-declaration"]
(* used in unit tests *)

val add_to_errlog :
     ?include_taint_events:bool (** to avoid showing unrelated taint traces for non-taint issues *)
  -> nesting:int
  -> t
  -> Errlog.loc_trace_elem list
  -> Errlog.loc_trace_elem list

val get_first_main_event : t -> event option

val exists_main : t -> f:(event -> bool) -> bool
