(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Abstraction of a path that represents a lock, special-casing equality and comparisons
    to work over type, base variable modulo this and access list *)
module Lock : sig
  include PrettyPrintable.PrintableOrderedType with type t = AccessPath.t

  val owner_class : t -> Typ.name option
  (** Class of the root variable of the path representing the lock *)

  val equal : t -> t -> bool
end

(** Represents the existence of a program path from the current method to the eventual acquisition
    of a lock or a blocking call.  Equality/comparison disregards the call trace but includes
    location. *)
module Event : sig
  type severity_t = Low | Medium | High [@@deriving compare]

  type event_t = LockAcquire of Lock.t | MayBlock of (string * severity_t) [@@deriving compare]

  include ExplicitTrace.TraceElem with type elem_t = event_t

  val make_trace : ?header:string -> Typ.Procname.t -> t -> Errlog.loc_trace
end

module EventDomain : ExplicitTrace.FiniteSet with type elt = Event.t

(** Represents the existence of a program path to the [first] lock being taken in the current
  method or, transitively, a callee *in the same class*, and, which continues (to potentially
  another class) until the [eventually] event, schematically -->first-->eventually.
  It is guaranteed that during the second part of the trace (first-->eventually) the lock [first]
  is not released.  *)
module Order : sig
  type order_t = private {first: Lock.t; eventually: Event.t}

  include ExplicitTrace.TraceElem with type elem_t = order_t

  val may_deadlock : t -> t -> bool
  (** check if two pairs are symmetric in terms of locks, where locks are compared modulo the
      variable name at the root of each path. *)

  val make_trace : ?header:string -> Typ.Procname.t -> t -> Errlog.loc_trace
end

module OrderDomain : ExplicitTrace.FiniteSet with type elt = Order.t

module LockState : AbstractDomain.WithBottom

module UIThreadExplanationDomain : sig
  include ExplicitTrace.TraceElem with type elem_t = string

  val make_trace : ?header:string -> Typ.Procname.t -> t -> Errlog.loc_trace
end

module UIThreadDomain :
  AbstractDomain.WithBottom
  with type astate = UIThreadExplanationDomain.t AbstractDomain.Types.bottom_lifted

type astate =
  { lock_state: LockState.astate
  ; events: EventDomain.astate
  ; order: OrderDomain.astate
  ; ui: UIThreadDomain.astate }

include AbstractDomain.WithBottom with type astate := astate

val acquire : astate -> Location.t -> Lock.t -> astate

val release : astate -> Lock.t -> astate

val blocking_call :
     caller:Typ.Procname.t
  -> callee:Typ.Procname.t
  -> Event.severity_t
  -> Location.t
  -> astate
  -> astate

val set_on_ui_thread : astate -> Location.t -> string -> astate
(** set the property "runs on UI thread" to true by attaching the given explanation string as to
    why this method is thought to do so *)

type summary = astate

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : astate -> Typ.Procname.t -> Location.t -> summary -> astate
