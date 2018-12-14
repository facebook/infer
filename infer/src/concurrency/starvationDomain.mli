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
  include ExplicitTrace.Element with type t = AccessPath.t

  val owner_class : t -> Typ.name option
  (** Class of the root variable of the path representing the lock *)

  val equal : t -> t -> bool

  val pp_locks : F.formatter -> t -> unit
end

(** Represents the existence of a program path from the current method to the eventual acquisition
    of a lock or a blocking call.  Equality/comparison disregards the call trace but includes
    location. *)
module Event : sig
  type severity_t = Low | Medium | High [@@deriving compare]

  type event_t =
    | LockAcquire of Lock.t
    | MayBlock of (string * severity_t)
    | StrictModeCall of string
  [@@deriving compare]

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
  with type t = UIThreadExplanationDomain.t AbstractDomain.Types.bottom_lifted

module GuardToLockMap : AbstractDomain.WithBottom

type t =
  { events: EventDomain.t
  ; guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; order: OrderDomain.t
  ; ui: UIThreadDomain.t }

include AbstractDomain.WithBottom with type t := t

val acquire : Tenv.t -> t -> Location.t -> Lock.t list -> t
(** simultaneously acquire a number of locks, no-op if list is empty *)

val release : t -> Lock.t list -> t
(** simultaneously release a number of locks, no-op if list is empty *)

val blocking_call : Typ.Procname.t -> Event.severity_t -> Location.t -> t -> t

val strict_mode_call : Typ.Procname.t -> Location.t -> t -> t

val set_on_ui_thread : t -> Location.t -> string -> t
(** set the property "runs on UI thread" to true by attaching the given explanation string as to
    why this method is thought to do so *)

val add_guard : Tenv.t -> t -> HilExp.t -> Lock.t -> acquire_now:bool -> Location.t -> t
(** Install a mapping from the guard expression to the lock provided, and optionally lock it. *)

val lock_guard : Tenv.t -> t -> HilExp.t -> Location.t -> t
(** Acquire the lock the guard was constructed with. *)

val remove_guard : t -> HilExp.t -> t
(** Destroy the guard and release its lock. *)

val unlock_guard : t -> HilExp.t -> t
(** Release the lock the guard was constructed with. *)

type summary = t

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : Tenv.t -> t -> Typ.Procname.t -> Location.t -> summary -> t
