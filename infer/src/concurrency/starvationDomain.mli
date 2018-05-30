(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

(** Event type.  Equality/comparison disregards the call trace but includes location. *)
module Event : sig
  type severity_t = Low | Medium | High [@@deriving compare]

  type event_t = LockAcquire of Lock.t | MayBlock of (string * severity_t) [@@deriving compare]

  val pp_event : F.formatter -> event_t -> unit

  type t = private {event: event_t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t
end

(** Represents either
- the existence of a program path from the current method to the eventual acquisition of a lock
  ("eventually"), or,
- the "first" lock being taken *in the current method* and, before its release, the eventual
  acquisition of "eventually" *)
module Order : sig
  type t = private {first: Event.t option; eventually: Event.t}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val may_deadlock : t -> t -> bool
  (** check if two pairs are symmetric in terms of locks, where locks are compared modulo the
      variable name at the root of each path. *)

  val make_loc_trace : t -> Errlog.loc_trace

  val get_loc : t -> Location.t
end

module OrderDomain : sig
  include PrettyPrintable.PPSet with type elt = Order.t

  include AbstractDomain.WithBottom with type astate = t
end

module LockState : AbstractDomain.WithBottom

module UIThreadDomain :
  AbstractDomain.WithBottom with type astate = string AbstractDomain.Types.bottom_lifted

type astate = {lock_state: LockState.astate; order: OrderDomain.astate; ui: UIThreadDomain.astate}

include AbstractDomain.WithBottom with type astate := astate

val acquire : astate -> Location.t -> Lock.t -> astate

val release : astate -> Lock.t -> astate

val blocking_call :
  caller:Typ.Procname.t -> callee:Typ.Procname.t -> Event.severity_t -> Location.t -> astate
  -> astate

val set_on_ui_thread : astate -> string -> astate
(** set the property "runs on UI thread" to true by attaching the given explanation string as to
    why this method is thought to do so *)

type summary = astate

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : astate -> Typ.Procname.t -> Location.t -> summary -> astate
