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
module LockIdentity : sig
  include PrettyPrintable.PrintableOrderedType with type t = AccessPath.t

  val owner_class : t -> Typ.name option
  (** Class of the root variable of the path representing the lock *)
end

(** A lock event.  Equality/comparison disregards the call trace but includes location. *)
module LockEvent : sig
  type event_t = private LockAcquire of LockIdentity.t | MayBlock of string

  val pp_event : F.formatter -> event_t -> unit

  type t = private {event: event_t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t
end

(** Represents either
- the existence of a program path from the current method to the eventual acquisition of a lock
  ("eventually"), or,
- the "first" lock being taken *in the current method* and, before its release, the eventual
  acquisition of "eventually" *)
module LockOrder : sig
  type t = private {first: LockEvent.t option; eventually: LockEvent.t}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val may_deadlock : t -> t -> bool
  (** check if two pairs are symmetric in terms of locks, where locks are compared modulo the
      variable name at the root of each path. *)

  val make_loc_trace : t -> Errlog.loc_trace
end

module LockOrderDomain : sig
  include PrettyPrintable.PPSet with type elt = LockOrder.t

  include AbstractDomain.WithBottom with type astate = t
end

include AbstractDomain.WithBottom

val acquire : astate -> Location.t -> LockIdentity.t -> astate

val release : astate -> LockIdentity.t -> astate

val blocking_call :
  caller:Typ.Procname.t -> callee:Typ.Procname.t -> Location.t -> astate -> astate

val set_on_main_thread : astate -> astate

type summary = LockOrderDomain.astate * bool

val pp_summary : F.formatter -> summary -> unit

val to_summary : astate -> summary

val integrate_summary : astate -> Typ.Procname.t -> Location.t -> summary -> astate
