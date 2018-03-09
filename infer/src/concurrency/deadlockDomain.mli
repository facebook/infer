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
  type t = AccessPath.t [@@deriving compare]

  val pp : F.formatter -> t -> unit  [@@warning "-32"]
end

(** A lock event.  Equality/comparison disregards the call trace but includes location. *)
module LockEvent : sig
  type t = private
    {lock: LockIdentity.t; loc: Location.t; trace: CallSite.t list}
    [@@deriving compare]

  val owner_class : t -> Typ.name option
    [@@warning "-32"]
  (** Class of the root variable of the path representing the lock *)

  val pp : F.formatter -> t -> unit  [@@warning "-32"]
end

module LockStack : AbstractDomain.WithBottom with type astate = LockEvent.t list

(** Represents either
- the existence of a program path from the current method to the eventual acquisition of a lock
  ("after"), or,
- the "before" lock being taken *in the current method* and, before its release, the eventual
  acquisition of "after" *)
module LockOrder : sig
  type t = private {before: LockEvent.t option; after: LockEvent.t} [@@deriving compare]

  val pp : F.formatter -> t -> unit

  val get_pair : t -> (LockEvent.t * LockEvent.t) option
    [@@warning "-32"]
  (** return the pair (b, after) if before is Some b *)

  val may_deadlock : t -> t -> bool  [@@warning "-32"]

  val make_loc_trace : t -> Errlog.loc_trace  [@@warning "-32"]
end

module LockOrderDomain : sig
  include module type of PrettyPrintable.MakePPSet (LockOrder)

  include AbstractDomain.WithBottom with type astate = t
end

module LockState : sig
  include AbstractDomain.WithBottom with type astate = LockStack.astate * LockOrderDomain.astate

  val lock : HilExp.t list -> astate -> Location.t -> astate  [@@warning "-32"]

  val unlock : HilExp.t list -> astate -> astate  [@@warning "-32"]

  val integrate_summary :
    caller_state:astate -> callee_summary:LockOrderDomain.t -> Typ.Procname.t -> Location.t
    -> astate
    [@@warning "-32"]

  val to_summary : astate -> LockOrderDomain.t  [@@warning "-32"]
end

type summary = LockOrderDomain.astate

include AbstractDomain.WithBottom with type astate = LockState.astate

val pp_summary : F.formatter -> summary -> unit
