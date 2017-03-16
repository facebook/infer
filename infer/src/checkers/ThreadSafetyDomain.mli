(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

module AccessPathSetDomain : module type of AbstractDomain.InvertedSet (AccessPath.UntypedRawSet)

module Access : sig
  type kind =
    | Read
    | Write
  [@@deriving compare]

  type t = AccessPath.Raw.t * kind [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module TraceElem : sig
  include TraceElem.S with module Kind = Access

  val is_write : t -> bool

  val is_read : t -> bool
end

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain : AbstractDomain.S with type astate = bool

module ThreadsDomain : AbstractDomain.S with type astate = bool

module PathDomain : module type of SinkTrace.Make(TraceElem)

(** attribute attached to a boolean variable specifying what it means when the boolean is true *)
module Choice : sig
  type t =
    | OnMainThread (** the current procedure is running on the main thread *)
    | LockHeld (** a lock is currently held *)
  [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module Attribute : sig
  type t =
    | OwnedIf of int option
    (** owned unconditionally if OwnedIf None, owned when formal at index i is owned otherwise *)
    | Functional
    (** holds a value returned from a callee marked @Functional *)
    | Choice of Choice.t
    (** holds a boolean choice variable *)
  [@@deriving compare]

  (** alias for OwnedIf None *)
  val unconditionally_owned : t

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t
end

module AttributeSetDomain : module type of AbstractDomain.InvertedSet (Attribute.Set)

module AttributeMapDomain : sig
  include module type of AbstractDomain.InvertedMap (AccessPath.UntypedRawMap) (AttributeSetDomain)

  val has_attribute : AccessPath.Raw.t -> Attribute.t -> astate -> bool

  (** get the formal index of the the formal that must own the given access path (if any) *)
  val get_conditional_ownership_index : AccessPath.Raw.t -> astate -> int option

  (** get the choice attributes associated with the given access path *)
  val get_choices : AccessPath.Raw.t -> astate -> Choice.t list

  val add_attribute : AccessPath.Raw.t -> Attribute.t -> astate -> astate
end

module AccessPrecondition : sig
  type t =
    | Protected
    (** access safe due to held lock (i.e., pre is true *)
    | ProtectedIf of int option
    (** access safe if the formal at index i is owned (i.e., pre is owned(i)).
        ProtectedIf None means unsafe (i.e., pre is false) *)
  [@@deriving compare]

  (** type of an unprotected access *)
  val unprotected : t

  val pp : F.formatter -> t -> unit

  module Map : PrettyPrintable.PPMap with type key = t
end

(** map of access precondition |-> set of accesses. the map should hold all accesses to a
    possibly-unowned access path *)
module AccessDomain : sig
  include module type of AbstractDomain.Map (AccessPrecondition.Map) (PathDomain)

  (* add the given (access, precondition) pair to the map *)
  val add_access : AccessPrecondition.t -> TraceElem.t -> astate -> astate

  (* get all accesses with the given precondition *)
  val get_accesses : AccessPrecondition.t -> astate -> PathDomain.astate
end

type astate =
  {
    threads : ThreadsDomain.astate;
    (** boolean that is true if we know we are on UI/main thread *)
    locks : LocksDomain.astate;
    (** boolean that is true if a lock must currently be held *)
    accesses : AccessDomain.astate;
    (** read and writes accesses performed without ownership permissions *)
    id_map : IdAccessPathMapDomain.astate;
    (** map used to decompile temporary variables into access paths *)
    attribute_map : AttributeMapDomain.astate;
    (** map of access paths to attributes such as owned, functional, ... *)
  }

(** same as astate, but without [id_map]/[owned] (since they are local) and with the addition of the
    attributes associated with the return value *)
type summary = ThreadsDomain.astate * LocksDomain.astate
               * AccessDomain.astate * AttributeSetDomain.astate

include AbstractDomain.WithBottom with type astate := astate

val make_access : AccessPath.Raw.t -> Access.kind -> Location.t -> TraceElem.t

val pp_summary : F.formatter -> summary -> unit
