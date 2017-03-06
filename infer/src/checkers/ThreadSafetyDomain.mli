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

module TraceElem : TraceElem.S with module Kind = AccessPath.Raw

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain : AbstractDomain.S with type astate = bool

module PathDomain : module type of SinkTrace.Make(TraceElem)

module Attribute : sig
  type t =
    | OwnedIf of int option
    (** owned unconditionally if OwnedIf None, owned when formal at index i is owned otherwise *)
    | Functional
    (** holds a value returned from a callee marked @Functional *)
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
    locks : LocksDomain.astate;
    (** boolean that is true if a lock must currently be held *)
    reads : PathDomain.astate;
    (** access paths read outside of synchronization *)
    writes : AccessDomain.astate;
    (** access paths written without ownership permissions *)
    id_map : IdAccessPathMapDomain.astate;
    (** map used to decompile temporary variables into access paths *)
    attribute_map : AttributeMapDomain.astate;
    (** map of access paths to attributes such as owned, functional, ... *)
  }

(** same as astate, but without [id_map]/[owned] (since they are local) and with the addition of the
    attributes associated with the return value *)
type summary =
  LocksDomain.astate * PathDomain.astate * AccessDomain.astate * AttributeSetDomain.astate

include AbstractDomain.WithBottom with type astate := astate

val make_access : AccessPath.Raw.t -> Location.t -> TraceElem.t

val pp_summary : F.formatter -> summary -> unit
