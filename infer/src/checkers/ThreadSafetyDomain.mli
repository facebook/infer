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

module Access : sig
  type t =
    | Read of AccessPath.t  (** Field or array read *)
    | Write of AccessPath.t  (** Field or array write *)
    | ContainerRead of AccessPath.t * Typ.Procname.t  (** Read of container object *)
    | ContainerWrite of AccessPath.t * Typ.Procname.t  (** Write to container object *)
    | InterfaceCall of Typ.Procname.t
        (** Call to method of interface not annotated with @ThreadSafe *)
    [@@deriving compare]

  val get_access_path : t -> AccessPath.t option

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
end

module TraceElem : sig
  include TraceElem.S with module Kind = Access

  val is_write : t -> bool

  val is_container_write : t -> bool
end

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain : AbstractDomain.S with type astate = bool

module ThreadsDomain : AbstractDomain.S with type astate = bool

module ThumbsUpDomain : AbstractDomain.S with type astate = bool

module PathDomain : module type of SinkTrace.Make (TraceElem)

(** Powerset domain on the formal indexes in OwnedIf with a distinguished bottom element (Owned) and top element (Unowned) *)
module OwnershipAbstractValue : sig
  type astate = private
    | Owned  (** Owned value; bottom of the lattice *)
    | OwnedIf of IntSet.t  (** Owned if the formals at the given indexes are owned in the caller *)
    | Unowned  (** Unowned value; top of the lattice *)
    [@@deriving compare]

  val owned : astate

  val unowned : astate

  val make_owned_if : int -> astate

  include AbstractDomain.S with type astate := astate
end

module OwnershipDomain : sig
  include module type of AbstractDomain.Map (AccessPath) (OwnershipAbstractValue)

  val get_owned : AccessPath.t -> astate -> OwnershipAbstractValue.astate

  val is_owned : AccessPath.t -> astate -> bool

  val find : [`Use_get_owned_instead]
end

(** attribute attached to a boolean variable specifying what it means when the boolean is true *)
module Choice : sig
  type t =
    | OnMainThread  (** the current procedure is running on the main thread *)
    | LockHeld  (** a lock is currently held *)
    [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module Attribute : sig
  type t =
    | Functional  (** holds a value returned from a callee marked @Functional *)
    | Choice of Choice.t  (** holds a boolean choice variable *)
    [@@deriving compare]

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t
end

module AttributeSetDomain : module type of AbstractDomain.InvertedSet (Attribute.Set)

module AttributeMapDomain : sig
  include module type of AbstractDomain.InvertedMap (AccessPath.Map) (AttributeSetDomain)

  val add : AccessPath.t -> AttributeSetDomain.astate -> astate -> astate

  val has_attribute : AccessPath.t -> Attribute.t -> astate -> bool

  val get_choices : AccessPath.t -> astate -> Choice.t list
  (** get the choice attributes associated with the given access path *)

  val add_attribute : AccessPath.t -> Attribute.t -> astate -> astate
end

(** Excluders: Two things can provide for mutual exclusion: holding a lock,
    and knowing that you are on a particular thread. Here, we
    abstract it to "some" lock and "any particular" thread (typically, UI thread)
    For a more precide semantics we would replace Thread by representatives of
    thread id's and Lock by multiple differnet lock id's.
    Both indicates that you both know the thread and hold a lock.
    There is no need for a lattice relation between Thread, Lock and Both:
    you don't ever join Thread and Lock, rather, they are treated pointwise.
 **)
module Excluder : sig
  type t = Thread | Lock | Both [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module AccessPrecondition : sig
  type t =
    | Protected of Excluder.t
        (** access potentially protected for mutual exclusion by
        lock or thread or both *)
    | Unprotected of IntSet.t
        (** access rooted in formal(s) at indexes i. Safe if actuals passed at given indexes are
            owned (i.e., !owned(i) implies an unsafe access). *)
    | TotallyUnprotected  (** access is unsafe unless a lock is held in a caller *)
    [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

(** map of access precondition |-> set of accesses. the map should hold all accesses to a
    possibly-unowned access path *)
module AccessDomain : sig
  include module type of AbstractDomain.Map (AccessPrecondition) (PathDomain)

  val add_access : AccessPrecondition.t -> TraceElem.t -> astate -> astate
  (** add the given (access, precondition) pair to the map *)

  val get_accesses : AccessPrecondition.t -> astate -> PathDomain.astate
  (** get all accesses with the given precondition *)
end

(** a formal or a local variable that may escape *)
module Escapee : sig
  type t = Formal of int | Local of Var.t [@@deriving compare]

  val pp : F.formatter -> t -> unit

  val of_access_path : OwnershipDomain.astate -> AccessPath.t -> t list
end

(** set of formals or locals that may escape *)
module EscapeeDomain : sig
  include module type of AbstractDomain.FiniteSet (Escapee)

  val add_from_list : astate -> Escapee.t list -> astate
end

(** Domain that only includes escaping formals, for use in summary *)
module FormalsDomain : sig
  include module type of AbstractDomain.FiniteSet (Int)

  val of_escapees : EscapeeDomain.astate -> astate
end

type astate =
  { thumbs_up: ThreadsDomain.astate  (** boolean that is true if we think we have a proof *)
  ; threads: ThreadsDomain.astate  (** boolean that is true if we know we are on UI/main thread *)
  ; locks: LocksDomain.astate  (** boolean that is true if a lock must currently be held *)
  ; accesses: AccessDomain.astate
        (** read and writes accesses performed without ownership permissions *)
  ; ownership: OwnershipDomain.astate  (** map of access paths to ownership predicates *)
  ; attribute_map: AttributeMapDomain.astate
        (** map of access paths to attributes such as owned, functional, ... *)
  ; escapees: EscapeeDomain.astate  (** set of formals and locals that may escape *) }

(** same as astate, but without [attribute_map] (since these involve locals)
    and with the addition of the attributes associated with the return value
    as well as the set of formals which may escape *)
type summary =
  ThumbsUpDomain.astate
  * ThreadsDomain.astate
  * LocksDomain.astate
  * AccessDomain.astate
  * OwnershipAbstractValue.astate
  * AttributeSetDomain.astate
  * FormalsDomain.astate

include AbstractDomain.WithBottom with type astate := astate

val make_container_access :
  AccessPath.t -> Typ.Procname.t -> is_write:bool -> Location.t -> TraceElem.t

val make_field_access : AccessPath.t -> is_write:bool -> Location.t -> TraceElem.t

val make_unannotated_call_access : Typ.Procname.t -> Location.t -> TraceElem.t

val pp_summary : F.formatter -> summary -> unit
