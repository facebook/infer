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
  type t = private
    | Read of AccessPath.t  (** Field or array read *)
    | Write of AccessPath.t  (** Field or array write *)
    | ContainerRead of AccessPath.t * Typ.Procname.t  (** Read of container object *)
    | ContainerWrite of AccessPath.t * Typ.Procname.t  (** Write to container object *)
    | InterfaceCall of Typ.Procname.t
        (** Call to method of interface not annotated with @ThreadSafe *)
  [@@deriving compare]

  val matches : caller:t -> callee:t -> bool
  (** returns true if the caller access matches the callee access after accounting for mismatch
      between the formals and actuals *)

  val get_access_path : t -> AccessPath.t option

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
end

module TraceElem : sig
  include TraceElem.S with module Kind = Access

  val is_write : t -> bool

  val is_container_write : t -> bool

  val map : f:(AccessPath.t -> AccessPath.t) -> t -> t

  val is_direct : t -> bool
  (** return true if the given trace elem represents a direct access, not a call that eventually
      leads to an access *)

  val make_container_access : AccessPath.t -> Typ.Procname.t -> is_write:bool -> Location.t -> t

  val make_field_access : AccessPath.t -> is_write:bool -> Location.t -> t

  val make_unannotated_call_access : Typ.Procname.t -> Location.t -> t
end

(** Overapproximation of number of locks that are currently held *)
module LocksDomain : sig
  include AbstractDomain.WithBottom

  val is_locked : astate -> bool
    [@@warning "-32"]
  (** returns true if the number of locks held is greater than zero or Top *)

  val add_lock : astate -> astate
  (** record acquisition of a lock *)

  val remove_lock : astate -> astate
  (** record release of a lock *)

  val integrate_summary : caller_astate:astate -> callee_astate:astate -> astate
  (** integrate current state with a callee summary *)
end

(** Abstraction of threads that may run in parallel with the current thread.
    NoThread < AnyThreadExceptSelf < AnyThread *)
module ThreadsDomain : sig
  type astate =
    | NoThread
        (** No threads can run in parallel with the current thread (concretization: empty set). We
        assume this by default unless we see evidence to the contrary (annotations, use of locks,
        etc.) *)
    | AnyThreadButSelf
        (** Current thread can run in parallel with other threads, but not with itself.
        (concretization : { t | t \in TIDs ^ t != t_cur } ) *)
    | AnyThread
        (** Current thread can run in parallel with any thread, including itself (concretization: set
        of all TIDs ) *)

  include AbstractDomain.WithBottom with type astate := astate

  val can_conflict : astate -> astate -> bool
  (** return true if two accesses with these thread values can run concurrently *)

  val is_any : astate -> bool

  val integrate_summary : caller_astate:astate -> callee_astate:astate -> astate
  (** integrate current state with a callee summary *)
end

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

  val find : [`Use_get_owned_instead]  [@@warning "-32"]
end

(** attribute attached to a boolean variable specifying what it means when the boolean is true *)
module Choice : sig
  type t =
    | OnMainThread  (** the current procedure is running on the main thread *)
    | LockHeld  (** a lock is currently held *)
  [@@deriving compare]
end

module Attribute : sig
  type t =
    | Functional  (** holds a value returned from a callee marked @Functional *)
    | Choice of Choice.t  (** holds a boolean choice variable *)
  [@@deriving compare]

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t
end

module AttributeSetDomain : module type of AbstractDomain.InvertedSet (Attribute)

module AttributeMapDomain : sig
  include module type of AbstractDomain.InvertedMap (AccessPath) (AttributeSetDomain)

  val add : AccessPath.t -> AttributeSetDomain.astate -> astate -> astate

  val has_attribute : AccessPath.t -> Attribute.t -> astate -> bool

  val get_choices : AccessPath.t -> astate -> Choice.t list
  (** get the choice attributes associated with the given access path *)

  val add_attribute : AccessPath.t -> Attribute.t -> astate -> astate
end

(** Data shared among a set of accesses: current thread, lock(s) held, ownership precondition *)
module AccessSnapshot : sig
  (** Precondition for owned access; access is owned if it evaluates to ture *)
  module Precondition : sig
    type t =
      | Conjunction of IntSet.t
          (** Conjunction of "formal index must be owned" predicates.
                                         true if empty *)
      | False
    [@@deriving compare]

    val is_true : t -> bool
    (** return [true] if the precondition evaluates to true *)

    val pp : F.formatter -> t -> unit  [@@warning "-32"]
  end

  type t = private
    {thread: ThreadsDomain.astate; lock: bool; ownership_precondition: Precondition.t}
  [@@deriving compare]

  val make : LocksDomain.astate -> ThreadsDomain.astate -> Precondition.t -> Procdesc.t -> t

  val is_unprotected : t -> bool
  (** return true if not protected by lock, thread, or ownership *)

  val pp : F.formatter -> t -> unit
end

(** map of access metadata |-> set of accesses. the map should hold all accesses to a
    possibly-unowned access path *)
module AccessDomain : sig
  include module type of AbstractDomain.Map (AccessSnapshot) (PathDomain)

  val add_access : AccessSnapshot.t -> TraceElem.t -> astate -> astate
  (** add the given (access, precondition) pair to the map *)

  val get_accesses : AccessSnapshot.t -> astate -> PathDomain.astate
  (** get all accesses with the given snapshot *)
end

module StabilityDomain : sig
  include module type of AccessTree.PathSet (AccessTree.DefaultConfig)

  val rebase_paths : HilExp.t list -> Procdesc.t -> t -> t

  val add_wobbly_paths_assign : AccessPath.t -> HilExp.t -> t -> t

  val add_path : AccessPath.t -> t -> t

  val add_wobbly_actuals : HilExp.t list -> t -> t
end

type astate =
  { threads: ThreadsDomain.astate  (** current thread: main, background, or unknown *)
  ; locks: LocksDomain.astate  (** boolean that is true if a lock must currently be held *)
  ; accesses: AccessDomain.astate
        (** read and writes accesses performed without ownership permissions *)
  ; ownership: OwnershipDomain.astate  (** map of access paths to ownership predicates *)
  ; attribute_map: AttributeMapDomain.astate
        (** map of access paths to attributes such as owned, functional, ... *)
  ; wobbly_paths: StabilityDomain.astate
        (** Record the "touched" paths that can compromise the race detection **) }

(** same as astate, but without [attribute_map] (since these involve locals) and with the addition
    of the ownership/attributes associated with the return value as well as the set of formals which
    may escape *)
type summary =
  { threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; return_ownership: OwnershipAbstractValue.astate
  ; return_attributes: AttributeSetDomain.astate
  ; wobbly_paths: StabilityDomain.astate }

include AbstractDomain.WithBottom with type astate := astate

val pp_summary : F.formatter -> summary -> unit

val attributes_of_expr :
  AttributeSetDomain.t AttributeMapDomain.t -> HilExp.t -> AttributeSetDomain.t
(** propagate attributes from the leaves to the root of an RHS Hil expression *)

val ownership_of_expr :
  HilExp.t -> OwnershipAbstractValue.astate AttributeMapDomain.t -> OwnershipAbstractValue.astate

val get_all_accesses : (TraceElem.t -> bool) -> PathDomain.t AccessDomain.t -> PathDomain.t
