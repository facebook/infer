(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format

val apply_to_first_actual : HilExp.t list -> 'a -> f:(HilExp.access_expression -> 'a) -> 'a

val pp_exp : F.formatter -> AccessExpression.t -> unit
(** language sensitive pretty-printer *)

module Access : sig
  type t =
    | Read of {exp: AccessExpression.t}  (** Field or array read *)
    | Write of {exp: AccessExpression.t}  (** Field or array write *)
    | ContainerRead of {exp: AccessExpression.t; pname: Procname.t}  (** Read of container object *)
    | ContainerWrite of {exp: AccessExpression.t; pname: Procname.t}
        (** Write to container object *)
    | InterfaceCall of {exp: AccessExpression.t; pname: Procname.t}
        (** Call to method of interface not annotated with [@ThreadSafe] *)

  val pp : F.formatter -> t -> unit

  val get_access_exp : t -> AccessExpression.t
end

(** Overapproximation of number of times the lock has been acquired *)
module LockDomain : sig
  include AbstractDomain.WithBottom

  val acquire_lock : t -> t
  (** record acquisition of a lock *)

  val release_lock : t -> t
  (** record release of a lock *)
end

(** Abstraction of threads that may run in parallel with the current thread. NoThread <
    AnyThreadExceptSelf < AnyThread *)
module ThreadsDomain : sig
  type t =
    | NoThread
        (** No threads can run in parallel with the current thread (concretization: empty set). We
            assume this by default unless we see evidence to the contrary (annotations, use of
            locks, etc.) *)
    | AnyThreadButSelf
        (** Current thread can run in parallel with other threads, but not with a copy of itself.
            (concretization : [{ t | t \in TIDs && t != t_cur }]) *)
    | AnyThread
        (** Current thread can run in parallel with any thread, including itself (concretization:
            set of all TIDs ) *)

  val can_conflict : t -> t -> bool
  (** return true if two accesses with these thread values can run concurrently *)

  val is_any : t -> bool
end

module OwnershipAbstractValue : sig
  type t = private
    | OwnedIf of IntSet.t
        (** Owned if the formals at the given indexes are owned in the caller; unconditionally owned
            if the set of formals is empty = bottom of the lattice *)
    | Unowned  (** Unowned value; top of the lattice *)

  val owned : t

  val make_owned_if : int -> t
end

(** snapshot of the relevant state at the time of a heap access: concurrent thread(s), lock(s) held,
    ownership precondition *)
module AccessSnapshot : sig
  module AccessSnapshotElem : sig
    type t =
      { access: Access.t
      ; thread: ThreadsDomain.t
      ; lock: bool
      ; ownership_precondition: OwnershipAbstractValue.t }
  end

  include ExplicitTrace.TraceElem with type elem_t = AccessSnapshotElem.t

  val is_write : t -> bool
  (** is it a write OR a container write *)

  val is_container_write : t -> bool

  val get_loc : t -> Location.t

  val is_unprotected : t -> bool
  (** return true if not protected by lock, thread, or ownership *)
end

module AccessDomain : AbstractDomain.FiniteSetS with type elt = AccessSnapshot.t

module OwnershipDomain : sig
  type t

  val empty : t

  val add : AccessExpression.t -> OwnershipAbstractValue.t -> t -> t

  val propagate_assignment : AccessExpression.t -> HilExp.t -> t -> t
end

module Attribute : sig
  type t =
    | Nothing
    | Functional  (** holds a value returned from a callee marked [@Functional] *)
    | OnMainThread  (** boolean is true if the current procedure is running on the main thread *)
    | LockHeld  (** boolean is true if a lock is currently held *)
    | Synchronized  (** the object is a synchronized data structure *)
end

module AttributeMapDomain : sig
  include
    AbstractDomain.InvertedMapS with type key = AccessExpression.t and type value = Attribute.t

  val get : AccessExpression.t -> t -> Attribute.t
  (** find the [Attribute.t] associated with a given access expression or return [Attribute.bottom] *)

  val is_functional : t -> AccessExpression.t -> bool

  val propagate_assignment : AccessExpression.t -> HilExp.t -> t -> t
  (** propagate attributes from the leaves to the root of an RHS Hil expression *)
end

module NeverReturns : AbstractDomain.S

type t =
  { threads: ThreadsDomain.t  (** current thread: main, background, or unknown *)
  ; locks: LockDomain.t  (** boolean that is true if a lock must currently be held *)
  ; never_returns: NeverReturns.t
        (** boolean which is true if a [noreturn] call is always reached *)
  ; accesses: AccessDomain.t
        (** read and writes accesses performed without ownership permissions *)
  ; ownership: OwnershipDomain.t  (** map of access paths to ownership predicates *)
  ; attribute_map: AttributeMapDomain.t
        (** map of access paths to attributes such as owned, functional, ... *) }

include AbstractDomain.S with type t := t

val initial : t

val add_unannotated_call_access : FormalMap.t -> Procname.t -> HilExp.t list -> Location.t -> t -> t

(** same as astate, but without [attribute_map] (since these involve locals) and with the addition
    of the ownership/attributes associated with the return value as well as the set of formals which
    may escape *)
type summary =
  { threads: ThreadsDomain.t
  ; locks: LockDomain.t
  ; never_returns: NeverReturns.t
  ; accesses: AccessDomain.t
  ; return_ownership: OwnershipAbstractValue.t
  ; return_attribute: Attribute.t
  ; attributes: AttributeMapDomain.t }

val empty_summary : summary

val pp_summary : F.formatter -> summary -> unit

val astate_to_summary : Procdesc.t -> FormalMap.t -> t -> summary

val add_access : Tenv.t -> FormalMap.t -> Location.t -> is_write:bool -> t -> HilExp.t -> t

val add_container_access :
     Tenv.t
  -> FormalMap.t
  -> is_write:bool
  -> AccessPath.base
  -> Procname.t
  -> HilExp.t list
  -> Location.t
  -> t
  -> t

val add_reads_of_hilexps : Tenv.t -> FormalMap.t -> HilExp.t list -> Location.t -> t -> t

val integrate_summary :
     FormalMap.t
  -> callee_proc_attrs:ProcAttributes.t
  -> summary
  -> HilExp.access_expression
  -> Procname.t
  -> HilExp.t list
  -> Location.t
  -> t
  -> t

val acquire_lock : t -> t

val release_lock : t -> t

val lock_if_true : HilExp.access_expression -> t -> t

val branch_never_returns : unit -> t
