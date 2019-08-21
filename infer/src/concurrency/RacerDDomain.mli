(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format

val pp_exp : F.formatter -> AccessExpression.t -> unit
(** language sensitive pretty-printer *)

module Access : sig
  type t =
    | Read of {exp: AccessExpression.t}  (** Field or array read *)
    | Write of {exp: AccessExpression.t}  (** Field or array write *)
    | ContainerRead of {exp: AccessExpression.t; pname: Typ.Procname.t}
        (** Read of container object *)
    | ContainerWrite of {exp: AccessExpression.t; pname: Typ.Procname.t}
        (** Write to container object *)
    | InterfaceCall of Typ.Procname.t
        (** Call to method of interface not annotated with @ThreadSafe *)
  [@@deriving compare]

  include ExplicitTrace.Element with type t := t

  val get_access_exp : t -> AccessExpression.t option
end

module TraceElem : sig
  include ExplicitTrace.TraceElem with type elem_t = Access.t

  val is_write : t -> bool
  (** is it a write OR a container write *)

  val is_container_write : t -> bool

  val map : f:(AccessExpression.t -> AccessExpression.t) -> t -> t

  val make_container_access :
    AccessExpression.t -> Typ.Procname.t -> is_write:bool -> Location.t -> t

  val make_field_access : AccessExpression.t -> is_write:bool -> Location.t -> t
end

(** Overapproximation of number of locks that are currently held *)
module LocksDomain : sig
  type t

  val acquire_lock : t -> t
  (** record acquisition of a lock *)

  val release_lock : t -> t
  (** record release of a lock *)

  val integrate_summary : caller_astate:t -> callee_astate:t -> t
  (** integrate current state with a callee summary *)
end

(** Abstraction of threads that may run in parallel with the current thread.
    NoThread < AnyThreadExceptSelf < AnyThread *)
module ThreadsDomain : sig
  type t =
    | NoThread
        (** No threads can run in parallel with the current thread (concretization: empty set). We
        assume this by default unless we see evidence to the contrary (annotations, use of locks,
        etc.) *)
    | AnyThreadButSelf
        (** Current thread can run in parallel with other threads, but not with a copy of itself.
        (concretization : {% \{ t | t \in TIDs ^ t != t_cur \} %} ) *)
    | AnyThread
        (** Current thread can run in parallel with any thread, including itself (concretization:
            set of all TIDs ) *)

  val can_conflict : t -> t -> bool
  (** return true if two accesses with these thread values can run concurrently *)

  val is_any : t -> bool

  val integrate_summary : caller_astate:t -> callee_astate:t -> t
  (** integrate current state with a callee summary *)
end

(** snapshot of the relevant state at the time of a heap access: concurrent thread(s), lock(s) held,
    ownership precondition *)
module AccessSnapshot : sig
  (** precondition for owned access; access is owned if it evaluates to true *)
  module OwnershipPrecondition : sig
    type t =
      | Conjunction of IntSet.t
          (** Conjunction of "formal index must be owned" predicates. true if empty *)
      | False

    include PrettyPrintable.PrintableOrderedType with type t := t

    val is_true : t -> bool
    (** return [true] if the precondition evaluates to true *)
  end

  type t = private
    { access: TraceElem.t
    ; thread: ThreadsDomain.t
    ; lock: bool
    ; ownership_precondition: OwnershipPrecondition.t }

  include PrettyPrintable.PrintableOrderedType with type t := t

  val make :
       TraceElem.t
    -> LocksDomain.t
    -> ThreadsDomain.t
    -> OwnershipPrecondition.t
    -> Procdesc.t
    -> t option

  val make_from_snapshot : TraceElem.t -> t -> t option

  val is_unprotected : t -> bool
  (** return true if not protected by lock, thread, or ownership *)
end

(** map of access metadata |-> set of accesses. the map should hold all accesses to a
    possibly-unowned access path *)
module AccessDomain : sig
  include AbstractDomain.FiniteSetS with type elt = AccessSnapshot.t

  val add_opt : elt option -> t -> t
end

module OwnershipAbstractValue : sig
  type t = private
    | OwnedIf of IntSet.t
        (** Owned if the formals at the given indexes are owned in the caller; unconditionally owned 
        if the set of formals is empty = bottom of the lattice *)
    | Unowned  (** Unowned value; top of the lattice *)
  [@@deriving compare]

  val owned : t

  val unowned : t

  val make_owned_if : int -> t
end

module OwnershipDomain : sig
  type t

  val empty : t

  val add : AccessExpression.t -> OwnershipAbstractValue.t -> t -> t

  val get_owned : AccessExpression.t -> t -> OwnershipAbstractValue.t

  val propagate_assignment : AccessExpression.t -> HilExp.t -> t -> t

  val propagate_return : AccessExpression.t -> OwnershipAbstractValue.t -> HilExp.t list -> t -> t

  val get_precondition : AccessExpression.t -> t -> AccessSnapshot.OwnershipPrecondition.t
end

(** attribute attached to a boolean variable specifying what it means when the boolean is true *)
module Choice : sig
  type t =
    | OnMainThread  (** the current procedure is running on the main thread *)
    | LockHeld  (** a lock is currently held *)

  include PrettyPrintable.PrintableOrderedType with type t := t
end

module Attribute : sig
  type t =
    | Functional  (** holds a value returned from a callee marked @Functional *)
    | Choice of Choice.t  (** holds a boolean choice variable *)

  include PrettyPrintable.PrintableOrderedType with type t := t
end

module AttributeSetDomain : sig
  type t

  val empty : t
end

module AttributeMapDomain : sig
  type t

  val find : AccessExpression.t -> t -> AttributeSetDomain.t

  val add : AccessExpression.t -> AttributeSetDomain.t -> t -> t

  val has_attribute : AccessExpression.t -> Attribute.t -> t -> bool

  val get_choices : AccessExpression.t -> t -> Choice.t list
  (** get the choice attributes associated with the given access path *)

  val add_attribute : AccessExpression.t -> Attribute.t -> t -> t

  val propagate_assignment : AccessExpression.t -> HilExp.t -> t -> t
  (** propagate attributes from the leaves to the root of an RHS Hil expression *)
end

type t =
  { threads: ThreadsDomain.t  (** current thread: main, background, or unknown *)
  ; locks: LocksDomain.t  (** boolean that is true if a lock must currently be held *)
  ; accesses: AccessDomain.t
        (** read and writes accesses performed without ownership permissions *)
  ; ownership: OwnershipDomain.t  (** map of access paths to ownership predicates *)
  ; attribute_map: AttributeMapDomain.t
        (** map of access paths to attributes such as owned, functional, ... *) }

(** same as astate, but without [attribute_map] (since these involve locals) and with the addition
    of the ownership/attributes associated with the return value as well as the set of formals which
    may escape *)
type summary =
  { threads: ThreadsDomain.t
  ; locks: LocksDomain.t
  ; accesses: AccessDomain.t
  ; return_ownership: OwnershipAbstractValue.t
  ; return_attributes: AttributeSetDomain.t }

val empty_summary : summary

include AbstractDomain.WithBottom with type t := t

val pp_summary : F.formatter -> summary -> unit

val add_unannotated_call_access : Typ.Procname.t -> Location.t -> Procdesc.t -> t -> t
