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

module RawAccessPath : sig
  type t = AccessPath.raw
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val pp_element : Format.formatter -> t -> unit
end

module RawAccessPathSet : PrettyPrintable.PPSet with type elt = RawAccessPath.t

module OwnershipDomain : module type of AbstractDomain.InvertedSet (RawAccessPathSet)

module TraceElem : TraceElem.S with module Kind = RawAccessPath

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain : AbstractDomain.S with type astate = bool

module PathDomain : module type of SinkTrace.Make(TraceElem)

module IntMap : PrettyPrintable.PPMap with type key = int

module ConditionalWritesDomain : module type of (AbstractDomain.Map (IntMap) (PathDomain))

(** the primary role of this domain is tracking *conditional* and *unconditional* writes.
    conditional writes are writes that are rooted in a formal of the current procedure, and they
    are safe only if the actual bound to that formal is owned at the call site (as in the foo
    example below). Unconditional writes are rooted in a local, and they are only safe if a lock is
    held in the caller.
    To demonstrate what conditional writes buy us, consider the following example:

    foo() {
      Object local = new Object();
      iWriteToAField(local);
    }

    We don't want to warn on this example because the object pointed to by local is owned by the
    caller foo, then ownership is transferred to the callee iWriteToAField. *)
type astate =
  {
    locks : LocksDomain.astate;
    (** boolean that is true if a lock must currently be held *)
    reads : PathDomain.astate;
    (** access paths read outside of synchronization *)
    conditional_writes : ConditionalWritesDomain.astate;
    (** map of (formal index) -> set of access paths rooted in the formal index that are read
        outside of synrchonization if the formal is not owned by the caller *)
    unconditional_writes : PathDomain.astate;
    (** access paths written outside of synchronization *)
    id_map : IdAccessPathMapDomain.astate;
    (** map used to decompile temporary variables into access paths *)
    owned : OwnershipDomain.astate;
    (** access paths that must be owned by the current function *)
  }

(** same as astate, but without [id_map]/[owned] (since they are local) *)
type summary =
  LocksDomain.astate * PathDomain.astate * ConditionalWritesDomain.astate * PathDomain.astate

include AbstractDomain.WithBottom with type astate := astate

val make_access : RawAccessPath.t -> Location.t -> TraceElem.t

val pp_summary : F.formatter -> summary -> unit
