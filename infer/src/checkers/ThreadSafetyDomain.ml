(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

module RawAccessPath = struct
  type t = AccessPath.raw
  let compare = AccessPath.compare_raw
  let pp = AccessPath.pp_raw
  let pp_element = pp
end

module OwnershipDomain = AbstractDomain.InvertedSet(PrettyPrintable.MakePPSet(RawAccessPath))

module TraceElem = struct
  module Kind = RawAccessPath

  type t = {
    site : CallSite.t;
    kind : Kind.t;
  } [@@deriving compare]

  let call_site { site; } = site

  let kind { kind; } = kind

  let make kind site = { kind; site; }

  let with_callsite t site = { t with site; }

  let pp fmt { site; kind; } =
    F.fprintf fmt "Unprotected access to %a at %a" Kind.pp kind CallSite.pp site

  module Set = PrettyPrintable.MakePPSet (struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)
end

let make_access kind loc =
  let site = CallSite.make Procname.empty_block loc in
  TraceElem.make kind site

(** A bool that is true if a lock is definitely held. Note that this is unsound because it assumes
    the existence of one global lock. In the case that a lock is held on the access to a variable,
    but the lock held is the wrong one, we will erroneously say that the access is thread-safe.
    However, this coarse abstraction saves us from the complexity of tracking which locks are held
    and which memory locations correspond to the same lock. *)
module LocksDomain = AbstractDomain.BooleanAnd

module PathDomain = SinkTrace.Make(TraceElem)

module IntMap = PrettyPrintable.MakePPMap(struct
    type t = int
    let compare = Pervasives.compare
    let pp_key fmt = F.fprintf fmt "%d"
  end)

module ConditionalWritesDomain = AbstractDomain.Map (IntMap) (PathDomain)

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
    caller foo, then ownership is transferred to the callee iWriteToAField.
*)

(** same as astate, but without [id_map]/[owned] (since they are local) *)
type summary =
  LocksDomain.astate * PathDomain.astate * ConditionalWritesDomain.astate *PathDomain.astate

let empty =
  let locks = false in
  let reads = PathDomain.empty in
  let conditional_writes = ConditionalWritesDomain.empty in
  let unconditional_writes = PathDomain.empty in
  let id_map = IdAccessPathMapDomain.empty in
  let owned = OwnershipDomain.empty in
  { locks; reads; conditional_writes; unconditional_writes; id_map; owned; }

let (<=) ~lhs ~rhs =
  if phys_equal lhs rhs
  then true
  else
    LocksDomain.(<=) ~lhs:lhs.locks ~rhs:rhs.locks &&
    PathDomain.(<=) ~lhs:lhs.reads ~rhs:rhs.reads &&
    ConditionalWritesDomain.(<=) ~lhs:lhs.conditional_writes ~rhs:rhs.conditional_writes &&
    PathDomain.(<=) ~lhs:lhs.unconditional_writes ~rhs:rhs.unconditional_writes &&
    IdAccessPathMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map &&
    OwnershipDomain.(<=) ~lhs:lhs.owned ~rhs:rhs.owned

let join astate1 astate2 =
  if phys_equal astate1 astate2
  then
    astate1
  else
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let reads = PathDomain.join astate1.reads astate2.reads in
    let conditional_writes =
      ConditionalWritesDomain.join astate1.conditional_writes astate2.conditional_writes in
    let unconditional_writes =
      PathDomain.join astate1.unconditional_writes astate2.unconditional_writes in
    let id_map = IdAccessPathMapDomain.join astate1.id_map astate2.id_map in
    let owned = OwnershipDomain.join astate1.owned astate2.owned in
    { locks; reads; conditional_writes; unconditional_writes; id_map; owned; }

let widen ~prev ~next ~num_iters =
  if phys_equal prev next
  then
    prev
  else
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let reads = PathDomain.widen ~prev:prev.reads ~next:next.reads ~num_iters in
    let conditional_writes =
      ConditionalWritesDomain.widen
        ~prev:prev.conditional_writes ~next:next.conditional_writes ~num_iters in
    let unconditional_writes =
      PathDomain.widen ~prev:prev.unconditional_writes ~next:next.unconditional_writes ~num_iters in
    let id_map = IdAccessPathMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
    let owned = OwnershipDomain.widen ~prev:prev.owned ~next:next.owned ~num_iters in
    { locks; reads; conditional_writes; unconditional_writes; id_map; owned; }

let pp_summary fmt (locks, reads, conditional_writes, unconditional_writes) =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Conditional Writes: %a Unconditional Writes: %a"
    LocksDomain.pp locks
    PathDomain.pp reads
    ConditionalWritesDomain.pp conditional_writes
    PathDomain.pp unconditional_writes

let pp fmt { locks; reads; conditional_writes; unconditional_writes; id_map; owned; } =
  F.fprintf
    fmt
    "%a Id Map: %a Owned: %a"
    pp_summary (locks, reads, conditional_writes, unconditional_writes)
    IdAccessPathMapDomain.pp id_map
    OwnershipDomain.pp owned
