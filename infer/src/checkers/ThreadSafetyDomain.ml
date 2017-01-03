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

type astate =
  {
    locks : LocksDomain.astate;
    (** boolean that is true if a lock must currently be held *)
    reads : PathDomain.astate;
    (** access paths read outside of synchronization *)
    writes : PathDomain.astate;
    (** access paths written outside of synchronization *)
    id_map : IdAccessPathMapDomain.astate;
    (** map used to decompile temporary variables into access paths *)
    owned : OwnershipDomain.astate;
    (** access paths that must be owned by the current function *)
  }

(** same as astate, but without [id_map] (since it's local) *)
type summary = LocksDomain.astate * PathDomain.astate * PathDomain.astate

let empty =
  let locks = false in
  let reads = PathDomain.empty in
  let writes = PathDomain.empty in
  let id_map = IdAccessPathMapDomain.empty in
  let owned = OwnershipDomain.empty in
  { locks; reads; writes; id_map; owned; }

let (<=) ~lhs ~rhs =
  if phys_equal lhs rhs
  then true
  else
    LocksDomain.(<=) ~lhs:lhs.locks ~rhs:rhs.locks &&
    PathDomain.(<=) ~lhs:lhs.reads ~rhs:rhs.reads &&
    PathDomain.(<=) ~lhs:lhs.writes ~rhs:rhs.writes &&
    IdAccessPathMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map &&
    OwnershipDomain.(<=) ~lhs:lhs.owned ~rhs:rhs.owned

let join astate1 astate2 =
  if phys_equal astate1 astate2
  then
    astate1
  else
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let reads = PathDomain.join astate1.reads astate2.reads in
    let writes = PathDomain.join astate1.writes astate2.writes in
    let id_map = IdAccessPathMapDomain.join astate1.id_map astate2.id_map in
    let owned = OwnershipDomain.join astate1.owned astate2.owned in
    { locks; reads; writes; id_map; owned; }

let widen ~prev ~next ~num_iters =
  if phys_equal prev next
  then
    prev
  else
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let reads = PathDomain.widen ~prev:prev.reads ~next:next.reads ~num_iters in
    let writes = PathDomain.widen ~prev:prev.writes ~next:next.writes ~num_iters in
    let id_map = IdAccessPathMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
    let owned = OwnershipDomain.widen ~prev:prev.owned ~next:next.owned ~num_iters in
    { locks; reads; writes; id_map; owned; }

let pp_summary fmt (locks, reads, writes) =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Writes: %a"
    LocksDomain.pp locks
    PathDomain.pp reads
    PathDomain.pp writes

let pp fmt { locks; reads; writes; id_map; owned; } =
  F.fprintf
    fmt
    "%a Id Map: %a Owned: %a"
    pp_summary (locks, reads, writes)
    IdAccessPathMapDomain.pp id_map
    OwnershipDomain.pp owned
