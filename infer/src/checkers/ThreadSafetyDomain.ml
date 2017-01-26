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

module OwnershipDomain = AbstractDomain.InvertedSet(AccessPath.RawSet)

module TraceElem = struct
  module Kind = AccessPath.Raw

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

module LocksDomain = AbstractDomain.BooleanAnd

module PathDomain = SinkTrace.Make(TraceElem)

module IntMap = PrettyPrintable.MakePPMap(struct
    type t = int
    let compare = Int.compare
    let pp_key fmt = F.fprintf fmt "%d"
  end)

module ConditionalWritesDomain = AbstractDomain.Map (IntMap) (PathDomain)

type astate =
  {
    locks : LocksDomain.astate;
    reads : PathDomain.astate;
    conditional_writes : ConditionalWritesDomain.astate;
    unconditional_writes : PathDomain.astate;
    id_map : IdAccessPathMapDomain.astate;
    owned : OwnershipDomain.astate;
  }


type summary =
  LocksDomain.astate * PathDomain.astate * ConditionalWritesDomain.astate * PathDomain.astate * bool

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

let pp_summary fmt (locks, reads, conditional_writes, unconditional_writes, retval_owned) =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Conditional Writes: %a Unconditional Writes: %a Retval owned: %b"
    LocksDomain.pp locks
    PathDomain.pp reads
    ConditionalWritesDomain.pp conditional_writes
    PathDomain.pp unconditional_writes
    retval_owned

let pp fmt { locks; reads; conditional_writes; unconditional_writes; id_map; owned; } =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Conditional Writes: %a Unconditional Writes: %a Id Map: %a Owned: %a"
    LocksDomain.pp locks
    PathDomain.pp reads
    ConditionalWritesDomain.pp conditional_writes
    PathDomain.pp unconditional_writes
    IdAccessPathMapDomain.pp id_map
    OwnershipDomain.pp owned
