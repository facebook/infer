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

module AccessPathSetDomain = AbstractDomain.InvertedSet(AccessPath.UntypedRawSet)

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
      let pp = pp
    end)
end

let make_access kind loc =
  let site = CallSite.make Procname.empty_block loc in
  TraceElem.make kind site

module LocksDomain = AbstractDomain.BooleanAnd

module PathDomain = SinkTrace.Make(TraceElem)

module Attribute = struct
  type t =
    | OwnedIf of int option
    | Functional
  [@@deriving compare]

  let pp fmt = function
    | OwnedIf None -> F.fprintf fmt "Owned"
    | OwnedIf (Some formal_index) -> F.fprintf fmt "Owned if formal %d is owned" formal_index
    | Functional -> F.fprintf fmt "Functional"

  let unconditionally_owned = OwnedIf None

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)
end

module AttributeSetDomain = AbstractDomain.InvertedSet (Attribute.Set)

module AttributeMapDomain = struct
  include AbstractDomain.InvertedMap (AccessPath.UntypedRawMap) (AttributeSetDomain)

  let has_attribute access_path attribute t =
    try
      find access_path t
      |> AttributeSetDomain.mem attribute
    with Not_found ->
      false

  let add_attribute access_path attribute t =
    let attribute_set =
      (try find access_path t
       with Not_found -> AttributeSetDomain.empty)
      |> AttributeSetDomain.add attribute in
    add access_path attribute_set t
end

module AccessPrecondition = struct
  type t =
    | Protected
    | ProtectedIf of int option
  [@@deriving compare]

  let unprotected = ProtectedIf None

  let pp fmt = function
    | Protected -> F.fprintf fmt "Protected"
    | ProtectedIf (Some index) -> F.fprintf fmt "ProtectedIf %d" index
    | ProtectedIf None -> F.fprintf fmt "Unprotected"

  module Map = PrettyPrintable.MakePPMap(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)
end

module AccessDomain = struct
  include AbstractDomain.Map (AccessPrecondition.Map) (PathDomain)

  let add_access precondition access_path t =
    let precondition_accesses =
      try find precondition t
      with Not_found -> PathDomain.empty in
    let precondition_accesses' = PathDomain.add_sink access_path precondition_accesses in
    add precondition precondition_accesses' t

  let get_accesses precondition t =
    try find precondition t
    with Not_found -> PathDomain.empty
end

type astate =
  {
    locks : LocksDomain.astate;
    reads : PathDomain.astate;
    writes : AccessDomain.astate;
    id_map : IdAccessPathMapDomain.astate;
    attribute_map : AttributeMapDomain.astate;
  }

type summary =
  LocksDomain.astate * PathDomain.astate * AccessDomain.astate * AttributeSetDomain.astate

let empty =
  let locks = false in
  let reads = PathDomain.empty in
  let writes = AccessDomain.empty in
  let id_map = IdAccessPathMapDomain.empty in
  let attribute_map = AccessPath.UntypedRawMap.empty in
  { locks; reads; writes; id_map; attribute_map; }

let (<=) ~lhs ~rhs =
  if phys_equal lhs rhs
  then true
  else
    LocksDomain.(<=) ~lhs:lhs.locks ~rhs:rhs.locks &&
    PathDomain.(<=) ~lhs:lhs.reads ~rhs:rhs.reads &&
    AccessDomain.(<=) ~lhs:lhs.writes ~rhs:rhs.writes &&
    IdAccessPathMapDomain.(<=) ~lhs:lhs.id_map ~rhs:rhs.id_map &&
    AttributeMapDomain.(<=) ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map

let join astate1 astate2 =
  if phys_equal astate1 astate2
  then
    astate1
  else
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let reads = PathDomain.join astate1.reads astate2.reads in
    let writes = AccessDomain.join astate1.writes astate2.writes in
    let id_map = IdAccessPathMapDomain.join astate1.id_map astate2.id_map in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    { locks; reads; writes; id_map; attribute_map; }

let widen ~prev ~next ~num_iters =
  if phys_equal prev next
  then
    prev
  else
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let reads = PathDomain.widen ~prev:prev.reads ~next:next.reads ~num_iters in
    let writes = AccessDomain.widen ~prev:prev.writes ~next:next.writes ~num_iters in
    let id_map = IdAccessPathMapDomain.widen ~prev:prev.id_map ~next:next.id_map ~num_iters in
    let attribute_map =
      AttributeMapDomain.widen ~prev:prev.attribute_map ~next:next.attribute_map ~num_iters in
    { locks; reads; writes; id_map; attribute_map; }

let pp_summary fmt (locks, reads, writes, return_attributes) =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Writes: %a Return Attributes: %a"
    LocksDomain.pp locks
    PathDomain.pp reads
    AccessDomain.pp writes
    AttributeSetDomain.pp return_attributes

let pp fmt { locks; reads; writes; id_map; attribute_map; } =
  F.fprintf
    fmt
    "Locks: %a Reads: %a Writes: %a Id Map: %a Attribute Map:\
     %a"
    LocksDomain.pp locks
    PathDomain.pp reads
    AccessDomain.pp writes
    IdAccessPathMapDomain.pp id_map
    AttributeMapDomain.pp attribute_map
