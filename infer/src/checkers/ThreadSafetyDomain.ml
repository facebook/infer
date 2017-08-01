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

module Access = struct
  type t =
    | Read of AccessPath.t
    | Write of AccessPath.t
    | ContainerRead of AccessPath.t * Typ.Procname.t
    | ContainerWrite of AccessPath.t * Typ.Procname.t
    | InterfaceCall of Typ.Procname.t
    [@@deriving compare]

  let make_field_access access_path ~is_write =
    if is_write then Write access_path else Read access_path

  let get_access_path = function
    | Read access_path
    | Write access_path
    | ContainerWrite (access_path, _)
    | ContainerRead (access_path, _)
     -> Some access_path
    | InterfaceCall _
     -> None

  let equal t1 t2 = Int.equal (compare t1 t2) 0

  let pp fmt = function
    | Read access_path
     -> F.fprintf fmt "Read of %a" AccessPath.pp access_path
    | Write access_path
     -> F.fprintf fmt "Write to %a" AccessPath.pp access_path
    | ContainerRead (access_path, pname)
     -> F.fprintf fmt "Read of container %a via %a" AccessPath.pp access_path Typ.Procname.pp pname
    | ContainerWrite (access_path, pname)
     -> F.fprintf fmt "Write to container %a via %a" AccessPath.pp access_path Typ.Procname.pp
          pname
    | InterfaceCall pname
     -> F.fprintf fmt "Call to un-annotated interface method %a" Typ.Procname.pp pname
end

module TraceElem = struct
  module Kind = Access

  type t = {site: CallSite.t; kind: Kind.t} [@@deriving compare]

  let is_write {kind} =
    match kind with
    | InterfaceCall _ | Read _ | ContainerRead _
     -> false
    | ContainerWrite _ | Write _
     -> true

  let is_container_write {kind} =
    match kind with
    | InterfaceCall _ | Read _ | Write _ | ContainerRead _
     -> false
    | ContainerWrite _
     -> true

  let call_site {site} = site

  let kind {kind} = kind

  let make ?indexes:_ kind site = {kind; site}

  let with_callsite t site = {t with site}

  let pp fmt {site; kind} = F.fprintf fmt "%a at %a" Access.pp kind CallSite.pp site

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end

let make_container_access access_path pname ~is_write loc =
  let site = CallSite.make Typ.Procname.empty_block loc in
  let access =
    if is_write then Access.ContainerWrite (access_path, pname)
    else Access.ContainerRead (access_path, pname)
  in
  TraceElem.make access site

let make_field_access access_path ~is_write loc =
  let site = CallSite.make Typ.Procname.empty_block loc in
  TraceElem.make (Access.make_field_access access_path ~is_write) site

let make_unannotated_call_access pname loc =
  let site = CallSite.make Typ.Procname.empty_block loc in
  TraceElem.make (Access.InterfaceCall pname) site

(* In this domain true<=false. The intended denotations [[.]] are
    [[true]] = the set of all states where we know according, to annotations
               or assertions or lock instructions, that some lock is held.
    [[false]] = the empty set
   The use of && for join in this domain enforces that, to know a lock is held, one must hold in
   all branches.
*)
module LocksDomain = AbstractDomain.BooleanAnd
module ThreadsDomain = AbstractDomain.BooleanAnd
module ThumbsUpDomain = AbstractDomain.BooleanAnd
module PathDomain = SinkTrace.Make (TraceElem)

module Choice = struct
  type t = OnMainThread | LockHeld [@@deriving compare]

  let pp fmt = function
    | OnMainThread
     -> F.fprintf fmt "OnMainThread"
    | LockHeld
     -> F.fprintf fmt "LockHeld"
end

module Attribute = struct
  type t = OwnedIf of int option | Functional | Choice of Choice.t [@@deriving compare]

  let pp fmt = function
    | OwnedIf None
     -> F.fprintf fmt "Owned"
    | OwnedIf Some formal_index
     -> F.fprintf fmt "Owned if formal %d is owned" formal_index
    | Functional
     -> F.fprintf fmt "Functional"
    | Choice choice
     -> Choice.pp fmt choice

  let unconditionally_owned = OwnedIf None

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end

module AttributeSetDomain = AbstractDomain.InvertedSet (Attribute.Set)

module AttributeMapDomain = struct
  include AbstractDomain.InvertedMap (AccessPath.Map) (AttributeSetDomain)

  let has_attribute access_path attribute t =
    try find access_path t |> AttributeSetDomain.mem attribute
    with Not_found -> false

  let get_conditional_ownership_index access_path t =
    try
      let attributes = find access_path t in
      List.find_map
        ~f:(function
            | Attribute.OwnedIf (Some _ as formal_index_opt) -> formal_index_opt | _ -> None)
        (AttributeSetDomain.elements attributes)
    with Not_found -> None

  let get_choices access_path t =
    try
      let attributes = find access_path t in
      List.filter_map
        ~f:(function Attribute.Choice c -> Some c | _ -> None)
        (AttributeSetDomain.elements attributes)
    with Not_found -> []

  let add_attribute access_path attribute t =
    let attribute_set =
      ( try find access_path t
        with Not_found -> AttributeSetDomain.empty )
      |> AttributeSetDomain.add attribute
    in
    add access_path attribute_set t
end

module Excluder = struct
  type t = Thread | Lock | Both [@@deriving compare]

  let pp fmt = function
    | Thread
     -> F.fprintf fmt "Thread"
    | Lock
     -> F.fprintf fmt "Lock"
    | Both
     -> F.fprintf fmt "both Thread and Lock"
end

module AccessPrecondition = struct
  type t = Protected of Excluder.t | Unprotected of int option [@@deriving compare]

  let unprotected = Unprotected None

  let pp fmt = function
    | Protected excl
     -> F.fprintf fmt "ProtectedBy(%a)" Excluder.pp excl
    | Unprotected Some index
     -> F.fprintf fmt "Unprotected(%d)" index
    | Unprotected None
     -> F.fprintf fmt "Unprotected"
end

module AccessDomain = struct
  include AbstractDomain.Map (AccessPrecondition) (PathDomain)

  let add_access precondition access_path t =
    let precondition_accesses =
      try find precondition t
      with Not_found -> PathDomain.empty
    in
    let precondition_accesses' = PathDomain.add_sink access_path precondition_accesses in
    add precondition precondition_accesses' t

  let get_accesses precondition t =
    try find precondition t
    with Not_found -> PathDomain.empty
end

module Escapee = struct
  type t = Formal of int | Local of Var.t [@@deriving compare]

  let pp fmt = function
    | Formal fml
     -> F.fprintf fmt "Formal(%d)" fml
    | Local loc
     -> F.fprintf fmt "Local(%a)" Var.pp loc

  let of_access_path extras (base, _) =
    match FormalMap.get_formal_index base extras with
    | Some fml
     -> Formal fml
    | None
     -> Local (fst base)
end

module EscapeeDomain = struct
  include AbstractDomain.FiniteSet (Escapee)

  let add_from_list escapees escapee_list =
    List.fold ~init:escapees escapee_list ~f:(fun acc v -> add v acc)
end

module FormalsDomain = struct
  include AbstractDomain.FiniteSet (Int)

  let of_escapees escapees =
    let aux escapee acc =
      match escapee with Escapee.Formal fml -> add fml acc | Escapee.Local _ -> acc
    in
    EscapeeDomain.fold aux escapees empty
end

type astate =
  { thumbs_up: ThumbsUpDomain.astate
  ; threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; attribute_map: AttributeMapDomain.astate
  ; escapees: EscapeeDomain.astate }

type summary =
  ThumbsUpDomain.astate
  * ThreadsDomain.astate
  * LocksDomain.astate
  * AccessDomain.astate
  * AttributeSetDomain.astate
  * FormalsDomain.astate

let empty =
  let thumbs_up = true in
  let threads = false in
  let locks = false in
  let accesses = AccessDomain.empty in
  let attribute_map = AccessPath.Map.empty in
  let escapees = EscapeeDomain.empty in
  {thumbs_up; threads; locks; accesses; attribute_map; escapees}

let ( <= ) ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else ThreadsDomain.( <= ) ~lhs:lhs.threads ~rhs:rhs.threads
    && LocksDomain.( <= ) ~lhs:lhs.locks ~rhs:rhs.locks
    && AccessDomain.( <= ) ~lhs:lhs.accesses ~rhs:rhs.accesses
    && AttributeMapDomain.( <= ) ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map
    && EscapeeDomain.( <= ) ~lhs:lhs.escapees ~rhs:rhs.escapees

let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let thumbs_up = ThreadsDomain.join astate1.thumbs_up astate2.thumbs_up in
    let threads = ThreadsDomain.join astate1.threads astate2.threads in
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let accesses = AccessDomain.join astate1.accesses astate2.accesses in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    let escapees = EscapeeDomain.join astate1.escapees astate2.escapees in
    {thumbs_up; threads; locks; accesses; attribute_map; escapees}

let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let thumbs_up = ThreadsDomain.widen ~prev:prev.thumbs_up ~next:next.thumbs_up ~num_iters in
    let threads = ThreadsDomain.widen ~prev:prev.threads ~next:next.threads ~num_iters in
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let accesses = AccessDomain.widen ~prev:prev.accesses ~next:next.accesses ~num_iters in
    let attribute_map =
      AttributeMapDomain.widen ~prev:prev.attribute_map ~next:next.attribute_map ~num_iters
    in
    let escapees = EscapeeDomain.widen ~prev:prev.escapees ~next:next.escapees ~num_iters in
    {thumbs_up; threads; locks; accesses; attribute_map; escapees}

let pp_summary fmt (thumbs_up, threads, locks, accesses, return_attributes, escapee_formals) =
  F.fprintf fmt
    "@\nThumbsUp: %a, Threads: %a, Locks: %a @\nAccesses %a @\nReturn Attributes: %a @\nEscapee Formals: %a @\n"
    ThumbsUpDomain.pp thumbs_up ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp
    accesses AttributeSetDomain.pp return_attributes FormalsDomain.pp escapee_formals

let pp fmt {thumbs_up; threads; locks; accesses; attribute_map; escapees} =
  F.fprintf fmt
    "@\nThumbsUp: %a, Threads: %a, Locks: %a @\nAccesses %a @\nReturn Attributes: %a @\nEscapees: %a @\n"
    ThumbsUpDomain.pp thumbs_up ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp
    accesses AttributeMapDomain.pp attribute_map EscapeeDomain.pp escapees
