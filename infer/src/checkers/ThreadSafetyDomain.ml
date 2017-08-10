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
  type t = Functional | Choice of Choice.t [@@deriving compare]

  let pp fmt = function
    | Functional
     -> F.fprintf fmt "Functional"
    | Choice choice
     -> Choice.pp fmt choice

  module Set = PrettyPrintable.MakePPSet (struct
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end

module AttributeSetDomain = AbstractDomain.InvertedSet (Attribute.Set)

module OwnershipAbstractValue = struct
  type astate = Owned | OwnedIf of IntSet.t | Unowned [@@deriving compare]

  let owned = Owned

  let unowned = Unowned

  let make_owned_if formal_index = OwnedIf (IntSet.singleton formal_index)

  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | _, Unowned
       -> true (* Unowned is top *)
      | Unowned, _
       -> false
      | Owned, _
       -> true (* Owned is bottom *)
      | OwnedIf s1, OwnedIf s2
       -> IntSet.subset s1 s2
      | OwnedIf _, Owned
       -> false

  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | _, Unowned | Unowned, _
       -> Unowned
      | astate, Owned | Owned, astate
       -> astate
      | OwnedIf s1, OwnedIf s2
       -> OwnedIf (IntSet.union s1 s2)

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Unowned
     -> F.fprintf fmt "Unowned"
    | OwnedIf s
     -> F.fprintf fmt "OwnedIf%a" (PrettyPrintable.pp_collection ~pp_item:Int.pp)
          (IntSet.elements s)
    | Owned
     -> F.fprintf fmt "Owned"
end

module OwnershipDomain = struct
  include AbstractDomain.Map (AccessPath) (OwnershipAbstractValue)

  let get_owned access_path astate =
    try find access_path astate
    with Not_found -> OwnershipAbstractValue.Unowned

  let is_owned access_path astate =
    match get_owned access_path astate with OwnershipAbstractValue.Owned -> true | _ -> false

  let find = `Use_get_owned_instead
end

module AttributeMapDomain = struct
  include AbstractDomain.InvertedMap (AccessPath.Map) (AttributeSetDomain)

  let add access_path attribute_set t =
    if AttributeSetDomain.is_empty attribute_set then t else add access_path attribute_set t

  let has_attribute access_path attribute t =
    try find access_path t |> AttributeSetDomain.mem attribute
    with Not_found -> false

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
  type t =
    | Protected of Excluder.t
    | Unprotected of IntSet.t
    | TotallyUnprotected
    [@@deriving compare]

  let pp fmt = function
    | Protected excl
     -> F.fprintf fmt "ProtectedBy(%a)" Excluder.pp excl
    | TotallyUnprotected
     -> F.fprintf fmt "TotallyUnprotected"
    | Unprotected indexes
     -> F.fprintf fmt "Unprotected(%a)" (PrettyPrintable.pp_collection ~pp_item:Int.pp)
          (IntSet.elements indexes)
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

  let of_access_path ownership (base, _) =
    match OwnershipDomain.get_owned (base, []) ownership with
    | OwnershipAbstractValue.OwnedIf formal_indexes
     -> List.map ~f:(fun formal_index -> Formal formal_index) (IntSet.elements formal_indexes)
    | _
     -> [Local (fst base)]
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
  ; ownership: OwnershipDomain.astate
  ; attribute_map: AttributeMapDomain.astate
  ; escapees: EscapeeDomain.astate }

type summary =
  ThumbsUpDomain.astate
  * ThreadsDomain.astate
  * LocksDomain.astate
  * AccessDomain.astate
  * OwnershipAbstractValue.astate
  * AttributeSetDomain.astate
  * FormalsDomain.astate

let empty =
  let thumbs_up = true in
  let threads = false in
  let locks = false in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AccessPath.Map.empty in
  let escapees = EscapeeDomain.empty in
  {thumbs_up; threads; locks; accesses; ownership; attribute_map; escapees}

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
    let ownership = OwnershipDomain.join astate1.ownership astate2.ownership in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    let escapees = EscapeeDomain.join astate1.escapees astate2.escapees in
    {thumbs_up; threads; locks; accesses; ownership; attribute_map; escapees}

let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let thumbs_up = ThreadsDomain.widen ~prev:prev.thumbs_up ~next:next.thumbs_up ~num_iters in
    let threads = ThreadsDomain.widen ~prev:prev.threads ~next:next.threads ~num_iters in
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let accesses = AccessDomain.widen ~prev:prev.accesses ~next:next.accesses ~num_iters in
    let ownership = OwnershipDomain.widen ~prev:prev.ownership ~next:next.ownership ~num_iters in
    let attribute_map =
      AttributeMapDomain.widen ~prev:prev.attribute_map ~next:next.attribute_map ~num_iters
    in
    let escapees = EscapeeDomain.widen ~prev:prev.escapees ~next:next.escapees ~num_iters in
    {thumbs_up; threads; locks; accesses; ownership; attribute_map; escapees}

let pp_summary fmt
    (thumbs_up, threads, locks, accesses, ownership, return_attributes, escapee_formals) =
  F.fprintf fmt
    "@\nThumbsUp: %a, Threads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nReturn Attributes: %a @\nEscapee Formals: %a @\n"
    ThumbsUpDomain.pp thumbs_up ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp
    accesses OwnershipAbstractValue.pp ownership AttributeSetDomain.pp return_attributes
    FormalsDomain.pp escapee_formals

let pp fmt {thumbs_up; threads; locks; accesses; ownership; attribute_map; escapees} =
  F.fprintf fmt
    "@\nThumbsUp: %a, Threads: %a, Locks: %a @\nAccesses %a @\n Ownership: %a @\nAttributes: %a @\nEscapees: %a @\n"
    ThumbsUpDomain.pp thumbs_up ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp
    accesses OwnershipDomain.pp ownership AttributeMapDomain.pp attribute_map EscapeeDomain.pp
    escapees
