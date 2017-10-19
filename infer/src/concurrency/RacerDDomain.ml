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

  let suffix_matches (_, accesses1) (_, accesses2) =
    match (List.rev accesses1, List.rev accesses2) with
    | access1 :: _, access2 :: _
     -> AccessPath.equal_access access1 access2
    | _
     -> false

  let matches ~caller ~callee =
    match (caller, callee) with
    | Read ap1, Read ap2 | Write ap1, Write ap2
     -> suffix_matches ap1 ap2
    | ContainerRead (ap1, pname1), ContainerRead (ap2, pname2)
    | ContainerWrite (ap1, pname1), ContainerWrite (ap2, pname2)
     -> Typ.Procname.equal pname1 pname2 && suffix_matches ap1 ap2
    | InterfaceCall pname1, InterfaceCall pname2
     -> Typ.Procname.equal pname1 pname2
    | _
     -> false

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

  let map ~f = function
    | Read access_path
     -> Read (f access_path)
    | Write access_path
     -> Write (f access_path)
    | ContainerWrite (access_path, pname)
     -> ContainerWrite (f access_path, pname)
    | ContainerRead (access_path, pname)
     -> ContainerRead (f access_path, pname)
    | InterfaceCall _ as intfcall
     -> intfcall

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

  let map ~f {site; kind} = {site; kind= Access.map ~f kind}

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

module ThreadsDomain = struct
  type astate = NoThread | AnyThreadButSelf | AnyThread [@@deriving compare]

  let empty = NoThread

  (* NoThread < AnyThreadButSelf < Any *)
  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | NoThread, _
     -> true
    | _, NoThread
     -> false
    | _, AnyThread
     -> true
    | AnyThread, _
     -> false
    | _
     -> Int.equal 0 (compare_astate lhs rhs)

  let join astate1 astate2 =
    match (astate1, astate2) with
    | NoThread, astate | astate, NoThread
     -> astate
    | AnyThread, _ | _, AnyThread
     -> AnyThread
    | AnyThreadButSelf, AnyThreadButSelf
     -> AnyThreadButSelf

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate =
    F.fprintf fmt
      ( match astate with
      | NoThread
       -> "NoThread"
      | AnyThreadButSelf
       -> "AnyThreadButSelf"
      | AnyThread
       -> "AnyThread" )

  let is_empty = function NoThread -> true | _ -> false

  let is_any_but_self = function AnyThreadButSelf -> true | _ -> false

  let is_any = function AnyThread -> true | _ -> false
end

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

module AttributeSetDomain = AbstractDomain.InvertedSet (Attribute)

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
  include AbstractDomain.InvertedMap (AccessPath) (AttributeSetDomain)

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

  let make locks thread pdesc =
    let is_main_thread = ThreadsDomain.is_any_but_self thread in
    let locked = locks || Procdesc.is_java_synchronized pdesc in
    if not locked && not is_main_thread then TotallyUnprotected
    else if locked && is_main_thread then Protected Excluder.Both
    else if locked then Protected Excluder.Lock
    else Protected Excluder.Thread
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

type astate =
  { threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; ownership: OwnershipDomain.astate
  ; attribute_map: AttributeMapDomain.astate }

let empty =
  let threads = ThreadsDomain.empty in
  let locks = false in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AttributeMapDomain.empty in
  {threads; locks; accesses; ownership; attribute_map}

let is_empty {threads; locks; accesses; ownership; attribute_map} =
  ThreadsDomain.is_empty threads && not locks && AccessDomain.is_empty accesses
  && OwnershipDomain.is_empty ownership && AttributeMapDomain.is_empty attribute_map

let ( <= ) ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else ThreadsDomain.( <= ) ~lhs:lhs.threads ~rhs:rhs.threads
    && LocksDomain.( <= ) ~lhs:lhs.locks ~rhs:rhs.locks
    && AccessDomain.( <= ) ~lhs:lhs.accesses ~rhs:rhs.accesses
    && AttributeMapDomain.( <= ) ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map

let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let threads = ThreadsDomain.join astate1.threads astate2.threads in
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let accesses = AccessDomain.join astate1.accesses astate2.accesses in
    let ownership = OwnershipDomain.join astate1.ownership astate2.ownership in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    {threads; locks; accesses; ownership; attribute_map}

let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let threads = ThreadsDomain.widen ~prev:prev.threads ~next:next.threads ~num_iters in
    let locks = LocksDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let accesses = AccessDomain.widen ~prev:prev.accesses ~next:next.accesses ~num_iters in
    let ownership = OwnershipDomain.widen ~prev:prev.ownership ~next:next.ownership ~num_iters in
    let attribute_map =
      AttributeMapDomain.widen ~prev:prev.attribute_map ~next:next.attribute_map ~num_iters
    in
    {threads; locks; accesses; ownership; attribute_map}

type summary =
  { threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; return_ownership: OwnershipAbstractValue.astate
  ; return_attributes: AttributeSetDomain.astate }

let pp_summary fmt {threads; locks; accesses; return_ownership; return_attributes} =
  F.fprintf fmt
    "@\nThreads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nReturn Attributes: %a @\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses
    OwnershipAbstractValue.pp return_ownership AttributeSetDomain.pp return_attributes

let pp fmt {threads; locks; accesses; ownership; attribute_map} =
  F.fprintf fmt "Threads: %a, Locks: %a @\nAccesses %a @\n Ownership: %a @\nAttributes: %a @\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses OwnershipDomain.pp
    ownership AttributeMapDomain.pp attribute_map
