(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Master function for deciding whether RacerD should completely ignore a variable as the root
    of an access path.  Currently fires on *static locals* and any variable which does not
    appear in source code (eg, temporary variables and frontend introduced variables).
    This is because currently reports on these variables would not be easily actionable.

    This is here and not in RacerDModels to avoid dependency cycles. *)
let should_skip_var v =
  (not (Var.appears_in_source_code v))
  || match v with Var.ProgramVar pvar -> Pvar.is_static_local pvar | _ -> false


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
    | access1 :: _, access2 :: _ ->
        AccessPath.equal_access access1 access2
    | _ ->
        false


  let matches ~caller ~callee =
    match (caller, callee) with
    | Read ap1, Read ap2 | Write ap1, Write ap2 ->
        suffix_matches ap1 ap2
    | ContainerRead (ap1, pname1), ContainerRead (ap2, pname2)
    | ContainerWrite (ap1, pname1), ContainerWrite (ap2, pname2) ->
        Typ.Procname.equal pname1 pname2 && suffix_matches ap1 ap2
    | InterfaceCall pname1, InterfaceCall pname2 ->
        Typ.Procname.equal pname1 pname2
    | _ ->
        false


  let make_field_access access_path ~is_write =
    if is_write then Write access_path else Read access_path


  let make_container_access access_path pname ~is_write =
    if is_write then ContainerWrite (access_path, pname) else ContainerRead (access_path, pname)


  let is_write = function
    | InterfaceCall _ | Read _ | ContainerRead _ ->
        false
    | ContainerWrite _ | Write _ ->
        true


  let is_container_write = function
    | InterfaceCall _ | Read _ | Write _ | ContainerRead _ ->
        false
    | ContainerWrite _ ->
        true


  let get_access_path = function
    | Read access_path
    | Write access_path
    | ContainerWrite (access_path, _)
    | ContainerRead (access_path, _) ->
        Some access_path
    | InterfaceCall _ ->
        None


  let map ~f = function
    | Read access_path ->
        Read (f access_path)
    | Write access_path ->
        Write (f access_path)
    | ContainerWrite (access_path, pname) ->
        ContainerWrite (f access_path, pname)
    | ContainerRead (access_path, pname) ->
        ContainerRead (f access_path, pname)
    | InterfaceCall _ as intfcall ->
        intfcall


  let pp fmt = function
    | Read access_path ->
        F.fprintf fmt "Read of %a" AccessPath.pp access_path
    | Write access_path ->
        F.fprintf fmt "Write to %a" AccessPath.pp access_path
    | ContainerRead (access_path, pname) ->
        F.fprintf fmt "Read of container %a via %a" AccessPath.pp access_path Typ.Procname.pp pname
    | ContainerWrite (access_path, pname) ->
        F.fprintf fmt "Write to container %a via %a" AccessPath.pp access_path Typ.Procname.pp
          pname
    | InterfaceCall pname ->
        F.fprintf fmt "Call to un-annotated interface method %a" Typ.Procname.pp pname
end

module TraceElem = struct
  module Kind = Access

  type t = {site: CallSite.t; kind: Kind.t} [@@deriving compare]

  let is_write {kind} = Access.is_write kind

  let is_container_write {kind} = Access.is_container_write kind

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

  let dummy_pname = Typ.Procname.empty_block

  let make_dummy_site loc = CallSite.make dummy_pname loc

  (* all trace elems are created with a dummy call site. any trace elem without a dummy call site
     represents a call that leads to an access rather than a direct access *)
  let is_direct {site} = Typ.Procname.equal (CallSite.pname site) dummy_pname

  let make_container_access access_path pname ~is_write loc =
    let site = make_dummy_site loc in
    make (Access.make_container_access access_path pname ~is_write) site


  let make_field_access access_path ~is_write loc =
    let site = make_dummy_site loc in
    make (Access.make_field_access access_path ~is_write) site


  let make_unannotated_call_access pname loc =
    let site = make_dummy_site loc in
    make (Access.InterfaceCall pname) site
end

module PathDomain = SinkTrace.Make (TraceElem)

module LockCount = AbstractDomain.CountDomain (struct
  let max = 5

  (* arbitrary threshold for max locks we expect to be held simultaneously *)
end)

module LocksDomain = struct
  include AbstractDomain.Map (AccessPath) (LockCount)

  (* TODO: eventually, we'll ask acquire_lock/release_lock to pass the lock name. for now, this is a hack
     to model having a single lock used everywhere *)
  let the_only_lock = ((Var.of_id (Ident.create Ident.knormal 0), Typ.void_star), [])

  let is_locked astate =
    (* we only add locks with positive count, so safe to just check emptiness *)
    not (is_empty astate)


  let lookup_count lock astate = try find lock astate with Caml.Not_found -> LockCount.empty

  let acquire_lock astate =
    let count = lookup_count the_only_lock astate in
    add the_only_lock (LockCount.increment count) astate


  let release_lock astate =
    let count = lookup_count the_only_lock astate in
    try
      let count' = LockCount.decrement count in
      if LockCount.is_empty count' then remove the_only_lock astate
      else add the_only_lock count' astate
    with Caml.Not_found -> astate


  let integrate_summary ~caller_astate ~callee_astate =
    let caller_count = lookup_count the_only_lock caller_astate in
    let callee_count = lookup_count the_only_lock callee_astate in
    let sum = LockCount.add caller_count callee_count in
    if LockCount.is_empty sum then caller_astate else add the_only_lock sum caller_astate
end

module ThreadsDomain = struct
  type astate = NoThread | AnyThreadButSelf | AnyThread [@@deriving compare]

  let empty = NoThread

  (* NoThread < AnyThreadButSelf < Any *)
  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    ||
    match (lhs, rhs) with
    | NoThread, _ ->
        true
    | _, NoThread ->
        false
    | _, AnyThread ->
        true
    | AnyThread, _ ->
        false
    | _ ->
        Int.equal 0 (compare_astate lhs rhs)


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | NoThread, astate | astate, NoThread ->
          astate
      | AnyThread, _ | _, AnyThread ->
          AnyThread
      | AnyThreadButSelf, AnyThreadButSelf ->
          AnyThreadButSelf


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt astate =
    F.pp_print_string fmt
      ( match astate with
      | NoThread ->
          "NoThread"
      | AnyThreadButSelf ->
          "AnyThreadButSelf"
      | AnyThread ->
          "AnyThread" )


  (* at least one access is known to occur on a background thread *)
  let can_conflict astate1 astate2 =
    match join astate1 astate2 with AnyThread -> true | NoThread | AnyThreadButSelf -> false


  let is_empty = function NoThread -> true | _ -> false

  let is_any_but_self = function AnyThreadButSelf -> true | _ -> false

  let is_any = function AnyThread -> true | _ -> false

  let integrate_summary ~caller_astate ~callee_astate =
    (* if we know the callee runs on the main thread, assume the caller does too. otherwise, keep
       the caller's thread context *)
    match callee_astate with AnyThreadButSelf -> callee_astate | _ -> caller_astate
end

module AccessSnapshot = struct
  module OwnershipPrecondition = struct
    type t = Conjunction of IntSet.t | False [@@deriving compare]

    (* precondition is true if the conjunction of owned indexes is empty *)
    let is_true = function False -> false | Conjunction set -> IntSet.is_empty set

    let pp fmt = function
      | Conjunction indexes ->
          F.fprintf fmt "Owned(%a)"
            (PrettyPrintable.pp_collection ~pp_item:Int.pp)
            (IntSet.elements indexes)
      | False ->
          F.pp_print_string fmt "False"
  end

  type t =
    { access: PathDomain.Sink.t
    ; thread: ThreadsDomain.astate
    ; lock: bool
    ; ownership_precondition: OwnershipPrecondition.t }
  [@@deriving compare]

  let make_from_snapshot access {lock; thread; ownership_precondition} =
    (* shouldn't be creating metadata for accesses that are known to be owned; we should discard
       such accesses *)
    assert (not (OwnershipPrecondition.is_true ownership_precondition)) ;
    {access; thread; lock; ownership_precondition}


  let make access lock thread ownership_precondition pdesc =
    assert (not (OwnershipPrecondition.is_true ownership_precondition)) ;
    let lock = LocksDomain.is_locked lock || Procdesc.is_java_synchronized pdesc in
    {access; lock; thread; ownership_precondition}


  let is_unprotected {thread; lock; ownership_precondition} =
    (not (ThreadsDomain.is_any_but_self thread))
    && (not lock)
    && not (OwnershipPrecondition.is_true ownership_precondition)


  let pp fmt {access; thread; lock; ownership_precondition} =
    F.fprintf fmt "Access: %a Thread: %a Lock: %b Pre: %a" TraceElem.pp access ThreadsDomain.pp
      thread lock OwnershipPrecondition.pp ownership_precondition
end

module AccessDomain = struct
  include AbstractDomain.FiniteSet (AccessSnapshot)

  let add ({AccessSnapshot.access= {kind}} as s) astate =
    let skip =
      Access.get_access_path kind
      |> Option.value_map ~default:false ~f:(fun ((v, _), _) -> should_skip_var v)
    in
    if skip then astate else add s astate
end

module OwnershipAbstractValue = struct
  type astate = Owned | OwnedIf of IntSet.t | Unowned [@@deriving compare]

  let owned = Owned

  let unowned = Unowned

  let make_owned_if formal_index = OwnedIf (IntSet.singleton formal_index)

  let ( <= ) ~lhs ~rhs =
    phys_equal lhs rhs
    ||
    match (lhs, rhs) with
    | _, Unowned ->
        true (* Unowned is top *)
    | Unowned, _ ->
        false
    | Owned, _ ->
        true (* Owned is bottom *)
    | OwnedIf s1, OwnedIf s2 ->
        IntSet.subset s1 s2
    | OwnedIf _, Owned ->
        false


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | _, Unowned | Unowned, _ ->
          Unowned
      | astate, Owned | Owned, astate ->
          astate
      | OwnedIf s1, OwnedIf s2 ->
          OwnedIf (IntSet.union s1 s2)


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Unowned ->
        F.pp_print_string fmt "Unowned"
    | OwnedIf s ->
        F.fprintf fmt "OwnedIf%a"
          (PrettyPrintable.pp_collection ~pp_item:Int.pp)
          (IntSet.elements s)
    | Owned ->
        F.pp_print_string fmt "Owned"
end

module OwnershipDomain = struct
  include AbstractDomain.Map (AccessPath) (OwnershipAbstractValue)

  (* return the first non-Unowned ownership value found when checking progressively shorter
     prefixes of [access_path] *)
  let rec get_owned access_path astate =
    let keep_looking access_path astate =
      match AccessPath.truncate access_path with
      | access_path', Some _ ->
          get_owned access_path' astate
      | _ ->
          OwnershipAbstractValue.Unowned
    in
    match find access_path astate with
    | (OwnershipAbstractValue.Owned | OwnedIf _) as v ->
        v
    | OwnershipAbstractValue.Unowned ->
        keep_looking access_path astate
    | exception Caml.Not_found ->
        keep_looking access_path astate


  let is_owned access_path astate =
    match get_owned access_path astate with
    | OwnershipAbstractValue.Owned ->
        true
    | OwnershipAbstractValue.OwnedIf _ | Unowned ->
        false


  let find = `Use_get_owned_instead

  let rec ownership_of_expr expr ownership =
    let open HilExp in
    match expr with
    | AccessExpression access_expr ->
        get_owned (AccessExpression.to_access_path access_expr) ownership
    | Constant _ ->
        OwnershipAbstractValue.owned
    | Exception e (* treat exceptions as transparent wrt ownership *) | Cast (_, e) ->
        ownership_of_expr e ownership
    | _ ->
        OwnershipAbstractValue.unowned


  let propagate_assignment ((lhs_root, _) as lhs_access_path) rhs_exp ownership =
    if Var.is_global (fst lhs_root) then
      (* do not assign ownership to access paths rooted at globals *)
      ownership
    else
      let rhs_ownership_value = ownership_of_expr rhs_exp ownership in
      add lhs_access_path rhs_ownership_value ownership


  let propagate_return ret_access_path return_ownership actuals ownership =
    let get_ownership formal_index acc =
      List.nth actuals formal_index
      |> Option.map ~f:(fun expr -> ownership_of_expr expr ownership)
      (* simply skip formal if we cannot find its actual, as opposed to assuming non-ownership *)
      |> Option.fold ~init:acc ~f:OwnershipAbstractValue.join
    in
    let ret_ownership_wrt_actuals =
      match return_ownership with
      | OwnershipAbstractValue.Owned | Unowned ->
          return_ownership
      | OwnershipAbstractValue.OwnedIf formal_indexes ->
          IntSet.fold get_ownership formal_indexes OwnershipAbstractValue.owned
    in
    add ret_access_path ret_ownership_wrt_actuals ownership
end

module Choice = struct
  type t = OnMainThread | LockHeld [@@deriving compare]

  let pp fmt = function
    | OnMainThread ->
        F.pp_print_string fmt "OnMainThread"
    | LockHeld ->
        F.pp_print_string fmt "LockHeld"
end

module Attribute = struct
  type t = Functional | Choice of Choice.t [@@deriving compare]

  let pp fmt = function
    | Functional ->
        F.pp_print_string fmt "Functional"
    | Choice choice ->
        Choice.pp fmt choice
end

module AttributeSetDomain = AbstractDomain.InvertedSet (Attribute)

module AttributeMapDomain = struct
  include AbstractDomain.InvertedMap (AccessPath) (AttributeSetDomain)

  let add access_path attribute_set t =
    if AttributeSetDomain.is_empty attribute_set then t else add access_path attribute_set t


  let has_attribute access_path attribute t =
    try find access_path t |> AttributeSetDomain.mem attribute with Caml.Not_found -> false


  let get_choices access_path t =
    try
      let attributes = find access_path t in
      AttributeSetDomain.fold
        (fun cc acc -> match cc with Attribute.Choice c -> c :: acc | _ -> acc)
        attributes []
    with Caml.Not_found -> []


  let add_attribute access_path attribute t =
    let attribute_set =
      (try find access_path t with Caml.Not_found -> AttributeSetDomain.empty)
      |> AttributeSetDomain.add attribute
    in
    add access_path attribute_set t


  let rec attributes_of_expr attribute_map e =
    let open HilExp in
    match e with
    | HilExp.AccessExpression access_expr -> (
      try find (AccessExpression.to_access_path access_expr) attribute_map with Caml.Not_found ->
        AttributeSetDomain.empty )
    | Constant _ ->
        AttributeSetDomain.singleton Attribute.Functional
    | Exception expr (* treat exceptions as transparent wrt attributes *) | Cast (_, expr) ->
        attributes_of_expr attribute_map expr
    | UnaryOperator (_, expr, _) ->
        attributes_of_expr attribute_map expr
    | BinaryOperator (_, expr1, expr2) ->
        let attributes1 = attributes_of_expr attribute_map expr1 in
        let attributes2 = attributes_of_expr attribute_map expr2 in
        AttributeSetDomain.join attributes1 attributes2
    | Closure _ | Sizeof _ ->
        AttributeSetDomain.empty


  let propagate_assignment lhs_access_path rhs_exp attribute_map =
    let rhs_attributes = attributes_of_expr attribute_map rhs_exp in
    add lhs_access_path rhs_attributes attribute_map
end

module StabilityDomain = struct
  include AbstractDomain.FiniteSet (AccessPath)

  let is_prefix (base, accesses) (base', accesses') =
    AccessPath.equal_base base base'
    && List.is_prefix ~equal:AccessPath.equal_access ~prefix:accesses accesses'


  let is_proper_prefix (base, accesses) (base', accesses') =
    let rec aux xs ys =
      match (xs, ys) with
      | [], _ ->
          not (List.is_empty ys)
      | _, [] ->
          false
      | w :: ws, z :: zs ->
          AccessPath.equal_access w z && aux ws zs
    in
    AccessPath.equal_base base base' && aux accesses accesses'


  let exists_prefix path_to_check t = exists (fun path -> is_prefix path path_to_check) t

  let exists_proper_prefix path_to_check t =
    exists (fun path -> is_proper_prefix path path_to_check) t


  let add_path path_to_add t =
    if
      (not Config.racerd_use_path_stability)
      || should_skip_var (fst path_to_add |> fst)
      || exists_prefix path_to_add t
    then t
    else filter (fun path -> is_prefix path_to_add path |> not) t |> add path_to_add


  let add_exp exp paths =
    if not Config.racerd_use_path_stability then paths
    else
      AccessExpression.to_access_paths (HilExp.get_access_exprs exp)
      |> List.fold ~f:(fun acc p -> add_path p acc) ~init:paths


  let add_assign lhs_path rhs_exp paths =
    if not Config.racerd_use_path_stability then paths
    else add_path lhs_path paths |> add_exp rhs_exp


  let actual_to_access_path actual_exp =
    match HilExp.get_access_exprs actual_exp with
    | [actual] ->
        Some (AccessExpression.to_access_path actual)
    | _ ->
        None


  let rebase actuals pdesc t =
    let formal_map = FormalMap.make pdesc in
    let expand_path ((base, accesses) as p) =
      FormalMap.get_formal_index base formal_map
      |> Option.bind ~f:(List.nth actuals)
      |> Option.bind ~f:actual_to_access_path
      |> Option.value_map ~default:p ~f:(fun ap -> AccessPath.append ap accesses)
    in
    fold
      (fun ((_, accesses) as path) acc ->
        if List.is_empty accesses then acc else add_path (expand_path path) acc )
      t empty


  let integrate_summary actuals pdesc_opt ~callee ~caller =
    if not Config.racerd_use_path_stability then caller
    else
      let rebased =
        Option.value_map pdesc_opt ~default:callee ~f:(fun pdesc -> rebase actuals pdesc callee)
      in
      let joined = join rebased caller in
      let actual_aps = List.filter_map actuals ~f:actual_to_access_path in
      let rec aux acc left right =
        match right with
        | [] ->
            acc
        | ap :: aps ->
            let all = List.rev_append left aps in
            let acc' = if List.exists all ~f:(is_prefix ap) then add_path ap acc else acc in
            aux acc' (ap :: left) aps
      in
      aux joined [] actual_aps
end

type astate =
  { threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; ownership: OwnershipDomain.astate
  ; attribute_map: AttributeMapDomain.astate
  ; wobbly_paths: StabilityDomain.astate }

let empty =
  let threads = ThreadsDomain.empty in
  let locks = LocksDomain.empty in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AttributeMapDomain.empty in
  let wobbly_paths = StabilityDomain.empty in
  {threads; locks; accesses; ownership; attribute_map; wobbly_paths}


let is_empty {threads; locks; accesses; ownership; attribute_map; wobbly_paths} =
  ThreadsDomain.is_empty threads && LocksDomain.is_empty locks && AccessDomain.is_empty accesses
  && OwnershipDomain.is_empty ownership
  && AttributeMapDomain.is_empty attribute_map
  && StabilityDomain.is_empty wobbly_paths


let ( <= ) ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else
    ThreadsDomain.( <= ) ~lhs:lhs.threads ~rhs:rhs.threads
    && LocksDomain.( <= ) ~lhs:lhs.locks ~rhs:rhs.locks
    && AccessDomain.( <= ) ~lhs:lhs.accesses ~rhs:rhs.accesses
    && AttributeMapDomain.( <= ) ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map
    && StabilityDomain.( <= ) ~lhs:lhs.wobbly_paths ~rhs:rhs.wobbly_paths


let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let threads = ThreadsDomain.join astate1.threads astate2.threads in
    let locks = LocksDomain.join astate1.locks astate2.locks in
    let accesses = AccessDomain.join astate1.accesses astate2.accesses in
    let ownership = OwnershipDomain.join astate1.ownership astate2.ownership in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    let wobbly_paths = StabilityDomain.join astate1.wobbly_paths astate2.wobbly_paths in
    {threads; locks; accesses; ownership; attribute_map; wobbly_paths}


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
    let wobbly_paths =
      StabilityDomain.widen ~prev:prev.wobbly_paths ~next:next.wobbly_paths ~num_iters
    in
    {threads; locks; accesses; ownership; attribute_map; wobbly_paths}


type summary =
  { threads: ThreadsDomain.astate
  ; locks: LocksDomain.astate
  ; accesses: AccessDomain.astate
  ; return_ownership: OwnershipAbstractValue.astate
  ; return_attributes: AttributeSetDomain.astate
  ; wobbly_paths: StabilityDomain.astate }

let pp_summary fmt {threads; locks; accesses; return_ownership; return_attributes; wobbly_paths} =
  F.fprintf fmt
    "@\n\
     Threads: %a, Locks: %a @\n\
     Accesses %a @\n\
     Ownership: %a @\n\
     Return Attributes: %a @\n\
     Wobbly Paths: %a@\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses
    OwnershipAbstractValue.pp return_ownership AttributeSetDomain.pp return_attributes
    StabilityDomain.pp wobbly_paths


let pp fmt {threads; locks; accesses; ownership; attribute_map; wobbly_paths} =
  F.fprintf fmt
    "Threads: %a, Locks: %a @\n\
     Accesses %a @\n\
    \ Ownership: %a @\n\
     Attributes: %a @\n\
     Non-stable Paths: %a@\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses OwnershipDomain.pp
    ownership AttributeMapDomain.pp attribute_map StabilityDomain.pp wobbly_paths
