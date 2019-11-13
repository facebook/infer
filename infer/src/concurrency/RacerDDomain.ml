(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format
module MF = MarkupFormatter

(** Master function for deciding whether RacerD should completely ignore a variable as the root
    of an access expression.  Currently fires on *static locals* and any variable which does not
    appear in source code (eg, temporary variables and frontend introduced variables).
    This is because currently reports on these variables would not be easily actionable.

    This is here and not in RacerDModels to avoid dependency cycles. *)
let should_skip_var v =
  (not (Var.appears_in_source_code v))
  || match v with Var.ProgramVar pvar -> Pvar.is_static_local pvar | _ -> false


let pp_exp fmt exp =
  match !Language.curr_language with
  | Clang ->
      AccessExpression.pp fmt exp
  | Java ->
      AccessPath.pp fmt (AccessExpression.to_access_path exp)


module Access = struct
  type t =
    | Read of {exp: AccessExpression.t}
    | Write of {exp: AccessExpression.t}
    | ContainerRead of {exp: AccessExpression.t; pname: Typ.Procname.t}
    | ContainerWrite of {exp: AccessExpression.t; pname: Typ.Procname.t}
    | InterfaceCall of Typ.Procname.t
  [@@deriving compare]

  let make_field_access exp ~is_write = if is_write then Write {exp} else Read {exp}

  let make_container_access exp pname ~is_write =
    if is_write then ContainerWrite {exp; pname} else ContainerRead {exp; pname}


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


  let get_access_exp = function
    | Read {exp} | Write {exp} | ContainerWrite {exp} | ContainerRead {exp} ->
        Some exp
    | InterfaceCall _ ->
        None


  let map ~f access =
    match access with
    | Read {exp} ->
        let exp' = f exp in
        if phys_equal exp exp' then access else Read {exp= exp'}
    | Write {exp} ->
        let exp' = f exp in
        if phys_equal exp exp' then access else Write {exp= exp'}
    | ContainerWrite ({exp} as record) ->
        let exp' = f exp in
        if phys_equal exp exp' then access else ContainerWrite {record with exp= exp'}
    | ContainerRead ({exp} as record) ->
        let exp' = f exp in
        if phys_equal exp exp' then access else ContainerRead {record with exp= exp'}
    | InterfaceCall _ as intfcall ->
        intfcall


  let pp fmt = function
    | Read {exp} ->
        F.fprintf fmt "Read of %a" AccessExpression.pp exp
    | Write {exp} ->
        F.fprintf fmt "Write to %a" AccessExpression.pp exp
    | ContainerRead {exp; pname} ->
        F.fprintf fmt "Read of container %a via %a" AccessExpression.pp exp Typ.Procname.pp pname
    | ContainerWrite {exp; pname} ->
        F.fprintf fmt "Write to container %a via %a" AccessExpression.pp exp Typ.Procname.pp pname
    | InterfaceCall pname ->
        F.fprintf fmt "Call to un-annotated interface method %a" Typ.Procname.pp pname


  let mono_lang_pp = MF.wrap_monospaced pp_exp

  let describe fmt = function
    | Read {exp} | Write {exp} ->
        F.fprintf fmt "access to %a" mono_lang_pp exp
    | ContainerRead {exp; pname} ->
        F.fprintf fmt "Read of container %a via call to %s" mono_lang_pp exp
          (MF.monospaced_to_string (Typ.Procname.get_method pname))
    | ContainerWrite {exp; pname} ->
        F.fprintf fmt "Write to container %a via call to %s" mono_lang_pp exp
          (MF.monospaced_to_string (Typ.Procname.get_method pname))
    | InterfaceCall pname ->
        F.fprintf fmt "Call to un-annotated interface method %a" Typ.Procname.pp pname
end

module CallPrinter = struct
  type t = CallSite.t

  let pp fmt cs = F.fprintf fmt "call to %a" Typ.Procname.pp (CallSite.pname cs)
end

module TraceElem = struct
  include ExplicitTrace.MakeTraceElemModuloLocation (Access) (CallPrinter)
  (** This choice means the comparator is insensitive to the location access. 
      This preserves correctness only if the overlying comparator (AccessSnapshot) 
      takes into account the characteristics of the access (eg lock status). *)

  let is_write {elem} = Access.is_write elem

  let is_container_write {elem} = Access.is_container_write elem

  let map ~f trace_elem = map ~f:(Access.map ~f) trace_elem

  let make_container_access access_exp pname ~is_write loc =
    make (Access.make_container_access access_exp pname ~is_write) loc


  let make_field_access access_exp ~is_write loc =
    make (Access.make_field_access access_exp ~is_write) loc


  let make_unannotated_call_access pname loc = make (Access.InterfaceCall pname) loc
end

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


  let lookup_count lock astate = find_opt lock astate |> Option.value ~default:LockCount.bottom

  let acquire_lock astate =
    let count = lookup_count the_only_lock astate in
    add the_only_lock (LockCount.increment count) astate


  let release_lock astate =
    let count = lookup_count the_only_lock astate in
    try
      let count' = LockCount.decrement count in
      if LockCount.is_bottom count' then remove the_only_lock astate
      else add the_only_lock count' astate
    with Caml.Not_found -> astate


  let integrate_summary ~caller_astate ~callee_astate =
    let caller_count = lookup_count the_only_lock caller_astate in
    let callee_count = lookup_count the_only_lock callee_astate in
    let sum = LockCount.add caller_count callee_count in
    if LockCount.is_bottom sum then caller_astate else add the_only_lock sum caller_astate
end

module ThreadsDomain = struct
  type t = NoThread | AnyThreadButSelf | AnyThread [@@deriving compare]

  let bottom = NoThread

  (* NoThread < AnyThreadButSelf < Any *)
  let leq ~lhs ~rhs =
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
        Int.equal 0 (compare lhs rhs)


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


  let is_bottom = function NoThread -> true | _ -> false

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
    { access: TraceElem.t
    ; thread: ThreadsDomain.t
    ; lock: bool
    ; ownership_precondition: OwnershipPrecondition.t }
  [@@deriving compare]

  let make_if_not_owned access lock thread ownership_precondition =
    if not (OwnershipPrecondition.is_true ownership_precondition) then
      Some {access; lock; thread; ownership_precondition}
    else None


  let make access lock thread ownership_precondition pdesc =
    let lock = LocksDomain.is_locked lock || Procdesc.is_java_synchronized pdesc in
    make_if_not_owned access lock thread ownership_precondition


  let make_from_snapshot access {lock; thread; ownership_precondition} =
    make_if_not_owned access lock thread ownership_precondition


  let is_unprotected {thread; lock; ownership_precondition} =
    (not (ThreadsDomain.is_any_but_self thread))
    && (not lock)
    && not (OwnershipPrecondition.is_true ownership_precondition)


  let pp fmt {access; thread; lock; ownership_precondition} =
    F.fprintf fmt "Loc: %a Access: %a Thread: %a Lock: %b Pre: %a" Location.pp
      (TraceElem.get_loc access) TraceElem.pp access ThreadsDomain.pp thread lock
      OwnershipPrecondition.pp ownership_precondition
end

module AccessDomain = struct
  include AbstractDomain.FiniteSet (AccessSnapshot)

  let add ({AccessSnapshot.access= {elem}} as s) astate =
    let skip =
      Access.get_access_exp elem
      |> Option.exists ~f:(fun exp -> AccessExpression.get_base exp |> fst |> should_skip_var)
    in
    if skip then astate else add s astate


  let add_opt snapshot_opt astate =
    Option.fold snapshot_opt ~init:astate ~f:(fun acc s -> add s acc)
end

module OwnershipAbstractValue = struct
  type t = OwnedIf of IntSet.t | Unowned [@@deriving compare]

  let owned = OwnedIf IntSet.empty

  let unowned = Unowned

  let make_owned_if formal_index = OwnedIf (IntSet.singleton formal_index)

  let leq ~lhs ~rhs =
    phys_equal lhs rhs
    ||
    match (lhs, rhs) with
    | _, Unowned ->
        true (* Unowned is top *)
    | Unowned, _ ->
        false
    | OwnedIf s1, OwnedIf s2 ->
        IntSet.subset s1 s2


  let join astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      match (astate1, astate2) with
      | _, Unowned | Unowned, _ ->
          Unowned
      | OwnedIf s1, OwnedIf s2 ->
          OwnedIf (IntSet.union s1 s2)


  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Unowned ->
        F.pp_print_string fmt "Unowned"
    | OwnedIf s ->
        if IntSet.is_empty s then F.pp_print_string fmt "Owned"
        else
          F.fprintf fmt "OwnedIf%a"
            (PrettyPrintable.pp_collection ~pp_item:Int.pp)
            (IntSet.elements s)
end

module OwnershipDomain = struct
  include AbstractDomain.Map (AccessExpression) (OwnershipAbstractValue)

  (* return the first non-Unowned ownership value found when checking progressively shorter
     prefixes of [access_exp] *)
  let rec get_owned (access_exp : AccessExpression.t) astate =
    match find_opt access_exp astate with
    | Some (OwnershipAbstractValue.OwnedIf _ as v) ->
        v
    | _ -> (
      match access_exp with
      | Base _ ->
          OwnershipAbstractValue.Unowned
      | AddressOf prefix | Dereference prefix | FieldOffset (prefix, _) | ArrayOffset (prefix, _, _)
        ->
          get_owned prefix astate )


  let rec ownership_of_expr expr ownership =
    let open HilExp in
    match expr with
    | AccessExpression access_expr ->
        get_owned access_expr ownership
    | Constant _ ->
        OwnershipAbstractValue.owned
    | Exception e (* treat exceptions as transparent wrt ownership *) | Cast (_, e) ->
        ownership_of_expr e ownership
    | _ ->
        OwnershipAbstractValue.unowned


  let propagate_assignment lhs_access_exp rhs_exp ownership =
    if AccessExpression.get_base lhs_access_exp |> fst |> Var.is_global then
      (* do not assign ownership to access expressions rooted at globals *)
      ownership
    else
      let rhs_ownership_value = ownership_of_expr rhs_exp ownership in
      add lhs_access_exp rhs_ownership_value ownership


  let propagate_return ret_access_exp return_ownership actuals ownership =
    let get_ownership formal_index init =
      List.nth actuals formal_index
      (* simply skip formal if we cannot find its actual, as opposed to assuming non-ownership *)
      |> Option.fold ~init ~f:(fun acc expr ->
             OwnershipAbstractValue.join acc (ownership_of_expr expr ownership) )
    in
    let ret_ownership_wrt_actuals =
      match return_ownership with
      | OwnershipAbstractValue.Unowned ->
          return_ownership
      | OwnershipAbstractValue.OwnedIf formal_indexes ->
          IntSet.fold get_ownership formal_indexes OwnershipAbstractValue.owned
    in
    add ret_access_exp ret_ownership_wrt_actuals ownership


  let get_precondition exp t =
    match get_owned exp t with
    | OwnedIf formal_indexes ->
        (* access expression conditionally owned if [formal_indexes] are owned *)
        AccessSnapshot.OwnershipPrecondition.Conjunction formal_indexes
    | Unowned ->
        (* access expression not rooted in a formal and not conditionally owned *)
        AccessSnapshot.OwnershipPrecondition.False
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
  include AbstractDomain.InvertedMap (AccessExpression) (AttributeSetDomain)

  let add access_expression attribute_set t =
    if AttributeSetDomain.is_empty attribute_set then t else add access_expression attribute_set t


  let has_attribute access_expression attribute t =
    try find access_expression t |> AttributeSetDomain.mem attribute with Caml.Not_found -> false


  let get_choices access_expression t =
    try
      let attributes = find access_expression t in
      AttributeSetDomain.fold
        (fun cc acc -> match cc with Attribute.Choice c -> c :: acc | _ -> acc)
        attributes []
    with Caml.Not_found -> []


  let add_attribute access_expression attribute t =
    update access_expression
      (function
        | Some attrs ->
            Some (AttributeSetDomain.add attribute attrs)
        | None ->
            Some (AttributeSetDomain.singleton attribute) )
      t


  let rec attributes_of_expr attribute_map (e : HilExp.t) =
    match e with
    | AccessExpression access_expr -> (
      try find access_expr attribute_map with Caml.Not_found -> AttributeSetDomain.empty )
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


  let propagate_assignment lhs_access_expression rhs_exp attribute_map =
    let rhs_attributes = attributes_of_expr attribute_map rhs_exp in
    add lhs_access_expression rhs_attributes attribute_map
end

type t =
  { threads: ThreadsDomain.t
  ; locks: LocksDomain.t
  ; accesses: AccessDomain.t
  ; ownership: OwnershipDomain.t
  ; attribute_map: AttributeMapDomain.t }

let bottom =
  let threads = ThreadsDomain.bottom in
  let locks = LocksDomain.empty in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AttributeMapDomain.empty in
  {threads; locks; accesses; ownership; attribute_map}


let is_bottom {threads; locks; accesses; ownership; attribute_map} =
  ThreadsDomain.is_bottom threads && LocksDomain.is_empty locks && AccessDomain.is_empty accesses
  && OwnershipDomain.is_empty ownership
  && AttributeMapDomain.is_empty attribute_map


let leq ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else
    ThreadsDomain.leq ~lhs:lhs.threads ~rhs:rhs.threads
    && LocksDomain.leq ~lhs:lhs.locks ~rhs:rhs.locks
    && AccessDomain.leq ~lhs:lhs.accesses ~rhs:rhs.accesses
    && AttributeMapDomain.leq ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map


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
  { threads: ThreadsDomain.t
  ; locks: LocksDomain.t
  ; accesses: AccessDomain.t
  ; return_ownership: OwnershipAbstractValue.t
  ; return_attributes: AttributeSetDomain.t }

let empty_summary =
  { threads= ThreadsDomain.bottom
  ; locks= LocksDomain.empty
  ; accesses= AccessDomain.empty
  ; return_ownership= OwnershipAbstractValue.unowned
  ; return_attributes= AttributeSetDomain.empty }


let pp_summary fmt {threads; locks; accesses; return_ownership; return_attributes} =
  F.fprintf fmt
    "@\nThreads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nReturn Attributes: %a @\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses OwnershipAbstractValue.pp
    return_ownership AttributeSetDomain.pp return_attributes


let pp fmt {threads; locks; accesses; ownership; attribute_map} =
  F.fprintf fmt "Threads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nAttributes: %a @\n"
    ThreadsDomain.pp threads LocksDomain.pp locks AccessDomain.pp accesses OwnershipDomain.pp
    ownership AttributeMapDomain.pp attribute_map


let add_unannotated_call_access pname loc pdesc (astate : t) =
  let access = TraceElem.make_unannotated_call_access pname loc in
  let snapshot = AccessSnapshot.make access astate.locks astate.threads False pdesc in
  {astate with accesses= AccessDomain.add_opt snapshot astate.accesses}
