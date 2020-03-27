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
    | ContainerRead of {exp: AccessExpression.t; pname: Procname.t}
    | ContainerWrite of {exp: AccessExpression.t; pname: Procname.t}
    | InterfaceCall of Procname.t
  [@@deriving compare]

  let make_field_access exp ~is_write = if is_write then Write {exp} else Read {exp}

  let make_container_access exp pname ~is_write =
    if is_write then ContainerWrite {exp; pname} else ContainerRead {exp; pname}


  let make_unannotated_call_access pname = InterfaceCall pname

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


  let should_keep formals access =
    let rec check_access (exp : AccessExpression.t) =
      match exp with
      | FieldOffset (prefix, fld) ->
          (not (String.is_substring ~substring:"$SwitchMap" (Fieldname.get_field_name fld)))
          && check_access prefix
      | ArrayOffset (prefix, _, _) | AddressOf prefix | Dereference prefix ->
          check_access prefix
      | Base (LogicalVar _, _) ->
          false
      | Base (((ProgramVar pvar as var), _) as base) ->
          Var.appears_in_source_code var
          && (not (Pvar.is_static_local pvar))
          && (Pvar.is_global pvar || FormalMap.is_formal base formals)
    in
    match get_access_exp access with None -> true | Some acc_exp -> check_access acc_exp


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
        F.fprintf fmt "Read of container %a via %a" AccessExpression.pp exp Procname.pp pname
    | ContainerWrite {exp; pname} ->
        F.fprintf fmt "Write to container %a via %a" AccessExpression.pp exp Procname.pp pname
    | InterfaceCall pname ->
        F.fprintf fmt "Call to un-annotated interface method %a" Procname.pp pname


  let mono_lang_pp = MF.wrap_monospaced pp_exp

  let describe fmt = function
    | Read {exp} | Write {exp} ->
        F.fprintf fmt "access to %a" mono_lang_pp exp
    | ContainerRead {exp; pname} ->
        F.fprintf fmt "Read of container %a via call to %s" mono_lang_pp exp
          (MF.monospaced_to_string (Procname.get_method pname))
    | ContainerWrite {exp; pname} ->
        F.fprintf fmt "Write to container %a via call to %s" mono_lang_pp exp
          (MF.monospaced_to_string (Procname.get_method pname))
    | InterfaceCall pname ->
        F.fprintf fmt "Call to un-annotated interface method %a" Procname.pp pname
end

module CallPrinter = struct
  type t = CallSite.t

  let pp fmt cs = F.fprintf fmt "call to %a" Procname.pp (CallSite.pname cs)
end

module LockDomain = struct
  include AbstractDomain.CountDomain (struct
    (** arbitrary threshold for max locks we expect to be held simultaneously *)
    let max = 5
  end)

  let acquire_lock = increment

  let release_lock = decrement

  let integrate_summary ~caller_astate ~callee_astate = add caller_astate callee_astate

  let is_locked t = not (is_bottom t)
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

module OwnershipAbstractValue = struct
  type t = OwnedIf of IntSet.t | Unowned [@@deriving compare]

  let owned = OwnedIf IntSet.empty

  let is_owned = function OwnedIf set -> IntSet.is_empty set | Unowned -> false

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

module AccessSnapshot = struct
  module AccessSnapshotElem = struct
    type t =
      { access: Access.t
      ; thread: ThreadsDomain.t
      ; lock: bool
      ; ownership_precondition: OwnershipAbstractValue.t }
    [@@deriving compare]

    let pp fmt {access; thread; lock; ownership_precondition} =
      F.fprintf fmt "Access: %a Thread: %a Lock: %b Pre: %a" Access.pp access ThreadsDomain.pp
        thread lock OwnershipAbstractValue.pp ownership_precondition


    let describe fmt {access} = Access.describe fmt access
  end

  include ExplicitTrace.MakeTraceElemModuloLocation (AccessSnapshotElem) (CallPrinter)

  let is_write {elem= {access}} = Access.is_write access

  let is_container_write {elem= {access}} = Access.is_container_write access

  let filter formals t =
    if
      (not (OwnershipAbstractValue.is_owned t.elem.ownership_precondition))
      && Access.should_keep formals t.elem.access
    then Some t
    else None


  let make_if_not_owned formals access lock thread ownership_precondition loc =
    make {access; lock; thread; ownership_precondition} loc |> filter formals


  let make_unannotated_call_access formals pname lock ownership loc =
    let lock = LockDomain.is_locked lock in
    let access = Access.make_unannotated_call_access pname in
    make_if_not_owned formals access lock ownership loc


  let make_access formals acc_exp ~is_write loc lock thread ownership_precondition =
    let lock = LockDomain.is_locked lock in
    let access = Access.make_field_access acc_exp ~is_write in
    make_if_not_owned formals access lock thread ownership_precondition loc


  let make_container_access formals acc_exp ~is_write callee loc lock thread ownership_precondition
      =
    let lock = LockDomain.is_locked lock in
    let access = Access.make_container_access acc_exp callee ~is_write in
    make_if_not_owned formals access lock thread ownership_precondition loc


  let map_opt formals ~f t =
    map t ~f:(fun elem -> {elem with access= Access.map ~f elem.access}) |> filter formals


  let update_callee_access formals snapshot callsite ownership_precondition threads locks =
    let thread =
      ThreadsDomain.integrate_summary ~callee_astate:snapshot.elem.thread ~caller_astate:threads
    in
    let lock = snapshot.elem.lock || LockDomain.is_locked locks in
    with_callsite snapshot callsite
    |> map ~f:(fun elem -> {elem with lock; thread; ownership_precondition})
    |> filter formals


  let is_unprotected {elem= {thread; lock; ownership_precondition}} =
    (not (ThreadsDomain.is_any_but_self thread))
    && (not lock)
    && not (OwnershipAbstractValue.is_owned ownership_precondition)
end

module AccessDomain = struct
  include AbstractDomain.FiniteSet (AccessSnapshot)

  let add_opt snapshot_opt astate =
    Option.fold snapshot_opt ~init:astate ~f:(fun acc s -> add s acc)
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
end

module Attribute = struct
  type t = Nothing | Functional | OnMainThread | LockHeld [@@deriving equal]

  let pp fmt t =
    ( match t with
    | Nothing ->
        "Nothing"
    | Functional ->
        "Functional"
    | OnMainThread ->
        "OnMainThread"
    | LockHeld ->
        "LockHeld" )
    |> F.pp_print_string fmt


  let top = Nothing

  let is_top = function Nothing -> true | _ -> false

  let join t t' = if equal t t' then t else Nothing

  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module AttributeMapDomain = struct
  include AbstractDomain.SafeInvertedMap (AccessExpression) (Attribute)

  let find acc_exp t = find_opt acc_exp t |> Option.value ~default:Attribute.top

  let is_functional t access_expression =
    match find_opt access_expression t with Some Functional -> true | _ -> false


  let rec attribute_of_expr attribute_map (e : HilExp.t) =
    match e with
    | AccessExpression access_expr ->
        find access_expr attribute_map
    | Constant _ ->
        Attribute.Functional
    | Exception expr (* treat exceptions as transparent wrt attributes *) | Cast (_, expr) ->
        attribute_of_expr attribute_map expr
    | UnaryOperator (_, expr, _) ->
        attribute_of_expr attribute_map expr
    | BinaryOperator (_, expr1, expr2) ->
        let attribute1 = attribute_of_expr attribute_map expr1 in
        let attribute2 = attribute_of_expr attribute_map expr2 in
        Attribute.join attribute1 attribute2
    | Closure _ | Sizeof _ ->
        Attribute.top


  let propagate_assignment lhs_access_expression rhs_exp attribute_map =
    let rhs_attribute = attribute_of_expr attribute_map rhs_exp in
    add lhs_access_expression rhs_attribute attribute_map
end

type t =
  { threads: ThreadsDomain.t
  ; locks: LockDomain.t
  ; accesses: AccessDomain.t
  ; ownership: OwnershipDomain.t
  ; attribute_map: AttributeMapDomain.t }

let bottom =
  let threads = ThreadsDomain.bottom in
  let locks = LockDomain.bottom in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AttributeMapDomain.empty in
  {threads; locks; accesses; ownership; attribute_map}


let is_bottom {threads; locks; accesses; ownership; attribute_map} =
  ThreadsDomain.is_bottom threads && LockDomain.is_bottom locks && AccessDomain.is_empty accesses
  && OwnershipDomain.is_empty ownership
  && AttributeMapDomain.is_empty attribute_map


let leq ~lhs ~rhs =
  if phys_equal lhs rhs then true
  else
    ThreadsDomain.leq ~lhs:lhs.threads ~rhs:rhs.threads
    && LockDomain.leq ~lhs:lhs.locks ~rhs:rhs.locks
    && AccessDomain.leq ~lhs:lhs.accesses ~rhs:rhs.accesses
    && AttributeMapDomain.leq ~lhs:lhs.attribute_map ~rhs:rhs.attribute_map


let join astate1 astate2 =
  if phys_equal astate1 astate2 then astate1
  else
    let threads = ThreadsDomain.join astate1.threads astate2.threads in
    let locks = LockDomain.join astate1.locks astate2.locks in
    let accesses = AccessDomain.join astate1.accesses astate2.accesses in
    let ownership = OwnershipDomain.join astate1.ownership astate2.ownership in
    let attribute_map = AttributeMapDomain.join astate1.attribute_map astate2.attribute_map in
    {threads; locks; accesses; ownership; attribute_map}


let widen ~prev ~next ~num_iters =
  if phys_equal prev next then prev
  else
    let threads = ThreadsDomain.widen ~prev:prev.threads ~next:next.threads ~num_iters in
    let locks = LockDomain.widen ~prev:prev.locks ~next:next.locks ~num_iters in
    let accesses = AccessDomain.widen ~prev:prev.accesses ~next:next.accesses ~num_iters in
    let ownership = OwnershipDomain.widen ~prev:prev.ownership ~next:next.ownership ~num_iters in
    let attribute_map =
      AttributeMapDomain.widen ~prev:prev.attribute_map ~next:next.attribute_map ~num_iters
    in
    {threads; locks; accesses; ownership; attribute_map}


type summary =
  { threads: ThreadsDomain.t
  ; locks: LockDomain.t
  ; accesses: AccessDomain.t
  ; return_ownership: OwnershipAbstractValue.t
  ; return_attribute: Attribute.t }

let empty_summary =
  { threads= ThreadsDomain.bottom
  ; locks= LockDomain.bottom
  ; accesses= AccessDomain.bottom
  ; return_ownership= OwnershipAbstractValue.unowned
  ; return_attribute= Attribute.top }


let pp_summary fmt {threads; locks; accesses; return_ownership; return_attribute} =
  F.fprintf fmt
    "@\nThreads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nReturn Attributes: %a @\n"
    ThreadsDomain.pp threads LockDomain.pp locks AccessDomain.pp accesses OwnershipAbstractValue.pp
    return_ownership Attribute.pp return_attribute


let pp fmt {threads; locks; accesses; ownership; attribute_map} =
  F.fprintf fmt "Threads: %a, Locks: %a @\nAccesses %a @\nOwnership: %a @\nAttributes: %a @\n"
    ThreadsDomain.pp threads LockDomain.pp locks AccessDomain.pp accesses OwnershipDomain.pp
    ownership AttributeMapDomain.pp attribute_map


let add_unannotated_call_access formals pname loc (astate : t) =
  let snapshot =
    AccessSnapshot.make_unannotated_call_access formals pname astate.locks astate.threads Unowned
      loc
  in
  {astate with accesses= AccessDomain.add_opt snapshot astate.accesses}
