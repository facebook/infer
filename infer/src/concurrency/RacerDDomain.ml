(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AccessExpression = HilExp.AccessExpression
module F = Format
module L = Logging
module MF = MarkupFormatter

let rec accexp_of_hilexp hil_exp =
  match (hil_exp : HilExp.t) with
  | AccessExpression access_expr ->
      Some access_expr
  | Cast (_, e) | Exception e ->
      accexp_of_hilexp e
  | _ ->
      None


let accexp_of_first_hilexp actuals = List.hd actuals |> Option.bind ~f:accexp_of_hilexp

let apply_to_first_actual actuals astate ~f =
  accexp_of_first_hilexp actuals |> Option.value_map ~default:astate ~f


let pp_exp fmt exp =
  match !Language.curr_language with
  | Clang ->
      AccessExpression.pp fmt exp
  | Java ->
      AccessPath.pp fmt (AccessExpression.to_access_path exp)
  | CIL ->
      AccessPath.pp fmt (AccessExpression.to_access_path exp)
  | Erlang ->
      L.die InternalError "Erlang not supported"
  | Hack ->
      L.die InternalError "Hack not supported"
  | Python ->
      L.die InternalError "Python not supported"


let rec should_keep_exp formals (exp : AccessExpression.t) =
  match exp with
  | FieldOffset (prefix, fld) ->
      (not (String.is_substring ~substring:"$SwitchMap" (Fieldname.get_field_name fld)))
      && should_keep_exp formals prefix
  | ArrayOffset (prefix, _, _) | AddressOf prefix | Dereference prefix ->
      should_keep_exp formals prefix
  | Base (LogicalVar _, _) ->
      false
  | Base (((ProgramVar pvar as var), _) as base) ->
      Var.appears_in_source_code var
      && (not (Pvar.is_static_local pvar))
      && (Pvar.is_global pvar || FormalMap.is_formal base formals)


module Access = struct
  type t =
    | Read of {exp: AccessExpression.t}
    | Write of {exp: AccessExpression.t}
    | ContainerRead of {exp: AccessExpression.t; pname: Procname.t}
    | ContainerWrite of {exp: AccessExpression.t; pname: Procname.t}
    | InterfaceCall of {exp: AccessExpression.t; pname: Procname.t}
  [@@deriving compare]

  let make_field_access exp ~is_write = if is_write then Write {exp} else Read {exp}

  let make_container_access exp pname ~is_write =
    if is_write then ContainerWrite {exp; pname} else ContainerRead {exp; pname}


  let make_unannotated_call_access exp pname = InterfaceCall {exp; pname}

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
    | Read {exp} | Write {exp} | ContainerWrite {exp} | ContainerRead {exp} | InterfaceCall {exp} ->
        exp


  let should_keep formals access = get_access_exp access |> should_keep_exp formals

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
    | InterfaceCall ({exp} as record) ->
        let exp' = f exp in
        if phys_equal exp exp' then access else InterfaceCall {record with exp= exp'}


  let pp fmt = function
    | Read {exp} ->
        F.fprintf fmt "Read of %a" AccessExpression.pp exp
    | Write {exp} ->
        F.fprintf fmt "Write to %a" AccessExpression.pp exp
    | ContainerRead {exp; pname} ->
        F.fprintf fmt "Read of container %a via %a" AccessExpression.pp exp Procname.pp pname
    | ContainerWrite {exp; pname} ->
        F.fprintf fmt "Write to container %a via %a" AccessExpression.pp exp Procname.pp pname
    | InterfaceCall {exp; pname} ->
        F.fprintf fmt "Call to un-annotated interface method %a.%a" AccessExpression.pp exp
          Procname.pp pname


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
    | InterfaceCall {pname} ->
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
  type t = NoThread | AnyThreadButSelf | AnyThread
  [@@deriving compare, equal, show {with_path= false}]

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
        equal lhs rhs


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

  (* at least one access is known to occur on a background thread *)
  let can_conflict astate1 astate2 =
    match join astate1 astate2 with AnyThread -> true | NoThread | AnyThreadButSelf -> false


  let is_any_but_self = function AnyThreadButSelf -> true | _ -> false

  let is_any = function AnyThread -> true | _ -> false

  let integrate_summary ~caller_astate ~callee_astate =
    (* if we know the callee runs on the main thread, assume the caller does too. otherwise, keep
       the caller's thread context *)
    match callee_astate with AnyThreadButSelf -> callee_astate | _ -> caller_astate


  let update_for_lock_use = function AnyThreadButSelf -> AnyThreadButSelf | _ -> AnyThread
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


  let make_unannotated_call_access formals exp pname lock ownership loc =
    let lock = LockDomain.is_locked lock in
    let access = Access.make_unannotated_call_access exp pname in
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


  let subst_actuals_into_formals callee_formals actuals_array (t : t) =
    let exp = Access.get_access_exp t.elem.access in
    match FormalMap.get_formal_index (AccessExpression.get_base exp) callee_formals with
    | None ->
        (* non-param base variable, leave unchanged *)
        Some t
    | Some formal_index when formal_index >= Array.length actuals_array ->
        (* vararg param which is missing, throw away *)
        None
    | Some formal_index -> (
      match actuals_array.(formal_index) with
      | None ->
          (* no useful argument can be substituted, throw away *)
          None
      | Some actual ->
          AccessExpression.append ~onto:actual exp
          |> Option.map ~f:(fun new_exp ->
                 map t ~f:(fun elem ->
                     {elem with access= Access.map ~f:(fun _ -> new_exp) elem.access} ) ) )


  let update_callee_access threads locks actuals_ownership (snapshot : t) =
    let update_ownership_precondition actual_index (acc : OwnershipAbstractValue.t) =
      if actual_index >= Array.length actuals_ownership then
        (* vararg methods can result into missing actuals so simply ignore *)
        acc
      else OwnershipAbstractValue.join acc actuals_ownership.(actual_index)
    in
    (* update precondition with caller ownership info *)
    let ownership_precondition =
      match snapshot.elem.ownership_precondition with
      | OwnedIf indexes ->
          IntSet.fold update_ownership_precondition indexes OwnershipAbstractValue.owned
      | Unowned ->
          snapshot.elem.ownership_precondition
    in
    let thread =
      ThreadsDomain.integrate_summary ~callee_astate:snapshot.elem.thread ~caller_astate:threads
    in
    let lock = snapshot.elem.lock || LockDomain.is_locked locks in
    map snapshot ~f:(fun elem -> {elem with lock; thread; ownership_precondition})


  let integrate_summary ~caller_formals ~callee_formals callsite threads locks actuals_array
      actuals_ownership snapshot =
    subst_actuals_into_formals callee_formals actuals_array snapshot
    |> Option.map ~f:(update_callee_access threads locks actuals_ownership)
    |> Option.map ~f:(fun snapshot -> with_callsite snapshot callsite)
    |> Option.bind ~f:(filter caller_formals)


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


  let rec ownership_of_expr ownership (expr : HilExp.t) =
    match expr with
    | AccessExpression access_expr ->
        get_owned access_expr ownership
    | Constant _ ->
        OwnershipAbstractValue.owned
    | Exception e (* treat exceptions as transparent wrt ownership *) | Cast (_, e) ->
        ownership_of_expr ownership e
    | _ ->
        OwnershipAbstractValue.unowned


  let propagate_assignment lhs_access_exp rhs_exp ownership =
    if AccessExpression.get_base lhs_access_exp |> fst |> Var.is_global then
      (* do not assign ownership to access expressions rooted at globals *)
      ownership
    else
      let rhs_ownership_value = ownership_of_expr ownership rhs_exp in
      add lhs_access_exp rhs_ownership_value ownership


  let propagate_return ret_access_exp return_ownership actuals ownership =
    let get_ownership formal_index init =
      List.nth actuals formal_index
      (* simply skip formal if we cannot find its actual, as opposed to assuming non-ownership *)
      |> Option.fold ~init ~f:(fun acc expr ->
             OwnershipAbstractValue.join acc (ownership_of_expr ownership expr) )
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
  type t = Nothing | Functional | OnMainThread | LockHeld | Synchronized [@@deriving equal]

  let pp fmt t =
    ( match t with
    | Nothing ->
        "Nothing"
    | Functional ->
        "Functional"
    | OnMainThread ->
        "OnMainThread"
    | LockHeld ->
        "LockHeld"
    | Synchronized ->
        "Synchronized" )
    |> F.pp_print_string fmt


  let top = Nothing

  let is_top = function Nothing -> true | _ -> false

  let join t t' = if equal t t' then t else Nothing

  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module AttributeMapDomain = struct
  include AbstractDomain.SafeInvertedMap (AccessExpression) (Attribute)

  let get acc_exp t = find_opt acc_exp t |> Option.value ~default:Attribute.top

  let is_functional t access_expression =
    match find_opt access_expression t with Some Functional -> true | _ -> false


  let is_synchronized t access_expression =
    match find_opt access_expression t with Some Synchronized -> true | _ -> false


  let rec attribute_of_expr attribute_map (e : HilExp.t) =
    match e with
    | AccessExpression access_expr ->
        get access_expr attribute_map
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

module NeverReturns = AbstractDomain.BooleanAnd

type t =
  { threads: ThreadsDomain.t
  ; locks: LockDomain.t
  ; never_returns: NeverReturns.t
  ; accesses: AccessDomain.t
  ; ownership: OwnershipDomain.t
  ; attribute_map: AttributeMapDomain.t }
[@@deriving abstract_domain]

let initial =
  let threads = ThreadsDomain.bottom in
  let locks = LockDomain.bottom in
  let never_returns = false in
  let accesses = AccessDomain.empty in
  let ownership = OwnershipDomain.empty in
  let attribute_map = AttributeMapDomain.empty in
  {threads; locks; never_returns; accesses; ownership; attribute_map}


type summary =
  { threads: ThreadsDomain.t
  ; locks: LockDomain.t
  ; never_returns: NeverReturns.t
  ; accesses: AccessDomain.t
  ; return_ownership: OwnershipAbstractValue.t
  ; return_attribute: Attribute.t
  ; attributes: AttributeMapDomain.t }

let empty_summary =
  { threads= ThreadsDomain.bottom
  ; locks= LockDomain.bottom
  ; never_returns= false
  ; accesses= AccessDomain.bottom
  ; return_ownership= OwnershipAbstractValue.unowned
  ; return_attribute= Attribute.top
  ; attributes= AttributeMapDomain.top }


let pp_summary fmt
    {threads; locks; never_returns; accesses; return_ownership; return_attribute; attributes} =
  F.fprintf fmt
    "@\n\
     Threads: %a, Locks: %a, NeverReturns: %a @\n\
     Accesses %a @\n\
     Ownership: %a @\n\
     Return Attribute: %a @\n\
     Attributes: %a @\n"
    ThreadsDomain.pp threads LockDomain.pp locks NeverReturns.pp never_returns AccessDomain.pp
    accesses OwnershipAbstractValue.pp return_ownership Attribute.pp return_attribute
    AttributeMapDomain.pp attributes


let pp fmt {threads; locks; never_returns; accesses; ownership; attribute_map} =
  F.fprintf fmt
    "Threads: %a, Locks: %a, NeverReturns: %a @\nAccesses %a @\nOwnership: %a @\nAttributes: %a @\n"
    ThreadsDomain.pp threads LockDomain.pp locks NeverReturns.pp never_returns AccessDomain.pp
    accesses OwnershipDomain.pp ownership AttributeMapDomain.pp attribute_map


let add_unannotated_call_access formals pname actuals loc (astate : t) =
  apply_to_first_actual actuals astate ~f:(fun receiver ->
      let ownership = OwnershipDomain.get_owned receiver astate.ownership in
      let access_opt =
        AccessSnapshot.make_unannotated_call_access formals receiver pname astate.locks
          astate.threads ownership loc
      in
      {astate with accesses= AccessDomain.add_opt access_opt astate.accesses} )


let astate_to_summary proc_desc formals
    {threads; locks; never_returns; accesses; ownership; attribute_map} =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let return_var_exp =
    AccessExpression.base
      (Var.of_pvar (Pvar.get_ret_pvar proc_name), Procdesc.get_ret_type proc_desc)
  in
  let return_ownership = OwnershipDomain.get_owned return_var_exp ownership in
  let return_attribute = AttributeMapDomain.get return_var_exp attribute_map in
  let locks =
    (* if method is [synchronized] released the lock once. *)
    if Procdesc.is_java_synchronized proc_desc || Procdesc.is_csharp_synchronized proc_desc then
      LockDomain.release_lock locks
    else locks
  in
  let attributes =
    (* store only the [Synchronized] attribute for class initializers/constructors *)
    if Procname.is_java_class_initializer proc_name || Procname.is_constructor proc_name then
      AttributeMapDomain.filter
        (fun exp attribute ->
          match attribute with Synchronized -> should_keep_exp formals exp | _ -> false )
        attribute_map
    else AttributeMapDomain.top
  in
  {threads; locks; never_returns; accesses; return_ownership; return_attribute; attributes}


let add_access tenv formals loc ~is_write (astate : t) exp =
  let rec add_field_accesses prefix_path acc = function
    | [] ->
        acc
    | access :: access_list ->
        let prefix_path' = Option.value_exn (AccessExpression.add_access prefix_path access) in
        if
          (not (MemoryAccess.is_field_or_array_access access))
          || RacerDModels.is_safe_access access prefix_path tenv
        then add_field_accesses prefix_path' acc access_list
        else
          let is_write = is_write && List.is_empty access_list in
          let pre = OwnershipDomain.get_owned prefix_path astate.ownership in
          let snapshot_opt =
            AccessSnapshot.make_access formals prefix_path' ~is_write loc astate.locks
              astate.threads pre
          in
          let access_acc' = AccessDomain.add_opt snapshot_opt acc in
          add_field_accesses prefix_path' access_acc' access_list
  in
  let accesses =
    List.fold (HilExp.get_access_exprs exp) ~init:astate.accesses ~f:(fun acc access_expr ->
        let base, accesses = AccessExpression.to_accesses access_expr in
        add_field_accesses base acc accesses )
  in
  {astate with accesses}


let add_container_access tenv formals ~is_write ret_base callee_pname actuals loc (astate : t) =
  match accexp_of_first_hilexp actuals with
  | None ->
      L.internal_error "Call to %a is marked as a container access, but has no receiver" Procname.pp
        callee_pname ;
      astate
  | Some receiver_expr
    when AttributeMapDomain.is_synchronized astate.attribute_map receiver_expr
         || RacerDModels.is_synchronized_container callee_pname receiver_expr tenv ->
      astate
  | Some receiver_expr ->
      let ownership_pre = OwnershipDomain.get_owned receiver_expr astate.ownership in
      let callee_access_opt =
        AccessSnapshot.make_container_access formals receiver_expr ~is_write callee_pname loc
          astate.locks astate.threads ownership_pre
      in
      let ownership =
        OwnershipDomain.add (AccessExpression.base ret_base) ownership_pre astate.ownership
      in
      let accesses = AccessDomain.add_opt callee_access_opt astate.accesses in
      {astate with accesses; ownership}


let add_reads_of_hilexps tenv formals exps loc astate =
  List.fold exps ~init:astate ~f:(add_access tenv formals loc ~is_write:false)


let add_callee_accesses ~caller_formals ~callee_formals ~callee_accesses callee_pname actuals loc
    (caller_astate : t) =
  let callsite = CallSite.make callee_pname loc in
  (* precompute arrays for actuals and ownership for fast random access *)
  let actuals_array = Array.of_list_map actuals ~f:accexp_of_hilexp in
  let actuals_ownership =
    Array.of_list_map actuals ~f:(OwnershipDomain.ownership_of_expr caller_astate.ownership)
  in
  let process snapshot acc =
    AccessSnapshot.integrate_summary ~caller_formals ~callee_formals callsite caller_astate.threads
      caller_astate.locks actuals_array actuals_ownership snapshot
    |> fun snapshot_opt -> AccessDomain.add_opt snapshot_opt acc
  in
  let accesses = AccessDomain.fold process callee_accesses caller_astate.accesses in
  {caller_astate with accesses}


let branch_never_returns () =
  (* Since this branch never returns but the successor node is the exit node as per [preanal.ml],
     we want to avoid changing the post at the exit node. Here we set all components to the appropriate
     identity element, except from those about not returning. *)
  { threads= ThreadsDomain.bottom
  ; locks= LockDomain.bottom
  ; never_returns= true
  ; accesses= AccessDomain.bottom
  ; ownership= OwnershipDomain.empty
  ; attribute_map=
      (* this is incorrect, as there is no identity element for an inverted set *)
      AttributeMapDomain.empty }


let integrate_summary formals ~callee_proc_attrs summary ret_access_exp callee_pname actuals loc
    astate =
  let {threads; locks; never_returns; return_ownership; return_attribute} = summary in
  if never_returns then branch_never_returns ()
  else
    let callee_formals = FormalMap.make callee_proc_attrs in
    let astate =
      add_callee_accesses ~caller_formals:formals ~callee_formals ~callee_accesses:summary.accesses
        callee_pname actuals loc astate
    in
    let locks = LockDomain.integrate_summary ~caller_astate:astate.locks ~callee_astate:locks in
    (* no treatment of [never_returns] as the callee's is [false] *)
    let ownership =
      OwnershipDomain.propagate_return ret_access_exp return_ownership actuals astate.ownership
    in
    let attribute_map =
      AttributeMapDomain.add ret_access_exp return_attribute astate.attribute_map
    in
    let threads =
      ThreadsDomain.integrate_summary ~caller_astate:astate.threads ~callee_astate:threads
    in
    {astate with locks; never_returns; threads; ownership; attribute_map}


let acquire_lock (astate : t) =
  { astate with
    locks= LockDomain.acquire_lock astate.locks
  ; threads= ThreadsDomain.update_for_lock_use astate.threads }


let release_lock (astate : t) =
  { astate with
    locks= LockDomain.release_lock astate.locks
  ; threads= ThreadsDomain.update_for_lock_use astate.threads }


let lock_if_true ret_access_exp (astate : t) =
  { astate with
    attribute_map= AttributeMapDomain.add ret_access_exp Attribute.LockHeld astate.attribute_map
  ; threads= ThreadsDomain.update_for_lock_use astate.threads }
