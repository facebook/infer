(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

module Lock = struct
  type t = AccessPath.t

  (* compare type, base variable modulo this and access list *)
  let compare (((base, typ), aclist) as lock) (((base', typ'), aclist') as lock') =
    if phys_equal lock lock' then 0
    else
      let res = Typ.compare typ typ' in
      if not (Int.equal res 0) then res
      else
        let res = Var.compare_modulo_this base base' in
        if not (Int.equal res 0) then res
        else List.compare AccessPath.compare_access aclist aclist'


  let equal lock lock' = Int.equal 0 (compare lock lock')

  let equal_modulo_base (((root, typ), aclist) as l) (((root', typ'), aclist') as l') =
    if phys_equal l l' then true
    else
      match (root, root') with
      | Var.LogicalVar _, Var.LogicalVar _ ->
          (* only class objects are supposed to appear as idents *)
          equal l l'
      | Var.ProgramVar _, Var.ProgramVar _ ->
          Typ.equal typ typ' && AccessPath.equal_access_list aclist aclist'
      | _, _ ->
          false


  let pp fmt (((_, typ), _) as lock) =
    F.fprintf fmt "locks %a in class %a"
      (MF.wrap_monospaced AccessPath.pp)
      lock
      (MF.wrap_monospaced (Typ.pp_full Pp.text))
      typ


  let owner_class ((_, typ), _) = Typ.inner_name typ
end

module Event = struct
  type severity_t = Low | Medium | High [@@deriving compare]

  type event_t = LockAcquire of Lock.t | MayBlock of (string * severity_t) [@@deriving compare]

  let pp_event fmt = function
    | LockAcquire lock ->
        Lock.pp fmt lock
    | MayBlock (msg, _) ->
        F.pp_print_string fmt msg


  type t = {event: event_t; loc: Location.t; trace: CallSite.t list}

  let is_lock_event e = match e.event with LockAcquire _ -> true | _ -> false

  (* ignore trace when comparing *)
  let compare e e' =
    if phys_equal e e' then 0
    else
      let res = compare_event_t e.event e'.event in
      if not (Int.equal res 0) then res else Location.compare e.loc e'.loc


  let locks_equal e e' =
    match (e.event, e'.event) with
    | LockAcquire lock, LockAcquire lock' ->
        Lock.equal lock lock'
    | _, _ ->
        false


  let locks_equal_modulo_base e e' =
    match (e.event, e'.event) with
    | LockAcquire lock, LockAcquire lock' ->
        Lock.equal_modulo_base lock lock'
    | _, _ ->
        false


  let pp fmt e =
    let pp_trace fmt = function
      | [] ->
          ()
      | trace ->
          F.fprintf fmt " (trace: %a)" (Pp.semicolon_seq CallSite.pp) trace
    in
    F.fprintf fmt "%a at %a%a" pp_event e.event Location.pp e.loc pp_trace e.trace


  let make_acquire lock loc = {event= LockAcquire lock; loc; trace= []}

  let make_blocks msg sev loc = {event= MayBlock (msg, sev); loc; trace= []}

  let make_blocking_call ~caller ~callee sev loc =
    let descr =
      F.asprintf "calls %a from %a"
        (MF.wrap_monospaced Typ.Procname.pp)
        callee
        (MF.wrap_monospaced Typ.Procname.pp)
        caller
    in
    make_blocks descr sev loc


  let get_loc {loc; trace} = List.hd trace |> Option.value_map ~default:loc ~f:CallSite.loc

  let make_loc_trace ?(reverse= false) e =
    let call_trace, nesting =
      List.fold e.trace ~init:([], 0) ~f:(fun (tr, ns) callsite ->
          let elem_descr =
            F.asprintf "Method call: %a"
              (MF.wrap_monospaced Typ.Procname.pp)
              (CallSite.pname callsite)
          in
          let elem = Errlog.make_trace_element ns (CallSite.loc callsite) elem_descr [] in
          (elem :: tr, ns + 1) )
    in
    let endpoint_descr = F.asprintf "%a" pp_event e.event in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    let res = endpoint :: call_trace in
    if reverse then res else List.rev res
end

module Order = struct
  type t = {first: Event.t option; eventually: Event.t} [@@deriving compare]

  let pp fmt o =
    match o.first with
    | None ->
        F.fprintf fmt "eventually %a" Event.pp o.eventually
    | Some lock ->
        F.fprintf fmt "first %a, and before releasing it, %a" Event.pp lock Event.pp o.eventually


  let may_deadlock elem elem' =
    match (elem.first, elem'.first) with
    | Some b, Some b' ->
        Event.locks_equal_modulo_base b elem'.eventually
        && Event.locks_equal_modulo_base b' elem.eventually
    | _, _ ->
        false


  let make_eventually eventually = {first= None; eventually}

  let make_first_and_eventually b eventually =
    if not (Event.is_lock_event b) then L.(die InternalError) "Expected a lock event first." ;
    {first= Some b; eventually}


  let with_callsite callsite o =
    {o with eventually= {o.eventually with Event.trace= callsite :: o.eventually.Event.trace}}


  let get_loc {first; eventually} =
    match first with Some event -> Event.get_loc event | None -> Event.get_loc eventually


  let make_loc_trace o =
    let first_trace =
      Option.value_map o.first ~default:[] ~f:(Event.make_loc_trace ~reverse:true)
    in
    let eventually_trace = Event.make_loc_trace o.eventually in
    List.rev_append first_trace eventually_trace
end

module OrderDomain = struct
  include AbstractDomain.FiniteSet (Order)

  let with_callsite callsite lo =
    fold (fun o acc -> add (Order.with_callsite callsite o) acc) lo empty


  let is_eventually_locked lock lo =
    Event.is_lock_event lock
    && exists (fun pair -> Event.locks_equal pair.Order.eventually lock) lo
end

module LockStack = AbstractDomain.StackDomain (Event)

module LockState = struct
  include AbstractDomain.InvertedMap (Lock) (LockStack)

  let is_taken lock_event map =
    match lock_event.Event.event with
    | Event.LockAcquire lock -> (
      try not (find lock map |> LockStack.is_empty) with Caml.Not_found -> false )
    | _ ->
        false


  let acquire lock_id lock_event map =
    let current_value = try find lock_id map with Caml.Not_found -> LockStack.empty in
    let new_value = LockStack.push lock_event current_value in
    add lock_id new_value map


  let release lock_id map =
    let current_value = try find lock_id map with Caml.Not_found -> LockStack.empty in
    if LockStack.is_empty current_value then map
    else
      let new_value = LockStack.pop current_value in
      if LockStack.is_empty new_value then remove lock_id map else add lock_id new_value map


  let fold_over_events f map init =
    let ff _ lock_state acc = List.fold lock_state ~init:acc ~f in
    fold ff map init
end

module UIThreadExplanationDomain = struct
  type astate = string

  let pp = String.pp

  let join lhs _ = lhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let ( <= ) ~lhs:_ ~rhs:_ = true
end

module UIThreadDomain = AbstractDomain.BottomLifted (UIThreadExplanationDomain)

type astate = {lock_state: LockState.astate; order: OrderDomain.astate; ui: UIThreadDomain.astate}

let empty = {lock_state= LockState.empty; order= OrderDomain.empty; ui= UIThreadDomain.empty}

let is_empty {lock_state; order; ui} =
  LockState.is_empty lock_state && OrderDomain.is_empty order && UIThreadDomain.is_empty ui


let pp fmt {lock_state; order; ui} =
  F.fprintf fmt "{lock_state= %a; order= %a; ui= %a}" LockState.pp lock_state OrderDomain.pp order
    UIThreadDomain.pp ui


let join lhs rhs =
  { lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; order= OrderDomain.join lhs.order rhs.order
  ; ui= UIThreadDomain.join lhs.ui rhs.ui }


let widen ~prev ~next ~num_iters:_ = join prev next

let ( <= ) ~lhs ~rhs =
  UIThreadDomain.( <= ) ~lhs:lhs.ui ~rhs:rhs.ui && OrderDomain.( <= ) ~lhs:lhs.order ~rhs:rhs.order
  && LockState.( <= ) ~lhs:lhs.lock_state ~rhs:rhs.lock_state


(* for every lock b held locally, add a pair (b, lock_event), plus (None, lock_event) *)
let add_order_pairs order lock_event acc =
  (* add no pairs whatsoever if we already hold that lock *)
  if LockState.is_taken lock_event order then acc
  else
    let add_eventually acc =
      (* don't add an eventually-locks pair if there is already another with same endpoint*)
      if OrderDomain.is_eventually_locked lock_event acc then acc
      else
        let elem = Order.make_eventually lock_event in
        OrderDomain.add elem acc
    in
    let add_first_and_eventually acc first =
      (* never add a pair of the form (a,a) -- should never happen due to the check above *)
      let elem = Order.make_first_and_eventually first lock_event in
      OrderDomain.add elem acc
    in
    LockState.fold_over_events add_first_and_eventually order acc |> add_eventually


let acquire ({lock_state; order} as astate) loc lockid =
  let newlock_event = Event.make_acquire lockid loc in
  { astate with
    lock_state= LockState.acquire lockid newlock_event lock_state
  ; order= add_order_pairs lock_state newlock_event order }


let blocking_call ~caller ~callee sev loc ({lock_state; order} as astate) =
  let newlock_event = Event.make_blocking_call ~caller ~callee sev loc in
  {astate with order= add_order_pairs lock_state newlock_event order}


let release ({lock_state} as astate) lockid =
  {astate with lock_state= LockState.release lockid lock_state}


let integrate_summary ({lock_state; order; ui} as astate) callee_pname loc callee_summary =
  let callee_order, callee_ui = callee_summary in
  (* for each pair (b,a) in the callee, add (l,b) and (l,a) to the current state, where
     l is held locally *)
  let do_elem elem acc =
    Option.value_map elem.Order.first ~default:acc ~f:(fun b -> add_order_pairs lock_state b acc)
    |> add_order_pairs lock_state elem.Order.eventually
  in
  let callsite = CallSite.make callee_pname loc in
  (* add callsite to the "eventually" trace *)
  let elems = OrderDomain.with_callsite callsite callee_order in
  {astate with order= OrderDomain.fold do_elem elems order; ui= UIThreadDomain.join ui callee_ui}


let set_on_ui_thread ({ui} as astate) explain =
  {astate with ui= UIThreadDomain.join ui (AbstractDomain.Types.NonBottom explain)}


let to_summary {order; ui} = (order, ui)

type summary = OrderDomain.astate * UIThreadDomain.astate

let pp_summary fmt (order, ui) =
  F.fprintf fmt "Order: %a, UIThread: %a" OrderDomain.pp order UIThreadDomain.pp ui
