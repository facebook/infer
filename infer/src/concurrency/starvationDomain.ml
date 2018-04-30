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

module LockIdentity = struct
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
    F.fprintf fmt "locks `%a` in class `%a`" AccessPath.pp lock (Typ.pp_full Pp.text) typ


  let owner_class ((_, typ), _) = Typ.inner_name typ
end

module LockEvent = struct
  type event_t = LockAcquire of LockIdentity.t | MayBlock of string [@@deriving compare]

  let pp_event fmt = function
    | LockAcquire lock ->
        LockIdentity.pp fmt lock
    | MayBlock msg ->
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
        LockIdentity.equal lock lock'
    | _, _ ->
        false


  let locks_equal_modulo_base e e' =
    match (e.event, e'.event) with
    | LockAcquire lock, LockAcquire lock' ->
        LockIdentity.equal_modulo_base lock lock'
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

  let make_blocks msg loc = {event= MayBlock msg; loc; trace= []}

  let make_blocking_call ~caller ~callee loc =
    let descr = F.asprintf "calls %a from %a" Typ.Procname.pp callee Typ.Procname.pp caller in
    make_blocks descr loc


  let make_loc_trace ?(reverse= false) e =
    let call_trace, nesting =
      List.fold e.trace ~init:([], 0) ~f:(fun (tr, ns) callsite ->
          let elem_descr =
            F.asprintf "Method call: %a" Typ.Procname.pp (CallSite.pname callsite)
          in
          let elem = Errlog.make_trace_element ns (CallSite.loc callsite) elem_descr [] in
          (elem :: tr, ns + 1) )
    in
    let endpoint_descr = F.asprintf "%a" pp_event e.event in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    let res = endpoint :: call_trace in
    if reverse then res else List.rev res
end

module LockOrder = struct
  type t = {first: LockEvent.t option; eventually: LockEvent.t} [@@deriving compare]

  let pp fmt o =
    match o.first with
    | None ->
        F.fprintf fmt "eventually %a" LockEvent.pp o.eventually
    | Some lock ->
        F.fprintf fmt "first %a, and before releasing it, %a" LockEvent.pp lock LockEvent.pp
          o.eventually


  let may_deadlock elem elem' =
    match (elem.first, elem'.first) with
    | Some b, Some b' ->
        LockEvent.locks_equal_modulo_base b elem'.eventually
        && LockEvent.locks_equal_modulo_base b' elem.eventually
    | _, _ ->
        false


  let make_eventually eventually = {first= None; eventually}

  let make_first_and_eventually b eventually =
    if not (LockEvent.is_lock_event b) then L.(die InternalError) "Expected a lock event first." ;
    {first= Some b; eventually}


  let with_callsite callsite o =
    { o with
      eventually= {o.eventually with LockEvent.trace= callsite :: o.eventually.LockEvent.trace} }


  let make_loc_trace o =
    let first_trace =
      Option.value_map o.first ~default:[] ~f:(LockEvent.make_loc_trace ~reverse:true)
    in
    let eventually_trace = LockEvent.make_loc_trace o.eventually in
    List.rev_append first_trace eventually_trace
end

module LockOrderDomain = struct
  include AbstractDomain.FiniteSet (LockOrder)

  let with_callsite callsite lo =
    fold (fun o acc -> add (LockOrder.with_callsite callsite o) acc) lo empty


  let is_eventually_locked lock lo =
    LockEvent.is_lock_event lock
    && exists (fun pair -> LockEvent.locks_equal pair.LockOrder.eventually lock) lo
end

module LockStack = AbstractDomain.StackDomain (LockEvent)

module LockState = struct
  include AbstractDomain.InvertedMap (LockIdentity) (LockStack)

  let is_taken lock_event map =
    match lock_event.LockEvent.event with
    | LockEvent.LockAcquire lock -> (
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

module MainThreadDomain = AbstractDomain.BooleanOr
include AbstractDomain.Pair (AbstractDomain.Pair (LockState) (LockOrderDomain)) (MainThreadDomain)

let empty = ((LockState.empty, LockOrderDomain.empty), false)

let is_empty ((ls, lo), main) =
  LockState.is_empty ls && LockOrderDomain.is_empty lo && MainThreadDomain.is_empty main


(* for every lock b held locally, add a pair (b, lock_event), plus (None, lock_event) *)
let add_order_pairs ls lock_event acc =
  (* add no pairs whatsoever if we already hold that lock *)
  if LockState.is_taken lock_event ls then acc
  else
    let add_eventually acc =
      (* don't add an eventually-locks pair if there is already another with same endpoint*)
      if LockOrderDomain.is_eventually_locked lock_event acc then acc
      else
        let elem = LockOrder.make_eventually lock_event in
        LockOrderDomain.add elem acc
    in
    let add_first_and_eventually acc first =
      (* never add a pair of the form (a,a) -- should never happen due to the check above *)
      let elem = LockOrder.make_first_and_eventually first lock_event in
      LockOrderDomain.add elem acc
    in
    LockState.fold_over_events add_first_and_eventually ls acc |> add_eventually


let acquire ((ls, lo), main) loc lockid =
  let newlock_event = LockEvent.make_acquire lockid loc in
  let lo' = add_order_pairs ls newlock_event lo in
  let ls' = LockState.acquire lockid newlock_event ls in
  ((ls', lo'), main)


let blocking_call ~caller ~callee loc ((ls, lo), main) =
  let newlock_event = LockEvent.make_blocking_call ~caller ~callee loc in
  let lo' = add_order_pairs ls newlock_event lo in
  ((ls, lo'), main)


let release ((ls, lo), main) lockid = ((LockState.release lockid ls, lo), main)

let integrate_summary ((ls, lo), main) callee_pname loc callee_summary =
  let callee_lo, callee_main = callee_summary in
  (* for each pair (b,a) in the callee, add (l,b) and (l,a) to the current state, where
     l is held locally *)
  let do_elem elem acc =
    Option.value_map elem.LockOrder.first ~default:acc ~f:(fun b -> add_order_pairs ls b acc)
    |> add_order_pairs ls elem.LockOrder.eventually
  in
  let callsite = CallSite.make callee_pname loc in
  (* add callsite to the "eventually" trace *)
  let elems = LockOrderDomain.with_callsite callsite callee_lo in
  let lo' = LockOrderDomain.fold do_elem elems lo in
  let main' = MainThreadDomain.join main callee_main in
  ((ls, lo'), main')


let set_on_main_thread (sum, _) = (sum, true)

let to_summary ((_, lo), main) = (lo, main)

type summary = LockOrderDomain.astate * MainThreadDomain.astate

let pp_summary fmt (lo, main) =
  F.fprintf fmt "LockOrder: %a, MainThread: %a" LockOrderDomain.pp lo MainThreadDomain.pp main
