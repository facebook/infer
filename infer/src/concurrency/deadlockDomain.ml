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

  let equal_modulo_base ((_, typ), aclist) ((_, typ'), aclist') =
    Typ.equal typ typ' && AccessPath.equal_access_list aclist aclist'


  let pp fmt (((_, typ), _) as lock) =
    Format.fprintf fmt "locks %a in class %a" AccessPath.pp lock (Typ.pp_full Pp.text) typ
end

module LockEvent = struct
  type t = {lock: LockIdentity.t; loc: Location.t; trace: CallSite.t list}

  (* ignore trace when comparing *)
  let compare e e' =
    if phys_equal e e' then 0
    else
      let res = LockIdentity.compare e.lock e'.lock in
      if not (Int.equal res 0) then res else Location.compare e.loc e'.loc


  let locks_equal e e' = LockIdentity.equal e.lock e'.lock

  let pp fmt e =
    let pp_trace fmt = function
      | [] ->
          ()
      | trace ->
          Format.fprintf fmt " (trace: %a)" (Pp.semicolon_seq CallSite.pp) trace
    in
    Format.fprintf fmt "%a at %a%a" LockIdentity.pp e.lock Location.pp e.loc pp_trace e.trace


  let owner_class e =
    let (_, typ), _ = e.lock in
    Typ.inner_name typ


  let make lock loc = {lock; loc; trace= []}

  let make_loc_trace ?(reverse= false) e =
    let call_trace, nesting =
      List.fold e.trace ~init:([], 0) ~f:(fun (tr, ns) callsite ->
          let elem_descr =
            Format.asprintf "Method call: %a" Typ.Procname.pp (CallSite.pname callsite)
          in
          let elem = Errlog.make_trace_element ns (CallSite.loc callsite) elem_descr [] in
          (elem :: tr, ns + 1) )
    in
    let endpoint_descr = Format.asprintf "Lock acquisition: %a" LockIdentity.pp e.lock in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    let res = endpoint :: call_trace in
    if reverse then res else List.rev res
end

module LockOrder = struct
  type t = {first: LockEvent.t option; eventually: LockEvent.t} [@@deriving compare]

  let pp fmt o =
    match o.first with
    | None ->
        Format.fprintf fmt "Eventually %a" LockEvent.pp o.eventually
    | Some lock ->
        Format.fprintf fmt "First %a and before releasing it %a" LockEvent.pp lock LockEvent.pp
          o.eventually


  let get_pair elem = match elem.first with None -> None | Some b -> Some (b, elem.eventually)

  let may_deadlock elem elem' =
    let locks_equal_modulo_base e e' =
      LockIdentity.equal_modulo_base e.LockEvent.lock e'.LockEvent.lock
    in
    match (elem.first, elem'.first) with
    | Some b, Some b' ->
        locks_equal_modulo_base b elem'.eventually && locks_equal_modulo_base b' elem.eventually
    | _, _ ->
        false


  let make_eventually_locks eventually = {first= None; eventually}

  let make_holds_and_locks b eventually = {first= Some b; eventually}

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
    exists (fun pair -> LockEvent.locks_equal pair.LockOrder.eventually lock) lo
end

module LockStack = AbstractDomain.StackDomain (LockEvent)

module LockState = struct
  include AbstractDomain.InvertedMap (LockIdentity) (LockStack)

  let is_taken lock map = try not (find lock map |> LockStack.is_empty) with Not_found -> false

  let acquire lock_event map =
    let lock_id = lock_event.LockEvent.lock in
    let current_value = try find lock_id map with Not_found -> LockStack.empty in
    let new_value = LockStack.push lock_event current_value in
    add lock_id new_value map


  let release lock_id map =
    let current_value = try find lock_id map with Not_found -> LockStack.empty in
    if LockStack.is_empty current_value then map
    else
      let new_value = LockStack.pop current_value in
      if LockStack.is_empty new_value then remove lock_id map else add lock_id new_value map


  let fold_over_events f map init =
    let ff _ lock_state acc = List.fold lock_state ~init:acc ~f in
    fold ff map init
end

include AbstractDomain.Pair (LockState) (LockOrderDomain)

let empty = (LockState.empty, LockOrderDomain.empty)

let is_empty (ls, lo) = LockState.is_empty ls && LockOrderDomain.is_empty lo

(* for every lock b held locally, add a pair (b, lock_event), plus (None, lock_event) *)
let add_order_pairs ls lock_event acc =
  (* add no pairs whatsoever if we already hold that lock *)
  if LockState.is_taken lock_event.LockEvent.lock ls then acc
  else
    let add_eventually_locks acc =
      (* don't add an eventually-locks pair if there is already another with same endpoint*)
      if LockOrderDomain.is_eventually_locked lock_event acc then acc
      else
        let elem = LockOrder.make_eventually_locks lock_event in
        LockOrderDomain.add elem acc
    in
    let add_holds_and_locks acc first =
      (* never add a pair of the form (a,a) -- should never happen due to the check above *)
      let elem = LockOrder.make_holds_and_locks first lock_event in
      LockOrderDomain.add elem acc
    in
    LockState.fold_over_events add_holds_and_locks ls acc |> add_eventually_locks


let acquire lockid (ls, lo) loc =
  let newlock_event = LockEvent.make lockid loc in
  let lo' = add_order_pairs ls newlock_event lo in
  let ls' = LockState.acquire newlock_event ls in
  (ls', lo')


let release lockid (ls, lo) = (LockState.release lockid ls, lo)

let integrate_summary ~caller_state:(ls, lo) ~callee_summary callee_pname loc =
  (* for each pair (b,a) in the callee, add (l,b) and (l,a) to the current state, where
     l is held locally *)
  let do_elem elem acc =
    Option.value_map elem.LockOrder.first ~default:acc ~f:(fun b -> add_order_pairs ls b acc)
    |> add_order_pairs ls elem.LockOrder.eventually
  in
  let callsite = CallSite.make callee_pname loc in
  (* add callsite to the "eventually" trace *)
  let elems = LockOrderDomain.with_callsite callsite callee_summary in
  let lo' = LockOrderDomain.fold do_elem elems lo in
  (ls, lo')


let to_summary astate = snd astate

type summary = LockOrderDomain.astate

let pp_summary = LockOrderDomain.pp
