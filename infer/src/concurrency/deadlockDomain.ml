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

module StackDomain (Element : PrettyPrintable.PrintableOrderedType) = struct
  type astate = Element.t list [@@deriving compare]

  let push = List.cons

  let pop = List.tl_exn

  let is_empty = List.is_empty

  let empty = []

  let pp fmt x = Pp.semicolon_seq Element.pp fmt x

  let ( <= ) ~lhs ~rhs =
    let rec aux lhs rhs =
      match (lhs, rhs) with
      | [], _ ->
          true
      | _, [] ->
          false
      | x :: xs, y :: ys ->
          Int.equal 0 (Element.compare x y) && aux xs ys
    in
    aux (List.rev lhs) (List.rev rhs)


  let join lhs rhs =
    let rec aux acc a b =
      match (a, b) with
      | [], _ | _, [] ->
          acc
      | x :: xs, y :: ys ->
          if not (Int.equal 0 (Element.compare x y)) then [] else aux (x :: acc) xs ys
    in
    aux [] (List.rev lhs) (List.rev rhs)


  let widen ~prev ~next ~num_iters:_ = join prev next
end

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

  let pp fmt (((_, typ), _) as lock) =
    Format.fprintf fmt "locks %a in class %a" AccessPath.pp lock (Typ.pp Pp.text) typ
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

  let pp_trace fmt = function
    | [] ->
        ()
    | trace ->
        Format.fprintf fmt " (trace: %a)" (Pp.semicolon_seq CallSite.pp) trace


  let pp fmt e =
    Format.fprintf fmt "%a at %a%a" LockIdentity.pp e.lock Location.pp e.loc pp_trace e.trace


  let owner_class e =
    let (_, typ), _ = e.lock in
    Typ.inner_name typ


  let make lock loc = {lock; loc; trace= []}

  let make_loc_trace e =
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
    List.rev (endpoint :: call_trace)
end

module LockOrder = struct
  type t = {before: LockEvent.t option; after: LockEvent.t} [@@deriving compare]

  let pp fmt o =
    match o.before with
    | None ->
        Format.fprintf fmt "Eventually %a" LockEvent.pp o.after
    | Some lock ->
        Format.fprintf fmt "First %a and before releasing it %a" LockEvent.pp lock LockEvent.pp
          o.after


  let get_pair elem = match elem.before with None -> None | Some b -> Some (b, elem.after)

  let may_deadlock elem elem' =
    match (elem.before, elem'.before) with
    | Some b, Some b' ->
        LockEvent.locks_equal b elem'.after && LockEvent.locks_equal b' elem.after
    | _, _ ->
        false


  let make_eventually_locks after = {before= None; after}

  let make_holds_and_locks b after = {before= Some b; after}

  let with_callsite callsite o =
    {o with after= {o.after with LockEvent.trace= callsite :: o.after.LockEvent.trace}}


  let make_loc_trace o =
    let before_trace = Option.value_map o.before ~default:[] ~f:LockEvent.make_loc_trace in
    let after_trace = LockEvent.make_loc_trace o.after in
    List.append before_trace after_trace
end

module LockOrderDomain = struct
  include AbstractDomain.FiniteSet (LockOrder)

  let with_callsite callsite lo =
    fold (fun o acc -> add (LockOrder.with_callsite callsite o) acc) lo empty


  let is_eventually_locked lock lo =
    exists (fun pair -> LockEvent.locks_equal pair.LockOrder.after lock) lo
end

module LockStack = StackDomain (LockEvent)

module LockState = struct
  include AbstractDomain.Pair (LockStack) (LockOrderDomain)

  let empty = (LockStack.empty, LockOrderDomain.empty)

  let is_empty (ls, lo) = LockStack.is_empty ls && LockOrderDomain.is_empty lo

  (* for every lock b held locally, add a pair (b, lock_event), plus (None, lock_event) *)
  let add_order_pairs ls lock_event acc =
    (* add no pairs whatsoever if we already hold that lock *)
    if List.exists ls ~f:(LockEvent.locks_equal lock_event) then acc
    else
      let add_eventually_locks acc =
        (* don't add an eventually-locks pair if there is already another with same endpoint*)
        if LockOrderDomain.is_eventually_locked lock_event acc then acc
        else
          let elem = LockOrder.make_eventually_locks lock_event in
          LockOrderDomain.add elem acc
      in
      let add_holds_and_locks acc before =
        (* never add a pair of the form (a,a) -- should never happen due to the check above *)
        let elem = LockOrder.make_holds_and_locks before lock_event in
        LockOrderDomain.add elem acc
      in
      List.fold ls ~init:acc ~f:add_holds_and_locks |> add_eventually_locks


  let lock actuals ((ls, lo) as astate) loc =
    match actuals with
    | (HilExp.AccessExpression exp) :: _ ->
        let newlock_event = LockEvent.make (AccessExpression.to_access_path exp) loc in
        let lo' =
          (* do not add any order pairs if we already hold the lock *)
          if List.exists ls ~f:(LockEvent.locks_equal newlock_event) then lo
          else add_order_pairs ls newlock_event lo
        in
        let ls' = LockStack.push newlock_event ls in
        (ls', lo')
    | _ ->
        astate


  let unlock _ (ls, lo) = ((if LockStack.is_empty ls then ls else LockStack.pop ls), lo)

  let integrate_summary ~caller_state:(ls, lo) ~callee_summary callee_pname loc =
    (* for each pair (b,a) in the callee, add (l,b) and (l,a) to the current state, where
       l is held locally *)
    let do_elem elem acc =
      Option.value_map elem.LockOrder.before ~default:acc ~f:(fun b -> add_order_pairs ls b acc)
      |> add_order_pairs ls elem.LockOrder.after
    in
    let callsite = CallSite.make callee_pname loc in
    (* add callsite to the "after" trace *)
    let elems = LockOrderDomain.with_callsite callsite callee_summary in
    let lo' = LockOrderDomain.fold do_elem elems lo in
    (ls, lo')


  let to_summary astate = snd astate
end

include LockState

type summary = LockOrderDomain.astate

let pp_summary = LockOrderDomain.pp
