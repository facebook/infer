(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

module type TraceElem = sig
  type elem_t

  type t = private {elem: elem_t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val make : elem_t -> Location.t -> t

  val get_loc : t -> Location.t

  val make_loc_trace : ?reverse:bool -> t -> Errlog.loc_trace

  val with_callsite : t -> CallSite.t -> t
end

module MakeTraceElem (Elem : PrettyPrintable.PrintableOrderedType) :
  TraceElem with type elem_t = Elem.t =
struct
  type elem_t = Elem.t

  type t = {elem: Elem.t; loc: Location.t; trace: CallSite.t list [@compare.ignore]}
  [@@deriving compare]

  let pp fmt e =
    let pp_trace fmt = function
      | [] ->
          ()
      | trace ->
          F.fprintf fmt " (trace: %a)" (Pp.semicolon_seq CallSite.pp) trace
    in
    F.fprintf fmt "%a at %a%a" Elem.pp e.elem Location.pp e.loc pp_trace e.trace


  let make elem loc = {elem; loc; trace= []}

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
    let endpoint_descr = F.asprintf "%a" Elem.pp e.elem in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    let res = endpoint :: call_trace in
    if reverse then res else List.rev res


  let with_callsite elem callsite = {elem with trace= callsite :: elem.trace}
end

module Event = struct
  type severity_t = Low | Medium | High [@@deriving compare]

  type event_t = LockAcquire of Lock.t | MayBlock of (string * severity_t) [@@deriving compare]

  let pp_event fmt = function
    | LockAcquire lock ->
        Lock.pp fmt lock
    | MayBlock (msg, _) ->
        F.pp_print_string fmt msg


  include MakeTraceElem (struct
    type t = event_t [@@deriving compare]

    let pp = pp_event
  end)

  let is_lock_event e = match e.elem with LockAcquire _ -> true | _ -> false

  let locks_equal e e' =
    match (e.elem, e'.elem) with
    | LockAcquire lock, LockAcquire lock' ->
        Lock.equal lock lock'
    | _, _ ->
        false


  let locks_equal_modulo_base e e' =
    match (e.elem, e'.elem) with
    | LockAcquire lock, LockAcquire lock' ->
        Lock.equal_modulo_base lock lock'
    | _, _ ->
        false


  let make_acquire lock loc = make (LockAcquire lock) loc

  let make_blocking_call ~caller ~callee sev loc =
    let descr =
      F.asprintf "calls %a from %a"
        (MF.wrap_monospaced Typ.Procname.pp)
        callee
        (MF.wrap_monospaced Typ.Procname.pp)
        caller
    in
    make (MayBlock (descr, sev)) loc
end

module EventDomain = struct
  include AbstractDomain.FiniteSet (Event)

  let with_callsite astate callsite =
    fold (fun e acc -> add (Event.with_callsite e callsite) acc) astate empty
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
    if not (Event.is_lock_event b) then L.(die InternalError) "Expected a lock elem first." ;
    {first= Some b; eventually}


  let with_callsite callsite o = {o with eventually= Event.with_callsite o.eventually callsite}

  let get_loc {first; eventually} =
    match first with Some elem -> Event.get_loc elem | None -> Event.get_loc eventually


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
    match lock_event.Event.elem with
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

type astate =
  { lock_state: LockState.astate
  ; events: EventDomain.astate
  ; order: OrderDomain.astate
  ; ui: UIThreadDomain.astate }

let empty =
  { lock_state= LockState.empty
  ; events= EventDomain.empty
  ; order= OrderDomain.empty
  ; ui= UIThreadDomain.empty }


let is_empty {lock_state; events; order; ui} =
  UIThreadDomain.is_empty ui && EventDomain.is_empty events && OrderDomain.is_empty order
  && LockState.is_empty lock_state


let pp fmt {lock_state; events; order; ui} =
  F.fprintf fmt "{lock_state= %a; events= %a; order= %a; ui= %a}" LockState.pp lock_state
    EventDomain.pp events OrderDomain.pp order UIThreadDomain.pp ui


let join lhs rhs =
  { lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; events= EventDomain.join lhs.events rhs.events
  ; order= OrderDomain.join lhs.order rhs.order
  ; ui= UIThreadDomain.join lhs.ui rhs.ui }


let widen ~prev ~next ~num_iters:_ = join prev next

let ( <= ) ~lhs ~rhs =
  UIThreadDomain.( <= ) ~lhs:lhs.ui ~rhs:rhs.ui
  && EventDomain.( <= ) ~lhs:lhs.events ~rhs:rhs.events
  && OrderDomain.( <= ) ~lhs:lhs.order ~rhs:rhs.order
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


let acquire ({lock_state; events; order} as astate) loc lockid =
  let new_event = Event.make_acquire lockid loc in
  { astate with
    lock_state= LockState.acquire lockid new_event lock_state
  ; events= EventDomain.add new_event events
  ; order= add_order_pairs lock_state new_event order }


let blocking_call ~caller ~callee sev loc ({lock_state; events; order} as astate) =
  let new_event = Event.make_blocking_call ~caller ~callee sev loc in
  { astate with
    events= EventDomain.add new_event events; order= add_order_pairs lock_state new_event order }


let release ({lock_state} as astate) lockid =
  {astate with lock_state= LockState.release lockid lock_state}


let integrate_summary ({lock_state; events; order; ui} as astate) callee_pname loc callee_summary =
  let callee_order = callee_summary.order in
  let callee_ui = callee_summary.ui in
  let callee_events = callee_summary.events in
  (* for each pair (b,a) in the callee, add (l,b) and (l,a) to the current state, where
     l is held locally *)
  let do_elem elem acc =
    Option.value_map elem.Order.first ~default:acc ~f:(fun b -> add_order_pairs lock_state b acc)
    |> add_order_pairs lock_state elem.Order.eventually
  in
  let callsite = CallSite.make callee_pname loc in
  (* add callsite to the "eventually" trace *)
  let elems = OrderDomain.with_callsite callsite callee_order in
  { astate with
    events= EventDomain.join events (EventDomain.with_callsite callee_events callsite)
  ; order= OrderDomain.fold do_elem elems order
  ; ui= UIThreadDomain.join ui callee_ui }


let set_on_ui_thread ({ui} as astate) explain =
  {astate with ui= UIThreadDomain.join ui (AbstractDomain.Types.NonBottom explain)}


type summary = astate

let pp_summary = pp
