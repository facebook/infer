(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module MF = MarkupFormatter

let pname_pp = MF.wrap_monospaced Typ.Procname.pp

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

  type event_t =
    | LockAcquire of Lock.t
    | MayBlock of (string * severity_t)
    | StrictModeCall of string
  [@@deriving compare]

  include ExplicitTrace.MakeTraceElem (struct
    type t = event_t [@@deriving compare]

    let pp fmt = function
      | LockAcquire lock ->
          Lock.pp fmt lock
      | MayBlock (msg, _) ->
          F.pp_print_string fmt msg
      | StrictModeCall msg ->
          F.pp_print_string fmt msg
  end)

  let make_acquire lock loc = make (LockAcquire lock) loc

  let make_call_descr callee = F.asprintf "calls %a" pname_pp callee

  let make_blocking_call callee sev loc =
    let descr = make_call_descr callee in
    make (MayBlock (descr, sev)) loc


  let make_strict_mode_call callee loc =
    let descr = make_call_descr callee in
    make (StrictModeCall descr) loc


  let make_trace ?(header = "") pname elem =
    let trace = make_loc_trace elem in
    let trace_descr = Format.asprintf "%s%a" header (MF.wrap_monospaced Typ.Procname.pp) pname in
    let start_loc = get_loc elem in
    let header_step = Errlog.make_trace_element 0 start_loc trace_descr [] in
    header_step :: trace
end

module EventDomain = Event.FiniteSet

module Order = struct
  type order_t = {first: Lock.t; eventually: Event.t} [@@deriving compare]

  module E = struct
    type t = order_t

    let compare = compare_order_t

    let pp fmt {first} = Lock.pp fmt first
  end

  include ExplicitTrace.MakeTraceElem (E)

  let may_deadlock {elem= {first; eventually}} {elem= {first= first'; eventually= eventually'}} =
    match (eventually.elem, eventually'.elem) with
    | LockAcquire e, LockAcquire e' ->
        Lock.equal_modulo_base first e' && Lock.equal_modulo_base first' e
    | _, _ ->
        false


  let make_loc_trace ?(nesting = 0) ({elem= {eventually}} as order) =
    let first_trace = make_loc_trace ~nesting order in
    let first_nesting = List.length first_trace in
    let eventually_trace = Event.make_loc_trace ~nesting:first_nesting eventually in
    first_trace @ eventually_trace


  let make_trace ?(header = "") pname elem =
    let trace = make_loc_trace elem in
    let trace_descr = Format.asprintf "%s%a" header (MF.wrap_monospaced Typ.Procname.pp) pname in
    let start_loc = get_loc elem in
    let header_step = Errlog.make_trace_element 0 start_loc trace_descr [] in
    header_step :: trace
end

module OrderDomain = Order.FiniteSet
module LockStack = AbstractDomain.StackDomain (Event)

module LockState = struct
  include AbstractDomain.InvertedMap (Lock) (LockStack)

  let is_taken lock_event map =
    match lock_event.Event.elem with
    | Event.LockAcquire lock -> (
      try not (find lock map |> LockStack.is_top) with Caml.Not_found -> false )
    | _ ->
        false


  let acquire lock_id lock_event map =
    let current_value = try find lock_id map with Caml.Not_found -> LockStack.top in
    let new_value = LockStack.push lock_event current_value in
    add lock_id new_value map


  let release lock_id map =
    let current_value = try find lock_id map with Caml.Not_found -> LockStack.top in
    if LockStack.is_top current_value then map
    else
      let new_value = LockStack.pop current_value in
      if LockStack.is_top new_value then remove lock_id map else add lock_id new_value map


  let fold_over_events f map init =
    let ff _ lock_state acc = List.fold lock_state ~init:acc ~f in
    fold ff map init
end

module UIThreadExplanationDomain = struct
  include ExplicitTrace.MakeTraceElem (struct
    type t = string [@@deriving compare]

    let pp = String.pp
  end)

  let join lhs rhs = if List.length lhs.trace <= List.length rhs.trace then lhs else rhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let ( <= ) ~lhs:_ ~rhs:_ = true

  let make_trace ?(header = "") pname elem =
    let trace = make_loc_trace elem in
    let trace_descr = Format.asprintf "%s%a" header (MF.wrap_monospaced Typ.Procname.pp) pname in
    let start_loc = get_loc elem in
    let header_step = Errlog.make_trace_element 0 start_loc trace_descr [] in
    header_step :: trace
end

module UIThreadDomain = struct
  include AbstractDomain.BottomLifted (UIThreadExplanationDomain)

  let with_callsite astate callsite =
    match astate with
    | AbstractDomain.Types.Bottom ->
        astate
    | AbstractDomain.Types.NonBottom ui_explain ->
        AbstractDomain.Types.NonBottom
          (UIThreadExplanationDomain.with_callsite ui_explain callsite)
end

type t =
  {lock_state: LockState.t; events: EventDomain.t; order: OrderDomain.t; ui: UIThreadDomain.t}

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


(* for every lock b held locally, add a pair (b, event) *)
let add_order_pairs lock_state event acc =
  (* add no pairs whatsoever if we already hold that lock *)
  if LockState.is_taken event lock_state then acc
  else
    let add_first_and_eventually acc f =
      match f.Event.elem with
      | LockAcquire first ->
          let elem = Order.make {first; eventually= event} f.Event.loc in
          OrderDomain.add elem acc
      | _ ->
          acc
    in
    LockState.fold_over_events add_first_and_eventually lock_state acc


let acquire ({lock_state; events; order} as astate) loc locks =
  let new_events = List.map locks ~f:(fun lock -> Event.make_acquire lock loc) in
  { astate with
    events= List.fold new_events ~init:events ~f:(fun acc e -> EventDomain.add e acc)
  ; order=
      List.fold new_events ~init:order ~f:(fun acc e ->
          OrderDomain.union acc (add_order_pairs lock_state e order) )
  ; lock_state=
      List.fold2_exn locks new_events ~init:lock_state ~f:(fun acc lock e ->
          LockState.acquire lock e acc ) }


let blocking_call callee sev loc ({lock_state; events; order} as astate) =
  let new_event = Event.make_blocking_call callee sev loc in
  { astate with
    events= EventDomain.add new_event events; order= add_order_pairs lock_state new_event order }


let strict_mode_call callee loc ({lock_state; events; order} as astate) =
  let new_event = Event.make_strict_mode_call callee loc in
  { astate with
    events= EventDomain.add new_event events; order= add_order_pairs lock_state new_event order }


let release ({lock_state} as astate) locks =
  { astate with
    lock_state= List.fold locks ~init:lock_state ~f:(fun acc l -> LockState.release l acc) }


let integrate_summary ({lock_state; events; order; ui} as astate) callee_pname loc callee_summary =
  let callsite = CallSite.make callee_pname loc in
  let callee_order = OrderDomain.with_callsite callee_summary.order callsite in
  let callee_ui = UIThreadDomain.with_callsite callee_summary.ui callsite in
  let filtered_order =
    OrderDomain.filter
      (fun {elem= {eventually}} -> LockState.is_taken eventually lock_state |> not)
      callee_order
  in
  let callee_events = EventDomain.with_callsite callee_summary.events callsite in
  let filtered_events =
    EventDomain.filter (fun e -> LockState.is_taken e lock_state |> not) callee_events
  in
  let order' = EventDomain.fold (add_order_pairs lock_state) filtered_events filtered_order in
  { astate with
    events= EventDomain.join events filtered_events
  ; order= OrderDomain.join order order'
  ; ui= UIThreadDomain.join ui callee_ui }


let set_on_ui_thread ({ui} as astate) loc explain =
  let ui =
    UIThreadDomain.join ui
      (AbstractDomain.Types.NonBottom (UIThreadExplanationDomain.make explain loc))
  in
  {astate with ui}


type summary = t

let pp_summary = pp
