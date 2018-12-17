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

let pname_pp = MF.wrap_monospaced Typ.Procname.pp

module Lock = struct
  (* TODO (T37174859): change to [HilExp.t] *)
  type t = AccessPath.t

  type var = Var.t

  let compare_var = Var.compare_modulo_this

  (* compare type, base variable modulo this and access list *)
  let compare lock lock' =
    if phys_equal lock lock' then 0
    else [%compare: (var * Typ.t) * AccessPath.access list] lock lock'


  let equal = [%compare.equal: t]

  let equal_modulo_base (((root, typ), aclist) as l) (((root', typ'), aclist') as l') =
    phys_equal l l'
    ||
    match (root, root') with
    | Var.LogicalVar _, Var.LogicalVar _ ->
        (* only class objects are supposed to appear as idents *)
        equal l l'
    | Var.ProgramVar _, Var.ProgramVar _ ->
        [%compare.equal: Typ.t * AccessPath.access list] (typ, aclist) (typ', aclist')
    | _, _ ->
        false


  let pp = AccessPath.pp

  let owner_class ((_, {Typ.desc}), _) =
    match desc with
    | Typ.Tstruct name | Typ.Tptr ({desc= Tstruct name}, _) ->
        Some name
    | _ ->
        None


  let pp_human fmt lock =
    let pp_owner fmt lock =
      owner_class lock |> Option.iter ~f:(F.fprintf fmt " in %a" (MF.wrap_monospaced Typ.Name.pp))
    in
    F.fprintf fmt "%a%a" (MF.wrap_monospaced pp) lock pp_owner lock


  let pp_locks fmt lock = F.fprintf fmt " locks %a" pp_human lock
end

module Event = struct
  type severity_t = Low | Medium | High [@@deriving compare]

  let pp_severity fmt sev =
    let msg = match sev with Low -> "Low" | Medium -> "Medium" | High -> "High" in
    F.pp_print_string fmt msg


  type event_t =
    | LockAcquire of Lock.t
    | MayBlock of (string * severity_t)
    | StrictModeCall of string
  [@@deriving compare]

  module EventElement = struct
    type t = event_t [@@deriving compare]

    let pp fmt = function
      | LockAcquire lock ->
          F.fprintf fmt "LockAcquire(%a)" Lock.pp lock
      | MayBlock (msg, sev) ->
          F.fprintf fmt "MayBlock(%s, %a)" msg pp_severity sev
      | StrictModeCall msg ->
          F.fprintf fmt "StrictModeCall(%s)" msg


    let pp_human fmt elem =
      match elem with
      | LockAcquire lock ->
          Lock.pp_locks fmt lock
      | MayBlock (msg, _) ->
          F.pp_print_string fmt msg
      | StrictModeCall msg ->
          F.pp_print_string fmt msg
  end

  include ExplicitTrace.MakeTraceElem (EventElement)

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
    let trace_descr = F.asprintf "%s%a" header pname_pp pname in
    let start_loc = get_loc elem in
    let header_step = Errlog.make_trace_element 0 start_loc trace_descr [] in
    header_step :: trace
end

module EventDomain = Event.FiniteSet

module Order = struct
  type order_t = {first: Lock.t; eventually: Event.t} [@@deriving compare]

  module OrderElement = struct
    type t = order_t

    let compare = compare_order_t

    let pp fmt {first; eventually} =
      F.fprintf fmt "{first= %a; eventually= %a}" Lock.pp first Event.pp eventually


    let pp_human fmt {first} = Lock.pp_locks fmt first
  end

  include ExplicitTrace.MakeTraceElem (OrderElement)

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
    let trace_descr = F.asprintf "%s%a" header pname_pp pname in
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


  let acquire map lock_id lock_event =
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
  module StringElement = struct
    include String

    let pp_human = pp
  end

  include ExplicitTrace.MakeTraceElem (StringElement)

  let join lhs rhs = if List.length lhs.trace <= List.length rhs.trace then lhs else rhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let ( <= ) ~lhs:_ ~rhs:_ = true

  let make_trace ?(header = "") pname elem =
    let trace = make_loc_trace elem in
    let trace_descr = Format.asprintf "%s%a" header pname_pp pname in
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

module FlatLock = AbstractDomain.Flat (Lock)

module GuardToLockMap = struct
  include AbstractDomain.InvertedMap (HilExp) (FlatLock)

  let remove_guard astate guard = remove guard astate

  let add_guard astate ~guard ~lock = add guard (FlatLock.v lock) astate
end

type t =
  { events: EventDomain.t
  ; guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; order: OrderDomain.t
  ; ui: UIThreadDomain.t }

let empty =
  { events= EventDomain.empty
  ; guard_map= GuardToLockMap.empty
  ; lock_state= LockState.empty
  ; order= OrderDomain.empty
  ; ui= UIThreadDomain.empty }


let is_empty {events; guard_map; lock_state; order; ui} =
  EventDomain.is_empty events && GuardToLockMap.is_empty guard_map && OrderDomain.is_empty order
  && LockState.is_empty lock_state && UIThreadDomain.is_empty ui


let pp fmt {events; guard_map; lock_state; order; ui} =
  F.fprintf fmt "{events= %a; guard_map= %a; lock_state= %a;  order= %a; ui= %a}" EventDomain.pp
    events GuardToLockMap.pp guard_map LockState.pp lock_state OrderDomain.pp order
    UIThreadDomain.pp ui


let join lhs rhs =
  { events= EventDomain.join lhs.events rhs.events
  ; guard_map= GuardToLockMap.join lhs.guard_map rhs.guard_map
  ; lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; order= OrderDomain.join lhs.order rhs.order
  ; ui= UIThreadDomain.join lhs.ui rhs.ui }


let widen ~prev ~next ~num_iters:_ = join prev next

let ( <= ) ~lhs ~rhs =
  EventDomain.( <= ) ~lhs:lhs.events ~rhs:rhs.events
  && GuardToLockMap.( <= ) ~lhs:lhs.guard_map ~rhs:rhs.guard_map
  && OrderDomain.( <= ) ~lhs:lhs.order ~rhs:rhs.order
  && LockState.( <= ) ~lhs:lhs.lock_state ~rhs:rhs.lock_state
  && UIThreadDomain.( <= ) ~lhs:lhs.ui ~rhs:rhs.ui


let is_recursive_lock event tenv =
  let is_class_and_recursive_lock = function
    | {Typ.desc= Tptr ({desc= Tstruct name}, _)} | {desc= Tstruct name} ->
        ConcurrencyModels.is_recursive_lock_type name
    | typ ->
        L.debug Analysis Verbose "Asked if non-struct type %a is a recursive lock type.@."
          (Typ.pp_full Pp.text) typ ;
        true
  in
  match event with
  | {Event.elem= LockAcquire lock_path} ->
      AccessPath.get_typ lock_path tenv |> Option.exists ~f:is_class_and_recursive_lock
  | _ ->
      false


(** skip adding an order pair [(_, event)] if  
  - we have no tenv, or,
  - [event] is not a lock event, or,
  - we do not hold the lock, or,
  - the lock is not recursive. *)
let should_skip tenv_opt event lock_state =
  Option.exists tenv_opt ~f:(fun tenv ->
      LockState.is_taken event lock_state && is_recursive_lock event tenv )


(* for every lock b held locally, add a pair (b, event) *)
let add_order_pairs tenv_opt lock_state event acc =
  if should_skip tenv_opt event lock_state then acc
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


let acquire tenv ({lock_state; events; order} as astate) loc locks =
  let new_events = List.map locks ~f:(fun lock -> Event.make_acquire lock loc) in
  { astate with
    events= List.fold new_events ~init:events ~f:(fun acc e -> EventDomain.add e acc)
  ; order=
      List.fold new_events ~init:order ~f:(fun acc e ->
          OrderDomain.union acc (add_order_pairs (Some tenv) lock_state e order) )
  ; lock_state= List.fold2_exn locks new_events ~init:lock_state ~f:LockState.acquire }


let make_call_with_event tenv_opt new_event astate =
  { astate with
    events= EventDomain.add new_event astate.events
  ; order= add_order_pairs tenv_opt astate.lock_state new_event astate.order }


let blocking_call callee sev loc astate =
  let new_event = Event.make_blocking_call callee sev loc in
  make_call_with_event None new_event astate


let strict_mode_call callee loc astate =
  let new_event = Event.make_strict_mode_call callee loc in
  make_call_with_event None new_event astate


let release ({lock_state} as astate) locks =
  { astate with
    lock_state= List.fold locks ~init:lock_state ~f:(fun acc l -> LockState.release l acc) }


let integrate_summary tenv ({lock_state; events; order; ui} as astate) callee_pname loc
    callee_summary =
  let callsite = CallSite.make callee_pname loc in
  let callee_order = OrderDomain.with_callsite callee_summary.order callsite in
  let callee_ui = UIThreadDomain.with_callsite callee_summary.ui callsite in
  let should_keep event = should_skip (Some tenv) event lock_state |> not in
  let filtered_order =
    OrderDomain.filter (fun {elem= {eventually}} -> should_keep eventually) callee_order
  in
  let callee_events = EventDomain.with_callsite callee_summary.events callsite in
  let filtered_events = EventDomain.filter should_keep callee_events in
  let order' =
    EventDomain.fold (add_order_pairs (Some tenv) lock_state) filtered_events filtered_order
  in
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


let add_guard tenv astate guard lock ~acquire_now loc =
  let astate = {astate with guard_map= GuardToLockMap.add_guard ~guard ~lock astate.guard_map} in
  if acquire_now then acquire tenv astate loc [lock] else astate


let remove_guard astate guard =
  GuardToLockMap.find_opt guard astate.guard_map
  |> Option.value_map ~default:astate ~f:(fun lock_opt ->
         let locks = FlatLock.get lock_opt |> Option.to_list in
         let astate = release astate locks in
         {astate with guard_map= GuardToLockMap.remove_guard astate.guard_map guard} )


let unlock_guard astate guard =
  GuardToLockMap.find_opt guard astate.guard_map
  |> Option.value_map ~default:astate ~f:(fun lock_opt ->
         FlatLock.get lock_opt |> Option.to_list |> release astate )


let lock_guard tenv astate guard loc =
  GuardToLockMap.find_opt guard astate.guard_map
  |> Option.value_map ~default:astate ~f:(fun lock_opt ->
         FlatLock.get lock_opt |> Option.to_list |> acquire tenv astate loc )


type summary = t

let pp_summary = pp
