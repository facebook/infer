(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

  let pp = AccessPath.pp

  let owner_class ((_, {Typ.desc}), _) =
    match desc with
    | Typ.Tstruct name | Typ.Tptr ({desc= Tstruct name}, _) ->
        Some name
    | _ ->
        None


  let describe fmt lock =
    let pp_owner fmt lock =
      owner_class lock |> Option.iter ~f:(F.fprintf fmt " in %a" (MF.wrap_monospaced Typ.Name.pp))
    in
    F.fprintf fmt "%a%a" (MF.wrap_monospaced pp) lock pp_owner lock


  let pp_locks fmt lock = F.fprintf fmt " locks %a" describe lock
end

module Event = struct
  type severity_t = Low | Medium | High [@@deriving compare]

  let pp_severity fmt sev =
    let msg = match sev with Low -> "Low" | Medium -> "Medium" | High -> "High" in
    F.pp_print_string fmt msg


  type t = LockAcquire of Lock.t | MayBlock of (string * severity_t) | StrictModeCall of string
  [@@deriving compare]

  let pp fmt = function
    | LockAcquire lock ->
        F.fprintf fmt "LockAcquire(%a)" Lock.pp lock
    | MayBlock (msg, sev) ->
        F.fprintf fmt "MayBlock(%s, %a)" msg pp_severity sev
    | StrictModeCall msg ->
        F.fprintf fmt "StrictModeCall(%s)" msg


  let describe fmt elem =
    match elem with
    | LockAcquire lock ->
        Lock.pp_locks fmt lock
    | MayBlock (msg, _) ->
        F.pp_print_string fmt msg
    | StrictModeCall msg ->
        F.pp_print_string fmt msg


  let make_acquire lock = LockAcquire lock

  let make_call_descr callee = F.asprintf "calls %a" pname_pp callee

  let make_blocking_call callee sev =
    let descr = make_call_descr callee in
    MayBlock (descr, sev)


  let make_strict_mode_call callee =
    let descr = make_call_descr callee in
    StrictModeCall descr
end

(** A lock acquisition with source location and procname in which it occurs.
    The location & procname are *ignored* for comparisons, and are only for reporting. *)
module Acquisition = struct
  type t =
    {lock: Lock.t; loc: Location.t [@compare.ignore]; procname: Typ.Procname.t [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {lock} = Lock.pp_locks fmt lock

  let make ~procname ~loc lock = {lock; loc; procname}

  let compare_loc {loc= loc1} {loc= loc2} = Location.compare loc1 loc2

  let make_trace_step acquisition =
    let description = F.asprintf "%a" pp acquisition in
    Errlog.make_trace_element 0 acquisition.loc description []


  let make_dummy lock = {lock; loc= Location.dummy; procname= Typ.Procname.Linters_dummy_method}
end

(** Set of acquisitions; due to order over acquisitions, each lock appears at most once. *)
module Acquisitions = struct
  include PrettyPrintable.MakePPSet (Acquisition)

  (* use the fact that location/procname are ignored in comparisons *)
  let lock_is_held lock acquisitions = mem (Acquisition.make_dummy lock) acquisitions
end

module LockState : sig
  include AbstractDomain.WithTop

  val acquire : procname:Typ.Procname.t -> loc:Location.t -> Lock.t -> t -> t

  val release : Lock.t -> t -> t

  val is_lock_taken : Event.t -> t -> bool

  val get_acquisitions : t -> Acquisitions.t
end = struct
  module LockStack = AbstractDomain.StackDomain (Acquisition)
  module Map = AbstractDomain.InvertedMap (Lock) (LockStack)

  (* [acquisitions] has the currently held locks, so as to avoid a linear fold in [get_acquisitions]. 
     This should also increase sharing across returned values from [get_acquisitions]. *)
  type t = {map: Map.t; acquisitions: Acquisitions.t}

  let get_acquisitions {acquisitions} = acquisitions

  let pp fmt {map; acquisitions} =
    F.fprintf fmt "{map= %a; acquisitions= %a}" Map.pp map Acquisitions.pp acquisitions


  let join lhs rhs =
    let map = Map.join lhs.map rhs.map in
    let acquisitions = Acquisitions.inter lhs.acquisitions rhs.acquisitions in
    {map; acquisitions}


  let widen ~prev ~next ~num_iters =
    let map = Map.widen ~prev:prev.map ~next:next.map ~num_iters in
    let acquisitions = Acquisitions.inter prev.acquisitions next.acquisitions in
    {map; acquisitions}


  let ( <= ) ~lhs ~rhs = Map.( <= ) ~lhs:lhs.map ~rhs:rhs.map

  let top = {map= Map.top; acquisitions= Acquisitions.empty}

  let is_top {map} = Map.is_top map

  let is_lock_taken event {acquisitions} =
    match event with
    | Event.LockAcquire lock ->
        Acquisitions.mem (Acquisition.make_dummy lock) acquisitions
    | _ ->
        false


  let get_stack lock map = Map.find_opt lock map |> Option.value ~default:LockStack.top

  let acquire ~procname ~loc lock {map; acquisitions} =
    let acquisition = Acquisition.make ~procname ~loc lock in
    let current_value = get_stack lock map in
    let new_value = LockStack.push acquisition current_value in
    let map = Map.add lock new_value map in
    let acquisitions =
      (* add new acquisition only if lock was not held before *)
      if LockStack.is_top current_value then Acquisitions.add acquisition acquisitions
      else acquisitions
    in
    {map; acquisitions}


  let release lock ({map; acquisitions} as astate) =
    let current_value = get_stack lock map in
    if LockStack.is_top current_value then (* lock was not held *) astate
    else
      let new_value = LockStack.pop current_value in
      if LockStack.is_top new_value then
        (* lock is now not held *)
        let map = Map.remove lock map in
        let acquisition = Acquisition.make_dummy lock in
        let acquisitions = Acquisitions.remove acquisition acquisitions in
        {map; acquisitions}
      else
        (* lock is still held as it was acquired more than once *)
        let map = Map.add lock new_value map in
        {map; acquisitions}
end

module CriticalPairElement = struct
  type t = {acquisitions: Acquisitions.t; event: Event.t} [@@deriving compare]

  let pp fmt {acquisitions; event} =
    F.fprintf fmt "{acquisitions= %a; event= %a}" Acquisitions.pp acquisitions Event.pp event


  let describe = pp
end

module CriticalPair = struct
  include ExplicitTrace.MakeTraceElem (CriticalPairElement) (ExplicitTrace.DefaultCallPrinter)

  let make ~loc acquisitions event = make {acquisitions; event} loc

  let is_blocking_call {elem= {event}} = match event with LockAcquire _ -> true | _ -> false

  let get_final_acquire {elem= {event}} =
    match event with LockAcquire lock -> Some lock | _ -> None


  let may_deadlock ({elem= pair1} as t1 : t) ({elem= pair2} as t2 : t) =
    Option.both (get_final_acquire t1) (get_final_acquire t2)
    |> Option.exists ~f:(fun (lock1, lock2) ->
           (not (Lock.equal lock1 lock2))
           && Acquisitions.lock_is_held lock2 pair1.acquisitions
           && Acquisitions.lock_is_held lock1 pair2.acquisitions
           && Acquisitions.inter pair1.acquisitions pair2.acquisitions |> Acquisitions.is_empty )


  let with_callsite t existing_acquisitions call_site =
    let f ({acquisitions} as elem : CriticalPairElement.t) =
      {elem with acquisitions= Acquisitions.union existing_acquisitions acquisitions}
    in
    let new_t = map ~f t in
    with_callsite new_t call_site


  let get_earliest_lock_or_call_loc ~procname ({elem= {acquisitions}} as t) =
    let initial_loc = get_loc t in
    Acquisitions.fold
      (fun {procname= acq_procname; loc= acq_loc} acc ->
        if
          Typ.Procname.equal procname acq_procname
          && Int.is_negative (Location.compare acq_loc acc)
        then acq_loc
        else acc )
      acquisitions initial_loc


  let make_trace ?(header = "") ?(include_acquisitions = true) top_pname
      {elem= {acquisitions; event}; trace; loc} =
    let acquisitions_map =
      if include_acquisitions then
        Acquisitions.fold
          (fun ({procname} as acq : Acquisition.t) acc ->
            Typ.Procname.Map.update procname
              (function None -> Some [acq] | Some acqs -> Some (acq :: acqs))
              acc )
          acquisitions Typ.Procname.Map.empty
      else Typ.Procname.Map.empty
    in
    let header_step =
      let description = F.asprintf "%s%a" header pname_pp top_pname in
      let loc = Location.dummy in
      Errlog.make_trace_element 0 loc description []
    in
    (* construct the trace segment starting at [call_site] and ending at next call *)
    let make_call_stack_step fake_first_call call_site =
      let procname = CallSite.pname call_site in
      let trace =
        Typ.Procname.Map.find_opt procname acquisitions_map
        |> Option.value ~default:[]
        (* many acquisitions can be on same line (eg, std::lock) so use stable sort 
           to produce a deterministic trace *)
        |> List.stable_sort ~compare:Acquisition.compare_loc
        |> List.map ~f:Acquisition.make_trace_step
      in
      if CallSite.equal call_site fake_first_call then trace
      else
        let descr = F.asprintf "%a" ExplicitTrace.DefaultCallPrinter.pp call_site in
        let call_step = Errlog.make_trace_element 0 (CallSite.loc call_site) descr [] in
        call_step :: trace
    in
    (* construct a call stack trace with the lock acquisitions interleaved *)
    let call_stack =
      (* fake outermost call so as to include acquisitions in the top level caller *)
      let fake_first_call = CallSite.make top_pname Location.dummy in
      List.map (fake_first_call :: trace) ~f:(make_call_stack_step fake_first_call)
    in
    let endpoint_step =
      let endpoint_descr = F.asprintf "%a" Event.describe event in
      Errlog.make_trace_element 0 loc endpoint_descr []
    in
    List.concat (([header_step] :: call_stack) @ [[endpoint_step]])
end

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
  | Event.LockAcquire lock_path ->
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
      LockState.is_lock_taken event lock_state && is_recursive_lock event tenv )


module CriticalPairs = struct
  include CriticalPair.FiniteSet

  let with_callsite astate tenv_opt lock_state call_site =
    let existing_acquisitions = LockState.get_acquisitions lock_state in
    fold
      (fun ({elem= {event}} as critical_pair : CriticalPair.t) acc ->
        if should_skip tenv_opt event lock_state then acc
        else
          let new_pair =
            CriticalPair.with_callsite critical_pair existing_acquisitions call_site
          in
          add new_pair acc )
      astate empty
end

module UIThreadExplanationDomain = struct
  module StringElement = struct
    include String

    let describe = pp
  end

  include ExplicitTrace.MakeTraceElem (StringElement) (ExplicitTrace.DefaultCallPrinter)

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
  { guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: CriticalPairs.t
  ; ui: UIThreadDomain.t }

let bottom =
  { guard_map= GuardToLockMap.empty
  ; lock_state= LockState.top
  ; critical_pairs= CriticalPairs.empty
  ; ui= UIThreadDomain.bottom }


let is_bottom {guard_map; lock_state; critical_pairs; ui} =
  GuardToLockMap.is_empty guard_map && LockState.is_top lock_state
  && CriticalPairs.is_empty critical_pairs
  && UIThreadDomain.is_bottom ui


let pp fmt {guard_map; lock_state; critical_pairs; ui} =
  F.fprintf fmt "{guard_map= %a; lock_state= %a; critical_pairs= %a; ui= %a}" GuardToLockMap.pp
    guard_map LockState.pp lock_state CriticalPairs.pp critical_pairs UIThreadDomain.pp ui


let join lhs rhs =
  { guard_map= GuardToLockMap.join lhs.guard_map rhs.guard_map
  ; lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; critical_pairs= CriticalPairs.join lhs.critical_pairs rhs.critical_pairs
  ; ui= UIThreadDomain.join lhs.ui rhs.ui }


let widen ~prev ~next ~num_iters:_ = join prev next

let ( <= ) ~lhs ~rhs =
  GuardToLockMap.( <= ) ~lhs:lhs.guard_map ~rhs:rhs.guard_map
  && LockState.( <= ) ~lhs:lhs.lock_state ~rhs:rhs.lock_state
  && CriticalPairs.( <= ) ~lhs:lhs.critical_pairs ~rhs:rhs.critical_pairs
  && UIThreadDomain.( <= ) ~lhs:lhs.ui ~rhs:rhs.ui


(* for every lock b held locally, add a pair (b, event) *)
let add_critical_pair tenv_opt lock_state event ~loc acc =
  if should_skip tenv_opt event lock_state then acc
  else
    (* FIXME we should not do this repeatedly in the fold below *)
    let acquisitions = LockState.get_acquisitions lock_state in
    let critical_pair = CriticalPair.make ~loc acquisitions event in
    CriticalPairs.add critical_pair acc


let acquire tenv ({lock_state; critical_pairs} as astate) ~procname ~loc locks =
  { astate with
    (* FIXME do one fold not two *)
    critical_pairs=
      List.fold locks ~init:critical_pairs ~f:(fun acc lock ->
          let event = Event.make_acquire lock in
          add_critical_pair (Some tenv) lock_state event ~loc acc )
  ; lock_state=
      List.fold locks ~init:lock_state ~f:(fun acc lock ->
          LockState.acquire ~procname ~loc lock acc ) }


let make_call_with_event tenv_opt new_event ~loc astate =
  { astate with
    critical_pairs=
      add_critical_pair tenv_opt astate.lock_state new_event ~loc astate.critical_pairs }


let blocking_call ~callee sev ~loc astate =
  let new_event = Event.make_blocking_call callee sev in
  make_call_with_event None new_event ~loc astate


let strict_mode_call ~callee ~loc astate =
  let new_event = Event.make_strict_mode_call callee in
  make_call_with_event None new_event ~loc astate


let release ({lock_state} as astate) locks =
  { astate with
    lock_state= List.fold locks ~init:lock_state ~f:(fun acc l -> LockState.release l acc) }


let integrate_summary tenv ~caller_summary:({lock_state; critical_pairs; ui} as astate) ~callee
    ~loc ~callee_summary =
  let callsite = CallSite.make callee loc in
  let critical_pairs' =
    CriticalPairs.with_callsite callee_summary.critical_pairs (Some tenv) lock_state callsite
  in
  let callee_ui = UIThreadDomain.with_callsite callee_summary.ui callsite in
  { astate with
    critical_pairs= CriticalPairs.join critical_pairs critical_pairs'
  ; ui= UIThreadDomain.join ui callee_ui }


let set_on_ui_thread ({ui} as astate) ~loc explain =
  let ui =
    UIThreadDomain.join ui
      (AbstractDomain.Types.NonBottom (UIThreadExplanationDomain.make explain loc))
  in
  {astate with ui}


let add_guard ~acquire_now ~procname ~loc tenv astate guard lock =
  let astate = {astate with guard_map= GuardToLockMap.add_guard ~guard ~lock astate.guard_map} in
  if acquire_now then acquire tenv astate ~procname ~loc [lock] else astate


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


let lock_guard ~procname ~loc tenv astate guard =
  GuardToLockMap.find_opt guard astate.guard_map
  |> Option.value_map ~default:astate ~f:(fun lock_opt ->
         FlatLock.get lock_opt |> Option.to_list |> acquire tenv astate ~procname ~loc )


let filter_blocking_calls ({critical_pairs} as astate) =
  {astate with critical_pairs= CriticalPairs.filter CriticalPair.is_blocking_call critical_pairs}


type summary = t

let pp_summary = pp
