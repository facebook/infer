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

module ThreadDomain = struct
  type t = UnknownThread | UIThread | BGThread | AnyThread [@@deriving compare, equal]

  let bottom = UnknownThread

  let is_bottom = function UnknownThread -> true | _ -> false

  let join lhs rhs =
    match (lhs, rhs) with
    | UnknownThread, other | other, UnknownThread ->
        other
    | UIThread, UIThread | BGThread, BGThread ->
        lhs
    | _, _ ->
        AnyThread


  (* type is just an int, so use [join] to define [leq] *)
  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt st =
    ( match st with
    | UnknownThread ->
        "UnknownThread"
    | UIThread ->
        "UIThread"
    | BGThread ->
        "BGThread"
    | AnyThread ->
        "AnyThread" )
    |> F.pp_print_string fmt


  (** Can two thread statuses occur in parallel? Only [UIThread, UIThread] is forbidden. 
      In addition, this is monotonic wrt the lattice (increasing either argument cannot 
      transition from true to false). *)
  let can_run_in_parallel st1 st2 =
    match (st1, st2) with UIThread, UIThread -> false | _, _ -> true


  let is_uithread = function UIThread -> true | _ -> false

  (* If we know that either the caller is a UI/BG thread or both, keep it that way.
     Otherwise, we have no info on caller, so use callee's info. *)
  let integrate_summary ~caller ~callee = if is_bottom caller then callee else caller

  (** given the current thread state [caller_thread] and the thread state under which a critical pair
      occurred, [pair_thread], decide whether to throw away the pair (returning [None]) because it 
      cannot occur within a call from the current state, or adapt its thread state appropriately. *)
  let apply_to_pair caller_thread pair_thread =
    match (caller_thread, pair_thread) with
    | UnknownThread, _ ->
        (* callee pair knows more than us *)
        Some pair_thread
    | AnyThread, UnknownThread ->
        (* callee pair knows nothing and caller has abstracted away info *)
        Some AnyThread
    | AnyThread, _ ->
        (* callee pair is UI / BG / Any and caller has abstracted away info so use callee's knowledge *)
        Some pair_thread
    | UIThread, BGThread | BGThread, UIThread ->
        (* annotations or assertions are incorrectly used in code, just drop the callee pair *)
        None
    | _, _ ->
        (* caller is UI or BG and callee does not disagree, so use that *)
        Some caller_thread
end

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
    match desc with Typ.Tstruct name | Typ.Tptr ({desc= Tstruct name}, _) -> Some name | _ -> None


  let describe fmt lock =
    let pp_owner fmt lock =
      owner_class lock |> Option.iter ~f:(F.fprintf fmt " in %a" (MF.wrap_monospaced Typ.Name.pp))
    in
    F.fprintf fmt "%a%a" (MF.wrap_monospaced pp) lock pp_owner lock


  let pp_locks fmt lock = F.fprintf fmt " locks %a" describe lock
end

module Event = struct
  type t =
    | LockAcquire of Lock.t
    | MayBlock of (string * StarvationModels.severity)
    | StrictModeCall of string
  [@@deriving compare]

  let pp fmt = function
    | LockAcquire lock ->
        F.fprintf fmt "LockAcquire(%a)" Lock.pp lock
    | MayBlock (msg, sev) ->
        F.fprintf fmt "MayBlock(%s, %a)" msg StarvationModels.pp_severity sev
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
  (* abstraction limit for lock counts *)
  let max_lock_depth_allowed = 5

  module LockCount = AbstractDomain.DownwardIntDomain (struct
    let max = max_lock_depth_allowed
  end)

  module Map = AbstractDomain.InvertedMap (Lock) (LockCount)

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


  let leq ~lhs ~rhs = Map.leq ~lhs:lhs.map ~rhs:rhs.map

  let top = {map= Map.top; acquisitions= Acquisitions.empty}

  let is_top {map} = Map.is_top map

  let is_lock_taken event {acquisitions} =
    match event with
    | Event.LockAcquire lock ->
        Acquisitions.mem (Acquisition.make_dummy lock) acquisitions
    | _ ->
        false


  let acquire ~procname ~loc lock {map; acquisitions} =
    let should_add_acquisition = ref false in
    let map =
      Map.update lock
        (function
          | None ->
              (* lock was not already held, so add it to [acquisitions] *)
              should_add_acquisition := true ;
              Some LockCount.(increment top)
          | Some count ->
              Some (LockCount.increment count) )
        map
    in
    let acquisitions =
      if !should_add_acquisition then
        let acquisition = Acquisition.make ~procname ~loc lock in
        Acquisitions.add acquisition acquisitions
      else acquisitions
    in
    {map; acquisitions}


  let release lock {map; acquisitions} =
    let should_remove_acquisition = ref false in
    let map =
      Map.update lock
        (function
          | None ->
              None
          | Some count ->
              let new_count = LockCount.decrement count in
              if LockCount.is_top new_count then (
                (* lock was held, but now it is not, so remove from [aqcuisitions] *)
                should_remove_acquisition := true ;
                None )
              else Some new_count )
        map
    in
    let acquisitions =
      if !should_remove_acquisition then
        let acquisition = Acquisition.make_dummy lock in
        Acquisitions.remove acquisition acquisitions
      else acquisitions
    in
    {map; acquisitions}
end

module CriticalPairElement = struct
  type t = {acquisitions: Acquisitions.t; event: Event.t; thread: ThreadDomain.t}
  [@@deriving compare]

  let pp fmt {acquisitions; event} =
    F.fprintf fmt "{acquisitions= %a; event= %a}" Acquisitions.pp acquisitions Event.pp event


  let describe = pp
end

module CriticalPair = struct
  include ExplicitTrace.MakeTraceElem (CriticalPairElement) (ExplicitTrace.DefaultCallPrinter)

  let make ~loc acquisitions event thread = make {acquisitions; event; thread} loc

  let is_blocking_call {elem= {event}} = match event with LockAcquire _ -> true | _ -> false

  let get_final_acquire {elem= {event}} =
    match event with LockAcquire lock -> Some lock | _ -> None


  let may_deadlock ({elem= pair1} as t1 : t) ({elem= pair2} as t2 : t) =
    ThreadDomain.can_run_in_parallel pair1.thread pair2.thread
    && Option.both (get_final_acquire t1) (get_final_acquire t2)
       |> Option.exists ~f:(fun (lock1, lock2) ->
              (not (Lock.equal lock1 lock2))
              && Acquisitions.lock_is_held lock2 pair1.acquisitions
              && Acquisitions.lock_is_held lock1 pair2.acquisitions
              && Acquisitions.inter pair1.acquisitions pair2.acquisitions |> Acquisitions.is_empty
          )


  let integrate_summary_opt existing_acquisitions call_site (caller_thread : ThreadDomain.t)
      (callee_pair : t) =
    ThreadDomain.apply_to_pair caller_thread callee_pair.elem.thread
    |> Option.map ~f:(fun thread ->
           let f (elem : CriticalPairElement.t) =
             let acquisitions = Acquisitions.union existing_acquisitions elem.acquisitions in
             ({elem with acquisitions; thread} : elem_t)
           in
           with_callsite (map ~f callee_pair) call_site )


  let get_earliest_lock_or_call_loc ~procname ({elem= {acquisitions}} as t) =
    let initial_loc = get_loc t in
    Acquisitions.fold
      (fun {procname= acq_procname; loc= acq_loc} acc ->
        if
          Typ.Procname.equal procname acq_procname && Int.is_negative (Location.compare acq_loc acc)
        then acq_loc
        else acc )
      acquisitions initial_loc


  let make_trace ?(header = "") ?(include_acquisitions = true) top_pname
      ({elem= {acquisitions; event}; trace; loc} as pair) =
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
      let loc = get_loc pair in
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


  let is_uithread t = ThreadDomain.is_uithread t.elem.thread

  let can_run_in_parallel t1 t2 = ThreadDomain.can_run_in_parallel t1.elem.thread t2.elem.thread
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

  let with_callsite astate tenv lock_state call_site thread =
    let existing_acquisitions = LockState.get_acquisitions lock_state in
    fold
      (fun ({elem= {event}} as critical_pair : CriticalPair.t) acc ->
        if should_skip (Some tenv) event lock_state then acc
        else
          CriticalPair.integrate_summary_opt existing_acquisitions call_site thread critical_pair
          |> Option.fold ~init:acc ~f:(fun acc new_pair -> add new_pair acc) )
      astate empty
end

module FlatLock = AbstractDomain.Flat (Lock)

module GuardToLockMap = struct
  include AbstractDomain.InvertedMap (HilExp) (FlatLock)

  let remove_guard astate guard = remove guard astate

  let add_guard astate ~guard ~lock = add guard (FlatLock.v lock) astate
end

module BranchGuard = struct
  type t = Nothing | Thread

  let top = Nothing

  let is_top = function Nothing -> true | _ -> false

  let pp fmt t = F.fprintf fmt (match t with Nothing -> "Nothing" | Thread -> "Thread")

  let leq ~lhs ~rhs =
    match (lhs, rhs) with _, Nothing -> true | Nothing, _ -> false | Thread, Thread -> true


  let join lhs rhs = match (lhs, rhs) with Thread, Thread -> lhs | _ -> Nothing

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module BranchGuardDomain = struct
  include AbstractDomain.InvertedMap (HilExp.AccessExpression) (BranchGuard)

  let is_thread_guard acc_exp t =
    find_opt acc_exp t |> Option.exists ~f:(function BranchGuard.Thread -> true | _ -> false)
end

module ScheduledWorkItem = struct
  type t = {procname: Typ.Procname.t; loc: Location.t; thread: ThreadDomain.t} [@@deriving compare]

  let pp fmt {procname; loc; thread} =
    F.fprintf fmt "{procname= %a; loc= %a; thread= %a}" Typ.Procname.pp procname Location.pp loc
      ThreadDomain.pp thread
end

module ScheduledWorkDomain = AbstractDomain.FiniteSet (ScheduledWorkItem)

type t =
  { guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: CriticalPairs.t
  ; branch_guards: BranchGuardDomain.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t }

let bottom =
  { guard_map= GuardToLockMap.empty
  ; lock_state= LockState.top
  ; critical_pairs= CriticalPairs.empty
  ; branch_guards= BranchGuardDomain.empty
  ; thread= ThreadDomain.bottom
  ; scheduled_work= ScheduledWorkDomain.bottom }


let is_bottom astate =
  GuardToLockMap.is_empty astate.guard_map
  && LockState.is_top astate.lock_state
  && CriticalPairs.is_empty astate.critical_pairs
  && BranchGuardDomain.is_top astate.branch_guards
  && ThreadDomain.is_bottom astate.thread
  && ScheduledWorkDomain.is_bottom astate.scheduled_work


let pp fmt astate =
  F.fprintf fmt
    "{guard_map= %a; lock_state= %a; critical_pairs= %a; branch_guards= %a; thread= %a; \
     scheduled_work= %a}"
    GuardToLockMap.pp astate.guard_map LockState.pp astate.lock_state CriticalPairs.pp
    astate.critical_pairs BranchGuardDomain.pp astate.branch_guards ThreadDomain.pp astate.thread
    ScheduledWorkDomain.pp astate.scheduled_work


let join lhs rhs =
  { guard_map= GuardToLockMap.join lhs.guard_map rhs.guard_map
  ; lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; critical_pairs= CriticalPairs.join lhs.critical_pairs rhs.critical_pairs
  ; branch_guards= BranchGuardDomain.join lhs.branch_guards rhs.branch_guards
  ; thread= ThreadDomain.join lhs.thread rhs.thread
  ; scheduled_work= ScheduledWorkDomain.join lhs.scheduled_work rhs.scheduled_work }


let widen ~prev ~next ~num_iters:_ = join prev next

let leq ~lhs ~rhs =
  GuardToLockMap.leq ~lhs:lhs.guard_map ~rhs:rhs.guard_map
  && LockState.leq ~lhs:lhs.lock_state ~rhs:rhs.lock_state
  && CriticalPairs.leq ~lhs:lhs.critical_pairs ~rhs:rhs.critical_pairs
  && BranchGuardDomain.leq ~lhs:lhs.branch_guards ~rhs:rhs.branch_guards
  && ThreadDomain.leq ~lhs:lhs.thread ~rhs:rhs.thread
  && ScheduledWorkDomain.leq ~lhs:lhs.scheduled_work ~rhs:rhs.scheduled_work


let add_critical_pair tenv_opt lock_state event thread ~loc acc =
  if should_skip tenv_opt event lock_state then acc
  else
    let acquisitions = LockState.get_acquisitions lock_state in
    let critical_pair = CriticalPair.make ~loc acquisitions event thread in
    CriticalPairs.add critical_pair acc


let acquire tenv ({lock_state; critical_pairs} as astate) ~procname ~loc locks =
  { astate with
    critical_pairs=
      List.fold locks ~init:critical_pairs ~f:(fun acc lock ->
          let event = Event.make_acquire lock in
          add_critical_pair (Some tenv) lock_state event astate.thread ~loc acc )
  ; lock_state=
      List.fold locks ~init:lock_state ~f:(fun acc lock -> LockState.acquire ~procname ~loc lock acc)
  }


let make_call_with_event new_event ~loc astate =
  { astate with
    critical_pairs=
      add_critical_pair None astate.lock_state new_event astate.thread ~loc astate.critical_pairs }


let blocking_call ~callee sev ~loc astate =
  let new_event = Event.make_blocking_call callee sev in
  make_call_with_event new_event ~loc astate


let strict_mode_call ~callee ~loc astate =
  let new_event = Event.make_strict_mode_call callee in
  make_call_with_event new_event ~loc astate


let release ({lock_state} as astate) locks =
  { astate with
    lock_state= List.fold locks ~init:lock_state ~f:(fun acc l -> LockState.release l acc) }


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


let schedule_work loc thread_constraint astate procname =
  let thread =
    match thread_constraint with
    | StarvationModels.ForUIThread ->
        ThreadDomain.UIThread
    | StarvationModels.ForNonUIThread ->
        ThreadDomain.BGThread
  in
  let work_item = ScheduledWorkItem.{procname; loc; thread} in
  {astate with scheduled_work= ScheduledWorkDomain.add work_item astate.scheduled_work}


type summary =
  {critical_pairs: CriticalPairs.t; thread: ThreadDomain.t; scheduled_work: ScheduledWorkDomain.t}

let pp_summary fmt (summary : summary) =
  F.fprintf fmt "{thread= %a; critical_pairs= %a; scheduled_work= %a}" ThreadDomain.pp
    summary.thread CriticalPairs.pp summary.critical_pairs ScheduledWorkDomain.pp
    summary.scheduled_work


let integrate_summary tenv callsite (astate : t) (summary : summary) =
  let critical_pairs' =
    CriticalPairs.with_callsite summary.critical_pairs tenv astate.lock_state callsite astate.thread
  in
  { astate with
    critical_pairs= CriticalPairs.join astate.critical_pairs critical_pairs'
  ; thread= ThreadDomain.integrate_summary ~caller:astate.thread ~callee:summary.thread }


let summary_of_astate : t -> summary =
 fun astate ->
  { critical_pairs= astate.critical_pairs
  ; thread= astate.thread
  ; scheduled_work= astate.scheduled_work }
