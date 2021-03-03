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

let describe_pname = MF.wrap_monospaced Procname.pp

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


  (** Can two thread statuses occur in parallel? Only [UIThread, UIThread] is forbidden. In
      addition, this is monotonic wrt the lattice (increasing either argument cannot transition from
      true to false). *)
  let can_run_in_parallel st1 st2 =
    match (st1, st2) with UIThread, UIThread -> false | _, _ -> true


  let is_uithread = function UIThread -> true | _ -> false

  (* If we know that either the caller is a UI/BG thread or both, keep it that way.
     Otherwise, we have no info on caller, so use callee's info. *)
  let integrate_summary ~caller ~callee = if is_bottom caller then callee else caller

  (** given the current thread state [caller_thread] and the thread state under which a critical
      pair occurred, [pair_thread], decide whether to throw away the pair (returning [None]) because
      it cannot occur within a call from the current state, or adapt its thread state appropriately. *)
  let apply_caller_thread ~caller ~callee =
    match (caller, callee) with
    | UnknownThread, _ ->
        (* callee pair knows more than us *)
        Some callee
    | AnyThread, UnknownThread ->
        (* callee pair knows nothing and caller has abstracted away info *)
        Some AnyThread
    | AnyThread, _ ->
        (* callee pair is UI / BG / Any and caller has abstracted away info so use callee's knowledge *)
        Some callee
    | UIThread, BGThread | BGThread, UIThread ->
        (* annotations or assertions are incorrectly used in code, or callee is path-sensitive on
           thread-identity, just drop the callee pair *)
        None
    | _, _ ->
        (* caller is UI or BG and callee does not disagree, so use that *)
        Some caller
end

module Lock = struct
  include AbstractAddress

  let pp_locks fmt lock = F.fprintf fmt " locks %a" describe lock

  let make_java_synchronized formals procname =
    match procname with
    | Procname.Java java_pname when Procname.Java.is_static java_pname ->
        (* this is crafted so as to match synchronized(CLASSNAME.class) constructs *)
        let typename_str = Procname.Java.get_class_type_name java_pname |> Typ.Name.name in
        let hilexp = HilExp.(Constant (Cclass (Ident.string_to_name typename_str))) in
        make formals hilexp
    | Procname.Java _ ->
        FormalMap.get_formal_base 0 formals
        |> Option.bind ~f:(fun base ->
               let hilexp = HilExp.(AccessExpression (AccessExpression.base base)) in
               make formals hilexp )
    | _ ->
        L.die InternalError "Non-Java methods cannot be synchronized.@\n"


  let compare_wrt_reporting t1 t2 =
    let mk_str t = root_class t |> Option.value_map ~default:"" ~f:Typ.Name.to_string in
    (* use string comparison on types as a stable order to decide whether to report a deadlock *)
    String.compare (mk_str t1) (mk_str t2)


  let apply_subst_to_list subst l =
    let rec apply_subst_to_list_inner l =
      match l with
      | [] ->
          ([], false)
      | lock :: locks -> (
          let locks', modified = apply_subst_to_list_inner locks in
          match apply_subst subst lock with
          | None ->
              (locks', true)
          | Some lock' when (not modified) && phys_equal lock lock' ->
              (l, false)
          | Some lock' ->
              (lock' :: locks', true) )
    in
    apply_subst_to_list_inner l |> fst


  let is_recursive tenv lock =
    let is_class_and_recursive_lock = function
      | {Typ.desc= Tptr ({desc= Tstruct name}, _)} | {desc= Tstruct name} ->
          ConcurrencyModels.is_recursive_lock_type name
      | typ ->
          L.debug Analysis Verbose "Asked if non-struct type %a is a recursive lock type.@."
            (Typ.pp_full Pp.text) typ ;
          true
    in
    get_typ tenv lock |> Option.exists ~f:is_class_and_recursive_lock
end

module AccessExpressionDomain = struct
  open AbstractDomain.Types

  type t = HilExp.AccessExpression.t top_lifted [@@deriving equal]

  let pp fmt = function
    | Top ->
        F.pp_print_string fmt "AccExpTop"
    | NonTop lock ->
        HilExp.AccessExpression.pp fmt lock


  let top = Top

  let is_top = function Top -> true | NonTop _ -> false

  let join lhs rhs = if equal lhs rhs then lhs else top

  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module VarDomain = struct
  include AbstractDomain.SafeInvertedMap (Var) (AccessExpressionDomain)

  let exit_scope init deadvars =
    List.fold deadvars ~init ~f:(fun acc deadvar ->
        filter
          (fun _key acc_exp_opt ->
            match acc_exp_opt with
            | Top ->
                (* should never happen in a safe inverted map *)
                false
            | NonTop acc_exp ->
                let var, _ = HilExp.AccessExpression.get_base acc_exp in
                not (Var.equal var deadvar) )
          acc
        |> remove deadvar )


  let get var astate =
    match find_opt var astate with None | Some Top -> None | Some (NonTop x) -> Some x


  let set var acc_exp astate = add var (NonTop acc_exp) astate
end

module Event = struct
  type t =
    | Ipc of {callee: Procname.t; thread: ThreadDomain.t}
    | LockAcquire of {locks: Lock.t list; thread: ThreadDomain.t}
    | MayBlock of {callee: Procname.t; thread: ThreadDomain.t}
    | MonitorWait of {lock: Lock.t; thread: ThreadDomain.t}
    | MustNotOccurUnderLock of {callee: Procname.t; thread: ThreadDomain.t}
    | StrictModeCall of {callee: Procname.t; thread: ThreadDomain.t}
  [@@deriving compare]

  let pp fmt = function
    | Ipc {callee; thread} ->
        F.fprintf fmt "Ipc(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | LockAcquire {locks; thread} ->
        F.fprintf fmt "LockAcquire(%a, %a)"
          (PrettyPrintable.pp_collection ~pp_item:Lock.pp)
          locks ThreadDomain.pp thread
    | MayBlock {callee; thread} ->
        F.fprintf fmt "MayBlock(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | MonitorWait {lock; thread} ->
        F.fprintf fmt "MonitorWait(%a, %a)" Lock.pp lock ThreadDomain.pp thread
    | MustNotOccurUnderLock {callee; thread} ->
        F.fprintf fmt "MustNotOccurUnderLock(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | StrictModeCall {callee; thread} ->
        F.fprintf fmt "StrictModeCall(%a, %a)" Procname.pp callee ThreadDomain.pp thread


  let describe fmt elem =
    match elem with
    | LockAcquire {locks} ->
        Pp.comma_seq Lock.pp_locks fmt locks
    | Ipc {callee} | MayBlock {callee} | MustNotOccurUnderLock {callee} | StrictModeCall {callee} ->
        F.fprintf fmt "calls %a" describe_pname callee
    | MonitorWait {lock} ->
        F.fprintf fmt "calls `wait` on %a" Lock.describe lock


  let get_thread = function
    | Ipc {thread}
    | LockAcquire {thread}
    | MayBlock {thread}
    | MonitorWait {thread}
    | MustNotOccurUnderLock {thread}
    | StrictModeCall {thread} ->
        thread


  let with_thread event thread =
    if ThreadDomain.equal thread (get_thread event) then event
    else
      match event with
      | Ipc ipc ->
          Ipc {ipc with thread}
      | LockAcquire lock_acquire ->
          LockAcquire {lock_acquire with thread}
      | MayBlock may_block ->
          MayBlock {may_block with thread}
      | MonitorWait monitor_wait ->
          MonitorWait {monitor_wait with thread}
      | MustNotOccurUnderLock not_under_lock ->
          MustNotOccurUnderLock {not_under_lock with thread}
      | StrictModeCall strict_mode_call ->
          StrictModeCall {strict_mode_call with thread}


  let apply_caller_thread caller_thread event =
    match ThreadDomain.apply_caller_thread ~caller:caller_thread ~callee:(get_thread event) with
    | None ->
        None
    | Some thread ->
        Some (with_thread event thread)


  let make_acquire locks thread = LockAcquire {locks; thread}

  let make_blocking_call callee thread = MayBlock {callee; thread}

  let make_strict_mode_call callee thread = StrictModeCall {callee; thread}

  let make_object_wait lock thread = MonitorWait {lock; thread}

  let make_arbitrary_code_exec callee thread = MustNotOccurUnderLock {callee; thread}

  let make_ipc callee thread = Ipc {callee; thread}

  let get_acquired_locks = function LockAcquire {locks} -> locks | _ -> []

  let apply_subst subst event =
    match event with
    | Ipc _ | MayBlock _ | StrictModeCall _ | MustNotOccurUnderLock _ ->
        Some event
    | MonitorWait {lock; thread} -> (
      match Lock.apply_subst subst lock with
      | None ->
          None
      | Some lock' when phys_equal lock lock' ->
          Some event
      | Some lock ->
          Some (MonitorWait {lock; thread}) )
    | LockAcquire {locks; thread} -> (
      match Lock.apply_subst_to_list subst locks with
      | [] ->
          None
      | locks' when phys_equal locks locks' ->
          Some event
      | locks ->
          Some (LockAcquire {locks; thread}) )


  let has_recursive_lock tenv event =
    get_acquired_locks event |> List.exists ~f:(Lock.is_recursive tenv)
end

(** A lock acquisition with source location and procname in which it occurs. The location & procname
    are *ignored* for comparisons, and are only for reporting. *)
module Acquisition = struct
  type t = {lock: Lock.t; loc: Location.t [@compare.ignore]; procname: Procname.t [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {lock; loc; procname} =
    F.fprintf fmt "<@[lock=%a;@;loc=%a;@;procname=%a@]>" Lock.pp lock Location.pp loc Procname.pp
      procname


  let describe fmt {lock} = Lock.pp_locks fmt lock

  let make ~procname ~loc lock = {lock; loc; procname}

  let compare_loc {loc= loc1} {loc= loc2} = Location.compare loc1 loc2

  let make_trace_step acquisition =
    let description = F.asprintf "%a" describe acquisition in
    Errlog.make_trace_element 0 acquisition.loc description []


  let make_dummy lock = {lock; loc= Location.dummy; procname= Procname.Linters_dummy_method}

  let apply_subst subst acquisition =
    match Lock.apply_subst subst acquisition.lock with
    | None ->
        None
    | Some lock when phys_equal acquisition.lock lock ->
        Some acquisition
    | Some lock ->
        Some {acquisition with lock}
end

(** Set of acquisitions; due to order over acquisitions, each lock appears at most once. *)
module Acquisitions = struct
  include PrettyPrintable.MakePPSet (Acquisition)

  (* use the fact that location/procname are ignored in comparisons *)
  let lock_is_held lock acquisitions = mem (Acquisition.make_dummy lock) acquisitions

  let lock_is_held_in_other_thread tenv lock acquisitions =
    exists (fun acq -> Lock.equal_across_threads tenv lock acq.lock) acquisitions


  let no_locks_common_across_threads tenv acqs1 acqs2 =
    for_all (fun acq1 -> not (lock_is_held_in_other_thread tenv acq1.lock acqs2)) acqs1


  let apply_subst subst acqs =
    fold
      (fun acq acc ->
        match Acquisition.apply_subst subst acq with None -> acc | Some acq' -> add acq' acc )
      acqs empty
end

module LockState : sig
  include AbstractDomain.WithTop

  val acquire : procname:Procname.t -> loc:Location.t -> Lock.t -> t -> t

  val release : Lock.t -> t -> t

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
    F.fprintf fmt "{@[map= %a;@;acquisitions= %a@]}" Map.pp map Acquisitions.pp acquisitions


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
  type t = {acquisitions: Acquisitions.t; event: Event.t} [@@deriving compare]

  let pp fmt {acquisitions; event} =
    F.fprintf fmt "{@[acquisitions= %a;@;event= %a@]}" Acquisitions.pp acquisitions Event.pp event


  let describe = pp

  let get_thread {event} = Event.get_thread event

  let apply_subst subst elem =
    match Event.apply_subst subst elem.event with
    | None ->
        None
    | Some event ->
        let acquisitions = Acquisitions.apply_subst subst elem.acquisitions in
        Some {acquisitions; event}
end

module CriticalPair = struct
  include ExplicitTrace.MakeTraceElemModuloLocation
            (CriticalPairElement)
            (ExplicitTrace.DefaultCallPrinter)

  let make ~loc acquisitions event = make {acquisitions; event} loc

  let get_thread {elem} = CriticalPairElement.get_thread elem

  let may_deadlock tenv ~(lhs : t) ~lhs_lock ~(rhs : t) =
    if ThreadDomain.can_run_in_parallel (get_thread lhs) (get_thread rhs) then
      Event.get_acquired_locks rhs.elem.event
      |> List.find ~f:(fun rhs_lock ->
             (not (Lock.equal_across_threads tenv lhs_lock rhs_lock))
             && Acquisitions.lock_is_held_in_other_thread tenv rhs_lock lhs.elem.acquisitions
             && Acquisitions.lock_is_held_in_other_thread tenv lhs_lock rhs.elem.acquisitions
             && Acquisitions.no_locks_common_across_threads tenv lhs.elem.acquisitions
                  rhs.elem.acquisitions )
    else None


  let apply_subst subst pair =
    match CriticalPairElement.apply_subst subst pair.elem with
    | None ->
        None
    | Some elem' ->
        Some (map ~f:(fun _elem -> elem') pair)


  (** if given [Some tenv], transform a pair so as to remove reentrant locks that are already in
      [held_locks] *)
  let filter_out_reentrant_relocks tenv_opt held_locks pair =
    match (tenv_opt, pair.elem.event) with
    | Some tenv, LockAcquire {locks; thread} -> (
        let filtered_locks =
          IList.filter_changed locks ~f:(fun lock ->
              (not (Acquisitions.lock_is_held lock held_locks))
              || not (Event.has_recursive_lock tenv pair.elem.event) )
        in
        match filtered_locks with
        | [] ->
            None
        | _ when phys_equal filtered_locks locks ->
            Some pair
        | locks ->
            Some (map pair ~f:(fun elem -> {elem with event= LockAcquire {locks; thread}})) )
    | _, _ ->
        Some pair


  let apply_caller_thread caller_thread callee_pair =
    match Event.apply_caller_thread caller_thread callee_pair.elem.event with
    | None ->
        None
    | Some event when phys_equal event callee_pair.elem.event ->
        Some callee_pair
    | Some event ->
        Some (map ~f:(fun (elem : CriticalPairElement.t) -> {elem with event}) callee_pair)


  let integrate_summary_opt ~subst ~tenv existing_acquisitions call_site
      (caller_thread : ThreadDomain.t) (callee_pair : t) =
    apply_subst subst callee_pair
    |> Option.bind ~f:(filter_out_reentrant_relocks (Some tenv) existing_acquisitions)
    |> Option.bind ~f:(apply_caller_thread caller_thread)
    |> Option.map ~f:(fun callee_pair ->
           let f (elem : CriticalPairElement.t) =
             {elem with acquisitions= Acquisitions.union existing_acquisitions elem.acquisitions}
           in
           map ~f callee_pair )
    |> Option.map ~f:(fun callee_pair -> with_callsite callee_pair call_site)


  let get_earliest_lock_or_call_loc ~procname ({elem= {acquisitions}} as t) =
    let initial_loc = get_loc t in
    Acquisitions.fold
      (fun {procname= acq_procname; loc= acq_loc} acc ->
        if Procname.equal procname acq_procname && Int.is_negative (Location.compare acq_loc acc)
        then acq_loc
        else acc )
      acquisitions initial_loc


  let make_trace ?(header = "") ?(include_acquisitions = true) top_pname
      ({elem= {acquisitions; event}; trace; loc} as pair) =
    let acquisitions_map =
      if include_acquisitions then
        Acquisitions.fold
          (fun ({procname} as acq : Acquisition.t) acc ->
            Procname.Map.update procname
              (function None -> Some [acq] | Some acqs -> Some (acq :: acqs))
              acc )
          acquisitions Procname.Map.empty
      else Procname.Map.empty
    in
    let header_step =
      let description = F.asprintf "%s%a" header describe_pname top_pname in
      let loc = get_loc pair in
      Errlog.make_trace_element 0 loc description []
    in
    (* construct the trace segment starting at [call_site] and ending at next call *)
    let make_call_stack_step fake_first_call call_site =
      let procname = CallSite.pname call_site in
      let trace =
        Procname.Map.find_opt procname acquisitions_map
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


  let is_uithread t = ThreadDomain.is_uithread (get_thread t)

  let can_run_in_parallel t1 t2 = ThreadDomain.can_run_in_parallel (get_thread t1) (get_thread t2)
end

module CriticalPairs = struct
  include CriticalPair.FiniteSet

  let with_callsite astate ~tenv ~subst lock_state call_site thread =
    let existing_acquisitions = LockState.get_acquisitions lock_state in
    fold
      (fun critical_pair acc ->
        CriticalPair.integrate_summary_opt ~subst ~tenv existing_acquisitions call_site thread
          critical_pair
        |> Option.bind
             ~f:(CriticalPair.filter_out_reentrant_relocks (Some tenv) existing_acquisitions)
        |> Option.value_map ~default:acc ~f:(fun new_pair -> add new_pair acc) )
      astate empty
end

module FlatLock = AbstractDomain.Flat (Lock)

module GuardToLockMap = struct
  include AbstractDomain.InvertedMap (HilExp) (FlatLock)

  let remove_guard astate guard = remove guard astate

  let add_guard astate ~guard ~lock = add guard (FlatLock.v lock) astate
end

module Attribute = struct
  type t =
    | Nothing
    | ThreadGuard
    | FutureDoneGuard of HilExp.AccessExpression.t
    | FutureDoneState of bool
    | Runnable of Procname.t
    | WorkScheduler of StarvationModels.scheduler_thread_constraint
    | Looper of StarvationModels.scheduler_thread_constraint
  [@@deriving equal]

  let top = Nothing

  let is_top = function Nothing -> true | _ -> false

  let pp fmt t =
    let pp_constr fmt c =
      StarvationModels.(
        match c with ForUIThread -> "UI" | ForNonUIThread -> "BG" | ForUnknownThread -> "Unknown")
      |> F.pp_print_string fmt
    in
    match t with
    | Nothing ->
        F.pp_print_string fmt "Nothing"
    | ThreadGuard ->
        F.pp_print_string fmt "ThreadGuard"
    | FutureDoneGuard exp ->
        F.fprintf fmt "FutureDoneGuard(%a)" HilExp.AccessExpression.pp exp
    | FutureDoneState state ->
        F.fprintf fmt "FutureDoneState(%b)" state
    | Runnable runproc ->
        F.fprintf fmt "Runnable(%a)" Procname.pp runproc
    | WorkScheduler c ->
        F.fprintf fmt "WorkScheduler(%a)" pp_constr c
    | Looper c ->
        F.fprintf fmt "Looper(%a)" pp_constr c


  let join lhs rhs = if equal lhs rhs then lhs else Nothing

  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module AttributeDomain = struct
  include AbstractDomain.SafeInvertedMap (HilExp.AccessExpression) (Attribute)

  let is_thread_guard acc_exp t =
    find_opt acc_exp t |> Option.exists ~f:(function Attribute.ThreadGuard -> true | _ -> false)


  let is_future_done_guard acc_exp t =
    find_opt acc_exp t
    |> Option.exists ~f:(function Attribute.FutureDoneGuard _ -> true | _ -> false)


  let exit_scope vars t =
    let pred key _value =
      HilExp.AccessExpression.get_base key
      |> fst
      |> fun v -> Var.is_this v || not (List.exists vars ~f:(Var.equal v))
    in
    filter pred t
end

module ScheduledWorkItem = struct
  type t = {procname: Procname.t; loc: Location.t; thread: ThreadDomain.t} [@@deriving compare]

  let pp fmt {procname; loc; thread} =
    F.fprintf fmt "{@[procname= %a;@;loc= %a;@;thread= %a@]}" Procname.pp procname Location.pp loc
      ThreadDomain.pp thread
end

module ScheduledWorkDomain = AbstractDomain.FiniteSet (ScheduledWorkItem)
module IgnoreBlockingCalls = AbstractDomain.BooleanOr

type t =
  { ignore_blocking_calls: IgnoreBlockingCalls.t
  ; guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: CriticalPairs.t
  ; attributes: AttributeDomain.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t
  ; var_state: VarDomain.t }

let initial =
  { ignore_blocking_calls= false
  ; guard_map= GuardToLockMap.empty
  ; lock_state= LockState.top
  ; critical_pairs= CriticalPairs.empty
  ; attributes= AttributeDomain.empty
  ; thread= ThreadDomain.bottom
  ; scheduled_work= ScheduledWorkDomain.bottom
  ; var_state= VarDomain.top }


let pp fmt astate =
  F.fprintf fmt
    "{@[guard_map= %a;@;\
     lock_state= %a;@;\
     critical_pairs= %a;@;\
     attributes= %a;@;\
     thread= %a;@;\
     scheduled_work= %a;@;\
     var_state= %a@]}" GuardToLockMap.pp astate.guard_map LockState.pp astate.lock_state
    CriticalPairs.pp astate.critical_pairs AttributeDomain.pp astate.attributes ThreadDomain.pp
    astate.thread ScheduledWorkDomain.pp astate.scheduled_work VarDomain.pp astate.var_state


let join lhs rhs =
  { ignore_blocking_calls=
      IgnoreBlockingCalls.join lhs.ignore_blocking_calls rhs.ignore_blocking_calls
  ; guard_map= GuardToLockMap.join lhs.guard_map rhs.guard_map
  ; lock_state= LockState.join lhs.lock_state rhs.lock_state
  ; critical_pairs= CriticalPairs.join lhs.critical_pairs rhs.critical_pairs
  ; attributes= AttributeDomain.join lhs.attributes rhs.attributes
  ; thread= ThreadDomain.join lhs.thread rhs.thread
  ; scheduled_work= ScheduledWorkDomain.join lhs.scheduled_work rhs.scheduled_work
  ; var_state= VarDomain.join lhs.var_state rhs.var_state }


let widen ~prev ~next ~num_iters:_ = join prev next

let leq ~lhs ~rhs =
  IgnoreBlockingCalls.leq ~lhs:lhs.ignore_blocking_calls ~rhs:rhs.ignore_blocking_calls
  && GuardToLockMap.leq ~lhs:lhs.guard_map ~rhs:rhs.guard_map
  && LockState.leq ~lhs:lhs.lock_state ~rhs:rhs.lock_state
  && CriticalPairs.leq ~lhs:lhs.critical_pairs ~rhs:rhs.critical_pairs
  && AttributeDomain.leq ~lhs:lhs.attributes ~rhs:rhs.attributes
  && ThreadDomain.leq ~lhs:lhs.thread ~rhs:rhs.thread
  && ScheduledWorkDomain.leq ~lhs:lhs.scheduled_work ~rhs:rhs.scheduled_work
  && VarDomain.leq ~lhs:lhs.var_state ~rhs:rhs.var_state


let add_critical_pair ~tenv_opt lock_state event ~loc acc =
  let acquisitions = LockState.get_acquisitions lock_state in
  let critical_pair = CriticalPair.make ~loc acquisitions event in
  CriticalPair.filter_out_reentrant_relocks tenv_opt acquisitions critical_pair
  |> Option.value_map ~default:acc ~f:(fun pair -> CriticalPairs.add pair acc)


let acquire ~tenv ({lock_state; critical_pairs} as astate) ~procname ~loc locks =
  { astate with
    critical_pairs=
      (let event = Event.make_acquire locks astate.thread in
       add_critical_pair ~tenv_opt:(Some tenv) lock_state event ~loc critical_pairs )
  ; lock_state=
      List.fold locks ~init:lock_state ~f:(fun acc lock ->
          LockState.acquire ~procname ~loc lock acc ) }


let make_call_with_event new_event ~loc astate =
  if astate.ignore_blocking_calls then astate
  else
    { astate with
      critical_pairs=
        add_critical_pair ~tenv_opt:None astate.lock_state new_event ~loc astate.critical_pairs }


let blocking_call ~callee ~loc astate =
  let new_event = Event.make_blocking_call callee astate.thread in
  make_call_with_event new_event ~loc astate


let ipc ~callee ~loc astate =
  let new_event = Event.make_ipc callee astate.thread in
  make_call_with_event new_event ~loc astate


let wait_on_monitor ~loc formals actuals astate =
  match actuals with
  | exp :: _ ->
      Lock.make formals exp
      |> Option.value_map ~default:astate ~f:(fun lock ->
             let new_event = Event.make_object_wait lock astate.thread in
             make_call_with_event new_event ~loc astate )
  | _ ->
      astate


let future_get ~callee ~loc actuals astate =
  match actuals with
  | HilExp.AccessExpression exp :: _
    when AttributeDomain.find_opt exp astate.attributes
         |> Option.exists ~f:(function Attribute.FutureDoneState x -> x | _ -> false) ->
      astate
  | HilExp.AccessExpression _ :: _ ->
      let new_event = Event.make_blocking_call callee astate.thread in
      make_call_with_event new_event ~loc astate
  | _ ->
      astate


let strict_mode_call ~callee ~loc astate =
  let new_event = Event.make_strict_mode_call callee astate.thread in
  make_call_with_event new_event ~loc astate


let arbitrary_code_execution ~callee ~loc astate =
  let new_event = Event.make_arbitrary_code_exec callee astate.thread in
  make_call_with_event new_event ~loc astate


let release ({lock_state} as astate) locks =
  { astate with
    lock_state= List.fold locks ~init:lock_state ~f:(fun acc l -> LockState.release l acc) }


let add_guard ~acquire_now ~procname ~loc tenv astate guard lock =
  let astate = {astate with guard_map= GuardToLockMap.add_guard ~guard ~lock astate.guard_map} in
  if acquire_now then acquire ~tenv astate ~procname ~loc [lock] else astate


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
         FlatLock.get lock_opt |> Option.to_list |> acquire ~tenv astate ~procname ~loc )


let schedule_work loc thread_constraint astate procname =
  let thread : ThreadDomain.t =
    match (thread_constraint : StarvationModels.scheduler_thread_constraint) with
    | ForUIThread ->
        UIThread
    | ForNonUIThread ->
        BGThread
    | ForUnknownThread ->
        UnknownThread
  in
  let work_item = ScheduledWorkItem.{procname; loc; thread} in
  {astate with scheduled_work= ScheduledWorkDomain.add work_item astate.scheduled_work}


type summary =
  { critical_pairs: CriticalPairs.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t
  ; attributes: AttributeDomain.t
  ; return_attribute: Attribute.t }

let empty_summary : summary =
  { critical_pairs= CriticalPairs.bottom
  ; thread= ThreadDomain.bottom
  ; scheduled_work= ScheduledWorkDomain.bottom
  ; attributes= AttributeDomain.top
  ; return_attribute= Attribute.top }


let pp_summary fmt (summary : summary) =
  F.fprintf fmt
    "{@[<v>thread= %a; return_attributes= %a;@;\
     critical_pairs=%a;@;\
     scheduled_work= %a;@;\
     attributes= %a@]}" ThreadDomain.pp summary.thread Attribute.pp summary.return_attribute
    CriticalPairs.pp summary.critical_pairs ScheduledWorkDomain.pp summary.scheduled_work
    AttributeDomain.pp summary.attributes


let integrate_summary ~tenv ~lhs ~subst callsite (astate : t) (summary : summary) =
  let critical_pairs' =
    CriticalPairs.with_callsite summary.critical_pairs ~tenv ~subst astate.lock_state callsite
      astate.thread
  in
  { astate with
    critical_pairs= CriticalPairs.join astate.critical_pairs critical_pairs'
  ; thread= ThreadDomain.integrate_summary ~caller:astate.thread ~callee:summary.thread
  ; attributes= AttributeDomain.add lhs summary.return_attribute astate.attributes }


let summary_of_astate : Procdesc.t -> t -> summary =
 fun proc_desc astate ->
  let proc_name = Procdesc.get_proc_name proc_desc in
  let attributes =
    let var_predicate =
      match proc_name with
      | Procname.Java jname when Procname.Java.is_class_initializer jname ->
          (* only keep static attributes for the class initializer *)
          fun v -> Var.is_global v
      | Procname.Java jname when Procname.Java.is_constructor jname ->
          (* only keep static attributes or ones that have [this] as their root *)
          fun v -> Var.is_this v || Var.is_global v
      | _ ->
          (* non-constructor/class initializer or non-java, don't keep any attributes *)
          Fn.const false
    in
    AttributeDomain.filter
      (fun exp _ -> HilExp.AccessExpression.get_base exp |> fst |> var_predicate)
      astate.attributes
  in
  let return_attribute =
    let return_var_exp =
      HilExp.AccessExpression.base
        (Var.of_pvar (Pvar.get_ret_pvar proc_name), Procdesc.get_ret_type proc_desc)
    in
    AttributeDomain.find_opt return_var_exp astate.attributes
    |> Option.value ~default:Attribute.Nothing
  in
  { critical_pairs= astate.critical_pairs
  ; thread= astate.thread
  ; scheduled_work= astate.scheduled_work
  ; attributes
  ; return_attribute }


let remove_dead_vars (astate : t) deadvars =
  let deadvars =
    (* The liveness analysis will kill any variable (such as [this]) immediately after its
       last use. This is bad for attributes that need to live until the end of the method,
       so we restrict to SSA variables. *)
    List.rev_filter deadvars ~f:(fun (v : Var.t) ->
        match v with LogicalVar _ -> true | ProgramVar pvar -> Pvar.is_ssa_frontend_tmp pvar )
  in
  let var_state = VarDomain.exit_scope astate.var_state deadvars in
  let attributes = AttributeDomain.exit_scope deadvars astate.attributes in
  {astate with var_state; attributes}


let set_ignore_blocking_calls_flag astate = {astate with ignore_blocking_calls= true}

let fold_critical_pairs_of_summary f (summary : summary) acc =
  CriticalPairs.fold f summary.critical_pairs acc
