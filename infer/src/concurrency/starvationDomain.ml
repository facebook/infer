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
  type t = UnknownThread | UIThread | BGThread | AnyThread | NamedThread of string
  [@@deriving compare, equal, show {with_path= false}]

  let bottom = UnknownThread

  let is_bottom = function UnknownThread -> true | _ -> false

  let join lhs rhs =
    match (lhs, rhs) with
    | UnknownThread, other | other, UnknownThread ->
        other
    | UIThread, UIThread | BGThread, BGThread ->
        lhs
    | NamedThread l, NamedThread r when String.equal l r ->
        lhs
    | _, _ ->
        AnyThread


  (* type is just an int, so use [join] to define [leq] *)
  let leq ~lhs ~rhs = equal (join lhs rhs) rhs

  let widen ~prev ~next ~num_iters:_ = join prev next

  (** Can two thread statuses occur in parallel? [UIThread, UIThread] and [NamedThread] of the same
      name are forbidden. In addition, this is monotonic wrt the lattice (increasing either argument
      cannot transition from true to false). *)
  let can_run_in_parallel st1 st2 =
    match (st1, st2) with
    | UIThread, UIThread ->
        false
    | NamedThread l, NamedThread r ->
        not (String.equal l r)
    | _, _ ->
        true


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
    (* annotations or assertions are incorrectly used in code, or callee is path-sensitive on
       thread-identity, just drop the callee pair *)
    | UIThread, BGThread
    | BGThread, UIThread
    | UIThread, NamedThread _
    | BGThread, NamedThread _
    | NamedThread _, UIThread
    | NamedThread _, BGThread ->
        None
    | NamedThread l, NamedThread r when not (String.equal l r) ->
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
    (* We default to recursive if the type can't be found or looks malformed.
       This reduces self-deadlock FPs. *)
    match get_typ tenv lock with
    | Some {Typ.desc= Tptr ({desc= Tstruct name}, _) | Tstruct name} ->
        ConcurrencyModels.is_recursive_lock_type name
    | Some typ ->
        (* weird type passed as a lock, return default *)
        L.debug Analysis Verbose "Asked if non-struct type %a is a recursive lock type.@\n"
          (Typ.pp_full Pp.text) typ ;
        true
    | None ->
        (* could not find type definition, return default *)
        L.debug Analysis Verbose "Could not resolve type for lock %a.@\n" pp lock ;
        true
end

module AccessExpressionOrConst = struct
  type t = AE of HilExp.AccessExpression.t | Const of Const.t [@@deriving equal]

  let pp fmt = function
    | AE exp ->
        HilExp.AccessExpression.pp fmt exp
    | Const c ->
        Const.pp Pp.text fmt c
end

module AccessExpressionDomain = struct
  open AbstractDomain.Types

  type t = AccessExpressionOrConst.t top_lifted [@@deriving equal]

  let pp fmt = function
    | Top ->
        F.pp_print_string fmt "AccExpTop"
    | NonTop lock ->
        AccessExpressionOrConst.pp fmt lock


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
            | NonTop (Const _) ->
                true
            | NonTop (AE acc_exp) ->
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
    | LockAcquire of
        { locks: Lock.t list
        ; thread: ThreadDomain.t
              (* callsite and call_context indicate that this lock was acquired 
               * through an interprocedrual call, otherwise they are None/[] *)
        ; callsite: CallSite.t option
        ; call_context: Errlog.loc_trace [@compare.ignore] }
    | MayBlock of {callee: Procname.t; thread: ThreadDomain.t}
    | MonitorWait of {lock: Lock.t; thread: ThreadDomain.t}
    | MustNotOccurUnderLock of {callee: Procname.t; thread: ThreadDomain.t}
    | RegexOp of {callee: Procname.t; thread: ThreadDomain.t}
    | StrictModeCall of {callee: Procname.t; thread: ThreadDomain.t}
  [@@deriving compare]

  let pp fmt = function
    | Ipc {callee; thread} ->
        F.fprintf fmt "Ipc(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | LockAcquire {locks; thread; callsite= None} ->
        F.fprintf fmt "LockAcquire(%a, %a)"
          (PrettyPrintable.pp_collection ~pp_item:Lock.pp)
          locks ThreadDomain.pp thread
    | LockAcquire {callsite= Some callsite; locks; thread} ->
        F.fprintf fmt "Interprocedural LockAcquire(%a, %a) at %a"
          (PrettyPrintable.pp_collection ~pp_item:Lock.pp)
          locks ThreadDomain.pp thread CallSite.pp callsite
    | MayBlock {callee; thread} ->
        F.fprintf fmt "MayBlock(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | MonitorWait {lock; thread} ->
        F.fprintf fmt "MonitorWait(%a, %a)" Lock.pp lock ThreadDomain.pp thread
    | MustNotOccurUnderLock {callee; thread} ->
        F.fprintf fmt "MustNotOccurUnderLock(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | RegexOp {callee; thread} ->
        F.fprintf fmt "RegexOp(%a, %a)" Procname.pp callee ThreadDomain.pp thread
    | StrictModeCall {callee; thread} ->
        F.fprintf fmt "StrictModeCall(%a, %a)" Procname.pp callee ThreadDomain.pp thread


  let describe fmt elem =
    match elem with
    | LockAcquire {locks; callsite= None} ->
        Pp.comma_seq Lock.pp_locks fmt locks
    | LockAcquire {locks; callsite= Some callsite} ->
        F.fprintf fmt "%a locked at %a" (Pp.comma_seq Lock.pp_locks) locks CallSite.pp callsite
    | Ipc {callee}
    | MayBlock {callee}
    | MustNotOccurUnderLock {callee}
    | RegexOp {callee}
    | StrictModeCall {callee} ->
        F.fprintf fmt "calls %a" describe_pname callee
    | MonitorWait {lock} ->
        F.fprintf fmt "calls `wait` on %a" Lock.describe lock


  let get_thread = function
    | Ipc {thread}
    | LockAcquire {thread}
    | MayBlock {thread}
    | MonitorWait {thread}
    | MustNotOccurUnderLock {thread}
    | RegexOp {thread}
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
      | RegexOp regex_op ->
          RegexOp {regex_op with thread}
      | StrictModeCall strict_mode_call ->
          StrictModeCall {strict_mode_call with thread}


  let apply_caller_thread caller_thread event =
    match ThreadDomain.apply_caller_thread ~caller:caller_thread ~callee:(get_thread event) with
    | None ->
        None
    | Some thread ->
        Some (with_thread event thread)


  let make_acquire locks thread = LockAcquire {locks; thread; call_context= []; callsite= None}

  let make_interprocedural_acquire callsite locks thread call_context =
    let callsite = Some callsite in
    LockAcquire {callsite; locks; thread; call_context}


  let make_arbitrary_code_exec callee thread = MustNotOccurUnderLock {callee; thread}

  let make_blocking_call callee thread = MayBlock {callee; thread}

  let make_ipc callee thread = Ipc {callee; thread}

  let make_object_wait lock thread = MonitorWait {lock; thread}

  let make_regex_op callee thread = RegexOp {callee; thread}

  let make_strict_mode_call callee thread = StrictModeCall {callee; thread}

  let get_acquired_locks = function LockAcquire {locks} -> locks | _ -> []

  let apply_subst subst event =
    match event with
    | Ipc _ | MayBlock _ | MustNotOccurUnderLock _ | RegexOp _ | StrictModeCall _ ->
        Some event
    | MonitorWait {lock; thread} -> (
      match Lock.apply_subst subst lock with
      | None ->
          None
      | Some lock' when phys_equal lock lock' ->
          Some event
      | Some lock ->
          Some (MonitorWait {lock; thread}) )
    | LockAcquire {locks; thread; callsite= None; call_context= []} -> (
      match Lock.apply_subst_to_list subst locks with
      | [] ->
          None
      | locks' when phys_equal locks locks' ->
          Some event
      | locks ->
          Some (LockAcquire {locks; thread; callsite= None; call_context= []}) )
    (* Don't do substitution if inter procedural lock acquire *)
    | LockAcquire {callsite= _; call_context= _} ->
        Some event


  let has_recursive_lock tenv event =
    get_acquired_locks event |> List.exists ~f:(Lock.is_recursive tenv)


  let is_blocking_call = function
    | LockAcquire _ | MustNotOccurUnderLock _ ->
        (* lock taking is not a method call (though it may block) and [MustNotOccurUnderLock] calls not necessarily blocking *)
        false
    | Ipc _ | MayBlock _ | MonitorWait _ | RegexOp _ | StrictModeCall _ ->
        true
end

(** A lock acquisition with source location and procname in which it occurs. The location & procname
    are *ignored* for comparisons, and are only for reporting. *)
module AcquisitionElem = struct
  type t = {lock: Lock.t; loc: Location.t [@compare.ignore]; procname: Procname.t [@compare.ignore]}
  [@@deriving compare]

  let pp fmt {lock; loc; procname} =
    F.fprintf fmt "<@[lock=%a;@;loc=%a;@;procname=%a@]>" Lock.pp lock Location.pp loc Procname.pp
      procname


  let describe fmt {lock} = Lock.pp_locks fmt lock

  let make ~procname ~loc lock = {lock; loc; procname}

  let compare_loc {loc= loc1} {loc= loc2} = Location.compare loc1 loc2

  let make_dummy lock = {lock; loc= Location.dummy; procname= Procname.from_string_c_fun ""}

  let apply_subst subst acquisition =
    match Lock.apply_subst subst acquisition.lock with
    | None ->
        None
    | Some lock when phys_equal acquisition.lock lock ->
        Some acquisition
    | Some lock ->
        Some {acquisition with lock}
end

module Acquisition = struct
  include
    ExplicitTrace.MakeTraceElemModuloLocation (AcquisitionElem) (ExplicitTrace.DefaultCallPrinter)

  let make_dummy lock = make (AcquisitionElem.make_dummy lock) Location.dummy

  let make ~procname ~loc lock = make (AcquisitionElem.make ~procname ~loc lock) loc

  let compare lhs rhs = AcquisitionElem.compare lhs.elem rhs.elem

  let apply_subst subst interproc_acquisition =
    match AcquisitionElem.apply_subst subst interproc_acquisition.elem with
    | None ->
        None
    | Some elem' ->
        Some (map ~f:(fun _elem -> elem') interproc_acquisition)


  let compare_loc a1 a2 = AcquisitionElem.compare_loc a1.elem a2.elem

  let make_trace_step = make_loc_trace

  let with_callsite_at_proc ~procname acq callsite =
    with_callsite acq callsite |> map ~f:(fun elem -> {elem with procname})
end

(** Set of acquisitions; due to order over acquisitions, each lock appears at most once. *)
module Acquisitions = struct
  include PrettyPrintable.MakePPSet (Acquisition)

  (* use the fact that location/procname are ignored in comparisons *)
  let lock_is_held lock acquisitions = mem (Acquisition.make_dummy lock) acquisitions

  let lock_is_held_in_other_thread tenv lock acquisitions =
    exists (fun acq -> Lock.equal_across_threads tenv lock acq.elem.lock) acquisitions


  let no_locks_common_across_threads tenv acqs1 acqs2 =
    for_all (fun acq1 -> not (lock_is_held_in_other_thread tenv acq1.elem.lock acqs2)) acqs1


  let apply_subst subst acqs =
    fold
      (fun acq acc ->
        match Acquisition.apply_subst subst acq with None -> acc | Some acq' -> add acq' acc )
      acqs empty
end

module LockState : sig
  include AbstractDomain.WithTop

  val acquire : procname:Procname.t -> loc:Location.t -> Lock.t -> t -> t

  val integrate_summary :
    procname:Procname.t -> callsite:CallSite.t -> subst:AbstractAddress.subst -> other:t -> t -> t

  val release : Lock.t -> t -> t

  val get_acquisitions : t -> Acquisitions.t
end = struct
  (* abstraction limit for lock counts *)
  let max_lock_depth_allowed = 5

  module LockCount = AbstractDomain.DownwardIntDomain (struct
    let max = max_lock_depth_allowed
  end)

  module UnlockCount = AbstractDomain.CountDomain (struct
    let max = max_lock_depth_allowed
  end)

  module HeldMap = AbstractDomain.InvertedMap (Lock) (LockCount)
  module UnlockedMap = AbstractDomain.InvertedMap (Lock) (UnlockCount)

  (* [held] has the locks currently held.
   * [unlocked] The lock function has unlocked (without locking them) 
   * [acquisitions] has the currently held locks, so as to avoid a linear fold in [get_acquisitions].
     This should also increase sharing across returned values from [get_acquisitions]. *)
  type t = {held: HeldMap.t; unlocked: UnlockedMap.t; acquisitions: Acquisitions.t}

  let get_acquisitions {acquisitions} = acquisitions

  let pp fmt {held; unlocked; acquisitions} =
    F.fprintf fmt "{@[map= %a;@;unlocked= %a;@;acquisitions= %a@]}" HeldMap.pp held UnlockedMap.pp
      unlocked Acquisitions.pp acquisitions


  let join lhs rhs =
    let held = HeldMap.join lhs.held rhs.held in
    let unlocked = UnlockedMap.join lhs.unlocked rhs.unlocked in
    let acquisitions = Acquisitions.inter lhs.acquisitions rhs.acquisitions in
    {held; unlocked; acquisitions}


  let widen ~prev ~next ~num_iters =
    let held = HeldMap.widen ~prev:prev.held ~next:next.held ~num_iters in
    let unlocked = UnlockedMap.widen ~prev:prev.unlocked ~next:next.unlocked ~num_iters in
    let acquisitions = Acquisitions.inter prev.acquisitions next.acquisitions in
    {held; unlocked; acquisitions}


  let leq ~lhs ~rhs = HeldMap.leq ~lhs:lhs.held ~rhs:rhs.held

  let top = {held= HeldMap.top; unlocked= UnlockedMap.top; acquisitions= Acquisitions.empty}

  let is_top {held} = HeldMap.is_top held

  let acquire' acquisition lock {held; acquisitions; unlocked} =
    let should_add_acquisition = ref false in
    let held =
      HeldMap.update lock
        (function
          | None ->
              (* lock was not already held, so add it to [acquisitions] *)
              should_add_acquisition := true ;
              Some LockCount.(increment top)
          | Some count ->
              Some (LockCount.increment count) )
        held
    in
    let unlocked =
      UnlockedMap.update lock
        (function
          | None ->
              None
          | Some count ->
              let new_count = UnlockCount.decrement count in
              if UnlockCount.is_bottom new_count then None else Some new_count )
        unlocked
    in
    let acquisitions =
      if !should_add_acquisition then Acquisitions.add acquisition acquisitions else acquisitions
    in
    {held; unlocked; acquisitions}


  let acquire ~procname ~loc lock =
    let maybe_added_acquisition = Acquisition.make ~procname ~loc lock in
    acquire' maybe_added_acquisition lock


  let release lock {held; unlocked; acquisitions} =
    let unbalanced_unlock = ref true in
    let should_remove_acquisition = ref false in
    let held =
      HeldMap.update lock
        (function
          | None ->
              None
          | Some count ->
              unbalanced_unlock := false ;
              let new_count = LockCount.decrement count in
              if LockCount.is_top new_count then (
                should_remove_acquisition := true ;
                None )
              else Some new_count )
        held
    in
    let unlocked =
      if !unbalanced_unlock then
        UnlockedMap.update lock
          (function
            | None ->
                Some UnlockCount.(increment bottom)
            | Some count ->
                Some (UnlockCount.increment count) )
          unlocked
      else unlocked
    in
    let acquisitions =
      if !should_remove_acquisition then
        let acquisition = Acquisition.make_dummy lock in
        Acquisitions.remove acquisition acquisitions
      else acquisitions
    in
    {held; unlocked; acquisitions}


  let integrate_summary ~procname ~callsite ~subst ~other lock_state =
    (* Release all locks that were unlocked by summary *)
    let subst_lock lock =
      match Lock.apply_subst subst lock with Some lock' -> lock' | None -> lock
    in
    let rec unlocked_folder lock other_count lock_state =
      if UnlockCount.is_bottom other_count then lock_state
      else
        unlocked_folder lock
          (UnlockCount.decrement other_count)
          (release (subst_lock lock) lock_state)
    in
    let lock_state = UnlockedMap.fold unlocked_folder other.unlocked lock_state in
    (* acquire all locks that are held in the summary, adding the callsite to the acquisitions *)
    let rec held_folder lock other_count lock_state =
      if LockCount.is_top other_count then lock_state
      else
        let acquisition = Acquisitions.find (Acquisition.make_dummy lock) other.acquisitions in
        let acquisition = Acquisition.with_callsite_at_proc ~procname acquisition callsite in
        let acquisition =
          match Acquisition.apply_subst subst acquisition with
          | None ->
              acquisition
          | Some acq ->
              acq
        in
        let lock_state = acquire' acquisition (subst_lock lock) lock_state in
        held_folder lock (LockCount.decrement other_count) lock_state
    in
    let lock_state = HeldMap.fold held_folder other.held lock_state in
    lock_state
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


  let is_blocking_call elt = Event.is_blocking_call elt.event
end

module CriticalPair = struct
  include
    ExplicitTrace.MakeTraceElemModuloLocation
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


  (* If any locks in critical pair are still held at the end of a funciton, it means lock is unbalanced and the critical pair assumptions don't hold *)
  let is_balanced (lock_state : LockState.t) ({elem= {acquisitions; event}} : t) =
    let still_held_locks = LockState.get_acquisitions lock_state in
    let unbalanced_lock_in_event =
      List.exists
        ~f:(fun lock -> Acquisitions.lock_is_held lock still_held_locks)
        (Event.get_acquired_locks event)
    in
    let balanced_lock_in_acquisitions = Acquisitions.disjoint still_held_locks acquisitions in
    (not unbalanced_lock_in_event) && balanced_lock_in_acquisitions


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
            Some (map pair ~f:(fun elem -> {elem with event= Event.make_acquire locks thread})) )
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


  let is_blocking_call pair = CriticalPairElement.is_blocking_call pair.elem

  let integrate_summary_opt ~subst ~tenv ~ignore_blocking_calls existing_acquisitions call_site
      (caller_thread : ThreadDomain.t) (callee_pair : t) =
    if ignore_blocking_calls && is_blocking_call callee_pair then None
    else
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
      (fun {elem= {procname= acq_procname; loc= acq_loc}} acc ->
        if Procname.equal procname acq_procname && Int.is_negative (Location.compare acq_loc acc)
        then acq_loc
        else acc )
      acquisitions initial_loc


  let make_trace ?(header = "") ?(include_acquisitions = true) top_pname
      ({elem= {acquisitions; event}; trace; loc} as pair) =
    let acquisitions_map =
      if include_acquisitions then
        Acquisitions.fold
          (fun ({elem= {procname}} as acq : Acquisition.t) acc ->
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
        |> List.concat
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
    let endpoint_steps =
      match event with
      | LockAcquire {callsite= Some _; call_context} as event ->
          let endpoint_descr = F.asprintf "%a" Event.describe event in
          Errlog.make_trace_element 0 loc endpoint_descr [] :: call_context
      | _ ->
          let endpoint_descr = F.asprintf "%a" Event.describe event in
          [Errlog.make_trace_element 0 loc endpoint_descr []]
    in
    List.concat (([header_step] :: call_stack) @ [endpoint_steps])


  let is_uithread t = ThreadDomain.is_uithread (get_thread t)

  let can_run_in_parallel t1 t2 = ThreadDomain.can_run_in_parallel (get_thread t1) (get_thread t2)
end

module CriticalPairs = CriticalPair.FiniteSet
module NullLocs = AbstractDomain.InvertedSet (HilExp.AccessExpression)
module LazilyInitialized = AbstractDomain.FiniteSet (HilExp.AccessExpression)

(** A critical pair qualified by the set of locations known to be [null] at pair creation time. A
    blocking call pair will be dropped from the summary if any of these locations are lazily
    initialised. *)
module NullLocsCriticalPair = struct
  type t = {null_locs: NullLocs.t; pair: CriticalPair.t} [@@deriving compare]

  let pp fmt {null_locs; pair} =
    F.fprintf fmt "@[null_checks= %a; pair= %a@]" NullLocs.pp null_locs CriticalPair.pp pair
end

module NullLocsCriticalPairs = struct
  include AbstractDomain.FiniteSet (NullLocsCriticalPair)

  let with_callsite astate ~tenv ~subst ~ignore_blocking_calls lock_state null_locs call_site thread
      =
    let existing_acquisitions = LockState.get_acquisitions lock_state in
    CriticalPairs.fold
      (fun pair acc ->
        CriticalPair.integrate_summary_opt ~subst ~tenv ~ignore_blocking_calls existing_acquisitions
          call_site thread pair
        |> Option.bind
             ~f:(CriticalPair.filter_out_reentrant_relocks (Some tenv) existing_acquisitions)
        |> Option.value_map ~default:acc ~f:(fun pair -> add {null_locs; pair} acc) )
      astate empty


  let to_critical_pairs still_held_lock lazily_initialized set =
    fold
      (fun {null_locs; pair} acc ->
        if
          (not (CriticalPair.is_balanced still_held_lock pair))
          || CriticalPair.is_blocking_call pair
             && (* drop a blocking call if it's qualified by a [null] location that was lazily
                   initialised at some point in this function *)
             NullLocs.exists
               (fun acc_exp -> LazilyInitialized.mem acc_exp lazily_initialized)
               null_locs
        then acc
        else CriticalPairs.add pair acc )
      set CriticalPairs.empty
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
        match c with ForUIThread -> "UI" | ForNonUIThread -> "BG" | ForUnknownThread -> "Unknown" )
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
  ; critical_pairs: NullLocsCriticalPairs.t
  ; attributes: AttributeDomain.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t
  ; var_state: VarDomain.t
  ; null_locs: NullLocs.t
  ; lazily_initalized: LazilyInitialized.t }
[@@deriving abstract_domain]

let initial =
  { ignore_blocking_calls= false
  ; guard_map= GuardToLockMap.empty
  ; lock_state= LockState.top
  ; critical_pairs= NullLocsCriticalPairs.empty
  ; attributes= AttributeDomain.empty
  ; thread= ThreadDomain.bottom
  ; scheduled_work= ScheduledWorkDomain.bottom
  ; var_state= VarDomain.top
  ; null_locs= NullLocs.empty
  ; lazily_initalized= LazilyInitialized.empty }


let pp fmt astate =
  F.fprintf fmt
    "{@[guard_map= %a;@;\
     lock_state= %a;@;\
     critical_pairs= %a;@;\
     attributes= %a;@;\
     thread= %a;@;\
     scheduled_work= %a;@;\
     var_state= %a;@;\
     null_locs= %a\n\
     lazily_initialized= %a\n\
    \     @]}" GuardToLockMap.pp astate.guard_map LockState.pp astate.lock_state
    NullLocsCriticalPairs.pp astate.critical_pairs AttributeDomain.pp astate.attributes
    ThreadDomain.pp astate.thread ScheduledWorkDomain.pp astate.scheduled_work VarDomain.pp
    astate.var_state NullLocs.pp astate.null_locs LazilyInitialized.pp astate.lazily_initalized


let add_critical_pair ~tenv_opt lock_state null_locs event ~loc acc =
  let acquisitions = LockState.get_acquisitions lock_state in
  let critical_pair = CriticalPair.make ~loc acquisitions event in
  CriticalPair.filter_out_reentrant_relocks tenv_opt acquisitions critical_pair
  |> Option.value_map ~default:acc ~f:(fun pair -> NullLocsCriticalPairs.add {null_locs; pair} acc)


let interproc_acquire ~tenv ~procname ~callsite ~subst summary_lock_state astate =
  let new_acquisitions = LockState.get_acquisitions summary_lock_state in
  let critical_pairs =
    Acquisitions.fold
      (fun acquisition acc ->
        let loc = CallSite.loc callsite in
        let event =
          let lock =
            Lock.apply_subst subst acquisition.elem.lock
            |> Option.value ~default:acquisition.elem.lock
          in
          Event.make_interprocedural_acquire callsite [lock] astate.thread
            (Acquisition.make_loc_trace acquisition)
        in
        add_critical_pair ~tenv_opt:(Some tenv) astate.lock_state astate.null_locs event ~loc acc )
      new_acquisitions astate.critical_pairs
  in
  let lock_state =
    LockState.integrate_summary ~procname ~callsite ~subst ~other:summary_lock_state
      astate.lock_state
  in
  {astate with critical_pairs; lock_state}


let acquire ~tenv ({lock_state; critical_pairs; null_locs} as astate) ~procname ~loc locks =
  { astate with
    critical_pairs=
      (let event = Event.make_acquire locks astate.thread in
       add_critical_pair ~tenv_opt:(Some tenv) lock_state null_locs event ~loc critical_pairs )
  ; lock_state=
      List.fold locks ~init:lock_state ~f:(fun acc lock ->
          LockState.acquire ~procname ~loc lock acc ) }


let make_call_with_event new_event ~loc astate =
  if astate.ignore_blocking_calls then astate
  else
    { astate with
      critical_pairs=
        add_critical_pair ~tenv_opt:None astate.lock_state astate.null_locs new_event ~loc
          astate.critical_pairs }


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


let regex_op ~callee ~loc astate =
  let new_event = Event.make_regex_op callee astate.thread in
  make_call_with_event new_event ~loc astate


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
  ; lock_state: LockState.t
  ; attributes: AttributeDomain.t
  ; return_attribute: Attribute.t }

let empty_summary : summary =
  { critical_pairs= CriticalPairs.bottom
  ; thread= ThreadDomain.bottom
  ; scheduled_work= ScheduledWorkDomain.bottom
  ; lock_state= LockState.top
  ; attributes= AttributeDomain.top
  ; return_attribute= Attribute.top }


let pp_summary fmt (summary : summary) =
  F.fprintf fmt
    "{@[<v>thread= %a; return_attributes= %a;@;\
     critical_pairs=%a;@;\
     scheduled_work= %a;@;\
     lock_state= %a;@;\
     attributes= %a@]}" ThreadDomain.pp summary.thread Attribute.pp summary.return_attribute
    CriticalPairs.pp summary.critical_pairs ScheduledWorkDomain.pp summary.scheduled_work
    LockState.pp summary.lock_state AttributeDomain.pp summary.attributes


let is_heap_loc formals acc_exp =
  match HilExp.AccessExpression.get_base acc_exp with
  | (ProgramVar pvar, _) as base when Pvar.is_global pvar || FormalMap.is_formal base formals ->
      true
  | _ ->
      false


let set_non_null formals acc_exp astate =
  if is_heap_loc formals acc_exp && NullLocs.mem acc_exp astate.null_locs then
    { astate with
      null_locs= NullLocs.remove acc_exp astate.null_locs
    ; lazily_initalized= LazilyInitialized.add acc_exp astate.lazily_initalized }
  else astate


let integrate_summary ~tenv ~procname ~lhs ~subst formals callsite (astate : t) (summary : summary)
    =
  let critical_pairs' =
    NullLocsCriticalPairs.with_callsite summary.critical_pairs ~tenv ~subst astate.lock_state
      astate.null_locs callsite astate.thread ~ignore_blocking_calls:astate.ignore_blocking_calls
  in
  (* apply summary held locks *)
  let astate = interproc_acquire ~procname ~callsite ~subst ~tenv summary.lock_state astate in
  let astate =
    { astate with
      critical_pairs= NullLocsCriticalPairs.join astate.critical_pairs critical_pairs'
    ; thread= ThreadDomain.integrate_summary ~caller:astate.thread ~callee:summary.thread
    ; attributes= AttributeDomain.add lhs summary.return_attribute astate.attributes }
  in
  (* optimistically assume non-null return values, for lazy-init detection purposes *)
  set_non_null formals lhs astate


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
  (* Interprocedural handling of guards is not implemented, so we remove guarded locks from
     summary *)
  let astate_without_guard =
    GuardToLockMap.fold (fun guard _lock acc -> unlock_guard acc guard) astate.guard_map astate
  in
  { critical_pairs=
      NullLocsCriticalPairs.to_critical_pairs astate.lock_state astate.lazily_initalized
        astate.critical_pairs
  ; thread= astate.thread
  ; scheduled_work= astate.scheduled_work
  ; lock_state= astate_without_guard.lock_state
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


let null_check formals exp astate =
  match HilExp.get_access_exprs exp with
  | [acc_exp] when is_heap_loc formals acc_exp ->
      {astate with null_locs= NullLocs.add acc_exp astate.null_locs}
  | _ ->
      astate
