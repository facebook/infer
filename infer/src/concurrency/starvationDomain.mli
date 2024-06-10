(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Domain for thread-type. The main goals are

    - Track code paths that are explicitly on UI/BG thread (via annotations, or assertions).
    - Maintain UI/BG-thread-ness through the call stack (if a caller is of unknown status and callee
      is on UI/BG thread then caller must be on the UI/BG thread too).
    - Traces with "UI-thread" status cannot interleave but all other combinations can.
    - Top is AnyThread, which means that there are executions on both UI and BG threads on this
      method.
    - Bottom is UnknownThread, and used as initial state. *)
module ThreadDomain : sig
  type t = UnknownThread | UIThread | BGThread | AnyThread | NamedThread of string

  include AbstractDomain.WithBottom with type t := t
end

(** Abstract address for a lock. There are two notions of equality:

    - Equality for comparing two addresses within the same thread/process/trace. Under this,
      identical globals and identical class objects compare equal. Locks represented by access paths
      rooted at method parameters must have equal access paths to compare equal. Paths rooted at
      locals are ignored.
    - Equality for comparing two addresses in two distinct threads/traces. Globals and class objects
      are compared in the same way, but locks represented by access paths rooted at parameters need
      only have equal access lists (ie [x.f.g == y.f.g]). This allows demonically aliasing
      parameters in *distinct* threads. This relation is used in [may_deadlock]. *)
module Lock : sig
  include module type of AbstractAddress

  val pp_locks : F.formatter -> t -> unit

  val make_java_synchronized : FormalMap.t -> Procname.t -> t option
  (** create the monitor locked when entering a synchronized java method *)

  val compare_wrt_reporting : t -> t -> int
  (** a stable order for avoiding reporting deadlocks twice based on the root variable type *)
end

module AccessExpressionOrConst : sig
  type t = AE of HilExp.AccessExpression.t | Const of Const.t [@@deriving equal]
end

module VarDomain : sig
  include AbstractDomain.WithTop

  type key = Var.t

  val get : key -> t -> AccessExpressionOrConst.t option

  val set : key -> AccessExpressionOrConst.t -> t -> t
end

module Event : sig
  type t =
    | Ipc of {callee: Procname.t; thread: ThreadDomain.t}
    | LockAcquire of
        { locks: Lock.t list
        ; thread: ThreadDomain.t
        ; callsite: CallSite.t option
        ; call_context: Errlog.loc_trace [@compare.ignore] }
    | MayBlock of {callee: Procname.t; thread: ThreadDomain.t}
    | MonitorWait of {lock: Lock.t; thread: ThreadDomain.t}
    | MustNotOccurUnderLock of {callee: Procname.t; thread: ThreadDomain.t}
    | RegexOp of {callee: Procname.t; thread: ThreadDomain.t}
    | StrictModeCall of {callee: Procname.t; thread: ThreadDomain.t}
  [@@deriving compare]

  val describe : F.formatter -> t -> unit

  val get_acquired_locks : t -> Lock.t list
end

(** a lock acquisition with location information *)
module AcquisitionElem : sig
  type t = private
    {lock: Lock.t; loc: Location.t [@compare.ignore]; procname: Procname.t [@compare.ignore]}
  [@@deriving compare]
end

module Acquisition : sig
  type t = private {elem: AcquisitionElem.t; loc: Location.t; trace: CallSite.t list}
end

module LockState : AbstractDomain.WithTop

(** A set of lock acquisitions with source locations and procnames. *)
module Acquisitions : sig
  include PrettyPrintable.PPSet with type elt = Acquisition.t

  val lock_is_held : Lock.t -> t -> bool
  (** is the given lock in the set *)

  val lock_is_held_in_other_thread : Tenv.t -> Lock.t -> t -> bool
  (** is the given lock held, modulo memory abstraction across threads *)
end

(** An event and the currently-held locks at the time it occurred. *)
module CriticalPairElement : sig
  type t = private {acquisitions: Acquisitions.t; event: Event.t}
end

(** A [CriticalPairElement] equipped with a call stack. The intuition is that if we have a critical
    pair `(locks, event)` in the summary of a method then there is a trace of that method where
    `event` occurs, and right before it occurs the locks held are exactly `locks` (no over/under
    approximation). We call it "critical" because the information here alone determines deadlock
    conditions. *)
module CriticalPair : sig
  type t = private {elem: CriticalPairElement.t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val get_loc : t -> Location.t
  (** outermost callsite location *)

  val get_earliest_lock_or_call_loc : procname:Procname.t -> t -> Location.t
  (** outermost callsite location OR lock acquisition *)

  val may_deadlock : Tenv.t -> lhs:t -> lhs_lock:Lock.t -> rhs:t -> Lock.t option
  (** if two pairs can run in parallel and satisfy the conditions for deadlock, when [lhs_lock] of
      [lhs] is involved return the lock involved from [rhs], as [LockAcquire] may involve more than
      one *)

  val make_trace :
    ?header:string -> ?include_acquisitions:bool -> Procname.t -> t -> Errlog.loc_trace

  val is_uithread : t -> bool
  (** is pair about an event on the UI thread *)

  val can_run_in_parallel : t -> t -> bool
  (** can two pairs describe events on two threads that can run in parallel *)

  val with_callsite : t -> CallSite.t -> t

  val apply_caller_thread : ThreadDomain.t -> t -> t option
end

module CriticalPairs : AbstractDomain.FiniteSetS with type elt = CriticalPair.t

module NullLocsCriticalPairs : AbstractDomain.FiniteSetS

module GuardToLockMap : AbstractDomain.WithTop

(** Tracks expression attributes *)
module Attribute : sig
  type t =
    | Nothing
    | ThreadGuard  (** is boolean equivalent to whether on UI thread *)
    | FutureDoneGuard of HilExp.AccessExpression.t  (** boolean equivalent to [Future.isDone()] *)
    | FutureDoneState of bool  (** is a [Future] ready for non-blocking consumption *)
    | Runnable of Procname.t  (** is a Runnable/Callable with given "run" procname *)
    | WorkScheduler of StarvationModels.scheduler_thread_constraint
        (** exp is something that schedules work on the given thread *)
    | Looper of StarvationModels.scheduler_thread_constraint  (** Android looper on given thread *)

  include AbstractDomain.WithTop with type t := t
end

(** Tracks all expressions assigned values of [Attribute] *)
module AttributeDomain : sig
  include
    AbstractDomain.InvertedMapS
      with type key = HilExp.AccessExpression.t
       and type value = Attribute.t

  val is_thread_guard : HilExp.AccessExpression.t -> t -> bool

  val is_future_done_guard : HilExp.AccessExpression.t -> t -> bool
  (** does the given expr has attribute [FutureDone x] return [Some x] else [None] *)
end

(** A record of scheduled parallel work: the method scheduled to run, where, and on what thread. *)
module ScheduledWorkItem : sig
  type t = {procname: Procname.t; loc: Location.t; thread: ThreadDomain.t}

  include PrettyPrintable.PrintableOrderedType with type t := t
end

module ScheduledWorkDomain : AbstractDomain.FiniteSetS with type elt = ScheduledWorkItem.t

module NullLocs : AbstractDomain.InvertedSetS with type elt = HilExp.AccessExpression.t

module LazilyInitialized : AbstractDomain.FiniteSetS with type elt = HilExp.AccessExpression.t

type t =
  { ignore_blocking_calls: bool
  ; guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: NullLocsCriticalPairs.t
  ; attributes: AttributeDomain.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t
  ; var_state: VarDomain.t
  ; null_locs: NullLocs.t
  ; lazily_initalized: LazilyInitialized.t }

include AbstractDomain.S with type t := t

val initial : t
(** initial domain state *)

val acquire : tenv:Tenv.t -> t -> procname:Procname.t -> loc:Location.t -> Lock.t list -> t
(** simultaneously acquire a number of locks, no-op if list is empty *)

val release : t -> Lock.t list -> t
(** simultaneously release a number of locks, no-op if list is empty *)

val blocking_call : callee:Procname.t -> loc:Location.t -> t -> t

val ipc : callee:Procname.t -> loc:Location.t -> t -> t

val wait_on_monitor : loc:Location.t -> FormalMap.t -> HilExp.t list -> t -> t

val future_get : callee:Procname.t -> loc:Location.t -> HilExp.t list -> t -> t

val regex_op : callee:Procname.t -> loc:Location.t -> t -> t

val strict_mode_call : callee:Procname.t -> loc:Location.t -> t -> t

val arbitrary_code_execution : callee:Procname.t -> loc:Location.t -> t -> t

val add_guard :
     acquire_now:bool
  -> procname:Procname.t
  -> loc:Location.t
  -> Tenv.t
  -> t
  -> HilExp.t
  -> Lock.t
  -> t
(** Install a mapping from the guard expression to the lock provided, and optionally lock it. *)

val lock_guard : procname:Procname.t -> loc:Location.t -> Tenv.t -> t -> HilExp.t -> t
(** Acquire the lock the guard was constructed with. *)

val remove_guard : t -> HilExp.t -> t
(** Destroy the guard and release its lock. *)

val unlock_guard : t -> HilExp.t -> t
(** Release the lock the guard was constructed with. *)

val schedule_work :
  Location.t -> StarvationModels.scheduler_thread_constraint -> t -> Procname.t -> t
(** record the fact that a method is scheduled to run on a certain thread/executor *)

type summary =
  { critical_pairs: CriticalPairs.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t
  ; lock_state: LockState.t
  ; attributes: AttributeDomain.t  (** final-state attributes that affect instance variables only *)
  ; return_attribute: Attribute.t }

val empty_summary : summary

val pp_summary : F.formatter -> summary -> unit

val integrate_summary :
     tenv:Tenv.t
  -> procname:Procname.t
  -> lhs:HilExp.AccessExpression.t
  -> subst:Lock.subst
  -> FormalMap.t
  -> CallSite.t
  -> t
  -> summary
  -> t
(** apply a callee summary to the current abstract state; [lhs] is the expression assigned the
    returned value, if any *)

val summary_of_astate : Procdesc.t -> t -> summary

val set_ignore_blocking_calls_flag : t -> t

val remove_dead_vars : t -> Var.t list -> t

val fold_critical_pairs_of_summary : (CriticalPair.t -> 'a -> 'a) -> summary -> 'a -> 'a

val null_check : FormalMap.t -> HilExp.t -> t -> t
(** if expression is a heap location, mark it as null in this branch *)

val set_non_null : FormalMap.t -> HilExp.AccessExpression.t -> t -> t
(** if expression is a heap location, mark it as set to non-null in this branch *)
