(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Domain for thread-type.  The main goals are 
    - Track code paths that are explicitly on UI/BG thread (via annotations, or assertions). 
    - Maintain UI/BG-thread-ness through the call stack (if a caller is of unknown status and 
      callee is on UI/BG thread then caller must be on the UI/BG thread too). 
    - Traces with "UI-thread" status cannot interleave but all other combinations can.  
    - Top is AnyThread, which means that there are executions on both UI and BG threads on 
      this method.
    - Bottom is UnknownThread, and used as initial state.
*)
module ThreadDomain : sig
  type t = UnknownThread | UIThread | BGThread | AnyThread

  include AbstractDomain.WithBottom with type t := t
end

(** Abstraction of a path that represents a lock, special-casing comparison
    to work over type, base variable modulo this and access list *)
module Lock : sig
  include PrettyPrintable.PrintableOrderedType with type t = AccessPath.t

  val owner_class : t -> Typ.name option
  (** Class of the root variable of the path representing the lock *)

  val pp_locks : F.formatter -> t -> unit

  val describe : F.formatter -> t -> unit
end

module Event : sig
  type t =
    | LockAcquire of Lock.t
    | MayBlock of (string * StarvationModels.severity)
    | StrictModeCall of string
  [@@deriving compare]

  val describe : F.formatter -> t -> unit
end

module LockState : AbstractDomain.WithTop

(** A set of lock acquisitions with source locations and procnames. *)
module Acquisitions : sig
  type t

  val lock_is_held : Lock.t -> t -> bool
  (** is the given lock in the set *)
end

(** An event and the currently-held locks at the time it occurred. *)
module CriticalPairElement : sig
  type t = private {acquisitions: Acquisitions.t; event: Event.t; thread: ThreadDomain.t}
end

(** A [CriticalPairElement] equipped with a call stack. 
    The intuition is that if we have a critical pair `(locks, event)` in the summary 
    of a method then there is a trace of that method where `event` occurs, and right 
    before it occurs the locks held are exactly `locks` (no over/under approximation).
    We call it "critical" because the information here alone determines deadlock conditions. 
*)
module CriticalPair : sig
  type t = private {elem: CriticalPairElement.t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val get_loc : t -> Location.t
  (** outermost callsite location *)

  val get_earliest_lock_or_call_loc : procname:Typ.Procname.t -> t -> Location.t
  (** outermost callsite location OR lock acquisition *)

  val may_deadlock : t -> t -> bool
  (** two pairs can run in parallel and satisfy the conditions for deadlock *)

  val make_trace :
    ?header:string -> ?include_acquisitions:bool -> Typ.Procname.t -> t -> Errlog.loc_trace

  val is_uithread : t -> bool
  (** is pair about an event on the UI thread *)

  val can_run_in_parallel : t -> t -> bool
  (** can two pairs describe events on two threads that can run in parallel *)
end

module CriticalPairs : AbstractDomain.FiniteSetS with type elt = CriticalPair.t

module GuardToLockMap : AbstractDomain.WithTop

(** tracks whether an expression has been tested for whether we execute on UI thread *)
module BranchGuard : sig
  type t = Nothing | Thread

  include AbstractDomain.WithTop with type t := t
end

(** tracks all expressions currently known to have been tested for conditions in [BranchGuard] *)
module BranchGuardDomain : sig
  include
    AbstractDomain.InvertedMapS
      with type key = HilExp.AccessExpression.t
       and type value = BranchGuard.t

  val is_thread_guard : HilExp.AccessExpression.t -> t -> bool
end

(** A record of scheduled parallel work: the method scheduled to run, where, and on what thread. *)
module ScheduledWorkItem : sig
  type t = {procname: Typ.Procname.t; loc: Location.t; thread: ThreadDomain.t}

  include PrettyPrintable.PrintableOrderedType with type t := t
end

module ScheduledWorkDomain : AbstractDomain.FiniteSetS with type elt = ScheduledWorkItem.t

type t =
  { guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: CriticalPairs.t
  ; branch_guards: BranchGuardDomain.t
  ; thread: ThreadDomain.t
  ; scheduled_work: ScheduledWorkDomain.t }

include AbstractDomain.WithBottom with type t := t

val acquire : Tenv.t -> t -> procname:Typ.Procname.t -> loc:Location.t -> Lock.t list -> t
(** simultaneously acquire a number of locks, no-op if list is empty *)

val release : t -> Lock.t list -> t
(** simultaneously release a number of locks, no-op if list is empty *)

val blocking_call : callee:Typ.Procname.t -> StarvationModels.severity -> loc:Location.t -> t -> t

val strict_mode_call : callee:Typ.Procname.t -> loc:Location.t -> t -> t

val add_guard :
     acquire_now:bool
  -> procname:Typ.Procname.t
  -> loc:Location.t
  -> Tenv.t
  -> t
  -> HilExp.t
  -> Lock.t
  -> t
(** Install a mapping from the guard expression to the lock provided, and optionally lock it. *)

val lock_guard : procname:Typ.Procname.t -> loc:Location.t -> Tenv.t -> t -> HilExp.t -> t
(** Acquire the lock the guard was constructed with. *)

val remove_guard : t -> HilExp.t -> t
(** Destroy the guard and release its lock. *)

val unlock_guard : t -> HilExp.t -> t
(** Release the lock the guard was constructed with. *)

val schedule_work :
  Location.t -> StarvationModels.executor_thread_constraint -> t -> Typ.Procname.t -> t
(** record the fact that a method is scheduled to run on a certain thread/executor *)

type summary =
  {critical_pairs: CriticalPairs.t; thread: ThreadDomain.t; scheduled_work: ScheduledWorkDomain.t}

val pp_summary : F.formatter -> summary -> unit

val integrate_summary : Tenv.t -> CallSite.t -> t -> summary -> t

val summary_of_astate : t -> summary

val filter_blocking_calls : t -> t
