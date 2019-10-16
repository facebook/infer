(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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
  type t = private {acquisitions: Acquisitions.t; event: Event.t}
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

  val make_trace :
    ?header:string -> ?include_acquisitions:bool -> Typ.Procname.t -> t -> Errlog.loc_trace
end

module CriticalPairs : AbstractDomain.FiniteSetS with type elt = CriticalPair.t

module UIThreadElement : sig
  type t = StarvationModels.uithread_explanation =
    | IsModeled of {proc_name: Typ.Procname.t}
    | CallsModeled of {proc_name: Typ.Procname.t; callee: Typ.Procname.t}
    | Annotated of {proc_name: Typ.Procname.t; trail: ConcurrencyModels.annotation_trail}
end

module UIThreadDomain : AbstractDomain.WithBottom

module GuardToLockMap : AbstractDomain.WithTop

type t =
  { guard_map: GuardToLockMap.t
  ; lock_state: LockState.t
  ; critical_pairs: CriticalPairs.t
  ; ui: UIThreadDomain.t }

include AbstractDomain.WithBottom with type t := t

val acquire : Tenv.t -> t -> procname:Typ.Procname.t -> loc:Location.t -> Lock.t list -> t
(** simultaneously acquire a number of locks, no-op if list is empty *)

val release : t -> Lock.t list -> t
(** simultaneously release a number of locks, no-op if list is empty *)

val blocking_call : callee:Typ.Procname.t -> StarvationModels.severity -> loc:Location.t -> t -> t

val strict_mode_call : callee:Typ.Procname.t -> loc:Location.t -> t -> t

val set_on_ui_thread : t -> loc:Location.t -> UIThreadElement.t -> t
(** set the property "runs on UI thread" to true by attaching the given explanation as to
    why this method is thought to do so *)

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

type summary = t

val pp_summary : F.formatter -> summary -> unit

val integrate_summary :
     Tenv.t
  -> caller_summary:t
  -> callee:Typ.Procname.t
  -> loc:Location.t
  -> callee_summary:summary
  -> t

val filter_blocking_calls : t -> t
