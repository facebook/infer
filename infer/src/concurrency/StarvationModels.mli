(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type severity = Low | Medium | High [@@deriving compare]

val pp_severity : F.formatter -> severity -> unit

val may_block : Tenv.t -> Typ.Procname.t -> HilExp.t list -> severity option
(** is the method call potentially blocking, given the actuals passed? *)

val is_strict_mode_violation : Tenv.t -> Typ.Procname.t -> HilExp.t list -> bool

val is_synchronized_library_call : Tenv.t -> Typ.Procname.t -> bool
(** does the method call lock-then-unlock the underlying object?
    legacy Java containers like Vector do this, and can interact with explicit locking *)

val should_skip_analysis : Tenv.t -> Typ.Procname.t -> HilExp.t list -> bool
(** should we treat a method call as skip (eg library methods in guava) to avoid FPs? *)

val is_annotated_nonblocking :
  attrs_of_pname:(Typ.Procname.t -> ProcAttributes.t option) -> Tenv.t -> Typ.Procname.t -> bool
(** is procedure transitively annotated [@Nonblocking] *)

val is_annotated_lockless :
  attrs_of_pname:(Typ.Procname.t -> ProcAttributes.t option) -> Tenv.t -> Typ.Procname.t -> bool
(** is procedure transitively annotated [@Lockless] *)

val schedules_work : Tenv.t -> Typ.Procname.t -> bool
(** call known to schedule runnable first argument to some thread/executor *)

(** an instance field holding a reference to an executor may be annotated as running on UI/non-UI thread *)
type executor_thread_constraint = ForUIThread | ForNonUIThread

val get_executor_thread_constraint :
  Tenv.t -> HilExp.AccessExpression.t -> executor_thread_constraint option
(** given an executor receiver, get its thread constraint, if any *)

val get_run_method_from_runnable : Tenv.t -> HilExp.AccessExpression.t -> Typ.Procname.t option
(** given a receiver, find the [run()] method in the appropriate class *)
