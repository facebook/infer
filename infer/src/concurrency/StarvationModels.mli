(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val may_block : Tenv.t -> Procname.t -> HilExp.t list -> bool
(** is the method call potentially blocking, given the actuals passed? *)

val may_do_ipc : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_regex_op : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_strict_mode_violation : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_monitor_wait : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_synchronized_library_call : Tenv.t -> Procname.t -> bool
(** does the method call lock-then-unlock the underlying object? legacy Java containers like Vector
    do this, and can interact with explicit locking *)

val should_skip_analysis : Tenv.t -> Procname.t -> HilExp.t list -> bool
(** should we treat a method call as skip (eg library methods in guava) to avoid FPs? *)

val is_annotated_nonblocking : Tenv.t -> Procname.t -> bool
(** is procedure transitively annotated [@Nonblocking] *)

val is_annotated_lockless : Tenv.t -> Procname.t -> bool
(** is procedure transitively annotated [@Lockless] *)

val schedules_work : Tenv.t -> Procname.t -> bool
(** call known to schedule runnable first argument to some executor/handler or subclass *)

(** an instance field holding a reference to an executor may be annotated as running on UI/non-UI
    thread *)
type scheduler_thread_constraint = ForUIThread | ForNonUIThread | ForUnknownThread
[@@deriving equal]

val get_executor_thread_annotation_constraint :
  Tenv.t -> HilExp.AccessExpression.t -> scheduler_thread_constraint option
(** given an executor receiver, get its thread constraint, if any. [None] means lookup somehow
    failed, whereas [Some UnknownThread] means the receiver is an unannotated executor. *)

val get_run_method_from_runnable : Tenv.t -> HilExp.AccessExpression.t -> Procname.t option
(** given a receiver, find the [run()] method in the appropriate class *)

val get_returned_executor :
  Tenv.t -> Procname.t -> HilExp.t list -> scheduler_thread_constraint option
(** does the function return an executor and of which thread? *)

val schedules_first_arg_on_ui_thread : Tenv.t -> Procname.t -> bool
(** method call known to directly schedule the runnable object provided as first procedure argument
    on the UI thread *)

val schedules_second_arg_on_ui_thread : Tenv.t -> Procname.t -> bool
(** method call known to directly schedule the runnable object provided as second procedure argument
    on a background thread *)

val schedules_first_arg_on_bg_thread : Tenv.t -> Procname.t -> bool
(** method call known to directly the runnable object provided as first procedure argument on a
    background thread *)

val is_getMainLooper : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_handler_constructor : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_thread_constructor : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_future_get : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_future_is_done : Tenv.t -> Procname.t -> HilExp.t list -> bool

val is_assume_true : Tenv.t -> Procname.t -> HilExp.t list -> bool
(** is the callee equivalent to assuming its first argument true *)

val is_java_main_method : Procname.t -> bool
(** does the method look like a Java [main] *)

val may_execute_arbitrary_code : Tenv.t -> Procname.t -> HilExp.t list -> bool
(** for example [com.google.common.util.concurrent.SettableFuture.set] *)
