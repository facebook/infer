(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Symbolic Operations and Failures: the units in which analysis work is measured *)

open! IStd

(** Internal state of the module *)
type t

(** if the wallclock alarm has expired, raise a timeout exception *)
val check_wallclock_alarm : unit -> unit

(** Return the time remaining before the wallclock alarm expires *)
val get_remaining_wallclock_time : unit -> float

(** Timeout in seconds for each function *)
val get_timeout_seconds : unit -> float

(** Return the total number of symop's since the beginning *)
val get_total : unit -> int

(** Count one symop *)
val pay : unit -> unit

(** Reset the total number of symop's *)
val reset_total : unit -> unit

(** Restore the old state. *)
val restore_state : t -> unit

(** Return the old state, and revert the current state to the initial one.
    If keep_symop_total is true, share the total counter. *)
val save_state : keep_symop_total:bool -> t

(** Reset the counter and activate the alarm *)
val set_alarm : unit -> unit

(** Set the wallclock alarm checked at every pay() *)
val set_wallclock_alarm : float -> unit

(** set the handler for the wallclock timeout *)
val set_wallclock_timeout_handler : (unit -> unit) -> unit

(** De-activate the alarm *)
val unset_alarm : unit -> unit

(** Unset the wallclock alarm checked at every pay() *)
val unset_wallclock_alarm : unit -> unit


type failure_kind =
  | FKtimeout (** max time exceeded *)
  | FKsymops_timeout of int (** max symop's exceeded *)
  | FKrecursion_timeout of int (** max recursion level exceeded *)
  | FKcrash of string (** uncaught exception or failed assertion *)

(** Timeout exception *)
exception Analysis_failure_exe of failure_kind

(** check that the exception is not a timeout exception *)
val exn_not_failure : exn -> bool

(** [try_finally ~fail_early f g] executes [f] and then [g] even if [f] raises an exception.
    Assuming that [g ()] terminates quickly [Analysis_failure_exe] exceptions are handled correctly.
    In particular, an exception raised by [f ()] is delayed until [g ()] finishes, so [g ()] should
    return reasonably quickly.  [~fail_early=true] can be passed to skip executing [g ()] when [f
    ()] raises a [Analysis_failure_exe] exception. *)
val try_finally : ?fail_early:bool -> (unit -> 'a) -> (unit -> unit) -> 'a

(** [finally_try g f] is equivalent to [try_finally f g]. *)
val finally_try : (unit -> unit) -> (unit -> 'a) -> 'a

val pp_failure_kind : Format.formatter -> failure_kind -> unit
