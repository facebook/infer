(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Operations and Failures: the units in which analysis work is measured *)

open! IStd

(** Internal state of the module *)
type t

val check_wallclock_alarm : unit -> unit
(** if the wallclock alarm has expired, raise a timeout exception *)

val get_remaining_wallclock_time : unit -> float
(** Return the time remaining before the wallclock alarm expires *)

val get_timeout_seconds : unit -> float option
(** Timeout in seconds for each function *)

val get_total : unit -> int
(** Return the total number of symop's since the beginning *)

val pay : unit -> unit
(** Count one symop *)

val reset_total : unit -> unit
(** Reset the total number of symop's *)

val restore_state : t -> unit
(** Restore the old state. *)

val save_state : keep_symop_total:bool -> t
(** Return the old state, and revert the current state to the initial one. If keep_symop_total is
    true, share the total counter. *)

val set_alarm : unit -> unit
(** Reset the counter and activate the alarm *)

val set_wallclock_alarm : float -> unit
(** Set the wallclock alarm checked at every pay() *)

val set_wallclock_timeout_handler : (unit -> unit) -> unit
(** set the handler for the wallclock timeout *)

val unset_alarm : unit -> unit
(** De-activate the alarm *)

val unset_wallclock_alarm : unit -> unit
(** Unset the wallclock alarm checked at every pay() *)
