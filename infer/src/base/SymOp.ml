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
module F = Format

type failure_kind =
  | FKtimeout  (** max time exceeded *)
  | FKsymops_timeout of int  (** max symop's exceeded *)
  | FKrecursion_timeout of int  (** max recursion level exceeded *)
  | FKcrash of string  (** uncaught exception or failed assertion *)

exception Analysis_failure_exe of failure_kind(** failure that prevented analysis from finishing *)

let exn_not_failure = function Analysis_failure_exe _ -> false | _ -> true

let try_finally ?(fail_early= false) f g =
  match f () with
  | r
   -> g () ; r
  | exception (Analysis_failure_exe _ as f_exn)
   -> ( if not fail_early then
          try g ()
          with _ -> () ) ;
      raise f_exn
  | exception f_exn ->
    match g () with
    | ()
     -> raise f_exn
    | exception (Analysis_failure_exe _ as g_exn)
     -> raise g_exn
    | exception _
     -> raise f_exn

let finally_try g f = try_finally f g

let pp_failure_kind fmt = function
  | FKtimeout
   -> F.fprintf fmt "TIMEOUT"
  | FKsymops_timeout symops
   -> F.fprintf fmt "SYMOPS TIMEOUT (%d)" symops
  | FKrecursion_timeout level
   -> F.fprintf fmt "RECURSION TIMEOUT(%d)" level
  | FKcrash msg
   -> F.fprintf fmt "CRASH (%s)" msg

(** Count the number of symbolic operations *)

(** Timeout in seconds for each function *)
let timeout_seconds =
  ref
    (Option.map Config.seconds_per_iteration ~f:(fun sec -> sec *. float_of_int Config.iterations))

(** Timeout in SymOps *)
let timeout_symops =
  ref (Option.map Config.symops_per_iteration ~f:(fun symops -> symops * Config.iterations))

let get_timeout_seconds () = !timeout_seconds

(** Internal state of the module *)
type t =
  { mutable alarm_active: bool  (** Only throw timeout exception when alarm is active *)
  ; mutable last_wallclock: float option  (** last wallclock set by an alarm, if any *)
  ; mutable symop_count: int  (** Number of symop's *)
  ; symop_total: int ref
        (** Counter for the total number of symop's.
        The new state created when save_state is called shares this counter
        if keep_symop_total is true. Otherwise, a new counter is created. *)
  }

let initial () : t = {alarm_active= false; last_wallclock= None; symop_count= 0; symop_total= ref 0}

(** Global State *)
let gs : t ref = ref (initial ())

(** Restore the old state. *)
let restore_state state = gs := state

(** Return the old state, and revert the current state to the initial one.
    If keep_symop_total is true, share the total counter. *)
let save_state ~keep_symop_total =
  let old_state = !gs in
  let new_state =
    let st = initial () in
    if keep_symop_total then {st with symop_total= old_state.symop_total} else st
  in
  gs := new_state ;
  old_state

(** handler for the wallclock timeout *)
let wallclock_timeout_handler = ref None

(** set the handler for the wallclock timeout *)
let set_wallclock_timeout_handler handler = wallclock_timeout_handler := Some handler

(** Set the wallclock alarm checked at every pay() *)
let set_wallclock_alarm nsecs = !gs.last_wallclock <- Some (Unix.gettimeofday () +. nsecs)

(** Unset the wallclock alarm checked at every pay() *)
let unset_wallclock_alarm () = !gs.last_wallclock <- None

(** if the wallclock alarm has expired, raise a timeout exception *)
let check_wallclock_alarm () =
  match (!gs.last_wallclock, !wallclock_timeout_handler) with
  | Some alarm_time, Some handler when Unix.gettimeofday () >= alarm_time
   -> unset_wallclock_alarm () ; handler ()
  | _
   -> ()

(** Return the time remaining before the wallclock alarm expires *)
let get_remaining_wallclock_time () =
  match !gs.last_wallclock with
  | Some alarm_time
   -> max 0.0 (alarm_time -. Unix.gettimeofday ())
  | None
   -> 0.0

(** Return the total number of symop's since the beginning *)
let get_total () = !(!gs.symop_total)

(** Reset the total number of symop's *)
let reset_total () = !gs.symop_total := 0

(** Count one symop *)
let pay () =
  !gs.symop_count <- !gs.symop_count + 1 ;
  !gs.symop_total := !(!gs.symop_total) + 1 ;
  ( match !timeout_symops with
  | Some symops when !gs.symop_count > symops && !gs.alarm_active
   -> raise (Analysis_failure_exe (FKsymops_timeout !gs.symop_count))
  | _
   -> () ) ;
  check_wallclock_alarm ()

(** Reset the counter *)
let reset_count () = !gs.symop_count <- 0

(** Reset the counter and activate the alarm *)
let set_alarm () =
  reset_count () ;
  !gs.alarm_active <- true

(** De-activate the alarm *)
let unset_alarm () = !gs.alarm_active <- false
