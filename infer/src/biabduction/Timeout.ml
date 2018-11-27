(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Handle timeout events *)

(** status of a timeout instance *)
type status =
  { seconds_remaining: float  (** Seconds remaining in the current timeout *)
  ; symop_state: SymOp.t  (** Internal State of SymOp *) }

(** stack of suspended timeout instances *)
type timeouts_stack = status list ref

module GlobalState = struct
  let stack : timeouts_stack = ref []

  let pop () =
    match !stack with
    | top_status :: l ->
        stack := l ;
        Some top_status
    | [] ->
        None


  let push status = stack := status :: !stack
end

let set_alarm nsecs =
  match Config.os_type with
  | Config.Unix | Config.Cygwin ->
      ignore
        (Unix.setitimer Unix.ITIMER_REAL
           { Unix.it_interval= 3.0
           ; (* try again after 3 seconds if the signal is lost *)
             Unix.it_value= nsecs })
  | Config.Win32 ->
      SymOp.set_wallclock_alarm nsecs


let unset_alarm () =
  match Config.os_type with
  | Config.Unix | Config.Cygwin ->
      set_alarm 0.0
  | Config.Win32 ->
      SymOp.unset_wallclock_alarm ()


let get_seconds_remaining () =
  match Config.os_type with
  | Config.Unix | Config.Cygwin ->
      (Unix.getitimer Unix.ITIMER_REAL).Unix.it_value
  | Config.Win32 ->
      SymOp.get_remaining_wallclock_time ()


let get_current_status ~keep_symop_total =
  let seconds_remaining = get_seconds_remaining () in
  let symop_state = SymOp.save_state ~keep_symop_total in
  {seconds_remaining; symop_state}


let set_status status =
  SymOp.restore_state status.symop_state ;
  set_alarm status.seconds_remaining


let timeout_action _ =
  unset_alarm () ;
  raise (SymOp.Analysis_failure_exe FKtimeout)


let () =
  (* Can't use Core since it wraps signal handlers and alarms with catch-all exception handlers that
     exit, while we need to propagate the timeout exceptions. *)
  let module Gc = Caml.Gc in
  let module Sys = Caml.Sys in
  match Config.os_type with
  | Config.Unix | Config.Cygwin ->
      Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle timeout_action) ;
      Sys.set_signal Sys.sigalrm (Sys.Signal_handle timeout_action)
  | Config.Win32 ->
      SymOp.set_wallclock_timeout_handler timeout_action ;
      (* use the Gc alarm for periodic timeout checks *)
      ignore (Gc.create_alarm SymOp.check_wallclock_alarm)


let unwind () = unset_alarm () ; SymOp.unset_alarm () ; GlobalState.pop ()

let suspend_existing_timeout ~keep_symop_total =
  let current_status = get_current_status ~keep_symop_total in
  unset_alarm () ; GlobalState.push current_status


let resume_previous_timeout () =
  let status_opt = unwind () in
  Option.iter ~f:set_status status_opt


let exe_timeout f x =
  let suspend_existing_timeout_and_start_new_one () =
    suspend_existing_timeout ~keep_symop_total:true ;
    Option.iter (SymOp.get_timeout_seconds ()) ~f:set_alarm ;
    SymOp.set_alarm ()
  in
  try
    SymOp.try_finally
      ~f:(fun () ->
        suspend_existing_timeout_and_start_new_one () ;
        f x ;
        None )
      ~finally:resume_previous_timeout
  with SymOp.Analysis_failure_exe kind ->
    let loc = State.get_loc () |> Option.value ~default:Location.dummy in
    Errdesc.warning_err loc "TIMEOUT: %a@." SymOp.pp_failure_kind kind ;
    Some kind
