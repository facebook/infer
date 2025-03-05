(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

exception Timeout of float

let now () = if Config.multicore then 0.0 else (Caml_unix.times ()).tms_utime

let timer = DLS.new_key (fun () -> None)

let start () = DLS.set timer (Some (now ()))

let time_since start_time = now () -. start_time

let get () =
  match DLS.get timer with
  | None ->
      L.die InternalError "trying to get the value of the timer but no timer is active"
  | Some start_time ->
      time_since start_time


type state = float option

let suspend () : state =
  let current_timer = DLS.get timer in
  DLS.set timer None ;
  Option.map current_timer ~f:time_since


let resume (t : state) =
  (* forget about the time spent between [suspend ()] and [resume _] by pretending the timer started
     at [now - previous_time_spent] *)
  Option.map t ~f:(fun previous_time_spent -> now () -. previous_time_spent) |> DLS.set timer


let check_timeout timeout =
  let span = get () in
  if Float.(span >. timeout) then (
    Stats.incr_timeouts () ;
    raise (Timeout span) )


let check_timeout () = if not Config.multicore then Option.iter ~f:check_timeout Config.timeout

let time timeable ~on_timeout ~f =
  let timer = suspend () in
  start () ;
  let result =
    Exn.protect
      ~f:(fun () -> try f () with Timeout span -> on_timeout span)
      ~finally:(fun () ->
        Stats.add_timing timeable (get ()) ;
        resume timer )
  in
  result
