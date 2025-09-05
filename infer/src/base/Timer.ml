(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

exception Timeout of float

type timer =
  { last_start_time: Mtime.t (* Last time the timer was started *)
  ; previous_time_spent: Mtime.span (* Duration already recorded before last start *) }

type state = Mtime.span option

let timer = DLS.new_key (fun () -> None)

let now () = Mtime_clock.now ()

let start () = DLS.set timer (Some {last_start_time= now (); previous_time_spent= Mtime.Span.zero})

let time_since {last_start_time; previous_time_spent} =
  Mtime.Span.add previous_time_spent (Mtime.span (now ()) last_start_time)


let suspend () : state =
  let current_timer = DLS.get timer in
  DLS.set timer None ;
  Option.map current_timer ~f:time_since


let resume (t : state) =
  Option.map t ~f:(fun previous_time_spent -> {last_start_time= now (); previous_time_spent})
  |> DLS.set timer


let get () =
  match DLS.get timer with
  | None ->
      L.die InternalError "trying to get the value of the timer but no timer is active"
  | Some start_time ->
      Float.(Mtime.Span.to_float_ns (time_since start_time) / 1_000_000_000.0)


let check_timeout () =
  let check_timeout timeout =
    let span = get () in
    if Float.(span >. timeout) then (
      Stats.incr_timeouts () ;
      raise (Timeout span) )
  in
  Option.iter ~f:check_timeout Config.timeout


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
