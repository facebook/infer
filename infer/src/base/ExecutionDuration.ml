(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module Duration = struct
  type t = {time: float; comp: float}

  let zero = {time= 0.; comp= 0.}

  (* https://en.wikipedia.org/wiki/Kahan_summation_algorithm *)
  let neumaier_sum_elapsed d elapsed =
    let time = d.time +. elapsed in
    let comp =
      if Float.(abs d.time >. abs elapsed) then d.comp +. (d.time -. time +. elapsed)
      else d.comp +. (elapsed -. time +. d.time)
    in
    {time; comp}


  let since from ~now = {time= now -. from; comp= 0.}

  let add d1 d2 = neumaier_sum_elapsed {d1 with comp= d1.comp +. d2.comp} d2.time

  let secs d = d.time +. d.comp
end

type t = {user: Duration.t; sys: Duration.t; wall: Mtime.Span.t}

type counter = {process_times: Unix.process_times; wall_time: Mtime_clock.counter}

type 'a evaluation_result = {result: 'a; execution_duration: t}

let zero = {user= Duration.zero; sys= Duration.zero; wall= Mtime.Span.zero}

let since {process_times; wall_time} =
  let now = Unix.times () in
  { user= Duration.since process_times.tms_utime ~now:now.tms_utime
  ; sys= Duration.since process_times.tms_stime ~now:now.tms_stime
  ; wall= Mtime_clock.count wall_time }


let add exe_d1 exe_d2 =
  { user= Duration.add exe_d1.user exe_d2.user
  ; sys= Duration.add exe_d1.sys exe_d2.sys
  ; wall= Mtime.Span.add exe_d1.wall exe_d2.wall }


let add_duration_since exe_duration from = add exe_duration (since from)

let user_time exe_duration = Duration.secs exe_duration.user

let sys_time exe_duration = Duration.secs exe_duration.sys

let wall_time exe_duration = exe_duration.wall

let wall_time_s exe_duration = wall_time exe_duration |> IMtime.span_to_s_float

let total_useful_s exe_duration = user_time exe_duration +. sys_time exe_duration

let oncpu_pct exe_duration =
  100.0 *. total_useful_s exe_duration /. wall_time_s exe_duration |> Float.round |> int_of_float


let pp ~prefix fmt exe_duration =
  F.fprintf fmt "%s_user= %.8f@\n%s_sys= %.8f@\n%s_wall= %.8f@\n%s_oncpu_pct= %d%%" prefix
    (user_time exe_duration) prefix (sys_time exe_duration) prefix (wall_time_s exe_duration) prefix
    (oncpu_pct exe_duration)


let counter () = {process_times= Unix.times (); wall_time= Mtime_clock.counter ()}

let timed_evaluate ~f =
  let start = counter () in
  let result = f () in
  {result; execution_duration= since start}


let to_scuba_entries ~prefix exe_duration =
  let secs_to_us s = s *. 1000_000. |> Float.to_int in
  [ LogEntry.mk_time ~label:(prefix ^ "_sys") ~duration_us:(sys_time exe_duration |> secs_to_us)
  ; LogEntry.mk_time ~label:(prefix ^ "_user") ~duration_us:(user_time exe_duration |> secs_to_us)
  ; LogEntry.mk_time ~label:(prefix ^ "_wall") ~duration_us:(wall_time_s exe_duration |> secs_to_us)
  ; LogEntry.mk_count ~label:(prefix ^ "_oncpu_pct") ~value:(oncpu_pct exe_duration) ]


let log ~prefix debug_kind exe_duration =
  L.debug debug_kind Quiet "%a@\n" (pp ~prefix) exe_duration ;
  ScubaLogging.log_many (to_scuba_entries ~prefix exe_duration)
