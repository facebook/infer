(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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

type t = {utime: Duration.t; stime: Duration.t}

type 'a evaluation_result = {result: 'a; execution_duration: t}

let zero = {utime= Duration.zero; stime= Duration.zero}

let since (from : Unix.process_times) =
  let now = Unix.times () in
  { utime= Duration.since from.tms_utime ~now:now.tms_utime
  ; stime= Duration.since from.tms_stime ~now:now.tms_stime }


let add exe_d1 exe_d2 =
  {utime= Duration.add exe_d1.utime exe_d2.utime; stime= Duration.add exe_d1.stime exe_d2.stime}


let add_duration_since exe_duration (from : Unix.process_times) = add exe_duration (since from)

let user_time exe_duration = Duration.secs exe_duration.utime

let sys_time exe_duration = Duration.secs exe_duration.stime

let pp ~field fmt exe_duration =
  F.fprintf fmt "%s_utime= %.8f@;%s_stime= %.8f" field (user_time exe_duration) field
    (sys_time exe_duration)


let timed_evaluate ~f =
  let start_time = Unix.times () in
  let result = f () in
  {result; execution_duration= since start_time}
