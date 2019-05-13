(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
(* FB-ONLY *)

(*
 * Utilities to log in "infer_events" key-value scuba table.
 *)
open! IStd

let hostname = Unix.gethostname ()

let maybe_add_normal ~name ~value sample =
  match value with None -> sample | Some value -> Scuba.add_normal ~name ~value sample


let set_common_fields sample =
  let open Scuba in
  sample
  |> add_int ~name:"pid" ~value:(ProcessPoolState.get_pid () |> Pid.to_int)
  |> add_int ~name:"is_main_process" ~value:(Bool.to_int CommandLineOption.is_originator)
  |> add_normal ~name:"hostname" ~value:hostname
  |> maybe_add_normal ~name:"job_id" ~value:Config.job_id
  |> add_normal ~name:"command" ~value:(InferCommand.to_string Config.command)
  |> add_normal ~name:"infer_commit" ~value:Version.commit


let create_sample ~event_name ~value =
  Scuba.new_sample ~time:None |> set_common_fields
  |> Scuba.add_normal ~name:"event" ~value:event_name
  |> Scuba.add_int ~name:"value" ~value


type event_type = Count | Time

let string_of_event_type = function Count -> "count" | Time -> "time"

let log event_type ~event_suffix ~value =
  let event_name = string_of_event_type event_type ^ "." ^ event_suffix in
  let sample = create_sample ~event_name ~value in
  Scuba.log Scuba.InferEvents [sample]


(* If scuba logging is disabled, we would not log anyway, but let's not even try
   to create samples to save perf *)
let log = if Config.scuba_logging then log else fun _ ~event_suffix:_ ~value:_ -> ()

let log_count ~name ~value = log Count ~event_suffix:name ~value

let log_time ~name ~duration_ms = log Time ~event_suffix:name ~value:duration_ms

let execute_with_time_logging name f =
  let start_time = Mtime_clock.counter () in
  let ret_val = f () in
  let duration_ms = Mtime_clock.count start_time |> Mtime.Span.to_ms |> int_of_float in
  log_time ~name ~duration_ms ; ret_val
