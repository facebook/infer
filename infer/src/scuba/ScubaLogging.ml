(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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


let set_command_line_normales sample =
  let add_normal ~key ~data = Scuba.add_normal ~name:key ~value:data in
  Map.fold Config.scuba_normals ~init:sample ~f:add_normal


let set_common_fields sample =
  let open Scuba in
  sample
  |> add_int ~name:"pid" ~value:(ProcessPoolState.get_pid () |> Pid.to_int)
  |> add_int ~name:"is_main_process" ~value:(Bool.to_int CommandLineOption.is_originator)
  |> add_normal ~name:"hostname" ~value:hostname
  |> maybe_add_normal ~name:"job_id" ~value:Config.job_id
  |> add_normal ~name:"command" ~value:(InferCommand.to_string Config.command)
  |> add_normal ~name:"infer_commit" ~value:Version.commit
  |> add_normal ~name:"execution_id" ~value:(Int64.to_string Config.execution_id)


let sample_from_event ({label; created_at_ts; data} : LogEntry.t) =
  let event_name, value =
    match data with
    | LogEntry.Count {value} ->
        (Printf.sprintf "count.%s" label, value)
    | LogEntry.Time {duration_ms} ->
        (Printf.sprintf "time.%s" label, duration_ms)
  in
  Scuba.new_sample ~time:(Some created_at_ts)
  |> set_common_fields |> set_command_line_normales
  |> Scuba.add_normal ~name:"event" ~value:event_name
  |> Scuba.add_int ~name:"value" ~value


(** Consider buffering or batching if proves to be a problem *)
let log_many entries =
  let samples = List.map entries ~f:sample_from_event in
  Scuba.log Scuba.InferEvents samples


(** If scuba logging is disabled, we would not log anyway, but let's not even try
    to create samples to save perf *)
let log_many = if Config.scuba_logging then log_many else fun _ -> ()

let log_one entry = log_many [entry]

let log_count ~label ~value = log_one (LogEntry.mk_count ~label ~value)

let execute_with_time_logging label f =
  let ret_val, duration_ms = Utils.timeit ~f in
  let entry = LogEntry.mk_time ~label ~duration_ms in
  log_one entry ; ret_val


let flush_log_events () =
  log_many (LogEntry.global_log_get ()) ;
  LogEntry.global_log_erase ()


let register_global_log_flushing_at_exit () =
  Epilogues.register ~f:flush_log_events ~description:"Flushing global log entries to Scuba"
