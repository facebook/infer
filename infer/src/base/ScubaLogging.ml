(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  |> maybe_add_normal ~name:"execution_id"
       ~value:(Option.map ~f:Int64.to_string Config.scuba_execution_id)


let set_command_line_tagsets sample =
  let add_tagset ~key ~data = Scuba.add_tagset ~name:key ~value:data in
  Map.fold Config.scuba_tags ~init:sample ~f:add_tagset


let set_common_fields sample =
  let open Scuba in
  sample
  |> add_int ~name:"pid" ~value:(ProcessPoolState.get_pid () |> Pid.to_int)
  |> add_int ~name:"is_main_process" ~value:(Bool.to_int Config.is_originator)
  |> add_normal ~name:"hostname" ~value:hostname
  |> maybe_add_normal ~name:"job_id" ~value:Config.job_id
  |> add_normal ~name:"command" ~value:(InferCommand.to_string Config.command)
  |> add_normal ~name:"infer_commit" ~value:Version.commit


let sample_from_event ~loc ({label; created_at_ts; data} : LogEntry.t) =
  let create_sample_with_label label =
    Scuba.new_sample ~time:(Some created_at_ts)
    |> set_common_fields
    |> Scuba.add_normal ~name:"event" ~value:label
    |> maybe_add_normal ~name:"location" ~value:loc
  in
  match data with
  | Count {value} ->
      create_sample_with_label (Printf.sprintf "count.%s" label)
      |> Scuba.add_int ~name:"value" ~value
  | Time {duration_us} ->
      create_sample_with_label (Printf.sprintf "time.%s" label)
      |> Scuba.add_int ~name:"value" ~value:duration_us
  | String {message} ->
      create_sample_with_label (Printf.sprintf "msg.%s" label)
      |> Scuba.add_normal ~name:"message" ~value:message


let log_to_debug samples =
  let log_file =
    (* cannot use [ResultsDir.get_path], it would introduce a circular dependency *)
    let dir_name = ResultsDirEntryName.get_path ~results_dir:Config.results_dir Stats in
    Utils.create_dir dir_name ;
    let file_name =
      match !ProcessPoolState.in_child with
      | None ->
          "stats.jsonl"
      | Some rank ->
          Printf.sprintf "stats-%d.jsonl" rank
    in
    dir_name ^/ file_name
  in
  Utils.with_file_out ~append:true log_file ~f:(fun out ->
      List.iter samples ~f:(fun sample ->
          Yojson.to_channel out (Scuba.sample_to_json sample) ;
          Printf.fprintf out "\n" ) )


(** Consider buffering or batching if proves to be a problem *)
let log_many ~loc entries =
  let samples = List.map entries ~f:(sample_from_event ~loc) in
  log_to_debug samples ;
  if Config.scuba_logging then
    (* add the columns that are relevant for scuba only at this point *)
    List.map samples ~f:(fun sample ->
        sample |> set_command_line_normales |> set_command_line_tagsets )
    |> Scuba.log InferEvents


let log_one ~loc entry = log_many ~loc [entry]

let log_message ~label ~loc ~message = log_one ~loc (LogEntry.mk_string ~label ~message)

let log_many = log_many ~loc:None

let log_one = log_one ~loc:None

let log_message_with_location ~label ~loc ~message = log_message ~label ~loc:(Some loc) ~message

let log_message = log_message ~loc:None

let log_count ~label ~value = log_one (LogEntry.mk_count ~label ~value)

let log_duration ~label ~duration_us = log_one (LogEntry.mk_time ~label ~duration_us)

let execute_with_time_logging label f =
  let ret_val, duration = Utils.timeit ~f in
  let duration_us = IMtime.span_to_us_int duration in
  let entry = LogEntry.mk_time ~label ~duration_us in
  log_one entry ;
  ret_val


let flush_log_events () =
  log_many (LogEntry.global_log_get ()) ;
  LogEntry.global_log_erase ()


let register_global_log_flushing_at_exit () =
  Epilogues.register ~f:flush_log_events ~description:"Flushing global log entries to Scuba"
