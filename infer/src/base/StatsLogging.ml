(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let hostname = Caml_unix.gethostname ()

let maybe_add_normal ~name ~value sample =
  match value with None -> sample | Some value -> StatsSample.add_normal ~name ~value sample


let set_common_fields sample =
  let open StatsSample in
  sample
  |> add_int ~name:"pid" ~value:(WorkerPoolState.get_pid () |> Pid.to_int)
  |> add_int ~name:"is_main_process" ~value:(Bool.to_int Config.is_originator)
  |> add_normal ~name:"hostname" ~value:hostname
  |> maybe_add_normal ~name:"job_id" ~value:Config.job_id
  |> add_normal ~name:"command" ~value:(InferCommand.to_string Config.command)
  |> add_normal ~name:"infer_commit" ~value:Version.commit


let sample_from_event ~loc ({label; created_at_ts; data} : LogEntry.t) =
  let open StatsSample in
  let create_sample_with_label label =
    new_sample ~time:(Some created_at_ts)
    |> set_common_fields
    |> add_normal ~name:"event" ~value:label
    |> maybe_add_normal ~name:"location" ~value:loc
  in
  match data with
  | Count {value} ->
      create_sample_with_label (Printf.sprintf "count.%s" label)
      |> StatsSample.add_int ~name:"value" ~value
  | Time {duration_us} ->
      create_sample_with_label (Printf.sprintf "time.%s" label)
      |> StatsSample.add_int ~name:"value" ~value:duration_us
  | String {message} ->
      create_sample_with_label (Printf.sprintf "msg.%s" label)
      |> StatsSample.add_normal ~name:"message" ~value:message


let log_many ~loc entries =
  let samples = List.map entries ~f:(sample_from_event ~loc) in
  let log_file =
    (* cannot use [ResultsDir.get_path], it would introduce a circular dependency *)
    let dir_name = ResultsDirEntryName.get_path ~results_dir:Config.results_dir Stats in
    Utils.create_dir dir_name ;
    let file_name =
      match WorkerPoolState.get_in_child () with
      | None ->
          "stats.jsonl"
      | Some rank ->
          Printf.sprintf "stats-%d.jsonl" rank
    in
    dir_name ^/ file_name
  in
  Utils.with_file_out ~append:true log_file ~f:(fun out ->
      List.iter samples ~f:(fun sample ->
          Yojson.to_channel out (StatsSample.sample_to_json sample) ;
          Printf.fprintf out "\n" ) )


let log_one ~loc entry = log_many ~loc [entry]

let log_message ~label ~loc ~message = log_one ~loc (LogEntry.mk_string ~label ~message)

let () = if not Config.is_running_unit_test then Random.self_init ()

let log_message_sampled ~label ~loc ~message ~sample_rate =
  let gen = Random.int sample_rate in
  (* Drop log entry unless 0 was randomly generated *)
  if Int.equal gen 0 then
    let label = Lazy.force label in
    let message = Lazy.force message in
    let loc = Option.map loc ~f:Lazy.force in
    log_one ~loc (LogEntry.mk_string ~label ~message)


let log_many = log_many ~loc:None

let log_one = log_one ~loc:None

let log_message_with_location ~label ~loc ~message = log_message ~label ~loc:(Some loc) ~message

let log_message_with_location_sampled ~label ~loc ~message ~sample_rate =
  log_message_sampled ~label ~loc:(Some loc) ~message ~sample_rate


let log_message = log_message ~loc:None

let log_message_sampled = log_message_sampled ~loc:None

let log_count ~label ~value = log_one (LogEntry.mk_count ~label ~value)

let log_duration ~label ~duration_us = log_one (LogEntry.mk_time ~label ~duration_us)

let execute_with_time_logging label f =
  let ret_val, duration = Utils.timeit ~f in
  let duration_us = IMtime.span_to_us_int duration in
  let entry = LogEntry.mk_time ~label ~duration_us in
  log_one entry ;
  ret_val
