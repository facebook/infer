(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module CLOpt = CommandLineOption

module IO = struct
  let log_file_extension = ".log"

  let events_dir =
    Option.value (Sys.getenv Config.infer_top_results_dir_env_var) ~default:Config.results_dir
    ^/ Config.events_dir_name


  let out_chan = ref None

  let close () =
    match !out_chan with
    | None ->
        ()
    | Some chan ->
        Out_channel.close chan ;
        out_chan := None


  let prepare () =
    if Config.log_events then (
      close () ;
      let fname = events_dir ^/ (Unix.getpid () |> Pid.to_string) ^ log_file_extension in
      let oc = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 fname in
      out_chan := Some oc )


  let write fmt =
    match !out_chan with Some oc -> Printf.fprintf oc fmt | _ -> Printf.ifprintf stdout fmt


  let dump () =
    let dump_file_to_stdout fname =
      let ic = In_channel.create fname in
      In_channel.iter_lines ic ~f:print_endline
    in
    let log_files = Utils.find_files ~path:events_dir ~extension:log_file_extension in
    List.iter log_files ~f:dump_file_to_stdout


  let () = Config.register_late_epilogue close
end

module Random_id : sig
  val get : unit -> string
end = struct
  let () = Random.self_init ()

  let generate () = Random.int64 1_000_000_000_000L |> Int64.to_string

  let infer_run_identifier_env_var = "INFER_RUN_IDENTIFIER"

  let get () =
    match Sys.getenv infer_run_identifier_env_var with
    | Some id ->
        id
    | None ->
        let new_id = generate () in
        Unix.putenv ~key:infer_run_identifier_env_var ~data:new_id ;
        new_id
end

let get_log_identifier () = Random_id.get ()

type event = UncaughtException of exn * int

let string_of_event event = match event with UncaughtException _ -> "UncaughtException"

let sequence_ctr = ref 0

let pid () = Pid.to_int (Unix.getpid ())

let sysname =
  try
    Utils.with_process_in "uname 2>/dev/null" (fun chan ->
        Scanf.bscanf (Scanf.Scanning.from_channel chan) "%s" (fun n -> n) )
    |> fst
  with _ -> "Unknown"


let create_row event =
  incr sequence_ctr ;
  let open JsonBuilder in
  let base =
    empty |> add_string ~key:"command" ~data:(CLOpt.name_of_command Config.command)
    |> add_string ~key:"event_tag" ~data:(string_of_event event)
    |> add_string ~key:"hostname" ~data:(Unix.gethostname ())
    |> add_string ~key:"infer_commit" ~data:Version.commit
    |> add_int ~key:"is_originator" ~data:(if CLOpt.is_originator then 1 else 0)
    |> add_int ~key:"pid" ~data:(pid ())
    |> add_string ~key:"run_identifier" ~data:(get_log_identifier ())
    |> add_int ~key:"sequence" ~data:(!sequence_ctr - 1) |> add_string ~key:"sysname" ~data:sysname
    |> add_int ~key:"time" ~data:(int_of_float (Unix.time ()))
  in
  ( match event with UncaughtException (exn, exitcode) ->
      base |> add_string ~key:"exception" ~data:(Caml.Printexc.exn_slot_name exn)
      |> add_string ~key:"exception_info" ~data:(Exn.to_string exn)
      |> add_int ~key:"exitcode" ~data:exitcode )
  |> JsonBuilder.to_json


let prepare = IO.prepare

let log event = IO.write "%s\n" (create_row event)

let dump = IO.dump
