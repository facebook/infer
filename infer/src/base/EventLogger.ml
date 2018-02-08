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

let bind_default opt map_func prev = match opt with Some x -> map_func x prev | None -> prev

type frontend_exception =
  { exception_type: string
  ; source_location_start: Location.t
  ; source_location_end: Location.t
  ; exception_file: string
  ; exception_line: int
  ; ast_node: string option
  ; lang: string }

let create_frontend_exception_row base record =
  let open JsonBuilder in
  base |> add_string ~key:"exception_type" ~data:record.exception_type
  |> add_string ~key:"source_location_start_file"
       ~data:(SourceFile.to_rel_path record.source_location_start.file)
  |> add_string ~key:"source_location_start_pos"
       ~data:
         (String.concat
            [ string_of_int record.source_location_start.line
            ; ":"
            ; string_of_int record.source_location_start.col ])
  |> add_string ~key:"source_location_end_file"
       ~data:(SourceFile.to_rel_path record.source_location_end.file)
  |> add_string ~key:"source_location_end_pos"
       ~data:
         (String.concat
            [ string_of_int record.source_location_end.line
            ; ":"
            ; string_of_int record.source_location_end.col ])
  |> add_string ~key:"exception_triggered_location"
       ~data:(String.concat [record.exception_file; ":"; string_of_int record.exception_line])
  |> bind_default record.ast_node (fun ast_node -> add_string ~key:"ast_node" ~data:ast_node)
  |> add_string ~key:"lang" ~data:record.lang


type procedures_translated =
  { procedures_translated_total: int
  ; procedures_translated_failed: int
  ; lang: string
  ; source_file: SourceFile.t }

let create_procedures_translated_row base record =
  let open JsonBuilder in
  base |> add_int ~key:"procedures_translated_total" ~data:record.procedures_translated_total
  |> add_int ~key:"procedures_translated_failed" ~data:record.procedures_translated_failed
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.source_file)


type analysis_stats =
  { num_preposts: int
  ; analysis_nodes_visited: int
  ; analysis_total_nodes: int
  ; symops: int
  ; method_location: Location.t
  ; lang: string
  ; clang_method_kind: ProcAttributes.clang_method_kind
  ; analysis_status: SymOp.failure_kind option
  ; method_name: string }

let create_analysis_stats_row base record =
  let open JsonBuilder in
  base |> add_int ~key:"num_preposts" ~data:record.num_preposts
  |> add_int ~key:"analysis_nodes_visited" ~data:record.analysis_nodes_visited
  |> add_int ~key:"analysis_total_nodes" ~data:record.analysis_total_nodes
  |> add_int ~key:"symops" ~data:record.symops
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.method_location.file)
  |> add_string ~key:"method_location"
       ~data:
         (String.concat
            [ string_of_int record.method_location.line
            ; ":"
            ; string_of_int record.method_location.col ])
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string ~key:"clang_method_kind"
       ~data:(ProcAttributes.string_of_clang_method_kind record.clang_method_kind)
  |> add_string ~key:"analysis_status"
       ~data:
         (Option.value_map record.analysis_status ~default:"OK" ~f:(fun stats_failure ->
              SymOp.failure_kind_to_string stats_failure ))
  |> add_string ~key:"method_name" ~data:record.method_name


type event =
  | UncaughtException of exn * int
  | FrontendException of frontend_exception
  | ProceduresTranslatedSummary of procedures_translated
  | AnalysisStats of analysis_stats

let string_of_event event =
  match event with
  | UncaughtException _ ->
      "UncaughtException"
  | FrontendException _ ->
      "FrontendException"
  | ProceduresTranslatedSummary _ ->
      "ProceduresTranslatedSummary"
  | AnalysisStats _ ->
      "AnalysisStats"


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
    empty |> add_string ~key:"command" ~data:(InferCommand.to_string Config.command)
    |> add_string ~key:"event_tag" ~data:(string_of_event event)
    |> add_string ~key:"hostname" ~data:(Unix.gethostname ())
    |> add_string ~key:"infer_commit" ~data:Version.commit
    |> add_int ~key:"is_originator" ~data:(if CLOpt.is_originator then 1 else 0)
    |> add_int ~key:"pid" ~data:(pid ())
    |> add_string ~key:"run_identifier" ~data:(get_log_identifier ())
    |> add_int ~key:"sequence" ~data:(!sequence_ctr - 1) |> add_string ~key:"sysname" ~data:sysname
    |> add_int ~key:"time" ~data:(int_of_float (Unix.time ()))
  in
  ( match event with
  | UncaughtException (exn, exitcode) ->
      base |> add_string ~key:"exception" ~data:(Caml.Printexc.exn_slot_name exn)
      |> add_string ~key:"exception_info" ~data:(Exn.to_string exn)
      |> add_int ~key:"exitcode" ~data:exitcode
  | FrontendException record ->
      create_frontend_exception_row base record
  | ProceduresTranslatedSummary record ->
      create_procedures_translated_row base record
  | AnalysisStats record ->
      create_analysis_stats_row base record )
  |> JsonBuilder.to_json


let prepare = IO.prepare

let log event = IO.write "%s\n" (create_row event)

let log_multiple events =
  let rows = List.map ~f:create_row events in
  let combinedJson =
    List.fold_right rows ~init:"" ~f:(fun row combined -> combined ^ row ^ "\n")
  in
  IO.write "%s" combinedJson


let dump = IO.dump
