(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
    close () ;
    let fname = events_dir ^/ (Unix.getpid () |> Pid.to_string) ^ log_file_extension in
    let oc = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 fname in
    out_chan := Some oc


  let write fmt =
    match !out_chan with Some oc -> Printf.fprintf oc fmt | _ -> Printf.ifprintf stdout fmt


  let write_skipped_pname pname =
    let fname = events_dir ^/ "skipped_functions" ^ log_file_extension in
    let oc = Pervasives.open_out_gen [Open_append; Open_creat] 0o666 fname in
    Out_channel.output_string oc pname ;
    Out_channel.output_char oc '\n' ;
    Out_channel.close oc


  let dump () =
    let dump_file_to_stdout fname =
      let ic = In_channel.create fname in
      In_channel.iter_lines ic ~f:print_endline
    in
    let log_files = Utils.find_files ~path:events_dir ~extension:log_file_extension in
    List.iter log_files ~f:dump_file_to_stdout


  let () = Epilogues.register_late ~f:close ~description:"closing EventLogger file"
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

type analysis_issue =
  { bug_kind: string
  ; bug_type: string
  ; clang_method_kind: string option
  ; exception_triggered_location: Logging.ocaml_pos option
  ; lang: string
  ; procedure_name: string
  ; source_location: Location.t }

let create_analysis_issue_row base record =
  let open JsonBuilder in
  base
  |> add_string ~key:"bug_kind" ~data:record.bug_kind
  |> add_string ~key:"bug_type" ~data:record.bug_type
  |> add_string_opt ~key:"clang_method_kind" ~data:record.clang_method_kind
  |> add_string_opt ~key:"exception_triggered_location"
       ~data:(Option.map ~f:Logging.ocaml_pos_to_string record.exception_triggered_location)
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string ~key:"procedure_name" ~data:record.procedure_name
  |> add_string ~key:"source_location"
       ~data:
         (String.concat
            [ string_of_int record.source_location.line
            ; ":"
            ; string_of_int record.source_location.col ])
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.source_location.file)


type analysis_stats =
  { analysis_nodes_visited: int
  ; analysis_status: SymOp.failure_kind option
  ; analysis_total_nodes: int
  ; clang_method_kind: string option
  ; lang: string
  ; method_location: Location.t
  ; method_name: string
  ; num_preposts: int
  ; symops: int }

let create_analysis_stats_row base record =
  let open JsonBuilder in
  base
  |> add_int ~key:"analysis_nodes_visited" ~data:record.analysis_nodes_visited
  |> add_string ~key:"analysis_status"
       ~data:
         (Option.value_map record.analysis_status ~default:"OK" ~f:(fun stats_failure ->
              SymOp.failure_kind_to_string stats_failure ))
  |> add_int ~key:"analysis_total_nodes" ~data:record.analysis_total_nodes
  |> add_string_opt ~key:"clang_method_kind" ~data:record.clang_method_kind
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string ~key:"method_location"
       ~data:
         (String.concat
            [ string_of_int record.method_location.line
            ; ":"
            ; string_of_int record.method_location.col ])
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.method_location.file)
  |> add_string ~key:"method_name" ~data:record.method_name
  |> add_int ~key:"num_preposts" ~data:record.num_preposts
  |> add_int ~key:"symops" ~data:record.symops


type dynamic_dispatch =
  | Dynamic_dispatch_successful
  | Dynamic_dispatch_parameters_arguments_mismatch
  | Dynamic_dispatch_model_specialization_failure

let string_of_dynamic_dispatch_opt dd =
  match dd with
  | Some Dynamic_dispatch_successful ->
      "dynamic dispatch successful"
  | Some Dynamic_dispatch_parameters_arguments_mismatch ->
      "dynamic dispatch failed with arguments mismatch"
  | Some Dynamic_dispatch_model_specialization_failure ->
      "dynamic dispatch model specialized failed"
  | None ->
      "no dynamic dispatch"


type call_trace =
  { call_location: Location.t
  ; call_result: string
  ; callee_clang_method_kind: string option
  ; callee_source_file: SourceFile.t option
  ; callee_name: string
  ; caller_name: string
  ; lang: string
  ; reason: string option
  ; dynamic_dispatch: dynamic_dispatch option }

let create_call_trace_row base record =
  let open JsonBuilder in
  base
  |> add_string ~key:"call_location"
       ~data:
         (String.concat
            [string_of_int record.call_location.line; ":"; string_of_int record.call_location.col])
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.call_location.file)
  |> add_string ~key:"call_result" ~data:record.call_result
  |> add_string_opt ~key:"callee_clang_method_kind" ~data:record.callee_clang_method_kind
  |> add_string_opt ~key:"callee_source_file"
       ~data:(Option.map ~f:SourceFile.to_rel_path record.callee_source_file)
  |> add_string ~key:"callee_name" ~data:record.callee_name
  |> add_string ~key:"caller_name" ~data:record.caller_name
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string_opt ~key:"reason" ~data:record.reason
  |> add_string ~key:"dynamic_dispatch"
       ~data:(string_of_dynamic_dispatch_opt record.dynamic_dispatch)


type frontend_exception =
  { ast_node: string option
  ; exception_triggered_location: Logging.ocaml_pos
  ; exception_type: string
  ; lang: string
  ; source_location_start: Location.t
  ; source_location_end: Location.t }

let create_frontend_exception_row base record =
  let open JsonBuilder in
  base
  |> add_string_opt ~key:"ast_node" ~data:record.ast_node
  |> add_string ~key:"exception_triggered_location"
       ~data:(Logging.ocaml_pos_to_string record.exception_triggered_location)
  |> add_string ~key:"exception_type" ~data:record.exception_type
  |> add_string ~key:"lang" ~data:record.lang
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


type mem_perf =
  { minor_heap_mem: float
  ; promoted_minor_heap_mem: float
  ; major_heap_mem: float
  ; total_allocated_mem: float
  ; minor_collections: int
  ; major_collections: int
  ; heap_compactions: int
  ; top_heap_size: int
  ; stack_size: int
  ; minor_heap_size: int }

type time_perf =
  { real_time: float
  ; user_time: float
  ; sys_time: float
  ; children_user_time: float
  ; children_sys_time: float }

type performance_stats =
  { lang: string
  ; source_file: SourceFile.t option
  ; stats_type: string
  ; mem_perf: mem_perf option
  ; time_perf: time_perf option }

let create_performance_stats_row base record =
  let open JsonBuilder in
  let add_mem_perf t =
    Option.value_map ~default:t record.mem_perf ~f:(fun mem_perf ->
        t
        |> add_float ~key:"minor_heap_mem" ~data:mem_perf.minor_heap_mem
        |> add_float ~key:"promoted_minor_heap_mem" ~data:mem_perf.promoted_minor_heap_mem
        |> add_float ~key:"major_heap_mem" ~data:mem_perf.major_heap_mem
        |> add_float ~key:"total_allocated_mem" ~data:mem_perf.total_allocated_mem
        |> add_int ~key:"minor_collections" ~data:mem_perf.minor_collections
        |> add_int ~key:"major_collections" ~data:mem_perf.major_collections
        |> add_int ~key:"heap_compactions" ~data:mem_perf.heap_compactions
        |> add_int ~key:"top_heap_size" ~data:mem_perf.top_heap_size
        |> add_int ~key:"stack_size" ~data:mem_perf.stack_size
        |> add_int ~key:"minor_heap_size" ~data:mem_perf.minor_heap_size )
  in
  let add_time_perf t =
    Option.value_map ~default:t record.time_perf ~f:(fun time_perf ->
        t
        |> add_float ~key:"real_time" ~data:time_perf.real_time
        |> add_float ~key:"user_time" ~data:time_perf.user_time
        |> add_float ~key:"sys_time" ~data:time_perf.sys_time
        |> add_float ~key:"children_user_time" ~data:time_perf.children_user_time
        |> add_float ~key:"children_sys_time" ~data:time_perf.children_sys_time )
  in
  base
  |> add_string ~key:"lang" ~data:record.lang
  |> add_string_opt ~key:"source_file"
       ~data:(Option.map ~f:SourceFile.to_rel_path record.source_file)
  |> add_string ~key:"stats_type" ~data:record.stats_type
  |> add_mem_perf |> add_time_perf


type procedures_translated =
  { lang: string
  ; procedures_translated_failed: int
  ; procedures_translated_total: int
  ; source_file: SourceFile.t }

let create_procedures_translated_row base record =
  let open JsonBuilder in
  base
  |> add_string ~key:"lang" ~data:record.lang
  |> add_int ~key:"procedures_translated_failed" ~data:record.procedures_translated_failed
  |> add_int ~key:"procedures_translated_total" ~data:record.procedures_translated_total
  |> add_string ~key:"source_file" ~data:(SourceFile.to_rel_path record.source_file)


type event =
  | AnalysisIssue of analysis_issue
  | AnalysisStats of analysis_stats
  | CallTrace of call_trace
  | FrontendException of frontend_exception
  | PerformanceStats of performance_stats
  | ProceduresTranslatedSummary of procedures_translated
  | UncaughtException of exn * int

let string_of_event event =
  match event with
  | AnalysisIssue _ ->
      "AnalysisIssue"
  | AnalysisStats _ ->
      "AnalysisStats"
  | CallTrace _ ->
      "CallTrace"
  | FrontendException _ ->
      "FrontendException"
  | PerformanceStats _ ->
      "PerformanceStats"
  | ProceduresTranslatedSummary _ ->
      "ProceduresTranslatedSummary"
  | UncaughtException _ ->
      "UncaughtException"


let sequence_ctr = ref 0

let pid () = Pid.to_int (Unix.getpid ())

let sysname =
  try
    Utils.with_process_in "uname 2>/dev/null" (fun chan ->
        Scanf.bscanf (Scanf.Scanning.from_channel chan) "%s" (fun n -> n) )
    |> fst
  with _ -> "Unknown"


module type S = sig
  val get_log_identifier : unit -> string

  val prepare : unit -> unit

  val log : event -> unit

  val log_skipped_pname : string -> unit

  val dump : unit -> unit
end

module LoggerImpl : S = struct
  let get_log_identifier () = Random_id.get ()

  let create_row event =
    incr sequence_ctr ;
    let open JsonBuilder in
    let base =
      empty
      |> add_string ~key:"command" ~data:(InferCommand.to_string Config.command)
      |> add_string ~key:"event_tag" ~data:(string_of_event event)
      |> add_string ~key:"hostname" ~data:(Unix.gethostname ())
      |> add_string ~key:"infer_commit" ~data:Version.commit
      |> add_int ~key:"is_originator" ~data:(if CLOpt.is_originator then 1 else 0)
      |> add_string_opt ~key:"job_id" ~data:Config.job_id
      |> add_int ~key:"pid" ~data:(pid ())
      |> add_string ~key:"run_identifier" ~data:(get_log_identifier ())
      |> add_int ~key:"sequence" ~data:(!sequence_ctr - 1)
      |> add_string ~key:"sysname" ~data:sysname
      |> add_int ~key:"time" ~data:(int_of_float (Unix.time ()))
    in
    ( match event with
    | AnalysisIssue record ->
        create_analysis_issue_row base record
    | AnalysisStats record ->
        create_analysis_stats_row base record
    | CallTrace record ->
        create_call_trace_row base record
    | FrontendException record ->
        create_frontend_exception_row base record
    | PerformanceStats record ->
        create_performance_stats_row base record
    | ProceduresTranslatedSummary record ->
        create_procedures_translated_row base record
    | UncaughtException (exn, exitcode) ->
        base
        |> add_string ~key:"exception" ~data:(Caml.Printexc.exn_slot_name exn)
        |> add_string ~key:"exception_info" ~data:(Exn.to_string exn)
        |> add_int ~key:"exitcode" ~data:exitcode )
    |> JsonBuilder.to_json


  let prepare = IO.prepare

  let log event = IO.write "%s\n" (create_row event)

  let dump = IO.dump

  let log_skipped_pname pname = if Config.log_skipped then IO.write_skipped_pname pname else ()
end

module DummyLogger : S = struct
  let get_log_identifier () = ""

  let prepare () = ()

  let log _ = ()

  let dump _ = ()

  let log_skipped_pname _ = ()
end

(* use real logger if logging is enabled, dummy logger otherwise *)
include (val if Config.log_events then (module LoggerImpl : S) else (module DummyLogger : S))
