(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Performance Statistics gathering and reporting *)

open! IStd
module F = Format
module L = Logging

type mem_perf =
  { minor_gb: float
  ; promoted_gb: float
  ; major_gb: float
  ; allocated_gb: float
  ; minor_collections: int
  ; major_collections: int
  ; compactions: int
  ; top_heap_gb: float
  ; stack_kb: float
  ; minor_heap_kb: float }

type time_perf = {rtime: float; utime: float; stime: float; cutime: float; cstime: float}

type perf_stats = {mem: mem_perf option; time: time_perf option}

type stats_kind = Time of Mtime_clock.counter * Unix.process_times | Memory | TimeAndMemory

type stats_type =
  | ClangLinters of SourceFile.t
  | ClangFrontend of SourceFile.t
  | ClangFrontendLinters of SourceFile.t
  | JavaFrontend of SourceFile.t
  | TotalFrontend
  | Backend of SourceFile.t
  | TotalBackend
  | Reporting
  | Driver

let source_file_of_stats_type = function
  | ClangLinters source_file
  | ClangFrontend source_file
  | ClangFrontendLinters source_file
  | JavaFrontend source_file
  | Backend source_file ->
      Some source_file
  | _ ->
      None


let relative_path_of_stats_type stats_type =
  let abbrev_source_file =
    Option.map ~f:DB.source_file_encoding (source_file_of_stats_type stats_type)
  in
  let filename =
    match abbrev_source_file with
    | Some abbrev ->
        F.sprintf "%s_%s.json" Config.perf_stats_prefix abbrev
    | None ->
        F.sprintf "%s.json" Config.perf_stats_prefix
  in
  let dirname =
    match stats_type with
    | ClangLinters _ ->
        Config.frontend_stats_dir_name
    | ClangFrontend _ ->
        Config.frontend_stats_dir_name
    | ClangFrontendLinters _ ->
        Config.frontend_stats_dir_name
    | JavaFrontend _ ->
        Config.frontend_stats_dir_name
    | TotalFrontend ->
        Config.frontend_stats_dir_name
    | Backend _ ->
        Config.backend_stats_dir_name
    | TotalBackend ->
        Config.backend_stats_dir_name
    | Reporting ->
        Config.reporting_stats_dir_name
    | Driver ->
        Config.driver_stats_dir_name
  in
  Filename.concat dirname filename


let string_of_stats_type = function
  | ClangLinters _ ->
      "linters"
  | ClangFrontend _ ->
      "clang_frontend"
  | ClangFrontendLinters _ ->
      "clang_frontend_and_linters"
  | JavaFrontend _ ->
      "java_frontend"
  | TotalFrontend ->
      "total_frontend"
  | Backend _ ->
      "backend"
  | TotalBackend ->
      "total_backend"
  | Reporting ->
      "reporting"
  | Driver ->
      "driver"


let to_json ps =
  let time =
    Option.value_map ~default:[] ps.time ~f:(fun time_perf ->
        [ ( "time"
          , `Assoc
              [ ("rtime", `Float time_perf.rtime)
              ; ("utime", `Float time_perf.utime)
              ; ("stime", `Float time_perf.stime)
              ; ("cutime", `Float time_perf.cutime)
              ; ("cstime", `Float time_perf.cstime) ] ) ] )
  in
  let mem =
    Option.value_map ~default:[] ps.mem ~f:(fun mem_perf ->
        [ ( "mem"
          , `Assoc
              [ ("minor_gb", `Float mem_perf.minor_gb)
              ; ("promoted_gb", `Float mem_perf.promoted_gb)
              ; ("major_gb", `Float mem_perf.major_gb)
              ; ("allocated_gb", `Float mem_perf.allocated_gb)
              ; ("minor_collections", `Int mem_perf.minor_collections)
              ; ("major_collections", `Int mem_perf.major_collections)
              ; ("compactions", `Int mem_perf.compactions)
              ; ("top_heap_gb", `Float mem_perf.top_heap_gb)
              ; ("stack_kb", `Float mem_perf.stack_kb)
              ; ("minor_heap_kb", `Float mem_perf.minor_heap_kb) ] ) ] )
  in
  `Assoc (time @ mem)


let from_json json =
  let open! Yojson.Basic.Util in
  let mem =
    json |> member "mem"
    |> to_option (fun mem_perf ->
           { minor_gb= mem_perf |> member "minor_gb" |> to_float
           ; promoted_gb= mem_perf |> member "promoted_gb" |> to_float
           ; major_gb= mem_perf |> member "major_gb" |> to_float
           ; allocated_gb= mem_perf |> member "allocated_gb" |> to_float
           ; minor_collections= mem_perf |> member "minor_collections" |> to_int
           ; major_collections= mem_perf |> member "major_collections" |> to_int
           ; compactions= mem_perf |> member "compactions" |> to_int
           ; top_heap_gb= mem_perf |> member "top_heap_gb" |> to_float
           ; stack_kb= mem_perf |> member "stack_kb" |> to_float
           ; minor_heap_kb= mem_perf |> member "minor_heap_kb" |> to_float } )
  in
  let time =
    json |> member "time"
    |> to_option (fun time_perf ->
           { rtime= time_perf |> member "rtime" |> to_float
           ; utime= time_perf |> member "utime" |> to_float
           ; stime= time_perf |> member "stime" |> to_float
           ; cutime= time_perf |> member "cutime" |> to_float
           ; cstime= time_perf |> member "cstime" |> to_float } )
  in
  {mem; time}


let aggregate_mem_stats s =
  let mk_stats f =
    StatisticsToolbox.compute_statistics
      (List.filter_map s ~f:(fun stats -> Option.map stats.mem ~f))
  in
  let aggr_minor_gb = mk_stats (fun mem_perf -> mem_perf.minor_gb) in
  let aggr_promoted_gb = mk_stats (fun mem_perf -> mem_perf.promoted_gb) in
  let aggr_major_gb = mk_stats (fun mem_perf -> mem_perf.major_gb) in
  let aggr_allocated_gb = mk_stats (fun mem_perf -> mem_perf.allocated_gb) in
  let aggr_minor_collections = mk_stats (fun mem -> float_of_int mem.minor_collections) in
  let aggr_major_collections = mk_stats (fun mem -> float_of_int mem.major_collections) in
  let aggr_compactions = mk_stats (fun mem -> float_of_int mem.compactions) in
  let aggr_top_heap_gb = mk_stats (fun mem -> mem.top_heap_gb) in
  let aggr_stack_kb = mk_stats (fun mem -> mem.stack_kb) in
  let aggr_minor_heap_kb = mk_stats (fun mem -> mem.minor_heap_kb) in
  [ ("minor_gb", aggr_minor_gb)
  ; ("promoted_gb", aggr_promoted_gb)
  ; ("major_gb", aggr_major_gb)
  ; ("allocated_gb", aggr_allocated_gb)
  ; ("minor_collections", aggr_minor_collections)
  ; ("major_collections", aggr_major_collections)
  ; ("compactions", aggr_compactions)
  ; ("top_heap_gb", aggr_top_heap_gb)
  ; ("stack_kb", aggr_stack_kb)
  ; ("minor_heap_kb", aggr_minor_heap_kb) ]


let aggregate_time_stats s =
  let mk_stats f =
    StatisticsToolbox.compute_statistics
      (List.filter_map s ~f:(fun stats -> Option.map stats.time ~f))
  in
  let aggr_rtime = mk_stats (fun time -> time.rtime) in
  let aggr_utime = mk_stats (fun time -> time.utime) in
  let aggr_stime = mk_stats (fun time -> time.stime) in
  let aggr_cutime = mk_stats (fun time -> time.cutime) in
  let aggr_cstime = mk_stats (fun time -> time.cstime) in
  [ ("rtime", aggr_rtime)
  ; ("utime", aggr_utime)
  ; ("stime", aggr_stime)
  ; ("cutime", aggr_cutime)
  ; ("cstime", aggr_cstime) ]


let aggregate s =
  let build_json_list =
    List.fold_right ~init:[] ~f:(fun (key, stats) l ->
        match stats with Some stats -> (key, StatisticsToolbox.to_json stats) :: l | None -> l )
  in
  let mem_stats = build_json_list (aggregate_mem_stats s) in
  let time_stats = build_json_list (aggregate_time_stats s) in
  let mem_json = if List.is_empty mem_stats then [] else [("mem", `Assoc mem_stats)] in
  let time_json = if List.is_empty time_stats then [] else [("time", `Assoc time_stats)] in
  `Assoc (time_json @ mem_json)


let compute_mem_stats () =
  let bytes_per_word = Sys.word_size / 8 in
  let words_to_bytes n = n *. float_of_int bytes_per_word in
  let words_to_kb n = words_to_bytes n /. 1024. in
  let words_to_mb n = words_to_kb n /. 1024. in
  let words_to_gb n = words_to_mb n /. 1024. in
  let gc_stats = Gc.quick_stat () in
  let allocated_words = gc_stats.minor_words +. gc_stats.major_words -. gc_stats.promoted_words in
  let gc_ctrl = Gc.get () in
  let stats =
    Some
      { minor_gb= words_to_gb gc_stats.minor_words
      ; promoted_gb= words_to_gb gc_stats.promoted_words
      ; major_gb= words_to_gb gc_stats.major_words
      ; allocated_gb= words_to_gb allocated_words
      ; minor_collections= gc_stats.minor_collections
      ; major_collections= gc_stats.major_collections
      ; compactions= gc_stats.compactions
      ; top_heap_gb= words_to_gb (float_of_int gc_stats.top_heap_words)
      ; stack_kb= words_to_kb (float_of_int gc_stats.stack_size)
      ; minor_heap_kb= words_to_kb (float_of_int gc_ctrl.minor_heap_size) }
  in
  (* We log number of bytes instead of a larger unit in EventLogger so the EventLogger output can
    display in whatever format fits best *)
  let mem =
    Some
      { EventLogger.minor_heap_mem= words_to_bytes gc_stats.minor_words
      ; promoted_minor_heap_mem= words_to_bytes gc_stats.promoted_words
      ; major_heap_mem= words_to_bytes gc_stats.major_words
      ; total_allocated_mem= words_to_bytes allocated_words
      ; minor_collections= gc_stats.minor_collections
      ; major_collections= gc_stats.major_collections
      ; heap_compactions= gc_stats.compactions
      ; top_heap_size= gc_stats.top_heap_words * bytes_per_word
      ; stack_size= gc_stats.stack_size * bytes_per_word
      ; minor_heap_size= gc_ctrl.minor_heap_size * bytes_per_word }
  in
  (stats, mem)


let compute_time_stats ?rtime_counter (initial_times : Unix.process_times) =
  let exit_times = Unix.times () in
  let rtime_span = Mtime_clock.elapsed () in
  let rtime =
    Option.value_map ~default:rtime_span ~f:Mtime_clock.count rtime_counter |> Mtime.Span.to_s
  in
  let utime = exit_times.tms_utime -. initial_times.tms_utime in
  let stime = exit_times.tms_stime -. initial_times.tms_stime in
  let cutime = exit_times.tms_cutime -. initial_times.tms_cutime in
  let cstime = exit_times.tms_cstime -. initial_times.tms_cstime in
  let stats = Some {rtime; utime; stime; cutime; cstime} in
  let time =
    Some
      { EventLogger.real_time= rtime
      ; user_time= utime
      ; sys_time= stime
      ; children_user_time= cutime
      ; children_sys_time= cstime }
  in
  (stats, time)


let compute_stats stats_kind stats_type =
  let (mem, mem_perf), (time, time_perf) =
    match stats_kind with
    | Time (rtime_counter, initial_times) ->
        ((None, None), compute_time_stats ~rtime_counter initial_times)
    | Memory ->
        (compute_mem_stats (), (None, None))
    | TimeAndMemory ->
        (compute_mem_stats (), compute_time_stats Utils.initial_times)
  in
  let stats = {mem; time} in
  let stats_event =
    EventLogger.PerformanceStats
      { lang= Language.to_explicit_string !Language.curr_language
      ; source_file= source_file_of_stats_type stats_type
      ; stats_type= string_of_stats_type stats_type
      ; mem_perf
      ; time_perf }
  in
  (stats, stats_event)


let report stats_kind file stats_type () =
  try
    let stats, stats_event = compute_stats stats_kind stats_type in
    let json_stats = to_json stats in
    EventLogger.log stats_event ;
    (* We always log to EventLogger, but json files are unnecessary to log outside of developer mode *)
    if Config.developer_mode then
      try
        Unix.mkdir_p (Filename.dirname file) ;
        (* the same report may be registered across different infer processes *)
        Utils.write_file_with_locking file ~f:(fun stats_oc ->
            Yojson.Basic.pretty_to_channel stats_oc json_stats )
      with exc ->
        L.internal_error "Info: failed to write stats to %s@\n%s@\n%s@\n%s@." file
          (Exn.to_string exc)
          (Yojson.Basic.pretty_to_string json_stats)
          (Printexc.get_backtrace ())
  with exc ->
    L.internal_error "Info: failed to compute stats for %s@\n%s@\n%s@." file (Exn.to_string exc)
      (Printexc.get_backtrace ())


let registered = String.Table.create ~size:4 ()

let register_report stats_kind stats_type =
  let relative_path = relative_path_of_stats_type stats_type in
  let absolute_path = Filename.concat Config.results_dir relative_path in
  let f = report stats_kind absolute_path stats_type in
  (* make sure to not double register the same perf stat report *)
  match String.Table.add registered ~key:relative_path ~data:f with
  | `Ok ->
      ()
  | `Duplicate ->
      L.d_warning "Attempting to register same perf stats report multiple times"


let dummy_reporter () = ()

let get_reporter stats_type =
  let relative_path = relative_path_of_stats_type stats_type in
  String.Table.find registered relative_path |> Option.value ~default:dummy_reporter


let register_report_at_exit stats_type =
  let relative_path = relative_path_of_stats_type stats_type in
  register_report TimeAndMemory stats_type ;
  Epilogues.register ~f:(get_reporter stats_type)
    ~description:("stats reporting in " ^ relative_path)
