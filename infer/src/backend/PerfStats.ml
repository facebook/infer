(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Performance Statistics gathering and reporting *)

open! IStd
module F = Format
module L = Logging

type perf_stats =
  { rtime: float
  ; utime: float
  ; stime: float
  ; cutime: float
  ; cstime: float
  ; minor_gb: float
  ; promoted_gb: float
  ; major_gb: float
  ; allocated_gb: float
  ; minor_collections: int
  ; major_collections: int
  ; compactions: int
  ; top_heap_gb: float
  ; stack_kb: float
  ; minor_heap_kb: float }

let to_json ps =
  `Assoc
    [ ("rtime", `Float ps.rtime)
    ; ("utime", `Float ps.utime)
    ; ("stime", `Float ps.stime)
    ; ("cutime", `Float ps.cutime)
    ; ("cstime", `Float ps.cstime)
    ; ("minor_gb", `Float ps.minor_gb)
    ; ("promoted_gb", `Float ps.promoted_gb)
    ; ("major_gb", `Float ps.major_gb)
    ; ("allocated_gb", `Float ps.allocated_gb)
    ; ("minor_collections", `Int ps.minor_collections)
    ; ("major_collections", `Int ps.major_collections)
    ; ("compactions", `Int ps.compactions)
    ; ("top_heap_gb", `Float ps.top_heap_gb)
    ; ("stack_kb", `Float ps.stack_kb)
    ; ("minor_heap_kb", `Float ps.minor_heap_kb) ]


let from_json json =
  let open! Yojson.Basic.Util in
  { rtime= json |> member "rtime" |> to_float
  ; utime= json |> member "utime" |> to_float
  ; stime= json |> member "stime" |> to_float
  ; cutime= json |> member "cutime" |> to_float
  ; cstime= json |> member "cstime" |> to_float
  ; minor_gb= json |> member "minor_gb" |> to_float
  ; promoted_gb= json |> member "promoted_gb" |> to_float
  ; major_gb= json |> member "major_gb" |> to_float
  ; allocated_gb= json |> member "allocated_gb" |> to_float
  ; minor_collections= json |> member "minor_collections" |> to_int
  ; major_collections= json |> member "major_collections" |> to_int
  ; compactions= json |> member "compactions" |> to_int
  ; top_heap_gb= json |> member "top_heap_gb" |> to_float
  ; stack_kb= json |> member "stack_kb" |> to_float
  ; minor_heap_kb= json |> member "minor_heap_kb" |> to_float }


let aggregate s =
  let mk_stats f = StatisticsToolbox.compute_statistics (List.map ~f s) in
  let aggr_rtime = mk_stats (fun stats -> stats.rtime) in
  let aggr_utime = mk_stats (fun stats -> stats.utime) in
  let aggr_stime = mk_stats (fun stats -> stats.stime) in
  let aggr_cutime = mk_stats (fun stats -> stats.cutime) in
  let aggr_cstime = mk_stats (fun stats -> stats.cstime) in
  let aggr_minor_gb = mk_stats (fun stats -> stats.minor_gb) in
  let aggr_promoted_gb = mk_stats (fun stats -> stats.promoted_gb) in
  let aggr_major_gb = mk_stats (fun stats -> stats.major_gb) in
  let aggr_allocated_gb = mk_stats (fun stats -> stats.allocated_gb) in
  let aggr_minor_collections = mk_stats (fun stats -> float_of_int stats.minor_collections) in
  let aggr_major_collections = mk_stats (fun stats -> float_of_int stats.major_collections) in
  let aggr_compactions = mk_stats (fun stats -> float_of_int stats.compactions) in
  let aggr_top_heap_gb = mk_stats (fun stats -> stats.top_heap_gb) in
  let aggr_stack_kb = mk_stats (fun stats -> stats.stack_kb) in
  let aggr_minor_heap_kb = mk_stats (fun stats -> stats.minor_heap_kb) in
  `Assoc
    [ ("rtime", StatisticsToolbox.to_json aggr_rtime)
    ; ("utime", StatisticsToolbox.to_json aggr_utime)
    ; ("stime", StatisticsToolbox.to_json aggr_stime)
    ; ("cutime", StatisticsToolbox.to_json aggr_cutime)
    ; ("cstime", StatisticsToolbox.to_json aggr_cstime)
    ; ("minor_gb", StatisticsToolbox.to_json aggr_minor_gb)
    ; ("promoted_gb", StatisticsToolbox.to_json aggr_promoted_gb)
    ; ("major_gb", StatisticsToolbox.to_json aggr_major_gb)
    ; ("allocated_gb", StatisticsToolbox.to_json aggr_allocated_gb)
    ; ("minor_collections", StatisticsToolbox.to_json aggr_minor_collections)
    ; ("major_collections", StatisticsToolbox.to_json aggr_major_collections)
    ; ("compactions", StatisticsToolbox.to_json aggr_compactions)
    ; ("top_heap_gb", StatisticsToolbox.to_json aggr_top_heap_gb)
    ; ("stack_kb", StatisticsToolbox.to_json aggr_stack_kb)
    ; ("minor_heap_kb", StatisticsToolbox.to_json aggr_minor_heap_kb) ]


let stats source_file stats_type =
  let bytes_per_word = Sys.word_size / 8 in
  let words_to_bytes n = n *. float_of_int bytes_per_word in
  let words_to_kb n = words_to_bytes n /. 1024. in
  let words_to_mb n = words_to_kb n /. 1024. in
  let words_to_gb n = words_to_mb n /. 1024. in
  let gc_stats = Gc.quick_stat () in
  let allocated_words = gc_stats.minor_words +. gc_stats.major_words -. gc_stats.promoted_words in
  let gc_ctrl = Gc.get () in
  let exit_times = Unix.times () in
  let rtime = Mtime_clock.elapsed () |> Mtime.Span.to_s in
  let utime = exit_times.tms_utime -. Utils.initial_times.tms_utime in
  let stime = exit_times.tms_stime -. Utils.initial_times.tms_stime in
  let cutime = exit_times.tms_cutime -. Utils.initial_times.tms_cutime in
  let cstime = exit_times.tms_cstime -. Utils.initial_times.tms_cstime in
  let stats =
    { rtime
    ; utime
    ; stime
    ; cutime
    ; cstime
    ; minor_gb= words_to_gb gc_stats.minor_words
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
  let stats_event =
    EventLogger.PerformanceStats
      { lang= Language.to_explicit_string !Language.curr_language
      ; source_file
      ; stats_type
      ; real_time= rtime
      ; user_time= utime
      ; sys_time= stime
      ; children_user_time= cutime
      ; children_sys_time= cstime
      ; minor_heap_mem= words_to_bytes gc_stats.minor_words
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
  (stats, stats_event)


let report_at_exit file source_file stats_type () =
  try
    let stats, stats_event = stats source_file stats_type in
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


let register_report_at_exit =
  let registered_files = String.Table.create ~size:4 () in
  fun filename ?source_file dirname ->
    let dir = Filename.concat Config.results_dir dirname in
    let file = Filename.concat dir filename in
    (* take care of not double-registering the same perf stat report *)
    if not (Hashtbl.mem registered_files file) then (
      String.Table.set registered_files ~key:file ~data:() ;
      if not Config.buck_cache_mode then
        Epilogues.register
          ~f:(report_at_exit file source_file dirname)
          ("stats reporting in " ^ file) )
