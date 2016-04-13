(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Performance Statistics gathering and reporting *)

open! Utils

let register_report_at_exit file =
  Pervasives.at_exit (fun () ->
      let gc_stats = Gc.quick_stat () in
      let exit_timeofday = Unix.gettimeofday () in
      let exit_times = Unix.times () in
      let stats =
        `Assoc [
          ("rtime", `Float (exit_timeofday -. initial_timeofday)) ;
          ("utime", `Float (exit_times.tms_utime -. initial_times.tms_utime)) ;
          ("stime", `Float (exit_times.tms_stime -. initial_times.tms_stime)) ;
          ("cutime", `Float (exit_times.tms_cutime -. initial_times.tms_cutime)) ;
          ("cstime", `Float (exit_times.tms_cstime -. initial_times.tms_cstime)) ;
          ("minor_words", `Float gc_stats.minor_words) ;
          ("promoted_words", `Float gc_stats.promoted_words) ;
          ("major_words", `Float gc_stats.major_words) ;
          ("minor_collections", `Int gc_stats.minor_collections) ;
          ("major_collections", `Int gc_stats.major_collections) ;
          ("compactions", `Int gc_stats.compactions) ;
          ("top_heap_words", `Int gc_stats.top_heap_words) ;
        ] in
      let stats_oc = open_out file in
      Yojson.Basic.pretty_to_channel stats_oc stats ;
      close_out stats_oc
    )
