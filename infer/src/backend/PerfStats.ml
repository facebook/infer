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

let words_to_kb n = n *. float_of_int (Sys.word_size / 8) /. 1024.
let words_to_mb n = words_to_kb n /. 1024.
let words_to_gb n = words_to_mb n /. 1024.

let register_report_at_exit file =
  Pervasives.at_exit (fun () ->
      try
        let gc_stats = Gc.quick_stat () in
        let allocated_words =
          gc_stats.minor_words +. gc_stats.major_words -. gc_stats.promoted_words in
        let gc_ctrl = Gc.get () in
        let exit_timeofday = Unix.gettimeofday () in
        let exit_times = Unix.times () in
        let stats =
          `Assoc [
            ("rtime", `Float (exit_timeofday -. initial_timeofday)) ;
            ("utime", `Float (exit_times.tms_utime -. initial_times.tms_utime)) ;
            ("stime", `Float (exit_times.tms_stime -. initial_times.tms_stime)) ;
            ("cutime", `Float (exit_times.tms_cutime -. initial_times.tms_cutime)) ;
            ("cstime", `Float (exit_times.tms_cstime -. initial_times.tms_cstime)) ;
            ("minor_gb", `Float (words_to_gb gc_stats.minor_words)) ;
            ("promoted_gb", `Float (words_to_gb gc_stats.promoted_words)) ;
            ("major_gb", `Float (words_to_gb gc_stats.major_words)) ;
            ("allocated_gb", `Float (words_to_gb allocated_words)) ;
            ("minor_collections", `Int gc_stats.minor_collections) ;
            ("major_collections", `Int gc_stats.major_collections) ;
            ("compactions", `Int gc_stats.compactions) ;
            ("top_heap_gb", `Float (words_to_gb (float_of_int gc_stats.top_heap_words))) ;
            ("stack_kb", `Float (words_to_kb (float_of_int gc_stats.stack_size))) ;
            ("minor_heap_kb", `Float (words_to_kb (float_of_int gc_ctrl.minor_heap_size))) ;
          ] in
        try
          let stats_oc = open_out file in
          Yojson.pretty_to_channel stats_oc stats ;
          close_out stats_oc
        with exc ->
          Format.eprintf "Info: failed to write stats to %s@\n%s@\n%s@\n%s@."
            file (Printexc.to_string exc) (Yojson.pretty_to_string stats)
            (Printexc.get_backtrace ())
      with exc ->
        Format.eprintf "Info: failed to compute stats for %s@\n%s@\n%s@."
          file (Printexc.to_string exc) (Printexc.get_backtrace ())
    )
