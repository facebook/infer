(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module Gc = Caml.Gc

(** a subset of {!Gc.stat} that can be aggregated across multiple processes *)
type t =
  { minor_words: float
  ; promoted_words: float
  ; major_words: float
  ; minor_collections: int
  ; major_collections: int
  ; compactions: int
  ; top_heap_words: int }

type since = ProgramStart | PreviousStats of t

let get ~since =
  (* Gc.quick_stat is much faster than Gc.stat because it doesn't calculate live_words/blocks,
     free_words/blocks, largest_free or fragments. We don't use those fields anyway, so no reason not
     to use quick_stat. *)
  let stats = Gc.quick_stat () in
  match since with
  | ProgramStart ->
      { minor_words= stats.minor_words
      ; promoted_words= stats.promoted_words
      ; major_words= stats.major_words
      ; minor_collections= stats.minor_collections
      ; major_collections= stats.major_collections
      ; compactions= stats.compactions
      ; top_heap_words= stats.top_heap_words }
  | PreviousStats stats_prev ->
      (* [top_heap_words] is going to be inaccurate if it was reached in the previous time period *)
      { minor_words= stats.minor_words -. stats_prev.minor_words
      ; promoted_words= stats.promoted_words -. stats_prev.promoted_words
      ; major_words= stats.major_words -. stats_prev.major_words
      ; minor_collections= stats.minor_collections - stats_prev.minor_collections
      ; major_collections= stats.major_collections - stats_prev.major_collections
      ; compactions= stats.compactions - stats_prev.compactions
      ; top_heap_words= stats.top_heap_words }


let pp f
    ({ minor_words
     ; promoted_words
     ; major_words
     ; minor_collections
     ; major_collections
     ; compactions
     ; top_heap_words } [@warning "+missing-record-field-pattern"] ) =
  F.fprintf f
    "@[<v2>  minor_words: %g@;\
     promoted_words: %g@;\
     major_words: %g@;\
     minor_collections: %d@;\
     major_collections: %d@;\
     compactions: %d@;\
     top_heap_words: %d@;\
     @]"
    minor_words promoted_words major_words minor_collections major_collections compactions
    top_heap_words


let to_scuba_entries ~prefix (stats : t) =
  let create_counter field value =
    LogEntry.mk_count ~label:(Printf.sprintf "%s_gc_stats.%s" prefix field) ~value
  in
  [ create_counter "minor_words" (Float.to_int stats.minor_words)
  ; create_counter "promoted_words" (Float.to_int stats.promoted_words)
  ; create_counter "major_words" (Float.to_int stats.major_words)
  ; create_counter "minor_collections" stats.minor_collections
  ; create_counter "major_collections" stats.major_collections
  ; create_counter "compactions" stats.compactions
  ; create_counter "top_heap_words" stats.top_heap_words ]


let log_to_scuba ~prefix stats = ScubaLogging.log_many (to_scuba_entries ~prefix stats)

let log ~name debug_kind stats =
  L.debug debug_kind Quiet "@[GC stats for %s:@\n%a@]@." name pp stats ;
  log_to_scuba ~prefix:name stats


let log_f ~name debug_kind f =
  let stats_before_f = get ~since:ProgramStart in
  let r = f () in
  let stats_f = get ~since:(PreviousStats stats_before_f) in
  log ~name debug_kind stats_f ;
  r


let merge_add stats1 stats2 =
  { minor_words= stats1.minor_words +. stats2.minor_words
  ; promoted_words= stats1.promoted_words +. stats2.promoted_words
  ; major_words= stats1.major_words +. stats2.major_words
  ; minor_collections= stats1.minor_collections + stats2.minor_collections
  ; major_collections= stats1.major_collections + stats2.major_collections
  ; compactions= stats1.compactions + stats2.compactions
  ; top_heap_words= stats1.top_heap_words + stats2.top_heap_words }


let merge_max stats1 stats2 =
  { minor_words= Float.max stats1.minor_words stats2.minor_words
  ; promoted_words= Float.max stats1.promoted_words stats2.promoted_words
  ; major_words= Float.max stats1.major_words stats2.major_words
  ; minor_collections= max stats1.minor_collections stats2.minor_collections
  ; major_collections= max stats1.major_collections stats2.major_collections
  ; compactions= max stats1.compactions stats2.compactions
  ; top_heap_words= max stats1.top_heap_words stats2.top_heap_words }


let log_aggregate ~prefix debug_kind stats_list =
  match stats_list with
  | [] ->
      ()
  | [stats] ->
      log ~name:prefix debug_kind stats
  | stats_one :: stats_rest ->
      let stats_add = List.fold ~init:stats_one stats_rest ~f:merge_add in
      let stats_max = List.fold ~init:stats_one stats_rest ~f:merge_max in
      let n = List.length stats_list in
      let stats_average =
        let n_f = Float.of_int n in
        { minor_words= stats_add.minor_words /. n_f
        ; promoted_words= stats_add.promoted_words /. n_f
        ; major_words= stats_add.major_words /. n_f
        ; minor_collections= stats_add.minor_collections / n
        ; major_collections= stats_add.major_collections / n
        ; compactions= stats_add.compactions / n
        ; top_heap_words= stats_add.top_heap_words / n }
      in
      log ~name:(prefix ^ "gc_stats_add.") debug_kind stats_add ;
      log ~name:(prefix ^ "gc_stats_max.") debug_kind stats_max ;
      L.debug debug_kind Quiet "Average over %d processes@\n" n ;
      log ~name:(prefix ^ "gc_stats_average.") debug_kind stats_average
