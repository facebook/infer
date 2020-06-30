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
  ; max_top_heap_words: int  (** like [Gc.top_heap_words], aggregated with [max] *)
  ; added_top_heap_words: int  (** like [Gc.top_heap_words], aggregated with [(+)] *) }

type since = ProgramStart | PreviousStats of t

let get ~since =
  let stats = Gc.stat () in
  match since with
  | ProgramStart ->
      { minor_words= stats.minor_words
      ; promoted_words= stats.promoted_words
      ; major_words= stats.major_words
      ; minor_collections= stats.minor_collections
      ; major_collections= stats.major_collections
      ; compactions= stats.compactions
      ; max_top_heap_words= stats.top_heap_words
      ; added_top_heap_words= stats.top_heap_words }
  | PreviousStats stats_prev ->
      (* [top_heap_words] is going to be inaccurate if it was reached in the previous time period *)
      { minor_words= stats.minor_words -. stats_prev.minor_words
      ; promoted_words= stats.promoted_words -. stats_prev.promoted_words
      ; major_words= stats.major_words -. stats_prev.major_words
      ; minor_collections= stats.minor_collections - stats_prev.minor_collections
      ; major_collections= stats.major_collections - stats_prev.major_collections
      ; compactions= stats.compactions - stats_prev.compactions
      ; max_top_heap_words= stats.top_heap_words
      ; added_top_heap_words= stats.top_heap_words }


let merge stats1 stats2 =
  { minor_words= stats1.minor_words +. stats2.minor_words
  ; promoted_words= stats1.promoted_words +. stats2.promoted_words
  ; major_words= stats1.major_words +. stats2.major_words
  ; minor_collections= stats1.minor_collections + stats2.minor_collections
  ; major_collections= stats1.major_collections + stats2.major_collections
  ; compactions= stats1.compactions + stats2.compactions
  ; max_top_heap_words= max stats1.max_top_heap_words stats2.max_top_heap_words
  ; added_top_heap_words= stats1.added_top_heap_words + stats2.added_top_heap_words }


let pp f
    ({ minor_words
     ; promoted_words
     ; major_words
     ; minor_collections
     ; major_collections
     ; compactions
     ; max_top_heap_words
     ; added_top_heap_words }[@warning "+9"]) =
  F.fprintf f
    "@[<v2>  minor_words: %g@;\
     promoted_words: %g@;\
     major_words: %g@;\
     minor_collections: %d@;\
     major_collections: %d@;\
     compactions: %d@;\
     max top_heap_words: %d@;\
     cumulative top_heap_words: %d@;\
     @]"
    minor_words promoted_words major_words minor_collections major_collections compactions
    max_top_heap_words added_top_heap_words


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
  ; create_counter "max_top_heap_words" stats.max_top_heap_words
  ; create_counter "added_top_heap_words" stats.added_top_heap_words ]


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
