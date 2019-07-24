(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

include struct
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-60"]

  type t =
    { mutable summary_file_try_load: int
    ; mutable summary_read_from_disk: int
    ; mutable summary_cache_hits: int
    ; mutable summary_cache_misses: int
    ; mutable summary_has_model_queries: int }
  [@@deriving fields]
end

let global_stats =
  { summary_file_try_load= 0
  ; summary_read_from_disk= 0
  ; summary_cache_hits= 0
  ; summary_cache_misses= 0
  ; summary_has_model_queries= 0 }


let get () = global_stats

let incr_summary_file_try_load () =
  global_stats.summary_file_try_load <- global_stats.summary_file_try_load + 1


let incr_summary_read_from_disk () =
  global_stats.summary_read_from_disk <- global_stats.summary_read_from_disk + 1


let incr_summary_cache_hits () =
  global_stats.summary_cache_hits <- global_stats.summary_cache_hits + 1


let incr_summary_cache_misses () =
  global_stats.summary_cache_misses <- global_stats.summary_cache_misses + 1


let incr_summary_has_model_queries () =
  global_stats.summary_has_model_queries <- global_stats.summary_has_model_queries + 1


let copy from ~into =
  (* so we don't forget to add new fields to [copy] *)
  let[@warning "+9"] { summary_file_try_load
                     ; summary_read_from_disk
                     ; summary_cache_hits
                     ; summary_cache_misses
                     ; summary_has_model_queries } =
    from
  in
  into.summary_file_try_load <- summary_file_try_load ;
  into.summary_read_from_disk <- summary_read_from_disk ;
  into.summary_cache_hits <- summary_cache_hits ;
  into.summary_cache_misses <- summary_cache_misses ;
  into.summary_has_model_queries <- summary_has_model_queries


let merge stats1 stats2 =
  { summary_file_try_load= stats1.summary_file_try_load + stats2.summary_file_try_load
  ; summary_read_from_disk= stats1.summary_read_from_disk + stats2.summary_read_from_disk
  ; summary_cache_hits= stats1.summary_cache_hits + stats2.summary_cache_hits
  ; summary_cache_misses= stats1.summary_cache_misses + stats2.summary_cache_misses
  ; summary_has_model_queries= stats1.summary_has_model_queries + stats2.summary_has_model_queries
  }


let initial =
  { summary_file_try_load= 0
  ; summary_read_from_disk= 0
  ; summary_cache_hits= 0
  ; summary_cache_misses= 0
  ; summary_has_model_queries= 0 }


let reset () = copy initial ~into:global_stats

let pp f stats =
  (* make sure we print all the fields *)
  let[@warning "+9"] { summary_file_try_load
                     ; summary_read_from_disk
                     ; summary_cache_hits
                     ; summary_cache_misses
                     ; summary_has_model_queries } =
    stats
  in
  let pp_hit_percent hit miss f =
    let total = hit + miss in
    if Int.equal total 0 then F.pp_print_string f "N/A%%"
    else F.fprintf f "%d%%" (hit * 100 / total)
  in
  F.fprintf f
    "@[Backend stats:@\n\
     @[<v2>  summary_file_try_load= %d@;summary_read_from_disk= %d@;summary_cache_hits= %d \
     (%t)@;summary_cache_misses= %d@;summary_has_model_queries= %d@;@]@]@."
    summary_file_try_load summary_read_from_disk summary_cache_hits
    (pp_hit_percent summary_cache_hits summary_cache_misses)
    summary_cache_misses summary_has_model_queries


let log_to_scuba stats =
  let create_counter field =
    LogEntry.mk_count ~label:("backend_stats." ^ Field.name field) ~value:(Field.get field stats)
  in
  let entries =
    Fields.to_list ~summary_file_try_load:create_counter ~summary_read_from_disk:create_counter
      ~summary_cache_hits:create_counter ~summary_cache_misses:create_counter
      ~summary_has_model_queries:create_counter
  in
  ScubaLogging.log_many entries
