(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t =
  { mutable summary_file_try_load: int
  ; mutable summary_read_from_disk: int
  ; mutable summary_cache_hits: int
  ; mutable summary_cache_misses: int
  ; mutable summary_has_model_queries: int }

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
     @[<v2>  file_try_load= %d@;read_from_disk= %d@;cache_hits= %d (%t)@;cache_misses= \
     %d@;has_model_queries= %d@;@]@]@."
    summary_file_try_load summary_read_from_disk summary_cache_hits
    (pp_hit_percent summary_cache_hits summary_cache_misses)
    summary_cache_misses summary_has_model_queries
