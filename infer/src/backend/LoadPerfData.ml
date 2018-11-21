(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

module PerfProfilerDataMap = Caml.Map.Make (struct
  type t = Typ.Procname.t

  let compare = Typ.Procname.compare
end)

let global_perf_profiler_data : Perf_profiler_t.perf_profiler_item PerfProfilerDataMap.t ref =
  ref PerfProfilerDataMap.empty


let split_class_method_name =
  let class_sep = String.Search_pattern.create "::" in
  fun qualified_method_name ->
    match String.Search_pattern.index class_sep ~in_:qualified_method_name with
    | Some class_sep_pos ->
        let class_name =
          String.sub qualified_method_name ~pos:0 ~len:class_sep_pos
          |> String.tr ~target:'/' ~replacement:'.'
        in
        let method_name =
          let prefix_len = class_sep_pos + 2 in
          String.sub qualified_method_name ~pos:prefix_len
            ~len:(String.length qualified_method_name - prefix_len)
        in
        Some (class_name, method_name)
    | _ ->
        None


let pp_perf_profiler_item itm =
  let open Perf_profiler_t in
  L.(debug Analysis Medium)
    "@\n\n\
    \               [Perf Profiler Log] Function: '%s'  @\n\
    \               count trace id = %i @\n\
    \               sum inclusive cpu time = %f@\n\
    \               avg inclusive time = %f @\n\
    \               sum exclusive cpu time = %f @\n\
    \               avg exclusive_time = %f  @\n\
    \               inclusive p90 = %f  @\n\
    \               exclusive p90 = %f @\n\
    \               inclusive p50 = %f  @\n\
    \               exclusive p50 = %f @\n\
    \               inclusive p25 = %f  @\n\
    \               exclusive p25 = %f @\n"
    itm.function_name itm.count_trace_id itm.sum_inclusive_cpu_time itm.avg_inclusive_cpu_time_ms
    itm.sum_exclusive_cpu_time itm.avg_exclusive_cpu_time_ms itm.p90_inclusive_cpu_time_ms
    itm.p90_exclusive_cpu_time_ms itm.p50_inclusive_cpu_time_ms itm.p50_exclusive_cpu_time_ms
    itm.p25_inclusive_cpu_time_ms itm.p25_exclusive_cpu_time_ms


let read_file_perf_data fname =
  let perf_profiler_data_str =
    match Utils.read_file fname with
    | Ok l ->
        List.map ~f:Perf_profiler_j.perf_profiler_of_string l
    | Error error ->
        L.user_error "Failed to read file '%s': %s@." fname error ;
        []
  in
  let do_item itm =
    pp_perf_profiler_item itm ;
    match split_class_method_name itm.Perf_profiler_t.function_name with
    | Some (classname, methodname) ->
        let signature = JavaProfilerSamples.JNI.void_method_with_no_arguments in
        L.(debug Analysis Medium)
          "@\n          classname = %s   methodname = %s @\n" classname methodname ;
        let procname = JavaProfilerSamples.create_procname ~classname ~methodname ~signature in
        L.(debug Analysis Medium) "          procname= %a @\n" Typ.Procname.pp procname ;
        global_perf_profiler_data :=
          PerfProfilerDataMap.add procname itm !global_perf_profiler_data
    | _ ->
        ()
  in
  List.iter ~f:(fun items -> List.iter ~f:do_item items) perf_profiler_data_str
