(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module PulseSumCountMap = Caml.Map.Make (Int)

module DurationItem = struct
  type t = {duration_us: int; pname: string} [@@deriving equal]

  let dummy = {duration_us= 0; pname= ""}

  let compare {duration_us= dr1} {duration_us= dr2} = Int.compare dr1 dr2

  let pp f {pname; duration_us} = F.fprintf f "%5dus: %s" duration_us pname
end

module LongestProcDurationHeap = struct
  module Heap = Binary_heap.Make (DurationItem)

  let to_list heap = Heap.fold (fun elt acc -> elt :: acc) heap []

  let update (new_elt : DurationItem.t) heap =
    Option.iter Config.top_longest_proc_duration_size ~f:(fun heap_size ->
        if Heap.length heap < heap_size then Heap.add heap new_elt
        else if new_elt.duration_us > (Heap.minimum heap).duration_us then (
          Heap.remove heap ;
          Heap.add heap new_elt ) )


  let merge heap1 heap2 =
    Heap.iter (fun elt -> update elt heap1) heap2 ;
    heap1


  let pp_sorted f heap =
    let heap = to_list heap |> List.sort ~compare:(fun x y -> DurationItem.compare y x) in
    F.fprintf f "%a" (F.pp_print_list ~pp_sep:(fun f () -> F.fprintf f "@;") DurationItem.pp) heap


  include Heap
end

(** Type for fields that contain non-[Marshal]-serializable data. These need to be serialized by
    [get ()] so they can safely be sent over the pipe to the orchestrator process, where these stats
    end up. *)
type ('t, 'serialized) serialize_before_marshal = T of 't | Serialized of 'serialized

let serialize serializer = function
  | T x ->
      Serialized (serializer x)
  | Serialized _ as serialized ->
      serialized


let deserialize deserializer = function T x -> x | Serialized s -> deserializer s

include struct
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-unused-module"]

  type t =
    { mutable summary_file_try_load: int
    ; mutable summary_read_from_disk: int
    ; mutable summary_cache_hits: int
    ; mutable summary_cache_misses: int
    ; mutable ondemand_procs_analyzed: int
    ; mutable proc_locker_lock_time: ExecutionDuration.t
    ; mutable proc_locker_unlock_time: ExecutionDuration.t
    ; mutable restart_scheduler_useful_time: ExecutionDuration.t
    ; mutable restart_scheduler_total_time: ExecutionDuration.t
    ; mutable pulse_aliasing_contradictions: int
    ; mutable pulse_args_length_contradictions: int
    ; mutable pulse_captured_vars_length_contradictions: int
    ; mutable pulse_summaries_count: int PulseSumCountMap.t
    ; mutable topl_reachable_calls: int
    ; mutable timeouts: int
    ; mutable timings: (Timings.t, Timings.serialized) serialize_before_marshal
    ; mutable longest_proc_duration_heap: LongestProcDurationHeap.t }
  [@@deriving fields]
end

let empty_duration_map = LongestProcDurationHeap.create ~dummy:DurationItem.dummy 10

let global_stats =
  { longest_proc_duration_heap= empty_duration_map
  ; summary_file_try_load= 0
  ; summary_read_from_disk= 0
  ; summary_cache_hits= 0
  ; summary_cache_misses= 0
  ; ondemand_procs_analyzed= 0
  ; proc_locker_lock_time= ExecutionDuration.zero
  ; proc_locker_unlock_time= ExecutionDuration.zero
  ; restart_scheduler_useful_time= ExecutionDuration.zero
  ; restart_scheduler_total_time= ExecutionDuration.zero
  ; pulse_aliasing_contradictions= 0
  ; pulse_args_length_contradictions= 0
  ; pulse_captured_vars_length_contradictions= 0
  ; pulse_summaries_count= PulseSumCountMap.empty
  ; topl_reachable_calls= 0
  ; timeouts= 0
  ; timings= T Timings.init }


let update_with field ~f =
  match Field.setter field with
  | None ->
      L.die InternalError "incr on non-mutable field %s" (Field.name field)
  | Some set ->
      set global_stats (f (Field.get field global_stats))


let incr field = update_with field ~f:(( + ) 1)

let add field exe_duration = update_with field ~f:(ExecutionDuration.add exe_duration)

let incr_summary_file_try_load () = incr Fields.summary_file_try_load

let incr_summary_read_from_disk () = incr Fields.summary_read_from_disk

let incr_summary_cache_hits () = incr Fields.summary_cache_hits

let incr_summary_cache_misses () = incr Fields.summary_cache_misses

let incr_ondemand_procs_analyzed () = incr Fields.ondemand_procs_analyzed

let add_to_proc_locker_lock_time execution_duration =
  add Fields.proc_locker_lock_time execution_duration


let add_to_proc_locker_unlock_time execution_duration =
  add Fields.proc_locker_unlock_time execution_duration


let add_to_restart_scheduler_useful_time execution_duration =
  add Fields.restart_scheduler_useful_time execution_duration


let add_to_restart_scheduler_total_time execution_duration =
  add Fields.restart_scheduler_total_time execution_duration


let incr_pulse_aliasing_contradictions () = incr Fields.pulse_aliasing_contradictions

let incr_pulse_args_length_contradictions () = incr Fields.pulse_args_length_contradictions

let incr_pulse_captured_vars_length_contradictions () =
  incr Fields.pulse_captured_vars_length_contradictions


let add_pulse_summaries_count n =
  update_with Fields.pulse_summaries_count ~f:(fun counters ->
      PulseSumCountMap.update n (fun i -> Some (1 + Option.value ~default:0 i)) counters )


let add_proc_duration_us pname duration_us =
  update_with Fields.longest_proc_duration_heap ~f:(fun heap ->
      let new_elt = DurationItem.{pname; duration_us} in
      LongestProcDurationHeap.update new_elt heap ;
      heap )


let incr_topl_reachable_calls () = incr Fields.topl_reachable_calls

let incr_timeouts () = incr Fields.timeouts

let add_timing timeable t =
  update_with Fields.timings ~f:(function timings ->
      T (Timings.add timeable t (deserialize Timings.deserialize timings)) )


let copy from ~into : unit =
  let ({ longest_proc_duration_heap
       ; summary_file_try_load
       ; summary_read_from_disk
       ; summary_cache_hits
       ; summary_cache_misses
       ; ondemand_procs_analyzed
       ; proc_locker_lock_time
       ; proc_locker_unlock_time
       ; restart_scheduler_useful_time
       ; restart_scheduler_total_time
       ; pulse_aliasing_contradictions
       ; pulse_args_length_contradictions
       ; pulse_captured_vars_length_contradictions
       ; pulse_summaries_count
       ; topl_reachable_calls
       ; timeouts
       ; timings } [@warning "+9"] ) =
    from
  in
  Fields.Direct.set_all_mutable_fields into ~longest_proc_duration_heap ~summary_file_try_load
    ~summary_read_from_disk ~summary_cache_hits ~summary_cache_misses ~ondemand_procs_analyzed
    ~proc_locker_lock_time ~proc_locker_unlock_time ~restart_scheduler_useful_time
    ~restart_scheduler_total_time ~pulse_aliasing_contradictions ~pulse_args_length_contradictions
    ~pulse_captured_vars_length_contradictions ~pulse_summaries_count ~topl_reachable_calls
    ~timeouts ~timings


let merge stats1 stats2 =
  { longest_proc_duration_heap=
      LongestProcDurationHeap.merge stats1.longest_proc_duration_heap
        stats2.longest_proc_duration_heap
  ; summary_file_try_load= stats1.summary_file_try_load + stats2.summary_file_try_load
  ; summary_read_from_disk= stats1.summary_read_from_disk + stats2.summary_read_from_disk
  ; summary_cache_hits= stats1.summary_cache_hits + stats2.summary_cache_hits
  ; summary_cache_misses= stats1.summary_cache_misses + stats2.summary_cache_misses
  ; ondemand_procs_analyzed= stats1.ondemand_procs_analyzed + stats2.ondemand_procs_analyzed
  ; proc_locker_lock_time=
      ExecutionDuration.add stats1.proc_locker_lock_time stats2.proc_locker_lock_time
  ; proc_locker_unlock_time=
      ExecutionDuration.add stats1.proc_locker_unlock_time stats2.proc_locker_unlock_time
  ; restart_scheduler_useful_time=
      ExecutionDuration.add stats1.restart_scheduler_useful_time
        stats2.restart_scheduler_useful_time
  ; restart_scheduler_total_time=
      ExecutionDuration.add stats1.restart_scheduler_total_time stats2.restart_scheduler_total_time
  ; pulse_aliasing_contradictions=
      stats1.pulse_aliasing_contradictions + stats2.pulse_aliasing_contradictions
  ; pulse_args_length_contradictions=
      stats1.pulse_args_length_contradictions + stats2.pulse_args_length_contradictions
  ; pulse_captured_vars_length_contradictions=
      stats1.pulse_captured_vars_length_contradictions
      + stats2.pulse_captured_vars_length_contradictions
  ; pulse_summaries_count=
      PulseSumCountMap.merge
        (fun _ i j -> Some (Option.value ~default:0 i + Option.value ~default:0 j))
        stats1.pulse_summaries_count stats2.pulse_summaries_count
  ; topl_reachable_calls= stats1.topl_reachable_calls + stats2.topl_reachable_calls
  ; timeouts= stats1.timeouts + stats2.timeouts
  ; timings=
      T
        (Timings.merge
           (deserialize Timings.deserialize stats1.timings)
           (deserialize Timings.deserialize stats2.timings) ) }


let initial =
  { longest_proc_duration_heap= empty_duration_map
  ; summary_file_try_load= 0
  ; summary_read_from_disk= 0
  ; summary_cache_hits= 0
  ; summary_cache_misses= 0
  ; ondemand_procs_analyzed= 0
  ; proc_locker_lock_time= ExecutionDuration.zero
  ; proc_locker_unlock_time= ExecutionDuration.zero
  ; restart_scheduler_useful_time= ExecutionDuration.zero
  ; restart_scheduler_total_time= ExecutionDuration.zero
  ; pulse_aliasing_contradictions= 0
  ; pulse_args_length_contradictions= 0
  ; pulse_captured_vars_length_contradictions= 0
  ; pulse_summaries_count= PulseSumCountMap.empty
  ; topl_reachable_calls= 0
  ; timeouts= 0
  ; timings= T Timings.init }


let reset () = copy initial ~into:global_stats

let pp fmt stats =
  let pp_field pp_value fmt field =
    F.fprintf fmt "%s= %a@;" (Field.name field) pp_value (Field.get field stats)
  in
  let pp_serialized_field deserializer pp_value fmt field =
    pp_field (fun fmt v -> pp_value fmt (deserialize deserializer v)) fmt field
  in
  let pp_hit_percent hit miss fmt =
    let total = hit + miss in
    if Int.equal total 0 then F.pp_print_string fmt "N/A%%"
    else F.fprintf fmt "%d%%" (hit * 100 / total)
  in
  let pp_int_field fmt field = pp_field F.pp_print_int fmt field in
  let pp_execution_duration_field fmt field =
    let field_value = Field.get field stats in
    let field_name = Field.name field in
    F.fprintf fmt "%a@;" (ExecutionDuration.pp ~prefix:field_name) field_value
  in
  let pp_cache_hits stats cache_misses fmt cache_hits_field =
    let cache_hits = Field.get cache_hits_field stats in
    F.fprintf fmt "%s= %d (%t)@;" (Field.name cache_hits_field) cache_hits
      (pp_hit_percent cache_hits cache_misses)
  in
  let pp_longest_proc_duration_heap fmt field =
    let heap : LongestProcDurationHeap.t = Field.get field stats in
    F.fprintf fmt "%s= [@\n@[<v>%a@]@\n]@;" (Field.name field) LongestProcDurationHeap.pp_sorted
      heap
  in
  let pp_pulse_summaries_count fmt field =
    let sumcounters : int PulseSumCountMap.t = Field.get field stats in
    let pp_binding fmt (n, count) = Format.fprintf fmt "%d -> %d" n count in
    F.fprintf fmt "%s= [%a]@;" (Field.name field)
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_binding)
      (PulseSumCountMap.bindings sumcounters)
  in
  let pp_stats fmt =
    Fields.iter ~summary_file_try_load:(pp_int_field fmt)
      ~longest_proc_duration_heap:(pp_longest_proc_duration_heap fmt)
      ~summary_read_from_disk:(pp_int_field fmt)
      ~summary_cache_hits:(pp_cache_hits stats stats.summary_cache_misses fmt)
      ~summary_cache_misses:(pp_int_field fmt) ~ondemand_procs_analyzed:(pp_int_field fmt)
      ~proc_locker_lock_time:(pp_execution_duration_field fmt)
      ~proc_locker_unlock_time:(pp_execution_duration_field fmt)
      ~restart_scheduler_useful_time:(pp_execution_duration_field fmt)
      ~restart_scheduler_total_time:(pp_execution_duration_field fmt)
      ~pulse_aliasing_contradictions:(pp_int_field fmt)
      ~pulse_args_length_contradictions:(pp_int_field fmt)
      ~pulse_captured_vars_length_contradictions:(pp_int_field fmt)
      ~pulse_summaries_count:(pp_pulse_summaries_count fmt) ~timeouts:(pp_int_field fmt)
      ~topl_reachable_calls:(pp_int_field fmt)
      ~timings:(pp_serialized_field Timings.deserialize Timings.pp fmt)
  in
  F.fprintf fmt "@[Backend stats:@\n@[<v2>  %t@]@]@." pp_stats


let log_to_scuba stats =
  let create_counter field =
    [LogEntry.mk_count ~label:("backend_stats." ^ Field.name field) ~value:(Field.get field stats)]
  in
  let create_longest_proc_duration_heap field =
    Field.get field stats |> LongestProcDurationHeap.to_list
    |> List.mapi ~f:(fun i DurationItem.{duration_us} ->
           LogEntry.mk_time
             ~label:(F.sprintf "backend_stats.longest_proc_duration_heap_%d" i)
             ~duration_us )
  in
  let create_time_entry field =
    Field.get field stats
    |> ExecutionDuration.to_scuba_entries ~prefix:("backend_stats." ^ Field.name field)
  in
  let create_pulse_summaries_count_entry field =
    let counters : int PulseSumCountMap.t = Field.get field stats in
    List.map
      ~f:(fun (n, count) ->
        LogEntry.mk_count ~label:(F.sprintf "backend_stats.pulse_summaries_count_%d" n) ~value:count
        )
      (PulseSumCountMap.bindings counters)
  in
  let create_timings_entry field =
    Field.get field stats |> deserialize Timings.deserialize |> Timings.to_scuba
  in
  let entries =
    Fields.to_list ~longest_proc_duration_heap:create_longest_proc_duration_heap
      ~summary_file_try_load:create_counter ~summary_read_from_disk:create_counter
      ~summary_cache_hits:create_counter ~summary_cache_misses:create_counter
      ~ondemand_procs_analyzed:create_counter ~proc_locker_lock_time:create_time_entry
      ~proc_locker_unlock_time:create_time_entry ~restart_scheduler_useful_time:create_time_entry
      ~restart_scheduler_total_time:create_time_entry ~pulse_aliasing_contradictions:create_counter
      ~pulse_args_length_contradictions:create_counter
      ~pulse_captured_vars_length_contradictions:create_counter
      ~pulse_summaries_count:create_pulse_summaries_count_entry ~timeouts:create_counter
      ~topl_reachable_calls:create_counter ~timings:create_timings_entry
    |> List.concat
  in
  ScubaLogging.log_many entries


let log_aggregate stats_list =
  match stats_list with
  | [] ->
      L.internal_error "Empty list of backend stats to aggregate, weird!@\n"
  | one :: rest ->
      let stats = List.fold rest ~init:one ~f:(fun aggregate one -> merge aggregate one) in
      L.debug Analysis Quiet "%a" pp stats ;
      log_to_scuba stats


let get () = {global_stats with timings= serialize Timings.serialize global_stats.timings}
