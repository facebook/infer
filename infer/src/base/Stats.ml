(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

module type Stat = sig
  type t

  val init : t

  val merge : t -> t -> t
end

module PulseSumCountMap = struct
  include IntMap

  let init = empty

  let merge = merge (fun _ i j -> Some (Option.value ~default:0 i + Option.value ~default:0 j))
end

module DurationItem = struct
  type t = {duration_us: int; file: string; pname: string} [@@deriving equal]

  let dummy = {duration_us= 0; file= ""; pname= ""}

  let compare {duration_us= dr1} {duration_us= dr2} = Int.compare dr1 dr2

  let pp f {pname; file; duration_us} = F.fprintf f "%5dus: %s: %s" duration_us file pname
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


  let init = Heap.create ~dummy:DurationItem.dummy 10

  include Heap
end

(** Type for fields that contain non-[Marshal]-serializable data. These need to be serialized by
    [get ()] so they can safely be sent over the pipe to the orchestrator process, where these stats
    end up. *)
module type UnserializableStat = sig
  include Stat

  type serialized

  val serialize : t -> serialized

  val deserialize : serialized -> t
end

module OfUnserializable (S : UnserializableStat) = struct
  type t = T of S.t | Serialized of S.serialized

  let init = T S.init

  let serialize = function
    | T x ->
        Serialized (S.serialize x)
    | Serialized _ as serialized ->
        serialized


  let deserialize = function T x -> x | Serialized s -> S.deserialize s

  let merge x y = T (S.merge (deserialize x) (deserialize y))
end

module TimingsStat = OfUnserializable (Timings)

module AdditiveIntCounter = struct
  type t = int

  let init = 0

  let merge = ( + )
end

module ExecutionDuration = struct
  include ExecutionDuration

  let init = zero

  let merge = add
end

include struct
  (* ignore dead modules added by @@deriving fields *)
  [@@@warning "-unused-module"]

  (* NOTE: there is a custom ppx for this data structure to generate boilerplate, see
     src/inferppx/StatsPpx.mli *)
  type t =
    { mutable summary_file_try_load: AdditiveIntCounter.t
    ; mutable summary_read_from_disk: AdditiveIntCounter.t
    ; mutable summary_cache_hits: AdditiveIntCounter.t
    ; mutable summary_cache_misses: AdditiveIntCounter.t
    ; mutable ondemand_procs_analyzed: AdditiveIntCounter.t
    ; mutable proc_locker_lock_time: ExecutionDuration.t
    ; mutable proc_locker_unlock_time: ExecutionDuration.t
    ; mutable restart_scheduler_useful_time: ExecutionDuration.t
    ; mutable restart_scheduler_total_time: ExecutionDuration.t
    ; mutable pulse_aliasing_contradictions: AdditiveIntCounter.t
    ; mutable pulse_args_length_contradictions: AdditiveIntCounter.t
    ; mutable pulse_captured_vars_length_contradictions: AdditiveIntCounter.t
    ; mutable pulse_disjuncts_dropped: AdditiveIntCounter.t
    ; mutable pulse_interrupted_loops: AdditiveIntCounter.t
    ; mutable pulse_summaries_contradictions: AdditiveIntCounter.t
    ; mutable pulse_summaries_count: AdditiveIntCounter.t PulseSumCountMap.t
    ; mutable topl_reachable_calls: AdditiveIntCounter.t
    ; mutable timeouts: AdditiveIntCounter.t
    ; mutable timings: TimingsStat.t
    ; mutable longest_proc_duration_heap: LongestProcDurationHeap.t
    ; mutable process_times: ExecutionDuration.t
    ; mutable useful_times: ExecutionDuration.t
    ; mutable spec_store_times: ExecutionDuration.t }
  [@@deriving fields, infer_stats]
end

let update_with field ~f =
  match Field.setter field with
  | None ->
      L.die InternalError "incr on non-mutable field %s" (Field.name field)
  | Some set ->
      set global_stats (f (Field.get field global_stats))


let add field n = update_with field ~f:(( + ) n)

let incr field = add field 1

let add_exe_duration field exe_duration = update_with field ~f:(ExecutionDuration.add exe_duration)

let incr_summary_file_try_load () = incr Fields.summary_file_try_load

let incr_summary_read_from_disk () = incr Fields.summary_read_from_disk

let incr_summary_cache_hits () = incr Fields.summary_cache_hits

let incr_summary_cache_misses () = incr Fields.summary_cache_misses

let incr_ondemand_procs_analyzed () = incr Fields.ondemand_procs_analyzed

let add_to_proc_locker_lock_time execution_duration =
  add_exe_duration Fields.proc_locker_lock_time execution_duration


let add_to_proc_locker_unlock_time execution_duration =
  add_exe_duration Fields.proc_locker_unlock_time execution_duration


let add_to_restart_scheduler_useful_time execution_duration =
  add_exe_duration Fields.restart_scheduler_useful_time execution_duration


let add_to_restart_scheduler_total_time execution_duration =
  add_exe_duration Fields.restart_scheduler_total_time execution_duration


let incr_pulse_aliasing_contradictions () = incr Fields.pulse_aliasing_contradictions

let incr_pulse_args_length_contradictions () = incr Fields.pulse_args_length_contradictions

let incr_pulse_captured_vars_length_contradictions () =
  incr Fields.pulse_captured_vars_length_contradictions


let add_pulse_disjuncts_dropped n = add Fields.pulse_disjuncts_dropped n

let add_pulse_interrupted_loops n = add Fields.pulse_interrupted_loops n

let incr_pulse_summaries_contradictions () = incr Fields.pulse_summaries_contradictions

let add_pulse_summaries_count n =
  update_with Fields.pulse_summaries_count ~f:(fun counters ->
      PulseSumCountMap.update n (fun i -> Some (1 + Option.value ~default:0 i)) counters )


let add_proc_duration_us file pname duration_us =
  update_with Fields.longest_proc_duration_heap ~f:(fun heap ->
      let new_elt = DurationItem.{pname; file; duration_us} in
      LongestProcDurationHeap.update new_elt heap ;
      heap )


let incr_topl_reachable_calls () = incr Fields.topl_reachable_calls

let incr_timeouts () = incr Fields.timeouts

let add_timing timeable t =
  update_with Fields.timings ~f:(function timings ->
      TimingsStat.T (Timings.add timeable t (TimingsStat.deserialize timings)) )


let set_process_times execution_duration =
  update_with Fields.process_times ~f:(fun _ -> execution_duration)


let set_useful_times execution_duration =
  update_with Fields.useful_times ~f:(fun _ -> execution_duration)


let incr_spec_store_times counter =
  update_with Fields.spec_store_times ~f:(fun t -> ExecutionDuration.add_duration_since t counter)


let reset () = copy initial ~into:global_stats

let pp fmt stats =
  let pp_field pp_value fmt field =
    F.fprintf fmt "%s= %a@;" (Field.name field) pp_value (Field.get field stats)
  in
  let pp_serialized_field deserializer pp_value fmt field =
    pp_field (fun fmt v -> pp_value fmt (deserializer v)) fmt field
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
      (PulseSumCountMap.bindings sumcounters) ;
    let total =
      PulseSumCountMap.bindings sumcounters
      |> List.fold ~init:0 ~f:(fun total (n, count) -> total + (n * count))
    in
    F.fprintf fmt "pulse_summaries_total_disjuncts= %d@;" total
  in
  let pp_stats fmt =
    Fields.iter ~summary_file_try_load:(pp_int_field fmt)
      ~useful_times:(pp_execution_duration_field fmt)
      ~longest_proc_duration_heap:(pp_longest_proc_duration_heap fmt)
      ~summary_read_from_disk:(pp_int_field fmt)
      ~summary_cache_hits:(pp_cache_hits stats stats.summary_cache_misses fmt)
      ~summary_cache_misses:(pp_int_field fmt) ~ondemand_procs_analyzed:(pp_int_field fmt)
      ~proc_locker_lock_time:(pp_execution_duration_field fmt)
      ~proc_locker_unlock_time:(pp_execution_duration_field fmt)
      ~process_times:(pp_execution_duration_field fmt)
      ~pulse_aliasing_contradictions:(pp_int_field fmt)
      ~pulse_args_length_contradictions:(pp_int_field fmt)
      ~pulse_captured_vars_length_contradictions:(pp_int_field fmt)
      ~pulse_disjuncts_dropped:(pp_int_field fmt) ~pulse_interrupted_loops:(pp_int_field fmt)
      ~pulse_summaries_contradictions:(pp_int_field fmt)
      ~pulse_summaries_count:(pp_pulse_summaries_count fmt) ~timeouts:(pp_int_field fmt)
      ~restart_scheduler_useful_time:(pp_execution_duration_field fmt)
      ~restart_scheduler_total_time:(pp_execution_duration_field fmt)
      ~spec_store_times:(pp_execution_duration_field fmt) ~topl_reachable_calls:(pp_int_field fmt)
      ~timings:(pp_serialized_field TimingsStat.deserialize Timings.pp fmt)
  in
  F.fprintf fmt "@[Backend stats:@\n@[<v2>  %t@]@]@." pp_stats


let log_to_scuba stats =
  let create_counter field =
    [LogEntry.mk_count ~label:("backend_stats." ^ Field.name field) ~value:(Field.get field stats)]
  in
  let create_longest_proc_duration_heap field =
    Field.get field stats |> LongestProcDurationHeap.to_list
    |> List.foldi ~init:[] ~f:(fun i acc DurationItem.{duration_us; file; pname} ->
           LogEntry.mk_time
             ~label:(F.sprintf "backend_stats.longest_proc_duration_heap_%d" i)
             ~duration_us
           :: LogEntry.mk_string
                ~label:(F.sprintf "backend_stats.longest_proc_duration_heap_name_%d" i)
                ~message:(F.sprintf "%s:%s" file pname)
           :: acc )
  in
  let create_time_entry field =
    Field.get field stats
    |> ExecutionDuration.to_scuba_entries ~prefix:("backend_stats." ^ Field.name field)
  in
  let create_pulse_summaries_count_entry field =
    let counters : int PulseSumCountMap.t = Field.get field stats in
    let total, counts =
      List.fold_map (PulseSumCountMap.bindings counters) ~init:0 ~f:(fun total (n, count) ->
          ( total + (n * count)
          , LogEntry.mk_count
              ~label:(F.sprintf "backend_stats.pulse_summaries_count_%d" n)
              ~value:count ) )
    in
    LogEntry.mk_count ~label:"backend_stats.pulse_summaries_total" ~value:total :: counts
  in
  let create_timings_entry field =
    Field.get field stats |> TimingsStat.deserialize |> Timings.to_scuba
  in
  let entries =
    Fields.to_list ~useful_times:create_time_entry
      ~longest_proc_duration_heap:create_longest_proc_duration_heap
      ~summary_file_try_load:create_counter ~summary_read_from_disk:create_counter
      ~summary_cache_hits:create_counter ~summary_cache_misses:create_counter
      ~ondemand_procs_analyzed:create_counter ~proc_locker_lock_time:create_time_entry
      ~proc_locker_unlock_time:create_time_entry ~process_times:create_time_entry
      ~pulse_aliasing_contradictions:create_counter ~pulse_args_length_contradictions:create_counter
      ~pulse_captured_vars_length_contradictions:create_counter
      ~pulse_disjuncts_dropped:create_counter ~pulse_interrupted_loops:create_counter
      ~pulse_summaries_contradictions:create_counter
      ~pulse_summaries_count:create_pulse_summaries_count_entry
      ~restart_scheduler_useful_time:create_time_entry
      ~restart_scheduler_total_time:create_time_entry ~spec_store_times:create_time_entry
      ~timeouts:create_counter ~topl_reachable_calls:create_counter ~timings:create_timings_entry
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


let get () = {global_stats with timings= TimingsStat.serialize global_stats.timings}
