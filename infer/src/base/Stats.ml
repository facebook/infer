(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

(** stat field datatypes must have their own module and implement at least this interface *)
module type Stat = sig
  type t

  val init : t

  val merge : t -> t -> t

  val to_log_entries : field_name:string -> t -> LogEntry.t list
end

(** Type for fields that contain non-[Marshal]-serializable data. These need to be serialized by
    [get ()] so they can safely be sent over the pipe to the orchestrator process, where these stats
    end up. *)
module type UnmarshallableStat = sig
  include Stat

  type serialized

  val serialize : t -> serialized

  val deserialize : serialized -> t
end

module OfUnmarshallable (S : UnmarshallableStat) = struct
  type t = T of S.t | Serialized of S.serialized

  let init = T S.init

  let serialize = function
    | T x ->
        Serialized (S.serialize x)
    | Serialized _ as serialized ->
        serialized


  let deserialize = function T x -> x | Serialized s -> S.deserialize s

  let merge x y = T (S.merge (deserialize x) (deserialize y))

  let to_log_entries ~field_name v = S.to_log_entries ~field_name (deserialize v)
end

module IntCounter = struct
  type t = int

  let init = 0

  let merge = ( + )

  let to_log_entries ~field_name n =
    [LogEntry.mk_count ~label:("backend_stats." ^ field_name) ~value:n]
end

module TimeCounter = struct
  include ExecutionDuration

  let init = zero

  let merge = add

  let to_log_entries ~field_name duration =
    ExecutionDuration.to_scuba_entries ~prefix:("backend_stats." ^ field_name) duration
end

module TimingsStat = OfUnmarshallable (struct
  include Timings

  let to_log_entries ~field_name:_ ts = Timings.to_scuba ts
end)

module PulseSummaryCountMap = struct
  include IntMap

  let init = empty

  let merge = merge (fun _ i j -> Some (Option.value ~default:0 i + Option.value ~default:0 j))

  let to_log_entries ~field_name:_ summary_counts =
    let total, counts =
      List.fold_map (bindings summary_counts) ~init:0 ~f:(fun total (n, count) ->
          ( total + (n * count)
          , LogEntry.mk_count
              ~label:(F.sprintf "backend_stats.pulse_summaries_count_%d" n)
              ~value:count ) )
    in
    LogEntry.mk_count ~label:"backend_stats.pulse_summaries_total" ~value:total :: counts
end

module DurationItem = struct
  type t = {duration_us: int; file: string; pname: string} [@@deriving equal]

  let dummy = {duration_us= 0; file= ""; pname= ""}

  let compare {duration_us= dr1} {duration_us= dr2} = Int.compare dr1 dr2
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


  let to_log_entries ~field_name:_ heap =
    to_list heap
    |> List.foldi ~init:[] ~f:(fun i acc DurationItem.{duration_us; file; pname} ->
           LogEntry.mk_time
             ~label:(F.sprintf "backend_stats.longest_proc_duration_heap_%d" i)
             ~duration_us
           :: LogEntry.mk_string
                ~label:(F.sprintf "backend_stats.longest_proc_duration_heap_name_%d" i)
                ~message:(F.sprintf "%s:%s" file pname)
           :: acc )


  let init = Heap.create ~dummy:DurationItem.dummy 10

  include Heap
end

(* NOTE: there is a custom ppx for this data structure to generate boilerplate, see
   src/inferppx/StatsPpx.mli *)
type t =
  { mutable summary_file_try_load: IntCounter.t
  ; mutable summary_read_from_disk: IntCounter.t
  ; mutable summary_cache_hits: IntCounter.t
  ; mutable summary_cache_misses: IntCounter.t
  ; mutable ondemand_procs_analyzed: IntCounter.t
  ; mutable proc_locker_lock_time: TimeCounter.t
  ; mutable proc_locker_unlock_time: TimeCounter.t
  ; mutable restart_scheduler_useful_time: TimeCounter.t
  ; mutable restart_scheduler_total_time: TimeCounter.t
  ; mutable pulse_aliasing_contradictions: IntCounter.t
  ; mutable pulse_args_length_contradictions: IntCounter.t
  ; mutable pulse_captured_vars_length_contradictions: IntCounter.t
  ; mutable pulse_disjuncts_dropped: IntCounter.t
  ; mutable pulse_interrupted_loops: IntCounter.t
  ; mutable pulse_summaries_contradictions: IntCounter.t
  ; mutable pulse_summaries_count: IntCounter.t PulseSummaryCountMap.t
  ; mutable topl_reachable_calls: IntCounter.t
  ; mutable timeouts: IntCounter.t
  ; mutable timings: TimingsStat.t
  ; mutable longest_proc_duration_heap: LongestProcDurationHeap.t
  ; mutable process_times: TimeCounter.t
  ; mutable useful_times: TimeCounter.t
  ; mutable spec_store_times: TimeCounter.t }
[@@deriving fields, infer_stats]

let reset () = copy initial ~into:global_stats

let log_to_scuba stats =
  let hit_percent hit miss =
    let total = hit + miss in
    if Int.equal total 0 then None else Some (hit * 100 / total)
  in
  let summary_cache_hit_percent_entry =
    hit_percent stats.summary_cache_hits stats.summary_cache_misses
    |> Option.map ~f:(fun hit_percent ->
           LogEntry.mk_count ~label:"backend_stats.summary_cache_hit_rate" ~value:hit_percent )
  in
  Option.to_list summary_cache_hit_percent_entry @ to_log_entries stats |> ScubaLogging.log_many


let log_aggregate stats_list =
  match stats_list with
  | [] ->
      L.internal_error "Empty list of backend stats to aggregate, weird!@\n"
  | one :: rest ->
      let stats = List.fold rest ~init:one ~f:(fun aggregate one -> merge aggregate one) in
      log_to_scuba stats


let get () = {global_stats with timings= TimingsStat.serialize global_stats.timings}

let update_with field ~f =
  match Field.setter field with
  | None ->
      L.die InternalError "incr on non-mutable field %s" (Field.name field)
  | Some set ->
      set global_stats (f (Field.get field global_stats))


let add field n = update_with field ~f:(( + ) n)

let incr field = add field 1

let add_exe_duration field exe_duration = update_with field ~f:(TimeCounter.add exe_duration)

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
      PulseSummaryCountMap.update n (fun i -> Some (1 + Option.value ~default:0 i)) counters )


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
  update_with Fields.spec_store_times ~f:(fun t -> TimeCounter.add_duration_since t counter)
