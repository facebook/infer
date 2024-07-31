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


  let pp fmt map =
    let bindings = bindings map in
    (* already sorted because we use Caml.Map *)
    let pp_binding fmt (key, value) = F.fprintf fmt "%d: %d" key value in
    let pp_sep fmt () = F.pp_print_string fmt ", " in
    F.fprintf fmt "{ %a }" (F.pp_print_list ~pp_sep pp_binding) bindings
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
  ; mutable pulse_unknown_calls: IntCounter.t
  ; mutable pulse_unknown_calls_on_hack_resource: IntCounter.t
  ; mutable pulse_summaries_contradictions: IntCounter.t
  ; mutable pulse_summaries_unsat_for_caller: IntCounter.t
  ; mutable pulse_summaries_unsat_for_caller_percent: IntCounter.t
  ; mutable pulse_summaries_with_some_unreachable_nodes: IntCounter.t
  ; mutable pulse_summaries_with_some_unreachable_nodes_percent: IntCounter.t
  ; mutable pulse_summaries_with_some_unreachable_returns: IntCounter.t
  ; mutable pulse_summaries_with_some_unreachable_returns_percent: IntCounter.t
  ; mutable pulse_summaries_count: IntCounter.t PulseSummaryCountMap.t
  ; mutable pulse_summaries_count_0_continue_program: IntCounter.t
  ; mutable pulse_summaries_count_0_percent: IntCounter.t
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


(** human-readable pretty-printing of all fields *)
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
  let pp_percent_field fmt field =
    pp_field (fun fmt p -> F.fprintf fmt "%.1f%%" (float_of_int p /. 10.)) fmt field
  in
  let pp_time_counter_field fmt field =
    let field_value = Field.get field stats in
    let field_name = Field.name field in
    F.fprintf fmt "%a@;" (TimeCounter.pp ~prefix:field_name) field_value
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
    let sumcounters : int PulseSummaryCountMap.t = Field.get field stats in
    let pp_binding fmt (n, count) = Format.fprintf fmt "%d -> %d" n count in
    F.fprintf fmt "%s= [%a]@;" (Field.name field)
      (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") pp_binding)
      (PulseSummaryCountMap.bindings sumcounters) ;
    let total =
      PulseSummaryCountMap.bindings sumcounters
      |> List.fold ~init:0 ~f:(fun total (n, count) -> total + (n * count))
    in
    F.fprintf fmt "pulse_summaries_total_disjuncts= %d@;" total
  in
  Fields.iter ~summary_file_try_load:(pp_int_field fmt) ~useful_times:(pp_time_counter_field fmt)
    ~longest_proc_duration_heap:(pp_longest_proc_duration_heap fmt)
    ~summary_read_from_disk:(pp_int_field fmt)
    ~summary_cache_hits:(pp_cache_hits stats stats.summary_cache_misses fmt)
    ~summary_cache_misses:(pp_int_field fmt) ~ondemand_procs_analyzed:(pp_int_field fmt)
    ~proc_locker_lock_time:(pp_time_counter_field fmt)
    ~proc_locker_unlock_time:(pp_time_counter_field fmt) ~process_times:(pp_time_counter_field fmt)
    ~pulse_aliasing_contradictions:(pp_int_field fmt)
    ~pulse_args_length_contradictions:(pp_int_field fmt)
    ~pulse_captured_vars_length_contradictions:(pp_int_field fmt)
    ~pulse_disjuncts_dropped:(pp_int_field fmt) ~pulse_interrupted_loops:(pp_int_field fmt)
    ~pulse_unknown_calls:(pp_int_field fmt) ~pulse_unknown_calls_on_hack_resource:(pp_int_field fmt)
    ~pulse_summaries_contradictions:(pp_int_field fmt)
    ~pulse_summaries_count:(pp_pulse_summaries_count fmt)
    ~pulse_summaries_count_0_continue_program:(pp_int_field fmt)
    ~pulse_summaries_count_0_percent:(pp_int_field fmt)
    ~pulse_summaries_unsat_for_caller:(pp_int_field fmt)
    ~pulse_summaries_unsat_for_caller_percent:(pp_int_field fmt)
    ~pulse_summaries_with_some_unreachable_nodes:(pp_int_field fmt)
    ~pulse_summaries_with_some_unreachable_nodes_percent:(pp_int_field fmt)
    ~pulse_summaries_with_some_unreachable_returns:(pp_int_field fmt)
    ~pulse_summaries_with_some_unreachable_returns_percent:(pp_percent_field fmt)
    ~timeouts:(pp_int_field fmt) ~restart_scheduler_useful_time:(pp_time_counter_field fmt)
    ~restart_scheduler_total_time:(pp_time_counter_field fmt)
    ~spec_store_times:(pp_time_counter_field fmt) ~topl_reachable_calls:(pp_int_field fmt)
    ~timings:(pp_serialized_field TimingsStat.deserialize Timings.pp fmt)


(** machine-readable printing of selected fields, for tests *)
let log_to_file
    { ondemand_procs_analyzed
    ; pulse_aliasing_contradictions
    ; pulse_args_length_contradictions
    ; pulse_captured_vars_length_contradictions
    ; pulse_disjuncts_dropped
    ; pulse_interrupted_loops
    ; pulse_unknown_calls
    ; pulse_unknown_calls_on_hack_resource
    ; pulse_summaries_contradictions
    ; pulse_summaries_unsat_for_caller
    ; pulse_summaries_unsat_for_caller_percent
    ; pulse_summaries_with_some_unreachable_nodes
    ; pulse_summaries_with_some_unreachable_nodes_percent
    ; pulse_summaries_with_some_unreachable_returns
    ; pulse_summaries_with_some_unreachable_returns_percent
    ; pulse_summaries_count
    ; pulse_summaries_count_0_continue_program
    ; pulse_summaries_count_0_percent } =
  let filename = Filename.concat Config.results_dir "stats/stats.txt" in
  let out_channel = Out_channel.create filename in
  let fmt = Format.formatter_of_out_channel out_channel in
  F.fprintf fmt "=== global counters ===@\n" ;
  F.fprintf fmt "ondemand_procs_analyzed: %d@\n" ondemand_procs_analyzed ;
  F.fprintf fmt "pulse_aliasing_contradictions: %d@\n" pulse_aliasing_contradictions ;
  F.fprintf fmt "pulse_args_length_contradictions: %d@\n" pulse_args_length_contradictions ;
  F.fprintf fmt "pulse_captured_vars_length_contradictions: %d@\n"
    pulse_captured_vars_length_contradictions ;
  F.fprintf fmt "pulse_disjuncts_dropped: %d@\n" pulse_disjuncts_dropped ;
  F.fprintf fmt "pulse_interrupted_loops: %d@\n" pulse_interrupted_loops ;
  F.fprintf fmt "pulse_unknown_calls: %d@\n" pulse_unknown_calls ;
  F.fprintf fmt "pulse_unknown_calls_on_hack_resource: %d@\n" pulse_unknown_calls_on_hack_resource ;
  F.fprintf fmt "pulse_summaries_contradictions: %d@\n" pulse_summaries_contradictions ;
  F.fprintf fmt "pulse_summaries_unsat_for_caller: %d@\n" pulse_summaries_unsat_for_caller ;
  F.fprintf fmt "pulse_summaries_with_some_unreachable_nodes: %d@\n"
    pulse_summaries_with_some_unreachable_nodes ;
  F.fprintf fmt "pulse_summaries_with_some_unreachable_returns: %d@\n"
    pulse_summaries_with_some_unreachable_returns ;
  F.fprintf fmt "pulse_summaries_count_0_continue_program: %d@\n"
    pulse_summaries_count_0_continue_program ;
  F.fprintf fmt "pulse_summaries_count: %a@\n" PulseSummaryCountMap.pp pulse_summaries_count ;
  F.fprintf fmt "=== percents per function and specialization ===@\n" ;
  F.fprintf fmt "percent of functions where a callee never returns: %d%%@\n"
    pulse_summaries_unsat_for_caller_percent ;
  F.fprintf fmt "percent of functions where at least one node got 0 disjuncts in its post: %d%%@\n"
    pulse_summaries_with_some_unreachable_nodes_percent ;
  F.fprintf fmt "percent of function where at least one return point got 0 disjuncts: %d%%@\n"
    pulse_summaries_with_some_unreachable_returns_percent ;
  F.fprintf fmt "percent of function where 0 disjuncts were returned: %d%%@\n"
    pulse_summaries_count_0_percent ;
  Out_channel.close out_channel


let log_aggregate stats_list =
  match stats_list with
  | [] ->
      L.internal_error "Empty list of backend stats to aggregate, weird!@\n"
  | one :: rest ->
      let stats = List.fold rest ~init:one ~f:(fun aggregate one -> merge aggregate one) in
      let stats =
        if stats.ondemand_procs_analyzed > 0 then
          let mk_percent value =
            int_of_float (100. *. float_of_int value /. float_of_int stats.ondemand_procs_analyzed)
          in
          let pulse_summaries_unsat_for_caller_percent =
            mk_percent stats.pulse_summaries_unsat_for_caller
          in
          let pulse_summaries_with_some_unreachable_returns_percent =
            mk_percent stats.pulse_summaries_with_some_unreachable_returns
          in
          let pulse_summaries_with_some_unreachable_nodes_percent =
            mk_percent stats.pulse_summaries_with_some_unreachable_nodes
          in
          let pulse_summaries_count_0_percent =
            mk_percent stats.pulse_summaries_count_0_continue_program
          in
          { stats with
            pulse_summaries_unsat_for_caller_percent
          ; pulse_summaries_with_some_unreachable_returns_percent
          ; pulse_summaries_with_some_unreachable_nodes_percent
          ; pulse_summaries_count_0_percent }
        else stats
      in
      L.debug Analysis Quiet "@[Backend stats:@\n@[<v2>  %a@]@]@\n" pp stats ;
      log_to_scuba stats ;
      log_to_file stats


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

let incr_pulse_unknown_calls () = incr Fields.pulse_unknown_calls

let incr_pulse_unknown_calls_on_hack_resource () = incr Fields.pulse_unknown_calls_on_hack_resource

let incr_pulse_summaries_contradictions () = incr Fields.pulse_summaries_contradictions

let incr_pulse_summaries_unsat_for_caller () = incr Fields.pulse_summaries_unsat_for_caller

let incr_pulse_summaries_with_some_unreachable_nodes () =
  incr Fields.pulse_summaries_with_some_unreachable_nodes


let incr_pulse_summaries_with_some_unreachable_returns () =
  incr Fields.pulse_summaries_with_some_unreachable_returns


let incr_pulse_summaries_count_0_continue_program () =
  incr Fields.pulse_summaries_count_0_continue_program


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
