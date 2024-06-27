(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** entries are of the form

    {v
  {
   "int": {
     "is_main_process": 1,
     "pid": 1234,
     "time": 420844200,
     "value": 123456
   },
   "normal": {
     "command": "run",
     "event": "time.dbwriter.useful_time_user",
     "hostname": "darkstar",
     "infer_commit": "deadbeef"
   },
   "tags": {}
  }
    v} *)

(* keep this sorted or else... (the [ok_exn] below will fail) *)
let stable_stat_events =
  [| "count.analysis_scheduler_gc_stats.compactions"
   ; "count.backend_stats.pulse_aliasing_contradictions"
   ; "count.backend_stats.pulse_captured_vars_length_contradictions"
   ; "count.backend_stats.pulse_summaries_count_0_percent"
   ; "count.backend_stats.pulse_summaries_count_1"
   ; "count.backend_stats.pulse_summaries_unsat_for_caller"
   ; "count.backend_stats.pulse_summaries_unsat_for_caller_percent"
   ; "count.backend_stats.pulse_summaries_with_some_unreachable_nodes"
   ; "count.backend_stats.pulse_summaries_with_some_unreachable_nodes_percent"
   ; "count.backend_stats.pulse_summaries_with_some_unreachable_returns"
   ; "count.backend_stats.pulse_summaries_with_some_unreachable_returns_percent"
   ; "count.backend_stats.timeouts"
   ; "count.num_analysis_workers"
   ; "count.source_files_to_analyze"
   ; "msg.analyzed_file" |]
  |> String.Set.of_sorted_array |> Or_error.ok_exn


let error ~expected json =
  L.die InternalError "when parsing json: expected %s but got '%a'" expected Yojson.Safe.pp json


let get_field field_name json =
  match json with
  | `Assoc assoc ->
      List.Assoc.find ~equal:String.equal assoc field_name
  | _ ->
      error ~expected:"record" json


let get_field_exn field_name json =
  match get_field field_name json with
  | Some json' ->
      json'
  | None ->
      error ~expected:(Printf.sprintf "field '%s'" field_name) json


let get_int_exn json = match json with `Int s -> s | json -> error ~expected:"int" json

let get_string_exn json = match json with `String s -> s | json -> error ~expected:"string" json

let get_event_exn json = json |> get_field_exn "normal" |> get_field_exn "event" |> get_string_exn

type value = Int of int | String of string [@@deriving compare, equal]

let json_of_value = function Int n -> `Int n | String s -> `String s

let pp_value fmt = function Int i -> Int.pp fmt i | String s -> String.pp fmt s

type entry = {event: string; value: value} [@@deriving compare]

let json_of_entry {event; value} = `Assoc [("event", `String event); ("value", json_of_value value)]

let json_of_entries entries = `List (List.map entries ~f:json_of_entry)

type changed_entry = {event: string; value_before: value; value_after: value} [@@deriving compare]

let json_of_changed_entry {event; value_before; value_after} =
  `Assoc
    [ ("event", `String event)
    ; ("value_before", json_of_value value_before)
    ; ("value_after", json_of_value value_after) ]


let json_of_changed_entries entries = `List (List.map entries ~f:json_of_changed_entry)

(** delta in percent if the values are numerical *)
let delta_of_changed_entry {value_before; value_after} =
  match (value_before, value_after) with
  | Int i1, Int i2 ->
      let delta = (float_of_int i2 -. float_of_int i1) *. 100. /. float_of_int i1 in
      Some (i1, i2, delta)
  | _ ->
      None


let get_value_exn json =
  (* some values are strings ("normal"), some are ints, none are neither *)
  match json |> get_field_exn "normal" |> get_field "message" with
  | Some value ->
      String (get_string_exn value)
  | None ->
      let n = json |> get_field_exn "int" |> get_field_exn "value" |> get_int_exn in
      Int n


let collate_stats_in_dir stats_dir =
  Iter.fold
    (fun json_rows dir_entry ->
      if Filename.check_suffix dir_entry ".jsonl" then
        Iter.fold
          (fun json_rows line -> Yojson.Safe.from_string line :: json_rows)
          json_rows
          (Iter.from_labelled_iter @@ In_channel.iter_lines @@ In_channel.create dir_entry)
      else json_rows )
    []
    (Iter.from_labelled_iter @@ Utils.iter_dir stats_dir)


let diff_values entry1 entry2 =
  let v1 = get_value_exn entry1 in
  let v2 = get_value_exn entry2 in
  if equal_value v1 v2 then `Same v1 else `Diff (v1, v2)


let compute_diff ~before ~after =
  let sort_entries entries =
    List.sort
      ~compare:(fun json1 json2 -> String.compare (get_event_exn json1) (get_event_exn json2))
      entries
  in
  let before = sort_entries before in
  let after = sort_entries after in
  let rec diff_aux ~extra_before ~extra_after ~unchanged ~diff before after =
    match (before, after) with
    | [], _ ->
        (extra_before, after @ extra_after, unchanged, diff)
    | _, [] ->
        (before @ extra_before, extra_after, unchanged, diff)
    | entry1 :: before', entry2 :: after' ->
        let event1 = get_event_exn entry1 in
        let event2 = get_event_exn entry2 in
        let cmp = String.compare event1 event2 in
        if Int.equal cmp 0 then
          (* [event1 = event2] *)
          match diff_values entry1 entry2 with
          | `Same v ->
              diff_aux ~extra_before ~extra_after
                ~unchanged:({event= event1; value= v} :: unchanged)
                ~diff before' after'
          | `Diff (value_before, value_after) ->
              diff_aux ~extra_before ~extra_after ~unchanged
                ~diff:({event= event1; value_before; value_after} :: diff)
                before' after'
        else
          let extra_before, extra_after, before'', after'' =
            if cmp < 0 then
              ((* [event1 < event2] *)
               entry1 :: extra_before, extra_after, before', after)
            else ((* [event1 > event2] *) extra_before, entry2 :: extra_after, before, after')
          in
          diff_aux ~extra_before ~extra_after ~unchanged ~diff before'' after''
  in
  diff_aux ~extra_before:[] ~extra_after:[] ~unchanged:[] ~diff:[] before after


let file_output (extra_before, extra_after, unchanged, diff) =
  let out_dir = ResultsDir.get_path Differential in
  Unix.mkdir_p out_dir ;
  Yojson.Safe.to_file (out_dir ^/ "stats_previous_only.json") (`List extra_before) ;
  Yojson.Safe.to_file (out_dir ^/ "stats_current_only.json") (`List extra_after) ;
  Yojson.Safe.to_file (out_dir ^/ "stats_unchanged.json") (json_of_entries unchanged) ;
  Yojson.Safe.to_file (out_dir ^/ "stats_diff.json") (json_of_changed_entries diff) ;
  ()


(** reverse-ordered floats to print stats in order of decreasing size of the change in value (taken
    as the absolute value of the delta percentage) *)
type float_inv_compare = float

let compare_float_inv_compare f1 f2 = Float.compare f2 f1

let int_width i = string_of_int i |> String.length

let pp_n_times n fmt c =
  for _ = 1 to n do
    F.pp_print_char fmt c
  done


let pp_string_with_width width_ref fmt s =
  F.pp_print_string fmt s ;
  let length = String.length s in
  if length > !width_ref then width_ref := length ;
  pp_n_times (!width_ref - length) fmt ' '


let pp_int_with_width width fmt i =
  F.pp_print_int fmt i ;
  pp_n_times (width - int_width i) fmt ' '


let pp_with_width pp width fmt x =
  let s = F.asprintf "%a" pp x in
  pp_string_with_width width fmt s


let pp_diff fmt diff =
  let max_event_length = ref 0 in
  let max_value_before_length = ref 0 in
  let max_value_after_length = ref 0 in
  let max_delta_length = ref 0 in
  let changed_entries =
    List.filter_map diff ~f:(fun ({event} as entry) ->
        match delta_of_changed_entry entry with
        | Some (i1, i2, delta) ->
            let delta_abs = Float.abs delta in
            let delta_s = Printf.sprintf "%+.2f%%" delta in
            max_event_length := max !max_event_length (String.length event) ;
            max_value_before_length := max !max_value_before_length (int_width i1) ;
            max_value_after_length := max !max_value_before_length (int_width i2) ;
            max_delta_length := max !max_delta_length (String.length delta_s) ;
            Some (delta_abs, delta_s, entry)
        | _ ->
            None )
    |> List.sort ~compare:[%compare: float_inv_compare * string * changed_entry]
  in
  let header =
    F.asprintf "| %a | %a | %a | %a |"
      (pp_string_with_width max_event_length)
      "event"
      (pp_string_with_width max_value_before_length)
      "previous value"
      (pp_string_with_width max_value_after_length)
      "new value"
      (pp_string_with_width max_delta_length)
      "delta"
  in
  F.fprintf fmt "%s@\n" header ;
  F.fprintf fmt "|-%a-|-%a-|-%a-|-%a-|@\n" (pp_n_times !max_event_length) '-'
    (pp_n_times !max_value_before_length)
    '-'
    (pp_n_times !max_value_after_length)
    '-' (pp_n_times !max_delta_length) '-' ;
  List.iter changed_entries ~f:(function _, delta, {event; value_before; value_after} ->
      F.fprintf fmt "| %a | %a | %a | %a |@\n"
        (pp_string_with_width max_event_length)
        event
        (pp_with_width pp_value max_value_before_length)
        value_before
        (pp_with_width pp_value max_value_after_length)
        value_after
        (pp_string_with_width max_delta_length)
        delta )


let is_changed_entry_significant entry =
  String.Set.mem stable_stat_events entry.event
  && Option.exists (delta_of_changed_entry entry) ~f:(fun (_, _, delta) -> Float.(abs delta > 0.5))


let text_output (extra_before, extra_after, unchanged, diff) =
  let n_extra_before = List.length extra_before in
  let n_extra_after = List.length extra_after in
  let n_unchanged = List.length unchanged in
  let n_changed = List.length diff in
  let max_width =
    int_width @@ max n_unchanged @@ max n_changed @@ max n_extra_before @@ n_extra_after
  in
  L.progress "%a entries only in previous version@\n" (pp_int_with_width max_width) n_extra_before ;
  L.progress "%a unchanged entries@\n" (pp_int_with_width max_width) n_unchanged ;
  L.progress "%a entries only in new version@\n" (pp_int_with_width max_width) n_extra_after ;
  L.progress "%a entries changed values (details for numerical values below)@\n@\n"
    (pp_int_with_width max_width) n_changed ;
  L.progress "%a" pp_diff diff ;
  L.progress "@\nUnchanged entries:@\n" ;
  List.sort ~compare:[%compare: entry] unchanged
  |> List.iter ~f:(fun {event; value} -> L.progress "%s: %a@\n" event pp_value value) ;
  let diff_to_print = List.filter diff ~f:is_changed_entry_significant in
  if not (List.is_empty diff_to_print) then L.result "%a" pp_diff diff_to_print


let diff ~previous:stats_dir_previous ~current:stats_dir_current =
  let stats_previous = collate_stats_in_dir stats_dir_previous in
  let stats_current = collate_stats_in_dir stats_dir_current in
  let diff = compute_diff ~before:stats_previous ~after:stats_current in
  text_output diff ;
  file_output diff ;
  ()
