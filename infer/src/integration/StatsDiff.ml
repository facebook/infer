(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(**

   entries are of the form


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
 *)

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


let get_string_exn json = match json with `String s -> s | json -> error ~expected:"string" json

let get_event_exn json = json |> get_field_exn "normal" |> get_field_exn "event" |> get_string_exn

let get_value_exn json =
  (* some values are strings ("normal"), some are ints, none are neither *)
  match json |> get_field_exn "normal" |> get_field "message" with
  | Some value ->
      value
  | None ->
      json |> get_field_exn "int" |> get_field_exn "value"


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
  if Yojson.Safe.equal v1 v2 then `Same v1 else `Diff (v1, v2)


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
              diff_aux ~extra_before ~extra_after ~unchanged:((event1, v) :: unchanged) ~diff
                before' after'
          | `Diff d ->
              diff_aux ~extra_before ~extra_after ~unchanged ~diff:((event1, d) :: diff) before'
                after'
        else
          let extra_before, extra_after, before'', after'' =
            if cmp < 0 then
              ((* [event1 < event2] *)
               entry1 :: extra_before, extra_after, before', after)
            else ((* [event1 > event2] *) extra_before, entry2 :: extra_after, before, after')
          in
          diff_aux ~extra_before ~extra_after ~unchanged ~diff before'' after''
  in
  let extra_before, extra_after, unchanged, diff =
    diff_aux ~extra_before:[] ~extra_after:[] ~unchanged:[] ~diff:[] before after
  in
  ( `List extra_before
  , `List extra_after
  , `List
      (List.map unchanged ~f:(fun (event, value) ->
           `Assoc [("event", `String event); ("value", value)] ) )
  , `List
      (List.map diff ~f:(fun (event, (before, after)) ->
           `Assoc [("event", `String event); ("value_before", before); ("value_after", after)] ) )
  )


let output_diff (extra_before, extra_after, unchanged, diff) =
  let out_dir = ResultsDir.get_path Differential in
  Unix.mkdir_p out_dir ;
  Yojson.Safe.to_file (out_dir ^/ "stats_previous_only.json") extra_before ;
  Yojson.Safe.to_file (out_dir ^/ "stats_current_only.json") extra_after ;
  Yojson.Safe.to_file (out_dir ^/ "stats_unchanged.json") unchanged ;
  Yojson.Safe.to_file (out_dir ^/ "stats_diff.json") diff ;
  ()


let diff ~previous:stats_dir_previous ~current:stats_dir_current =
  let stats_previous = collate_stats_in_dir stats_dir_previous in
  let stats_current = collate_stats_in_dir stats_dir_current in
  compute_diff ~before:stats_previous ~after:stats_current |> output_diff
