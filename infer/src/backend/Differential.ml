(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** set of lists of locations for remembering what trace ends have been reported *)
module LocListSet = struct
  include Caml.Set.Make (struct
    type t = Location.t list [@@deriving compare]
  end)

  let mem s xs = (not (List.is_empty xs)) && mem (List.sort ~compare:Location.compare xs) s

  let add s xs = if List.is_empty xs then s else add (List.sort ~compare:Location.compare xs) s
end

let is_duplicate_report end_locs reported_ends =
  Config.filtering && LocListSet.mem reported_ends end_locs


let sort_by_decreasing_preference_to_report issues =
  let compare (x : Jsonbug_t.jsonbug) (y : Jsonbug_t.jsonbug) =
    let n = Int.compare (List.length x.bug_trace) (List.length y.bug_trace) in
    if n <> 0 then n
    else
      let n = String.compare x.hash y.hash in
      if n <> 0 then n else Pervasives.compare x y
  in
  List.sort ~compare issues


let sort_by_location issues =
  let compare (x : Jsonbug_t.jsonbug) (y : Jsonbug_t.jsonbug) =
    [%compare: string * int * int] (x.file, x.line, x.column) (y.file, y.line, y.column)
  in
  List.sort ~compare issues


let dedup (issues : Jsonbug_t.jsonbug list) =
  List.fold (sort_by_decreasing_preference_to_report issues) ~init:(LocListSet.empty, [])
    ~f:(fun (reported_ends, nondup_issues) (issue : Jsonbug_t.jsonbug) ->
      match issue.access with
      | Some encoded ->
          let _, _, end_locs = IssueAuxData.decode encoded in
          if is_duplicate_report end_locs reported_ends then (reported_ends, nondup_issues)
          else (LocListSet.add reported_ends end_locs, {issue with access= None} :: nondup_issues)
      | None ->
          (reported_ends, {issue with access= None} :: nondup_issues) )
  |> snd |> sort_by_location


module CostsSummary = struct
  type 'a count = {top: 'a; zero: 'a; degrees: 'a Int.Map.t}

  let init = {top= 0; zero= 0; degrees= Int.Map.empty}

  type previous_current = {previous: int; current: int}

  let count costs =
    let count_aux t (e : Itv.NonNegativePolynomial.astate) =
      match Itv.NonNegativePolynomial.degree e with
      | None ->
          assert (Itv.NonNegativePolynomial.is_top e) ;
          {t with top= t.top + 1}
      | Some d when Itv.NonNegativePolynomial.is_zero e ->
          assert (Int.equal d 0) ;
          {t with zero= t.zero + 1}
      | Some d ->
          let degrees = Int.Map.update t.degrees d ~f:(function None -> 1 | Some x -> x + 1) in
          {t with degrees}
    in
    List.fold ~init
      ~f:(fun acc (v : Jsonbug_t.cost_item) ->
        count_aux acc (Itv.NonNegativePolynomial.decode v.polynomial) )
      costs


  let pair_counts ~current_counts ~previous_counts =
    let compute_degrees current previous =
      let merge_aux ~key:_ v =
        match v with
        | `Both (current, previous) ->
            Some {current; previous}
        | `Left current ->
            Some {current; previous= 0}
        | `Right previous ->
            Some {current= 0; previous}
      in
      Int.Map.merge ~f:merge_aux current previous
    in
    { top= {current= current_counts.top; previous= previous_counts.top}
    ; zero= {current= current_counts.zero; previous= previous_counts.zero}
    ; degrees= compute_degrees current_counts.degrees previous_counts.degrees }


  let to_json ~current_costs ~previous_costs =
    let current_counts = count current_costs in
    let previous_counts = count previous_costs in
    let paired_counts = pair_counts ~current_counts ~previous_counts in
    let json_degrees =
      Int.Map.to_alist ~key_order:`Increasing paired_counts.degrees
      |> List.map ~f:(fun (key, {current; previous}) ->
             `Assoc [("degree", `Int key); ("current", `Int current); ("previous", `Int previous)]
         )
    in
    let create_assoc current previous =
      `Assoc [("current", `Int current); ("previous", `Int previous)]
    in
    `Assoc
      [ ("top", create_assoc paired_counts.top.current paired_counts.top.previous)
      ; ("zero", create_assoc paired_counts.zero.current paired_counts.zero.previous)
      ; ("degrees", `List json_degrees) ]
end

type t =
  { introduced: Jsonbug_t.report
  ; fixed: Jsonbug_t.report
  ; preexisting: Jsonbug_t.report
  ; costs_summary: Yojson.Basic.json }

(** Set operations should keep duplicated issues with identical hashes *)
let of_reports ~(current_report : Jsonbug_t.report) ~(previous_report : Jsonbug_t.report)
    ~(current_costs : Jsonbug_t.costs_report) ~(previous_costs : Jsonbug_t.costs_report) : t =
  let to_map report =
    List.fold_left
      ~f:(fun map (issue : Jsonbug_t.jsonbug) ->
        Map.add_multi map ~key:issue.Jsonbug_t.hash ~data:issue )
      ~init:String.Map.empty report
  in
  let fold_aux ~key:_ ~data (left, both, right) =
    match data with
    | `Left left' ->
        (List.rev_append left' left, both, right)
    | `Both (both', _) ->
        (left, List.rev_append both' both, right)
    | `Right right' ->
        (left, both, List.rev_append right' right)
  in
  let introduced, preexisting, fixed =
    Map.fold2 (to_map current_report) (to_map previous_report) ~f:fold_aux ~init:([], [], [])
  in
  let costs_summary = CostsSummary.to_json ~current_costs ~previous_costs in
  {introduced= dedup introduced; fixed= dedup fixed; preexisting= dedup preexisting; costs_summary}


let to_files {introduced; fixed; preexisting; costs_summary} destdir =
  Out_channel.write_all (destdir ^/ "introduced.json")
    ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting) ;
  Out_channel.write_all (destdir ^/ "costs_summary.json")
    ~data:(Yojson.Basic.to_string costs_summary)
