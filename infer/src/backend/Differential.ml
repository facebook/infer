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
          let end_locs = IssueAuxData.decode encoded in
          if is_duplicate_report end_locs reported_ends then (reported_ends, nondup_issues)
          else (LocListSet.add reported_ends end_locs, {issue with access= None} :: nondup_issues)
      | None ->
          (reported_ends, {issue with access= None} :: nondup_issues) )
  |> snd |> sort_by_location


module CostsSummary = struct
  module DegreeMap = Caml.Map.Make (Polynomials.Degree)

  type 'a count = {top: 'a; zero: 'a; degrees: 'a DegreeMap.t}

  let init = {top= 0; zero= 0; degrees= DegreeMap.empty}

  type previous_current = {previous: int; current: int}

  let count costs =
    let count_aux t (e : CostDomain.BasicCost.t) =
      match CostDomain.BasicCost.degree e with
      | None ->
          assert (CostDomain.BasicCost.is_top e) ;
          {t with top= t.top + 1}
      | Some d when CostDomain.BasicCost.is_zero e ->
          assert (Polynomials.Degree.is_zero d) ;
          {t with zero= t.zero + 1}
      | Some d ->
          let degrees =
            DegreeMap.update d (function None -> Some 1 | Some x -> Some (x + 1)) t.degrees
          in
          {t with degrees}
    in
    List.fold ~init
      ~f:(fun acc (v : Jsonbug_t.cost_item) ->
        count_aux acc (CostDomain.BasicCost.decode v.polynomial) )
      costs


  let pair_counts ~current_counts ~previous_counts =
    let compute_degrees current previous =
      let merge_aux _ cur_opt prev_opt =
        match (cur_opt, prev_opt) with
        | Some current, Some previous ->
            Some {current; previous}
        | Some current, None ->
            Some {current; previous= 0}
        | None, Some previous ->
            Some {current= 0; previous}
        | None, None ->
            None
      in
      DegreeMap.merge merge_aux current previous
    in
    { top= {current= current_counts.top; previous= previous_counts.top}
    ; zero= {current= current_counts.zero; previous= previous_counts.zero}
    ; degrees= compute_degrees current_counts.degrees previous_counts.degrees }


  let to_json ~current_costs ~previous_costs =
    let current_counts = count current_costs in
    let previous_counts = count previous_costs in
    let paired_counts = pair_counts ~current_counts ~previous_counts in
    let json_degrees =
      DegreeMap.bindings paired_counts.degrees
      |> List.map ~f:(fun (key, {current; previous}) ->
             `Assoc
               [ ("degree", `Int (Polynomials.Degree.encode_to_int key))
               ; ("current", `Int current)
               ; ("previous", `Int previous) ] )
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

let to_map key_func report =
  List.fold_left
    ~f:(fun map elt -> Map.add_multi map ~key:(key_func elt) ~data:elt)
    ~init:String.Map.empty report


let issue_of_cost cost_info ~delta ~prev_cost ~curr_cost =
  let file = cost_info.Jsonbug_t.loc.file in
  let source_file = SourceFile.create ~warn_on_error:false file in
  let issue_type =
    if CostDomain.BasicCost.is_top curr_cost then IssueType.infinite_execution_time_call
    else if CostDomain.BasicCost.is_zero curr_cost then IssueType.zero_execution_time_call
    else IssueType.performance_variation
  in
  if (not Config.filtering) || issue_type.IssueType.enabled then
    let qualifier =
      let pp_delta fmt delta =
        match delta with
        | `Decreased ->
            Format.fprintf fmt "decreased"
        | `Increased ->
            Format.fprintf fmt "increased"
      in
      let pp_extra_msg fmt cost_polynomial =
        if Config.developer_mode then
          Format.fprintf fmt "Cost is %a (degree is %a)" CostDomain.BasicCost.pp cost_polynomial
            CostDomain.BasicCost.pp_degree cost_polynomial
        else Format.fprintf fmt "Please make sure this is an expected change."
      in
      Format.asprintf "Complexity of this function has %a from %a to %a. %a"
        (MarkupFormatter.wrap_bold pp_delta)
        delta
        (MarkupFormatter.wrap_monospaced CostDomain.BasicCost.pp_degree_hum)
        prev_cost
        (MarkupFormatter.wrap_monospaced CostDomain.BasicCost.pp_degree_hum)
        curr_cost pp_extra_msg curr_cost
    in
    let line = cost_info.Jsonbug_t.loc.lnum in
    let column = cost_info.Jsonbug_t.loc.cnum in
    let trace =
      [Errlog.make_trace_element 0 {Location.line; col= column; file= source_file} "" []]
    in
    let severity = Exceptions.Advice in
    Some
      { Jsonbug_j.bug_type= issue_type.IssueType.unique_id
      ; qualifier
      ; severity= Exceptions.severity_string severity
      ; visibility= Some (Exceptions.string_of_visibility Exceptions.Exn_user)
      ; line
      ; column
      ; procedure= cost_info.Jsonbug_t.procedure_id
      ; procedure_start_line= line
      ; file
      ; bug_trace= InferPrint.loc_trace_to_jsonbug_record trace severity
      ; key= ""
      ; node_key= None
      ; hash= cost_info.Jsonbug_t.hash
      ; dotty= None
      ; infer_source_loc= None
      ; bug_type_hum= issue_type.IssueType.hum
      ; linters_def_file= None
      ; doc_url= None
      ; traceview_id= None
      ; censored_reason= InferPrint.censored_reason issue_type source_file
      ; access= None
      ; extras= None }
  else None


(** Differential of cost reports, based on degree variations.
    Compare degree_before (DB), and degree_after (DA):
      DB > DA => fixed
      DB < DA => introduced
 *)
let of_costs ~(current_costs : Jsonbug_t.costs_report) ~(previous_costs : Jsonbug_t.costs_report) =
  let fold_aux ~key:_ ~data (left, both, right) =
    match data with
    | `Both (current, previous) ->
        let max_degree_polynomial l =
          let max =
            List.max_elt l ~compare:(fun (_, c1) (_, c2) ->
                CostDomain.BasicCost.compare_by_degree c1 c2 )
          in
          Option.value_exn max
        in
        let curr_cost_info, curr_cost = max_degree_polynomial current in
        let _, prev_cost = max_degree_polynomial previous in
        if
          Config.filtering
          && (CostDomain.BasicCost.is_one curr_cost || CostDomain.BasicCost.is_one prev_cost)
        then
          (* transitions to/from zero costs are obvious, no need to flag them *)
          (left, both, right)
        else
          let cmp = CostDomain.BasicCost.compare_by_degree curr_cost prev_cost in
          let concat_opt l v = match v with Some v' -> v' :: l | None -> l in
          if cmp > 0 then
            (* introduced *)
            let left' =
              issue_of_cost curr_cost_info ~delta:`Increased ~prev_cost ~curr_cost
              |> concat_opt left
            in
            (left', both, right)
          else if cmp < 0 then
            (* fixed *)
            let right' =
              issue_of_cost curr_cost_info ~delta:`Decreased ~prev_cost ~curr_cost
              |> concat_opt right
            in
            (left, both, right')
          else
            (* preexisting costs are not issues, since their values have not changed *)
            (left, both, right)
    | `Left _ | `Right _ ->
        (* costs available only on one of the two reports are discarded, since no comparison can be made *)
        (left, both, right)
  in
  let key_func (cost_info, _) = cost_info.Jsonbug_t.hash in
  let to_map = to_map key_func in
  let decoded_costs costs =
    List.map costs ~f:(fun c -> (c, CostDomain.BasicCost.decode c.Jsonbug_t.polynomial))
  in
  let current_costs' = decoded_costs current_costs in
  let previous_costs' = decoded_costs previous_costs in
  Map.fold2 (to_map current_costs') (to_map previous_costs') ~f:fold_aux ~init:([], [], [])


(** Set operations should keep duplicated issues with identical hashes *)
let of_reports ~(current_report : Jsonbug_t.report) ~(previous_report : Jsonbug_t.report)
    ~(current_costs : Jsonbug_t.costs_report) ~(previous_costs : Jsonbug_t.costs_report) : t =
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
    let key_func (issue : Jsonbug_t.jsonbug) = issue.hash in
    let to_map = to_map key_func in
    Map.fold2 (to_map current_report) (to_map previous_report) ~f:fold_aux ~init:([], [], [])
  in
  let costs_summary = CostsSummary.to_json ~current_costs ~previous_costs in
  let introduced_costs, preexisting_costs, fixed_costs = of_costs ~current_costs ~previous_costs in
  { introduced= List.rev_append introduced_costs (dedup introduced)
  ; fixed= List.rev_append fixed_costs (dedup fixed)
  ; preexisting= List.rev_append preexisting_costs (dedup preexisting)
  ; costs_summary }


let to_files {introduced; fixed; preexisting; costs_summary} destdir =
  Out_channel.write_all (destdir ^/ "introduced.json")
    ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting) ;
  Out_channel.write_all (destdir ^/ "costs_summary.json")
    ~data:(Yojson.Basic.to_string costs_summary)
