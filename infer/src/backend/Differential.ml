(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  (* FIXME(T54950303) replace use of filtering with deduplicate *)
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
  module DegreeMap = Caml.Map.Make (Int)

  type 'a count = {top: 'a; zero: 'a; degrees: 'a DegreeMap.t}

  let init = {top= 0; zero= 0; degrees= DegreeMap.empty}

  type previous_current = {previous: int; current: int}

  let count costs =
    let incr = function None -> Some 1 | Some x -> Some (x + 1) in
    let count_aux t (e : CostDomain.BasicCost.t) =
      match CostDomain.BasicCost.degree e with
      | None ->
          assert (CostDomain.BasicCost.is_top e) ;
          {t with top= t.top + 1}
      | Some d when CostDomain.BasicCost.is_zero e ->
          assert (Polynomials.Degree.is_zero d) ;
          {t with zero= t.zero + 1}
      | Some d ->
          let degrees = DegreeMap.update (Polynomials.Degree.encode_to_int d) incr t.degrees in
          {t with degrees}
    in
    let count_aux_degree t (degree : int option) =
      match degree with
      | None ->
          {t with top= t.top + 1}
      | Some d when Int.equal d 0 ->
          {t with zero= t.zero + 1}
      | Some d ->
          let degrees = DegreeMap.update d incr t.degrees in
          {t with degrees}
    in
    List.fold ~init
      ~f:(fun acc (v : Jsonbug_t.cost_item) ->
        CostIssues.CostKindMap.fold
          (fun _ CostIssues.{extract_cost_f} acc ->
            let {Jsonbug_t.polynomial_version; polynomial; degree} = extract_cost_f v in
            if Int.equal polynomial_version CostDomain.BasicCost.version then
              count_aux acc (CostDomain.BasicCost.decode polynomial)
            else count_aux_degree acc degree )
          CostIssues.enabled_cost_map acc )
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
  ; costs_summary: Yojson.Basic.t }

let to_map key_func report =
  List.fold_left
    ~f:(fun map elt -> Map.add_multi map ~key:(key_func elt) ~data:elt)
    ~init:String.Map.empty report


module CostItem = struct
  type t =
    { cost_item: Jsonbug_t.cost_item
    ; polynomial: Polynomials.NonNegativePolynomial.t option
    ; degree_with_term: Polynomials.NonNegativePolynomial.degree_with_term option
    ; degree: int option }

  let lift ~f_poly ~f_deg {polynomial; degree} =
    match polynomial with None -> f_deg degree | Some p -> f_poly p


  let is_top = lift ~f_poly:CostDomain.BasicCost.is_top ~f_deg:Option.is_none

  (* NOTE: incorrect when using [f_deg] *)
  let is_zero = lift ~f_poly:CostDomain.BasicCost.is_zero ~f_deg:(fun _ -> false)

  (* NOTE: incorrect when using [f_deg] *)
  let is_one = lift ~f_poly:CostDomain.BasicCost.is_one ~f_deg:(fun _ -> false)

  let compare_by_degree {polynomial= p1; degree= d1} {polynomial= p2; degree= d2} =
    match (p1, p2) with
    | Some p1, Some p2 ->
        CostDomain.BasicCost.compare_by_degree p1 p2
    | _, _ -> (
      match (d1, d2) with
      | None, None ->
          0
      | None, Some _ ->
          1
      | Some _, None ->
          -1
      | Some d1, Some d2 ->
          d1 - d2 )


  let pp_degree ~only_bigO fmt {degree_with_term; degree} =
    match (degree_with_term, degree) with
    | None, None ->
        Format.pp_print_string fmt "Top"
    | None, Some d ->
        Format.pp_print_int fmt d
    | Some degree_with_term, _ ->
        CostDomain.BasicCost.pp_degree ~only_bigO fmt degree_with_term


  let pp_cost_msg fmt ({polynomial} as curr_item) =
    let pp_cost fmt =
      match polynomial with
      | None ->
          Format.fprintf fmt "NA"
      | Some p ->
          CostDomain.BasicCost.pp_hum fmt p
    in
    Format.fprintf fmt "Cost is %t (degree is %a)" pp_cost (pp_degree ~only_bigO:false) curr_item
end

let issue_of_cost kind CostIssues.{complexity_increase_issue; zero_issue; infinite_issue} ~delta
    ~prev_item
    ~curr_item:
      ({CostItem.cost_item= cost_info; degree_with_term= curr_degree_with_term} as curr_item) =
  let file = cost_info.Jsonbug_t.loc.file in
  let method_name = cost_info.Jsonbug_t.procedure_name in
  let is_on_ui_thread = cost_info.Jsonbug_t.is_on_ui_thread in
  let class_name =
    match Str.split (Str.regexp_string ("." ^ method_name)) cost_info.Jsonbug_t.procedure_id with
    | [class_name; _] ->
        class_name
    | _ ->
        ""
  in
  let procname =
    JProcname.make_void_signature_procname ~classname:class_name ~methodname:method_name
  in
  let source_file = SourceFile.create ~warn_on_error:false file in
  let issue_type =
    if CostItem.is_top curr_item then infinite_issue
    else if CostItem.is_zero curr_item then zero_issue
    else
      let is_on_cold_start = ExternalPerfData.in_profiler_data_map procname in
      complexity_increase_issue ~is_on_cold_start ~is_on_ui_thread
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
      let pp_extra_msg fmt () =
        if Config.developer_mode then CostItem.pp_cost_msg fmt curr_item
        else
          Format.fprintf fmt
            "Please make sure this is an expected change. You can inspect the trace to understand \
             the complexity increase:"
      in
      let cold_start_or_ui_msg =
        let common_msg = "It is very important to avoid potential regressions in this phase." in
        if is_on_ui_thread then
          Format.asprintf "This function is called on the UI Thread! %s" common_msg
        else if ExternalPerfData.in_profiler_data_map procname then
          Format.asprintf "This function is called during cold start! %s" common_msg
        else ""
      in
      let msg =
        (* Java Only *)
        if String.equal method_name Typ.Procname.Java.constructor_method_name then "constructor"
        else if String.equal method_name Typ.Procname.Java.class_initializer_method_name then
          "class initializer"
        else "this function"
      in
      Format.asprintf "%s of %s has %a from %a to %a. %s %a"
        (CostKind.to_complexity_string kind)
        msg
        (MarkupFormatter.wrap_bold pp_delta)
        delta
        (MarkupFormatter.wrap_monospaced (CostItem.pp_degree ~only_bigO:true))
        prev_item
        (MarkupFormatter.wrap_monospaced (CostItem.pp_degree ~only_bigO:true))
        curr_item cold_start_or_ui_msg pp_extra_msg ()
    in
    let line = cost_info.Jsonbug_t.loc.lnum in
    let column = cost_info.Jsonbug_t.loc.cnum in
    let trace =
      let polynomial_traces =
        match curr_degree_with_term with
        | None ->
            []
        | Some (Below (_, degree_term)) ->
            Polynomials.NonNegativeNonTopPolynomial.get_symbols degree_term
            |> List.map ~f:Bounds.NonNegativeBound.make_err_trace
        | Some (Above traces) ->
            [("", Polynomials.TopTraces.make_err_trace traces)]
      in
      let curr_cost_trace =
        [ Errlog.make_trace_element 0
            {Location.line; col= column; file= source_file}
            (Format.asprintf "Updated %a" CostItem.pp_cost_msg curr_item)
            [] ]
      in
      ("", curr_cost_trace) :: polynomial_traces |> Errlog.concat_traces
    in
    let severity = Exceptions.Advice in
    Some
      { Jsonbug_j.bug_type= issue_type.IssueType.unique_id
      ; qualifier
      ; severity= Exceptions.severity_string severity
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
  let fold_aux kind issue_spec ~key:_ ~data (left, both, right) =
    match data with
    | `Both (current, previous) ->
        let max_degree_polynomial l =
          let max = List.max_elt l ~compare:CostItem.compare_by_degree in
          Option.value_exn max
        in
        let curr_item = max_degree_polynomial current in
        let prev_item = max_degree_polynomial previous in
        if Config.filtering && (CostItem.is_one curr_item || CostItem.is_one prev_item) then
          (* transitions to/from zero costs are obvious, no need to flag them *)
          (left, both, right)
        else
          let cmp = CostItem.compare_by_degree curr_item prev_item in
          let concat_opt l v = match v with Some v' -> v' :: l | None -> l in
          if cmp > 0 then
            (* introduced *)
            let left' =
              issue_of_cost kind issue_spec ~delta:`Increased ~prev_item ~curr_item
              |> concat_opt left
            in
            (left', both, right)
          else if cmp < 0 then
            (* fixed *)
            let right' =
              issue_of_cost kind issue_spec ~delta:`Decreased ~prev_item ~curr_item
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
  let key_func {CostItem.cost_item} = cost_item.Jsonbug_t.hash in
  let to_map = to_map key_func in
  let decoded_costs costs ~extract_cost_f =
    List.map costs ~f:(fun c ->
        let cost_info = extract_cost_f c in
        let polynomial, degree_with_term =
          if Int.equal cost_info.Jsonbug_t.polynomial_version CostDomain.BasicCost.version then
            let polynomial = CostDomain.BasicCost.decode cost_info.Jsonbug_t.polynomial in
            let degree_with_term = CostDomain.BasicCost.get_degree_with_term polynomial in
            (Some polynomial, Some degree_with_term)
          else (None, None)
        in
        let degree = cost_info.Jsonbug_t.degree in
        {CostItem.cost_item= c; polynomial; degree_with_term; degree} )
  in
  let get_current_costs = decoded_costs current_costs in
  let get_previous_costs = decoded_costs previous_costs in
  CostIssues.CostKindMap.fold
    (fun kind CostIssues.({extract_cost_f} as issue_spec) acc ->
      Map.fold2
        (to_map (get_current_costs ~extract_cost_f))
        (to_map (get_previous_costs ~extract_cost_f))
        ~f:(fold_aux kind issue_spec) ~init:acc )
    CostIssues.enabled_cost_map ([], [], [])


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
  Out_channel.write_all (destdir ^/ "introduced.json") ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting) ;
  Out_channel.write_all (destdir ^/ "costs_summary.json")
    ~data:(Yojson.Basic.to_string costs_summary)
