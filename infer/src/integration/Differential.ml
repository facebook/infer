(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module L = Logging

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
      if n <> 0 then n else Stdlib.compare x y
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


let create_json_bug ~qualifier ~suggestion ~line ~file ~source_file ~trace
    ~(item : Jsoncost_t.sub_item) ~(issue_type : IssueType.t) =
  { Jsonbug_t.bug_type= issue_type.unique_id
  ; qualifier
  ; suggestion
  ; severity= IssueType.string_of_severity Advice
  ; category= IssueType.string_of_category issue_type.category
  ; line
  ; column= item.loc.cnum
  ; procedure= item.procedure_id
  ; procedure_start_line= line
  ; file
  ; bug_trace= JsonReports.loc_trace_to_jsonbug_record trace
  ; bug_trace_length= Errlog.loc_trace_length trace
  ; bug_trace_max_depth= Errlog.loc_trace_max_depth trace
  ; key= ""
  ; node_key= None
  ; hash= item.hash
  ; dotty= None
  ; infer_source_loc= None
  ; bug_type_hum= issue_type.hum
  ; traceview_id= None
  ; censored_reason= JsonReports.censored_reason issue_type source_file
  ; access= None
  ; extras= None }


module CostsSummary = struct
  module DegreeMap = Caml.Map.Make (Int)

  type 'a count = {top: 'a; unreachable: 'a; zero: 'a; degrees: 'a DegreeMap.t}

  let init = {top= 0; unreachable= 0; zero= 0; degrees= DegreeMap.empty}

  type previous_current = {previous: int; current: int}

  let count costs =
    let incr = function None -> Some 1 | Some x -> Some (x + 1) in
    let count_aux t (e : CostDomain.BasicCost.t) =
      match CostDomain.BasicCost.degree e with
      | None ->
          if CostDomain.BasicCost.is_top e then {t with top= t.top + 1}
          else if CostDomain.BasicCost.is_unreachable e then
            {t with unreachable= t.unreachable + 1; zero= t.zero + 1}
          else (* a cost with no degree must be either T/bottom*) assert false
      | Some d ->
          let degrees = DegreeMap.update (Polynomials.Degree.encode_to_int d) incr t.degrees in
          {t with degrees}
    in
    let count_aux_degree t (degree : int option) =
      match degree with
      | None ->
          {t with top= t.top + 1}
      | Some d when Int.equal d 0 ->
          {t with unreachable= t.unreachable + 1; zero= t.zero + 1}
      | Some d ->
          let degrees = DegreeMap.update d incr t.degrees in
          {t with degrees}
    in
    List.fold ~init
      ~f:(fun acc (v : Jsoncost_t.item) ->
        CostIssues.CostKindMap.fold
          (fun _ CostIssues.{extract_cost_f} acc ->
            let {Jsoncost_t.polynomial_version; polynomial; degree} = extract_cost_f v in
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
    ; unreachable= {current= current_counts.unreachable; previous= previous_counts.unreachable}
    ; zero= {current= current_counts.zero; previous= previous_counts.zero}
    ; degrees= compute_degrees current_counts.degrees previous_counts.degrees }


  let to_json ~current_costs ~previous_costs =
    let current_counts = count current_costs in
    let previous_counts = count previous_costs in
    let paired_counts = pair_counts ~current_counts ~previous_counts in
    let json_degrees =
      DegreeMap.bindings paired_counts.degrees
      |> List.map ~f:(fun (key, {current; previous}) ->
             `Assoc [("degree", `Int key); ("current", `Int current); ("previous", `Int previous)] )
    in
    let create_assoc current previous =
      `Assoc [("current", `Int current); ("previous", `Int previous)]
    in
    `Assoc
      [ ("top", create_assoc paired_counts.top.current paired_counts.top.previous)
      ; ( "unreachable"
        , create_assoc paired_counts.unreachable.current paired_counts.unreachable.previous )
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


module Cost = struct
  module CostItem = struct
    type t =
      { cost_item: Jsoncost_t.item
      ; polynomial: Polynomials.NonNegativePolynomial.t option
      ; degree_with_term: Polynomials.NonNegativePolynomial.degree_with_term option
      ; degree: int option }

    let lift ~f_poly ~f_deg {polynomial; degree} =
      match polynomial with None -> f_deg degree | Some p -> f_poly p


    let is_top = lift ~f_poly:CostDomain.BasicCost.is_top ~f_deg:Option.is_none

    (* NOTE: incorrect when using [f_deg] *)
    let is_unreachable = lift ~f_poly:CostDomain.BasicCost.is_unreachable ~f_deg:(fun _ -> false)

    (* NOTE: incorrect when using [f_deg] *)
    let is_zero = lift ~f_poly:CostDomain.BasicCost.is_zero ~f_deg:(fun _ -> false)

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


    let pp_cost_msg fmt ({cost_item= {procedure_name}; polynomial} as curr_item) =
      let pp_cost fmt =
        match polynomial with
        | None ->
            Format.fprintf fmt "NA"
        | Some p ->
            CostDomain.BasicCost.pp_hum fmt p
      in
      Format.fprintf fmt "Cost of %s is %t (degree is %a)" procedure_name pp_cost
        (pp_degree ~only_bigO:false) curr_item
  end

  let polynomial_traces = function
    | None ->
        []
    | Some (Val (_, degree_term)) ->
        Polynomials.NonNegativeNonTopPolynomial.polynomial_traces degree_term
    | Some (Below traces) ->
        [("", Polynomials.UnreachableTraces.make_err_trace traces)]
    | Some (Above traces) ->
        [("", Polynomials.TopTraces.make_err_trace traces)]


  let issue_of_cost kind CostIssues.{complexity_increase_issue; unreachable_issue; infinite_issue}
      ~delta ~prev_item:({CostItem.degree_with_term= prev_degree_with_term} as prev_item)
      ~curr_item:
        ({CostItem.cost_item= cost_info; degree_with_term= curr_degree_with_term} as curr_item) =
    let file = cost_info.Jsoncost_t.loc.file in
    let method_name = cost_info.Jsoncost_t.procedure_name in
    let is_on_ui_thread = cost_info.Jsoncost_t.is_on_ui_thread in
    let source_file = SourceFile.create ~check_abs_path:false file in
    let issue_type =
      if CostItem.is_top curr_item then infinite_issue
      else if CostItem.is_unreachable curr_item then unreachable_issue
      else complexity_increase_issue ~is_on_ui_thread
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
        in
        let ui_msg =
          if is_on_ui_thread then
            Format.asprintf "%a %s" MarkupFormatter.pp_bold
              "This function is called on the UI Thread!"
              " It is important to avoid potential regressions in this phase. "
          else ""
        in
        let msg f =
          (* Java Only *)
          if String.equal method_name Procname.Java.constructor_method_name then
            Format.pp_print_string f "constructor"
          else if String.equal method_name Procname.Java.class_initializer_method_name then
            Format.pp_print_string f "class initializer"
          else
            Format.fprintf f "%a"
              (MarkupFormatter.wrap_monospaced Format.pp_print_string)
              method_name
        in
        Format.asprintf "%s of %t has %a from %a to %a. %s%a"
          (CostKind.to_complexity_string kind)
          msg
          (MarkupFormatter.wrap_bold pp_delta)
          delta
          (MarkupFormatter.wrap_monospaced (CostItem.pp_degree ~only_bigO:true))
          prev_item
          (MarkupFormatter.wrap_monospaced (CostItem.pp_degree ~only_bigO:true))
          curr_item ui_msg pp_extra_msg ()
      in
      let line = cost_info.Jsoncost_t.loc.lnum in
      let column = cost_info.Jsoncost_t.loc.cnum in
      let trace =
        let marker_cost_trace msg cost_item =
          [ Errlog.make_trace_element 0
              {Location.line; col= column; file= source_file; macro_file_opt= None; macro_line= -1}
              (Format.asprintf "%s %a" msg CostItem.pp_cost_msg cost_item)
              [] ]
        in
        (("", marker_cost_trace "Previous" prev_item) :: polynomial_traces prev_degree_with_term)
        @ (("", marker_cost_trace "Updated" curr_item) :: polynomial_traces curr_degree_with_term)
        |> Errlog.concat_traces
      in
      let convert Jsoncost_t.{hash; loc; procedure_name; procedure_id} : Jsoncost_t.sub_item =
        {hash; loc; procedure_name; procedure_id}
      in
      Some
        (create_json_bug ~qualifier ~suggestion:None ~line ~file ~source_file ~trace
           ~item:(convert cost_info) ~issue_type )
    else None


  (** Differential of cost reports, based on degree variations. Compare degree_before (DB), and
      degree_after (DA):

      - DB > DA => fixed
      - DB < DA => introduced *)
  let issues_of_reports ~(current_costs : Jsoncost_t.report) ~(previous_costs : Jsoncost_t.report) =
    let fold_aux kind issue_spec ~key:_ ~data (left, both, right) =
      match data with
      | `Both (current, previous) ->
          let max_degree_polynomial l =
            let max = List.max_elt l ~compare:CostItem.compare_by_degree in
            Option.value_exn max
          in
          let curr_item = max_degree_polynomial current in
          let prev_item = max_degree_polynomial previous in
          if Config.filtering && (CostItem.is_zero curr_item || CostItem.is_zero prev_item) then
            (* transitions to/from zero costs are obvious (they
               correspond to adding/removing code to a function with
               empty body), no need to flag them *)
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
    let key_func {CostItem.cost_item} = cost_item.Jsoncost_t.hash in
    let to_map = to_map key_func in
    let decoded_costs costs ~extract_cost_f =
      List.map costs ~f:(fun c ->
          let cost_info = extract_cost_f c in
          let polynomial, degree_with_term =
            if Int.equal cost_info.Jsoncost_t.polynomial_version CostDomain.BasicCost.version then
              let polynomial = CostDomain.BasicCost.decode cost_info.Jsoncost_t.polynomial in
              let degree_with_term = CostDomain.BasicCost.get_degree_with_term polynomial in
              (Some polynomial, Some degree_with_term)
            else (None, None)
          in
          let degree = cost_info.Jsoncost_t.degree in
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
end

module ConfigImpactItem = struct
  module UncheckedCallee = ConfigImpactAnalysis.UncheckedCallee
  module UncheckedCallees = ConfigImpactAnalysis.UncheckedCallees

  type t = {config_impact_item: Jsonconfigimpact_t.item; unchecked_callees: UncheckedCallees.t}

  type change_type = Added | Removed

  let pp_change_type f x =
    Format.pp_print_string f (match x with Added -> "added" | Removed -> "removed")


  let get_qualifier_trace ~change_type unchecked_callees =
    let nb_callees = UncheckedCallees.cardinal unchecked_callees in
    if nb_callees <= 0 then assert false ;
    let is_singleton = nb_callees <= 1 in
    let unchecked_callees =
      UncheckedCallees.elements unchecked_callees
      |> List.stable_sort ~compare:(fun x y ->
             (* Known expensive callees come first. *)
             compare_bool
               (UncheckedCallee.is_known_expensive y)
               (UncheckedCallee.is_known_expensive x) )
    in
    let qualifier =
      Format.asprintf
        "Function call%s to %a %s %a **without GK/QE**, which might cause new unexpected behavior. \
         %s"
        (if is_singleton then "" else "s")
        UncheckedCallee.pp_without_location_list unchecked_callees
        (if is_singleton then "is" else "are")
        pp_change_type change_type FbGKInteraction.action_message
    in
    (* Note: It takes only one trace among the callees. *)
    let trace = List.hd_exn unchecked_callees |> UncheckedCallee.make_err_trace in
    (qualifier, trace)


  let issue_of ~change_type (config_impact_item : Jsonconfigimpact_t.item) callees =
    let should_report =
      ((not Config.filtering) || IssueType.config_impact_analysis.enabled)
      && not (UncheckedCallees.is_empty callees)
    in
    if should_report then
      let qualifier, trace = get_qualifier_trace ~change_type callees in
      let file = config_impact_item.loc.file in
      let source_file = SourceFile.create ~check_abs_path:false file in
      let line = config_impact_item.loc.lnum in
      let convert Jsonconfigimpact_t.{hash; loc; procedure_name; procedure_id} : Jsoncost_t.sub_item
          =
        {hash; loc; procedure_name; procedure_id}
      in
      let issue_type =
        match config_impact_item.mode with
        | `Normal ->
            IssueType.config_impact_analysis
        | `Strict ->
            IssueType.config_impact_analysis_strict
      in
      Some
        (create_json_bug ~qualifier ~suggestion:None ~line ~file ~source_file ~trace
           ~item:(convert config_impact_item) ~issue_type )
    else None


  let issues_of_reports ~(current_config_impact : Jsonconfigimpact_t.report)
      ~(previous_config_impact : Jsonconfigimpact_t.report) =
    let join_unchecked_callees report =
      (* Note: Two methods with identical method names but different number/type of args will have
         the same hash. We join all unchecked callees of them. *)
      match report with
      | [] ->
          assert false
      | {config_impact_item; unchecked_callees} :: tl ->
          let unchecked_callees =
            List.fold tl ~init:unchecked_callees ~f:(fun acc {unchecked_callees} ->
                UncheckedCallees.join acc unchecked_callees )
          in
          {config_impact_item; unchecked_callees}
    in
    let fold_aux ~key:_ ~data ((acc_introduced, acc_fixed) as acc) =
      match data with
      | `Both (current_reports, previous_reports) ->
          (* current/previous reports cannot be empty. *)
          let current = join_unchecked_callees current_reports in
          let previous = join_unchecked_callees previous_reports in
          if
            ConfigImpactAnalysis.equal_mode current.config_impact_item.mode
              previous.config_impact_item.mode
          then
            let introduced =
              UncheckedCallees.diff current.unchecked_callees previous.unchecked_callees
              |> issue_of ~change_type:Added current.config_impact_item
            in
            let removed =
              UncheckedCallees.diff previous.unchecked_callees current.unchecked_callees
              |> issue_of ~change_type:Removed previous.config_impact_item
            in
            (Option.to_list introduced @ acc_introduced, Option.to_list removed @ acc_fixed)
          else (
            L.internal_error
              "Config Impact issues' strict modes are different in current(%a) and previous(%a).@\n"
              ConfigImpactAnalysis.pp_mode current.config_impact_item.mode
              ConfigImpactAnalysis.pp_mode previous.config_impact_item.mode ;
            acc )
      | `Left _ | `Right _ ->
          (* Note: The reports available on one side are ignored since we don't want to report on
             newly added (or freshly removed) functions. *)
          acc
    in
    let map_of_config_impact config_impact =
      List.fold ~init:String.Map.empty config_impact
        ~f:(fun acc ({Jsonconfigimpact_t.hash= key; unchecked_callees} as config_impact_item) ->
          let unchecked_callees = UncheckedCallees.decode unchecked_callees in
          String.Map.add_multi acc ~key ~data:{config_impact_item; unchecked_callees} )
    in
    let current_map = map_of_config_impact current_config_impact in
    let previous_map = map_of_config_impact previous_config_impact in
    Map.fold2 ~init:([], []) current_map previous_map ~f:fold_aux
end

module Report = struct
  let issues_of_reports ~current_report ~previous_report =
    let fold_aux ~key:_ ~data (left, both, right) =
      match data with
      | `Left left' ->
          (List.rev_append left' left, both, right)
      | `Both (both', _) ->
          (left, List.rev_append both' both, right)
      | `Right right' ->
          (left, both, List.rev_append right' right)
    in
    let key_func (issue : Jsonbug_t.jsonbug) = issue.hash in
    let to_map = to_map key_func in
    Map.fold2 (to_map current_report) (to_map previous_report) ~f:fold_aux ~init:([], [], [])
end

let combine_all ~report ~cost ~config_impact =
  dedup report |> List.rev_append cost |> List.rev_append config_impact


(** Set operations should keep duplicated issues with identical hashes *)
let issues_of_reports ~(current_report : Jsonbug_t.report) ~(previous_report : Jsonbug_t.report)
    ~(current_costs : Jsoncost_t.report) ~(previous_costs : Jsoncost_t.report)
    ~(current_config_impact : Jsonconfigimpact_t.report)
    ~(previous_config_impact : Jsonconfigimpact_t.report) : t =
  let introduced, preexisting, fixed = Report.issues_of_reports ~current_report ~previous_report in
  let introduced_costs, preexisting_costs, fixed_costs =
    Cost.issues_of_reports ~current_costs ~previous_costs
  in
  let introduced_config_impact, fixed_config_impact =
    ConfigImpactItem.issues_of_reports ~current_config_impact ~previous_config_impact
  in
  { introduced=
      combine_all ~report:introduced ~cost:introduced_costs ~config_impact:introduced_config_impact
  ; fixed= combine_all ~report:fixed ~cost:fixed_costs ~config_impact:fixed_config_impact
  ; preexisting= combine_all ~report:preexisting ~cost:preexisting_costs ~config_impact:[]
  ; costs_summary= CostsSummary.to_json ~current_costs ~previous_costs }


let to_files {introduced; fixed; preexisting; costs_summary} destdir =
  Out_channel.write_all (destdir ^/ "introduced.json") ~data:(Jsonbug_j.string_of_report introduced) ;
  Out_channel.write_all (destdir ^/ "fixed.json") ~data:(Jsonbug_j.string_of_report fixed) ;
  Out_channel.write_all (destdir ^/ "preexisting.json")
    ~data:(Jsonbug_j.string_of_report preexisting) ;
  Out_channel.write_all (destdir ^/ "costs_summary.json")
    ~data:(Yojson.Basic.to_string costs_summary)
