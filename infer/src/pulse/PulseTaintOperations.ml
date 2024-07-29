(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open TaintConfig
module FuncArg = ProcnameDispatcher.Call.FuncArg
module TaintItemMatcher = PulseTaintItemMatcher

(** {2 Methods for applying taint to relevant values} *)

let taint_value_origin (path : PathContext.t) location taint_item value_origin astate =
  match value_origin with
  | ValueOrigin.InMemory {src; access; dest= dest_addr, dest_hist} ->
      let taint_event = ValueHistory.TaintSource (taint_item, location, path.timestamp) in
      let dest_hist = ValueHistory.sequence taint_event dest_hist ~context:path.conditions in
      Memory.add_edge path src access (dest_addr, dest_hist) location astate
  | ValueOrigin.OnStack {var; addr_hist= addr, hist} ->
      let taint_event = ValueHistory.TaintSource (taint_item, location, path.timestamp) in
      let hist = ValueHistory.sequence taint_event hist ~context:path.conditions in
      Stack.add var (addr, hist) astate
  | ValueOrigin.Unknown _ ->
      astate


let taint_allocation tenv path location ~typ_desc ~alloc_desc ~allocator (v, hist) astate =
  (* Micro-optimisation: do not convert types to strings unless necessary *)
  if List.is_empty allocation_sources then (astate, (v, hist))
  else
    match typ_desc with
    | Typ.Tstruct class_name ->
        let check_type_name type_name =
          let type_name = Typ.Name.name type_name in
          let matching_allocations =
            List.filter allocation_sources ~f:(fun (class_name, _) ->
                String.equal class_name type_name )
          in
          (* Micro-optimisation: do not allocate `alloc_desc` when no matching taint sources are found *)
          if List.is_empty matching_allocations then None
          else
            let alloc_desc =
              Option.value_map allocator ~default:alloc_desc
                ~f:(Format.asprintf "%a" Attribute.pp_allocator)
            in
            let astate, hist =
              List.fold matching_allocations ~init:(astate, hist)
                ~f:(fun (astate, hist) (_, kinds) ->
                  let source =
                    let proc_name = Procname.from_string_c_fun alloc_desc in
                    let value = TaintItem.TaintProcedure proc_name in
                    let origin = TaintItem.Allocation {typ= type_name} in
                    {TaintItem.kinds; value_tuple= Basic {value; origin}}
                  in
                  let hist =
                    ValueHistory.sequence
                      (TaintSource (source, location, path.PathContext.timestamp))
                      hist
                  in
                  let tainted =
                    let time_trace = Timestamp.trace0 path.PathContext.timestamp in
                    {Attribute.Tainted.source; hist; time_trace; intra_procedural_only= false}
                  in
                  let astate =
                    AbductiveDomain.AddressAttributes.add_one v
                      (Tainted (Attribute.TaintedSet.singleton tainted))
                      astate
                  in
                  (astate, hist) )
            in
            Some (astate, hist)
        in
        L.d_printfln "Checking allocation at %a for taint matching %a" Location.pp location
          Typ.Name.pp class_name ;
        let astate_hist_opt = PatternMatch.supertype_find_map_opt tenv check_type_name class_name in
        let astate, hist = Option.value astate_hist_opt ~default:(astate, hist) in
        (astate, (v, hist))
    | _ ->
        (astate, (v, hist))


let fold_reachable_values ~(f : ValueOrigin.t -> AbductiveDomain.t -> AbductiveDomain.t)
    (value_origin0 : ValueOrigin.t) astate0 =
  let rec aux (astate, visited) value_origin =
    let addr = ValueOrigin.value value_origin in
    if AbstractValue.Set.mem addr visited then (astate, visited)
    else
      let visited = AbstractValue.Set.add addr visited in
      let astate = f value_origin astate in
      AbductiveDomain.Memory.fold_edges addr astate ~init:(astate, visited)
        ~f:(fun astate_visited (access, addr_hist) ->
          let value_origin =
            ValueOrigin.InMemory {src= ValueOrigin.addr_hist value_origin; access; dest= addr_hist}
          in
          aux astate_visited value_origin )
  in
  aux (astate0, AbstractValue.Set.empty) value_origin0 |> fst


let taint_sources path location ~intra_procedural_only tainted astate =
  let aux () =
    List.fold tainted ~init:astate ~f:(fun astate TaintItemMatcher.{taint= source; value_origin} ->
        let hist =
          ValueHistory.singleton (TaintSource (source, location, path.PathContext.timestamp))
        in
        let tainted =
          Attribute.Tainted.
            { source
            ; hist
            ; time_trace= Timestamp.trace0 path.PathContext.timestamp
            ; intra_procedural_only }
        in
        fold_reachable_values value_origin astate ~f:(fun value_origin astate ->
            let path_condition = astate.AbductiveDomain.path_condition in
            let addr = ValueOrigin.value value_origin in
            if not (PulseFormula.is_known_zero path_condition addr) then
              AbductiveDomain.AddressAttributes.add_one addr
                (Tainted (Attribute.TaintedSet.singleton tainted))
                astate
              |> taint_value_origin path location source value_origin
            else (
              L.d_printfln
                "Not adding Tainted attribute to the value %a because it is known to be zero"
                AbstractValue.pp addr ;
              astate ) ) )
  in
  L.with_indent "taint_sources" ~f:aux


let taint_sanitizers path location tainted astate =
  let aux () =
    List.fold tainted ~init:astate
      ~f:(fun astate TaintItemMatcher.{taint= sanitizer; value_origin} ->
        let _, history = ValueOrigin.addr_hist value_origin in
        let trace = Trace.Immediate {location; history} in
        let taint_sanitized =
          Attribute.TaintSanitized.
            {sanitizer; time_trace= Timestamp.trace0 path.PathContext.timestamp; trace}
        in
        fold_reachable_values value_origin astate ~f:(fun vo astate ->
            let v = ValueOrigin.value vo in
            AbductiveDomain.AddressAttributes.add_one v
              (TaintSanitized (Attribute.TaintSanitizedSet.singleton taint_sanitized))
              astate ) )
  in
  L.with_indent "taint_sanitizers" ~f:aux


let source_matches_sink_policy sink_kind {SinkPolicy.source_kinds= sink_sources} source_kind =
  (* We should ignore flows between data-flow-only sources and data-flow-only sinks *)
  (not (Kind.is_data_flow_only source_kind && Kind.is_data_flow_only sink_kind))
  && List.mem ~equal:Kind.equal sink_sources source_kind


let sanitizer_matches_sink_policy {SinkPolicy.sanitizer_kinds= sink_sanitizers} sanitizer =
  List.exists sanitizer.TaintItem.kinds ~f:(fun sanitizer_kind ->
      List.mem ~equal:Kind.equal sink_sanitizers sanitizer_kind )


let source_is_sanitized source_times sink_policy
    ({sanitizer; time_trace= sanitizer_times} : Attribute.TaintSanitized.t) =
  let is_sanitized_after_tainted = Timestamp.compare_trace source_times sanitizer_times <= 0 in
  is_sanitized_after_tainted && sanitizer_matches_sink_policy sink_policy sanitizer


let exclude_in_loc source_file exclude_in exclude_matching =
  let source_file = SourceFile.to_string source_file in
  let explicitly_excluded =
    match exclude_in with
    | Some exclude_in ->
        List.exists exclude_in ~f:(fun exclude_in ->
            String.is_substring source_file ~substring:exclude_in )
    | _ ->
        false
  in
  let excluded_by_regex =
    match exclude_matching with
    | Some exclude_matching ->
        List.exists exclude_matching ~f:(fun exclude_matching ->
            match Str.search_forward exclude_matching source_file 0 with
            | _ ->
                true
            | exception Caml.Not_found ->
                false )
    | _ ->
        false
  in
  explicitly_excluded || excluded_by_regex


type taint_policy_violation =
  { source_kind: Kind.t
  ; sink_kind: Kind.t
  ; description: string
  ; policy_id: int
  ; privacy_effect: string option
  ; report_as_issue_type: string option
  ; report_as_category: string option }

let check_source_against_sink_policy location ~source source_times intra_procedural_only hist
    sanitizers ~sink sink_kind sink_policy =
  let has_matching_taint_event_in_history source hist =
    if Config.pulse_taint_check_history then
      (* TODO(izorin): tainting based on value histories doesn't work for function calls arguments yet *)
      if TaintItem.is_argument_origin source && not intra_procedural_only then true
      else if TaintItem.equal source sink then true
      else
        let check = function
          | ValueHistory.TaintSource (taint_item, _, _) ->
              List.exists taint_item.kinds ~f:(source_matches_sink_policy sink_kind sink_policy)
          | ValueHistory.TaintPropagated _ ->
              true
          | _ ->
              false
        in
        ValueHistory.exists_main hist ~f:check
    else true
  in
  let open IOption.Let_syntax in
  let* source_kind =
    List.find source.TaintItem.kinds ~f:(source_matches_sink_policy sink_kind sink_policy)
  in
  L.d_printfln ~color:Red "TAINTED: %a -> %a" Kind.pp source_kind Kind.pp sink_kind ;
  let matching_sanitizers =
    Attribute.TaintSanitizedSet.filter (source_is_sanitized source_times sink_policy) sanitizers
  in
  let { SinkPolicy.description
      ; policy_id
      ; privacy_effect
      ; exclude_in
      ; exclude_matching
      ; report_as_issue_type
      ; report_as_category } =
    sink_policy
  in
  if exclude_in_loc location.Location.file exclude_in exclude_matching then (
    L.d_printfln ~color:Green "...but location %a should be excluded from reporting" SourceFile.pp
      location.Location.file ;
    None )
  else if not (has_matching_taint_event_in_history source hist) then (
    L.d_printfln ~color:Green
      "...but value history doesn't contain matching taint event. Value history: %a" ValueHistory.pp
      hist ;
    (* No relevant taint events in value history -> skip reporting taint *)
    None )
  else if Attribute.TaintSanitizedSet.is_empty matching_sanitizers then (
    L.d_printfln ~color:Red "Value history: %a" ValueHistory.pp hist ;
    Some
      { source_kind
      ; sink_kind
      ; description
      ; policy_id
      ; privacy_effect
      ; report_as_issue_type
      ; report_as_category } )
  else (
    L.d_printfln ~color:Green "...but sanitized by %a" Attribute.TaintSanitizedSet.pp
      matching_sanitizers ;
    None )


let check_policies ~sink ~source ~source_times ~intra_procedural_only ~hist ~sanitizers ~location =
  let check_against_sink acc sink_kind =
    match Hashtbl.find SinkPolicy.sink_policies sink_kind with
    | None ->
        (* This can happen when there's a sink with kind S but no policy with sink kind S. We
           should handle such cases gracefully. *)
        acc
    | Some policies ->
        let check_against_policy =
          check_source_against_sink_policy location ~source source_times intra_procedural_only hist
            sanitizers ~sink sink_kind
        in
        let rev_matching_sources = List.rev_filter_map policies ~f:check_against_policy in
        List.rev_append rev_matching_sources acc
  in
  List.fold sink.TaintItem.kinds ~init:[] ~f:check_against_sink


(** Preorder traversal of the tree formed by taint dependencies of [v] in [astate] *)
let fold_taint_dependencies addr_hist0 astate ~init ~f =
  let visited = ref AbstractValue.Set.empty in
  let should_follow_access (access : Access.t) =
    (* NOTE: if the value is an array we propagate the check to the array elements. Similarly for
       fields in Hack because we have use-cases around shapes where only a field is tainted, hence
       we either need to follow the fields or need to add taint operations into all relevant
       models.

       We could do the same for all accesses if we want the taint analysis to consider that the
       insides of objects are tainted whenever the object is. This might not be a very efficient
       way to do this though? *)
    match access with
    | ArrayAccess _ ->
        true
    | FieldAccess _ ->
        Language.curr_language_is Hack || Language.curr_language_is Python
    | Dereference ->
        false
  in
  let acc, sanitizers = f init Attribute.TaintSanitizedSet.empty addr_hist0 in
  let acc_ref = ref acc in
  let rec fold_taint_dependencies_aux sanitizers from_hist0 from_addr =
    if not (AbstractValue.Set.mem from_addr !visited) then (
      visited := AbstractValue.Set.add from_addr !visited ;
      (* Collect propagated taint from attributes *)
      Option.iter (AbductiveDomain.AddressAttributes.get_propagate_taint_from from_addr astate)
        ~f:(fun (reason, taints_in) ->
          List.iter taints_in ~f:(fun Attribute.{v= to_v; history= to_h} ->
              let hist =
                match (reason : Attribute.taint_propagation_reason) with
                | UnknownCall ->
                    (* for unknown calls [from_hist] should already contain the taint event *)
                    L.d_printfln
                      "Propagating from original history to %a (reason: %a), hist=[@[<hv>%a@]]"
                      AbstractValue.pp to_v Attribute.pp_taint_propagation_reason reason
                      ValueHistory.pp from_hist0 ;
                    from_hist0
                | UserConfig | InternalModel ->
                    (* but if the taint was propagated then we need to fall back to the history
                       present in the attribute even though it may not be accurate, because the
                       propagation of the tainting event cannot be accurately propagated to
                       histories *)
                    L.d_printfln "Propagating from attribute to %a (reason: %a), hist=[@[<hv>%a@]]"
                      AbstractValue.pp to_v Attribute.pp_taint_propagation_reason reason
                      ValueHistory.pp to_h ;
                    to_h
              in
              let acc, sanitizers = f !acc_ref sanitizers (to_v, hist) in
              acc_ref := acc ;
              fold_taint_dependencies_aux sanitizers from_hist0 to_v ) ) ;
      (* Collect taint from memory [from_addr_hist] points to *)
      AbductiveDomain.Memory.fold_edges from_addr astate ~init:()
        ~f:(fun () (access, (dest_addr, _)) ->
          if should_follow_access access then
            Option.iter (AbductiveDomain.Memory.find_edge_opt dest_addr Dereference astate)
              ~f:(fun deref_dest ->
                L.d_printfln "Propagating from edges to %a, hist=[@[<hv>%a@]]" AbstractValue.pp
                  (fst deref_dest) ValueHistory.pp (snd deref_dest) ;
                let acc, sanitizers = f !acc_ref sanitizers deref_dest in
                acc_ref := acc ;
                fold_taint_dependencies_aux sanitizers (snd deref_dest) (fst deref_dest) ) ) )
  in
  fold_taint_dependencies_aux sanitizers (snd addr_hist0) (fst addr_hist0) ;
  !acc_ref


module LocationIntSet = PrettyPrintable.MakePPSet (struct
  type nonrec t = Location.t * int [@@deriving compare]

  let pp = Pp.pair ~fst:Location.pp ~snd:Int.pp
end)

let dedup_reports results =
  let find_duplicate_taint loc policy_id (location, taint_policy) =
    Location.equal location loc && Int.equal policy_id taint_policy
  in
  let dedup_result result =
    match result with
    | Ok _ | FatalError _ ->
        result
    | Recoverable (a, errors) ->
        let dedup_errors error (acc, taint_found) =
          match error with
          | ReportableError {diagnostic= TaintFlow {location; policy_id}} ->
              let found_duplicate =
                LocationIntSet.exists (find_duplicate_taint location policy_id) taint_found
              in
              let taint_found = LocationIntSet.add (location, policy_id) taint_found in
              if found_duplicate then (acc, taint_found) else (error :: acc, taint_found)
          | _ ->
              (error :: acc, taint_found)
        in
        let new_errors, _ =
          List.fold_right ~f:dedup_errors ~init:([], LocationIntSet.empty) errors
        in
        Recoverable (a, new_errors)
  in
  List.map ~f:dedup_result results


let check_flows_wrt_sink_ ?(policy_violations_to_report = (IntSet.empty, [])) path location
    ~sink:(sink_item, sink_trace) ~source astate =
  let source_value, _ = source in
  let source_expr = Decompiler.find source_value astate in
  let mk_reportable_error diagnostic = ReportableError {astate; diagnostic} in
  let check_tainted_flows policy_violations_to_report sanitizers (v, hist) astate =
    L.d_printfln "Checking that %a%a is not tainted" AbstractValue.pp v
      (if Config.debug_level_analysis >= 3 then ValueHistory.pp else fun _ _ -> ())
      hist ;
    let sources, new_sanitizers =
      AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers v astate
    in
    let sanitizers = Attribute.TaintSanitizedSet.union sanitizers new_sanitizers in
    ( Attribute.TaintedSet.fold
        (fun {source; time_trace= source_times; hist= source_hist; intra_procedural_only}
             policy_violations_to_report ->
          L.d_printfln_escaped ~color:Red "Found source %a, checking policy..." TaintItem.pp source ;
          let potential_policy_violations =
            check_policies ~sink:sink_item ~source ~source_times ~intra_procedural_only ~hist
              ~sanitizers ~location
          in
          let report_policy_violation (reported_so_far, policy_violations_to_report)
              { source_kind
              ; sink_kind
              ; description= policy_description
              ; policy_id= violated_policy_id
              ; privacy_effect= policy_privacy_effect
              ; report_as_issue_type
              ; report_as_category } =
            if IntSet.mem violated_policy_id reported_so_far then
              (reported_so_far, policy_violations_to_report)
            else
              let flow_kind : Diagnostic.flow_kind =
                if TaintConfig.Kind.is_data_flow_only source_kind then FlowToSink
                else if TaintConfig.Kind.is_data_flow_only sink_kind then FlowFromSource
                else TaintedFlow
              in
              ( IntSet.add violated_policy_id reported_so_far
              , mk_reportable_error
                  (TaintFlow
                     { expr= source_expr
                     ; location
                     ; source= ({source with kinds= [source_kind]}, source_hist)
                     ; sink= ({sink_item with kinds= [sink_kind]}, sink_trace)
                     ; flow_kind
                     ; policy_description
                     ; policy_id= violated_policy_id
                     ; policy_privacy_effect
                     ; report_as_issue_type
                     ; report_as_category } )
                :: policy_violations_to_report )
          in
          List.fold potential_policy_violations ~init:policy_violations_to_report
            ~f:report_policy_violation )
        sources policy_violations_to_report
    , sanitizers )
  in
  L.with_indent "check flows wrt sink from %a (%a)" AbstractValue.pp source_value
    DecompilerExpr.pp_with_abstract_value source_expr ~collapsible:true ~f:(fun () ->
      fold_taint_dependencies source astate ~init:(astate, policy_violations_to_report)
        ~f:(fun (astate, policy_violations_to_report) sanitizers ((addr, _) as addr_hist) ->
          let astate =
            AbductiveDomain.AddressAttributes.add_taint_sink path sink_item sink_trace addr astate
          in
          let policy_violations_to_report, sanitizers =
            check_tainted_flows policy_violations_to_report sanitizers addr_hist astate
          in
          ((astate, policy_violations_to_report), sanitizers) ) )


let of_astate_with_accumulated_policy_violations (astate, (_policy_ids, policy_violations_to_report))
    =
  if List.is_empty policy_violations_to_report then Ok astate
  else Recoverable (astate, policy_violations_to_report)


(* for .mli: (implicitly) remove optional argument *)
let check_flows_wrt_sink path location ~sink ~source astate =
  check_flows_wrt_sink_ path location ~sink ~source astate
  |> of_astate_with_accumulated_policy_violations


let rec should_ignore_all_flows_to (potential_taint_value : TaintItem.value_tuple) =
  match potential_taint_value with
  | Basic {value= TaintProcedure proc_name} ->
      Procname.is_objc_dealloc proc_name || BuiltinDecl.is_declared proc_name
  | Basic _ ->
      false
  | FieldOf {value_tuple} | PointedToBy {value_tuple} ->
      should_ignore_all_flows_to value_tuple


let taint_sinks path location tainted astate =
  let aux () =
    PulseResult.list_fold tainted ~init:astate
      ~f:(fun astate TaintItemMatcher.{taint= sink; value_origin} ->
        if should_ignore_all_flows_to sink.value_tuple then Ok astate
        else
          let v, history = ValueOrigin.addr_hist value_origin in
          let sink_trace = Trace.Immediate {location; history} in
          let visited = ref AbstractValue.Set.empty in
          let rec mark_sinked policy_violations_to_report ?access ~(sink : TaintItem.t) v hist
              astate =
            let is_closure = Option.is_some (AddressAttributes.get_closure_proc_name v astate) in
            if AbstractValue.Set.mem v !visited || is_closure then
              (astate, policy_violations_to_report)
            else (
              visited := AbstractValue.Set.add v !visited ;
              let sink_value_tuple =
                match access with
                | Some (Access.FieldAccess fieldname) ->
                    TaintItem.FieldOf
                      {name= Fieldname.get_field_name fieldname; value_tuple= sink.value_tuple}
                | Some Access.Dereference ->
                    TaintItem.PointedToBy {value_tuple= sink.value_tuple}
                | _ ->
                    sink.value_tuple
              in
              let new_sink = {sink with value_tuple= sink_value_tuple} in
              let astate =
                AbductiveDomain.AddressAttributes.add_taint_sink path new_sink sink_trace v astate
              in
              let astate, policy_violations_to_report =
                check_flows_wrt_sink_ ~policy_violations_to_report path location
                  ~sink:(new_sink, sink_trace) ~source:(v, hist) astate
              in
              AbductiveDomain.Memory.fold_edges v astate ~init:(astate, policy_violations_to_report)
                ~f:(fun ((astate, policy_violations_to_report) as res) (access, (v, hist)) ->
                  match access with
                  | Access.FieldAccess fieldname
                    when Fieldname.equal fieldname PulseOperations.ModeledField.internal_string
                         || Fieldname.equal fieldname
                              PulseOperations.ModeledField.internal_ref_count ->
                      res
                  | _ ->
                      mark_sinked policy_violations_to_report ~access ~sink:new_sink v hist astate )
              )
          in
          mark_sinked (IntSet.empty, []) ~sink v history astate
          |> of_astate_with_accumulated_policy_violations )
  in
  L.with_indent "taint_sinks" ~f:aux


let propagate_to path location reason value_origin values call astate =
  let v = ValueOrigin.value value_origin in
  let path_condition = astate.AbductiveDomain.path_condition in
  if PulseFormula.is_known_zero path_condition v then
    AbductiveDomain.AddressAttributes.remove_taint_attrs v astate
  else
    let astate =
      if not (List.is_empty values) then
        let taints_in =
          List.map values ~f:(fun {FuncArg.arg_payload= value_origin} ->
              let v, history = ValueOrigin.addr_hist value_origin in
              Attribute.{v; history} )
        in
        AbductiveDomain.AddressAttributes.add_one v (PropagateTaintFrom (reason, taints_in)) astate
      else astate
    in
    fold_reachable_values value_origin astate ~f:(fun vo astate ->
        let v = ValueOrigin.value vo in
        List.fold values ~init:astate ~f:(fun astate {FuncArg.arg_payload= actual_value_origin} ->
            let actual = ValueOrigin.value actual_value_origin in
            let sources, sanitizers =
              AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers actual astate
            in
            if not (Attribute.TaintedSet.is_empty sources) then
              L.d_printfln "tainting %a as source" AbstractValue.pp v ;
            let astate =
              let tainted =
                Attribute.TaintedSet.map
                  (fun {source; time_trace; hist= source_hist; intra_procedural_only} ->
                    let hist =
                      ValueHistory.sequence ~context:path.PathContext.conditions
                        (Call
                           { f= call
                           ; location
                           ; timestamp= path.PathContext.timestamp
                           ; in_call= ValueHistory.epoch } )
                        source_hist
                    in
                    { source
                    ; time_trace= Timestamp.add_to_trace time_trace path.PathContext.timestamp
                    ; hist
                    ; intra_procedural_only } )
                  sources
              in
              let astate = AbductiveDomain.AddressAttributes.add_one v (Tainted tainted) astate in
              Attribute.TaintedSet.fold
                (fun {source} astate -> taint_value_origin path location source vo astate)
                sources astate
            in
            if not (Attribute.TaintSanitizedSet.is_empty sanitizers) then
              L.d_printfln "registering %a as sanitizer" AbstractValue.pp v ;
            let astate =
              let taint_sanitized =
                Attribute.TaintSanitizedSet.map
                  (fun {sanitizer; time_trace; trace} ->
                    let trace =
                      Trace.ViaCall {f= call; location; history= ValueHistory.epoch; in_call= trace}
                    in
                    { sanitizer
                    ; time_trace= Timestamp.add_to_trace time_trace path.PathContext.timestamp
                    ; trace } )
                  sanitizers
              in
              AbductiveDomain.AddressAttributes.add_one v (TaintSanitized taint_sanitized) astate
            in
            astate ) )


let taint_propagators path location reason proc_name actuals tainted astate =
  let aux () =
    List.fold tainted ~init:astate ~f:(fun astate TaintItemMatcher.{value_origin} ->
        let v = ValueOrigin.value value_origin in
        let other_actuals =
          List.filter actuals ~f:(fun {FuncArg.arg_payload= actual_value_origin} ->
              let actual = ValueOrigin.value actual_value_origin in
              not (AbstractValue.equal v actual) )
        in
        propagate_to path location reason value_origin other_actuals (Call proc_name) astate )
  in
  L.with_indent "taint_propagators" ~f:aux


let is_cpp_assignment_operator proc_name_opt =
  Option.exists proc_name_opt ~f:Procname.is_cpp_assignment_operator


(* returns boolean triple (propagate_to_return, propagate_to_receiver, propagate_to_last_actual) *)
let compute_taint_propagation_for_unknown_calls_generic tenv proc_name_opt return_typ actuals
    has_added_return_param =
  let is_static =
    Option.exists proc_name_opt ~f:(fun proc_name ->
        Option.value ~default:true (Procname.is_static proc_name) )
  in
  match (proc_name_opt, return_typ, actuals) with
  | Some proc_name, _, _ when Procname.is_constructor proc_name ->
      L.d_printfln "unknown constructor, propagating taint to receiver" ;
      (false, true, false)
  | _, {Typ.desc= Tvoid}, _ when has_added_return_param ->
      (false, false, true)
  | _, {Typ.desc= Tint _ | Tfloat _ | Tvoid}, _ when not is_static ->
      (* for instance methods with a non-Object return value, propagate the taint to the
             receiver *)
      L.d_printfln "non-object return type, propagating taint to receiver" ;
      (false, true, false)
  | ( Some proc_name
    , {Typ.desc= Tptr ({desc= Tstruct return_typename}, _)}
    , {FuncArg.typ= {desc= Tptr ({desc= Tstruct receiver_typename}, _)}} :: _ )
    when (not is_static)
         && Option.exists (Procname.get_class_type_name proc_name) ~f:(fun class_typename ->
                Typ.Name.equal return_typename class_typename
                && PatternMatch.supertype_exists tenv
                     (fun type_name _struct -> Typ.Name.equal class_typename type_name)
                     receiver_typename ) ->
      (* if the receiver and return type are the same, propagate to both. we're assuming the call
         is one of the common "builder-style" methods that both updates and returns the
         receiver *)
      L.d_printfln "chainable call, propagating taint to both return and receiver" ;
      (true, true, false)
  | _, {Typ.desc= Tptr _ | Tstruct _}, _ when is_cpp_assignment_operator proc_name_opt ->
      L.d_printfln "cpp operator=, propagating to receiver" ;
      (false, true, false)
  | _, {Typ.desc= Tptr _ | Tstruct _}, _ ->
      L.d_printfln "object return type, propagating taint to return" ;
      (true, false, false)
  | _, _, _ ->
      L.d_printfln "not propagating taint" ;
      (false, false, false)


let is_procname_from_std_namespace proc_name =
  match QualifiedCppName.to_list (Procname.get_qualifiers proc_name) with
  | "std" :: _ ->
      true
  | _ ->
      false


(* returns boolean triple (propagate_to_return, propagate_to_receiver, propagate_to_last_actual) *)
let compute_taint_propagation_for_unknown_calls tenv proc_name_opt return_typ actuals
    has_added_return_param =
  match proc_name_opt with
  | Some proc_name when Language.equal Language.Clang (Procname.get_language proc_name) -> (
    match Procname.get_method proc_name with
    | "operator+="
    | "operator-="
    | "operator*="
    | "operator/="
    | "operator%="
    | "operator<<="
    | "operator>>="
    | "operator&="
    | "operator^="
    | "operator|="
    | "memcpy"
    | "memmove"
    | "strcpy"
    | "strncpy" ->
        (true, true, false)
    | "swap" when is_procname_from_std_namespace proc_name ->
        (* A special case for std::.*::swap as it is marked as static
           and does not propagate to receiver as expected in the generic
           case for unknown functions *)
        (false, true, false)
    | "sprintf" ->
        (false, true, false)
    | _ ->
        compute_taint_propagation_for_unknown_calls_generic tenv proc_name_opt return_typ actuals
          has_added_return_param )
  | _ ->
      compute_taint_propagation_for_unknown_calls_generic tenv proc_name_opt return_typ actuals
        has_added_return_param


(* merge all taint from actuals into the return value *)
let propagate_taint_for_unknown_calls tenv path location (return, return_typ)
    ~has_added_return_param call proc_name_opt actuals astate =
  L.d_printfln "propagating all taint for unknown call" ;
  let return = Var.of_id return in
  let propagate_to_return, propagate_to_receiver, propagate_to_last_actual =
    compute_taint_propagation_for_unknown_calls tenv proc_name_opt return_typ actuals
      has_added_return_param
  in
  let astate =
    match actuals with
    | {FuncArg.arg_payload= this_path} :: _ when is_cpp_assignment_operator proc_name_opt ->
        let this = ValueOrigin.value this_path in
        L.d_printfln "remove taint info of %a for cpp operator=" AbstractValue.pp this ;
        AddressAttributes.remove_taint_attrs this astate
    | _ ->
        astate
  in
  let astate =
    match Stack.find_opt return astate with
    | Some return_addr_hist when propagate_to_return ->
        let return_value_origin = ValueOrigin.OnStack {var= return; addr_hist= return_addr_hist} in
        propagate_to path location UnknownCall return_value_origin actuals call astate
    | _ ->
        astate
  in
  let astate =
    match actuals with
    | {FuncArg.arg_payload= this} :: other_actuals when propagate_to_receiver ->
        propagate_to path location UnknownCall this other_actuals call astate
    | _ ->
        astate
  in
  match List.rev actuals with
  | {FuncArg.arg_payload= last} :: other_actuals when propagate_to_last_actual ->
      propagate_to path location UnknownCall last other_actuals call astate
  | _ ->
      astate


(* some pulse models are not a faithful reflection of the behaviour and should be treated as unknown
   wrt taint behaviour (i.e. propagate taint like an unknown function would) *)
let pulse_models_to_treat_as_unknown_for_taint =
  (* HACK: make a list of matchers just to reuse the matching code below *)
  let dummy_matcher_of_procedure_matcher procedure_matcher =
    {Unit.procedure_matcher; arguments= []; kinds= []; procedure_target= ReturnValue}
  in
  [ ClassAndMethodNames
      { class_names= ["java.lang.StringBuilder"]
      ; method_names= ["append"; "delete"; "replace"; "setLength"] }
  ; ProcedureNameRegex
      { name_regex= Str.regexp "std::basic_string<.*>::basic_string"
      ; exclude_in= None
      ; exclude_names= None } ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint tenv ?proc_attributes proc_name =
  Option.exists proc_attributes ~f:(fun attrs -> attrs.ProcAttributes.is_cpp_implicit)
  && Procname.is_constructor proc_name
  || TaintItemMatcher.procedure_matches_any tenv proc_name proc_attributes
       pulse_models_to_treat_as_unknown_for_taint


let call tenv path location return ~call_was_unknown (call : _ Either.t)
    (actuals : ValueOrigin.t FuncArg.t list) astate =
  L.with_indent "taint operations -> call" ~f:(fun () ->
      match call with
      | First call_exp ->
          L.d_printfln "call to expression [unknown=%b]" call_was_unknown ;
          if call_was_unknown then
            Ok
              (propagate_taint_for_unknown_calls tenv path location return
                 ~has_added_return_param:false (SkippedUnknownCall call_exp) None actuals astate )
          else Ok astate
      | Second proc_name ->
          let proc_attributes = IRAttributes.load proc_name in
          let has_added_return_param =
            match proc_attributes with
            | Some attrs when attrs.ProcAttributes.has_added_return_param ->
                true
            | _ ->
                false
          in
          let match_call matchers astate =
            TaintItemMatcher.match_procedure_call tenv path location ?proc_attributes
              ~has_added_return_param proc_name actuals return matchers astate
          in
          let call_was_unknown =
            call_was_unknown || should_treat_as_unknown_for_taint tenv ?proc_attributes proc_name
          in
          L.d_printfln "call to proc [unknown=%b]" call_was_unknown ;
          let astate =
            let astate, tainted = match_call sanitizer_matchers astate in
            taint_sanitizers path location tainted astate
          in
          let astate =
            let astate, tainted = match_call source_procedure_matchers astate in
            taint_sources path location ~intra_procedural_only:false tainted astate
          in
          let astate, propagator_matches = match_call propagator_matchers astate in
          let+ astate =
            let astate, tainted = match_call sink_procedure_matchers astate in
            taint_sinks path location tainted astate
          in
          let astate, should_propagate_for_unknown =
            let new_state =
              taint_propagators path location UserConfig proc_name actuals propagator_matches astate
            in
            (new_state, call_was_unknown && phys_equal astate new_state)
          in
          if should_propagate_for_unknown then
            propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param
              (SkippedKnownCall proc_name) (Some proc_name) actuals astate
          else astate )


let store tenv path location ~lhs:lhs_exp ~rhs:(rhs_exp, rhs_path, typ) astate =
  match lhs_exp with
  | Exp.Lfield (_, field_name, _) ->
      let rhs = {FuncArg.exp= rhs_exp; typ; arg_payload= rhs_path} in
      let astate, tainted =
        TaintItemMatcher.match_field tenv location field_name rhs sink_field_setters_matchers astate
      in
      taint_sinks path location tainted astate
  | _ ->
      Ok astate


let load procname tenv path location ~lhs:(lhs_id, typ) ~rhs:rhs_exp astate =
  match rhs_exp with
  | Exp.Lfield (_, field_name, _) when not (Procname.is_objc_dealloc procname) -> (
      let lhs_exp = Exp.Var lhs_id in
      let result = PulseOperations.eval_to_value_origin path NoAccess location lhs_exp astate in
      match PulseOperationResult.sat_ok result with
      | Some (astate, lhs_value_origin) ->
          let lhs = {FuncArg.exp= lhs_exp; typ; arg_payload= lhs_value_origin} in
          let match_field matchers astate =
            TaintItemMatcher.match_field tenv location field_name lhs matchers astate
          in
          let astate =
            let astate, tainted = match_field source_field_getters_matchers astate in
            taint_sources path location ~intra_procedural_only:false tainted astate
          in
          let astate =
            let astate, tainted = match_field sink_field_getters_matchers astate in
            taint_sinks path location tainted astate
          in
          astate
      | None ->
          L.internal_error
            "could not add taint to the exp %a, got an error or an unsat state starting from %a"
            Exp.pp lhs_exp AbductiveDomain.pp astate ;
          Ok astate )
  | _ ->
      Ok astate


let taint_initial tenv (proc_attrs : ProcAttributes.t) astate0 =
  let result =
    let++ astate, rev_actuals =
      List.fold
        (ProcAttributes.get_pvar_formals proc_attrs)
        ~init:(Sat (Ok (astate0, [])))
        ~f:(fun result (pvar, typ) ->
          let** astate, rev_actuals = result in
          let++ astate, actual_value_origin =
            PulseOperations.eval_deref_to_value_origin PathContext.initial proc_attrs.loc
              (Lvar pvar) astate
          in
          let actual = {FuncArg.exp= Lvar pvar; typ; arg_payload= actual_value_origin} in
          (astate, actual :: rev_actuals) )
    in
    let actuals = List.rev rev_actuals in
    let location = proc_attrs.loc in
    (* Apply taint sources *)
    let astate =
      let astate, tainted =
        match proc_attrs.block_as_arg_attributes with
        | Some {passed_to} ->
            (* process block *)
            let passed_to_attr = IRAttributes.load passed_to in
            TaintItemMatcher.match_block tenv location ?proc_attributes:passed_to_attr passed_to
              actuals source_block_matchers astate
        | None ->
            (* process regular proc *)
            TaintItemMatcher.match_procedure tenv proc_attrs actuals source_procedure_matchers
              astate
      in
      taint_sources PathContext.initial location ~intra_procedural_only:true tainted astate
    in
    (* Apply taint sanitizers *)
    let astate =
      let astate, sanitized =
        TaintItemMatcher.match_procedure tenv proc_attrs actuals sanitizer_matchers astate
      in
      taint_sanitizers PathContext.initial location sanitized astate
    in
    astate
  in
  match PulseOperationResult.sat_ok result with
  | Some astate_tainted ->
      astate_tainted
  | None ->
      L.internal_error
        "could not add taint to the initial state for %a, got an error or an unsat state starting \
         from %a"
        Procname.pp proc_attrs.proc_name AbductiveDomain.pp astate0 ;
      astate0


let procedure_matches_source tenv procname =
  TaintItemMatcher.procedure_matches tenv source_procedure_matchers procname []
  |> List.is_empty |> not
