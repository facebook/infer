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

(** {2 Sources, sinks, and sanitizers} *)

let fill_data_flow_kinds_from_config () =
  Config.pulse_taint_config.data_flow_kinds
  |> List.iter ~f:(fun kind -> Kind.of_string kind |> Kind.mark_data_flow_only)


let fill_policies_from_config () =
  Config.pulse_taint_config.policies
  |> List.iter
       ~f:(fun
            {Pulse_config_t.short_description= description; taint_flows; privacy_effect; exclude_in}
          ->
         let policy_id = SinkPolicy.next_policy_id () in
         List.iter taint_flows ~f:(fun {Pulse_config_t.source_kinds; sanitizer_kinds; sink_kinds} ->
             let source_kinds = List.map source_kinds ~f:Kind.of_string in
             let sanitizer_kinds = List.map sanitizer_kinds ~f:Kind.of_string in
             List.iter sink_kinds ~f:(fun sink_kind_s ->
                 let sink_kind = Kind.of_string sink_kind_s in
                 let flow =
                   { SinkPolicy.source_kinds
                   ; sanitizer_kinds
                   ; description
                   ; policy_id
                   ; privacy_effect
                   ; exclude_in }
                 in
                 Hashtbl.update SinkPolicy.sink_policies sink_kind ~f:(function
                   | None ->
                       [flow]
                   | Some flows ->
                       flow :: flows ) ) ) )


let () =
  Hashtbl.add SinkPolicy.sink_policies ~key:Kind.simple_kind
    ~data:
      [ { SinkPolicy.description=
            "Built-in Simple taint kind, matching any Simple source with any Simple sink except if \
             any Simple sanitizer is in the way"
        ; source_kinds= [Kind.simple_kind]
        ; sanitizer_kinds= [Kind.simple_kind]
        ; policy_id= SinkPolicy.next_policy_id ()
        ; privacy_effect= None
        ; exclude_in= None } ]
  |> ignore ;
  fill_data_flow_kinds_from_config () ;
  fill_policies_from_config ()


let get_procedure_field_matchers matchers ~field_matchers_allowed ~block_matchers_allowed
    ~option_name =
  let procedure_matchers, field_matchers =
    List.partition_map matchers ~f:(fun matcher ->
        match matcher with
        | TaintConfig.Unit.ProcedureUnit procedure_matcher ->
            Either.First procedure_matcher
        | TaintConfig.Unit.FieldUnit field_unit ->
            Either.Second field_unit )
  in
  let field_getters_matchers, field_setters_matchers =
    List.partition_map field_matchers ~f:(fun (field_unit : TaintConfig.Unit.field_unit) ->
        match field_unit.field_target with
        | GetField ->
            Either.First field_unit
        | SetField ->
            Either.Second field_unit )
  in
  let block_matchers, procedure_matchers =
    List.partition_map procedure_matchers ~f:(fun (matcher : TaintConfig.Unit.procedure_unit) ->
        match matcher.procedure_matcher with
        | TaintConfig.Unit.Block _ | TaintConfig.Unit.BlockNameRegex _ ->
            Either.First matcher
        | _ ->
            Either.Second matcher )
  in
  if (not field_matchers_allowed) && List.length field_matchers > 0 then
    L.die UserError
      "field_matchers are not allowed in the option %s but got the following\n   field matchers: %a"
      option_name
      (Pp.comma_seq TaintConfig.Unit.pp_field_unit)
      field_matchers ;
  if (not block_matchers_allowed) && List.length block_matchers > 0 then
    L.die UserError
      "block_matchers are not allowed in the option %s but got the following\n   block matchers: %a"
      option_name
      (Pp.comma_seq TaintConfig.Unit.pp_procedure_unit)
      block_matchers ;
  (procedure_matchers, block_matchers, field_getters_matchers, field_setters_matchers)


let ( allocation_sources
    , source_procedure_matchers
    , source_block_matchers
    , source_field_getters_matchers
    , source_field_setters_matchers ) =
  let option_name = "--pulse-taint-sources" in
  let all_source_matchers =
    TaintConfig.Unit.of_config ~default_taint_target:`ReturnValue ~option_name
      Config.pulse_taint_config.sources
  in
  let allocation_sources, source_matchers =
    List.partition_map all_source_matchers ~f:(fun matcher ->
        match matcher with
        | TaintConfig.Unit.ProcedureUnit {TaintConfig.Unit.procedure_matcher; kinds} -> (
          match procedure_matcher with
          | Allocation {class_name} ->
              Either.First (class_name, kinds)
          | _ ->
              Either.Second matcher )
        | _ ->
            Either.Second matcher )
  in
  let procedure_matchers, block_matchers, field_getters_matchers, field_setters_matchers =
    get_procedure_field_matchers source_matchers ~field_matchers_allowed:true
      ~block_matchers_allowed:true ~option_name
  in
  ( allocation_sources
  , procedure_matchers
  , block_matchers
  , field_getters_matchers
  , field_setters_matchers )


let sink_procedure_matchers, sink_field_getters_matchers, sink_field_setters_matchers =
  let option_name = "--pulse-taint-sinks" in
  let sink_matchers =
    TaintConfig.Unit.of_config ~default_taint_target:`AllArguments ~option_name
      Config.pulse_taint_config.sinks
  in
  let procedure_matchers, _, field_getters_matchers, field_setters_matchers =
    get_procedure_field_matchers sink_matchers ~field_matchers_allowed:true
      ~block_matchers_allowed:false ~option_name
  in
  (procedure_matchers, field_getters_matchers, field_setters_matchers)


let sanitizer_matchers =
  let option_name = "--pulse-taint-sanitizer" in
  let sink_matchers =
    TaintConfig.Unit.of_config ~default_taint_target:`AllArguments ~option_name
      Config.pulse_taint_config.sanitizers
  in
  let procedure_matchers, _, _, _ =
    get_procedure_field_matchers sink_matchers ~field_matchers_allowed:false
      ~block_matchers_allowed:false ~option_name
  in
  procedure_matchers


let propagator_matchers =
  let option_name = "--pulse-taint-propagators" in
  let propagator_matchers =
    TaintConfig.Unit.of_config ~default_taint_target:`ReturnValue ~option_name
      Config.pulse_taint_config.propagators
  in
  let procedure_matchers, _, _, _ =
    get_procedure_field_matchers propagator_matchers ~field_matchers_allowed:false
      ~block_matchers_allowed:false ~option_name
  in
  procedure_matchers


let log_taint_config () =
  L.debug Analysis Verbose "@\nSink policies:@\n%a@." SinkPolicy.pp_sink_policies
    SinkPolicy.sink_policies ;
  L.debug Analysis Verbose "Procedure source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    source_procedure_matchers ;
  L.debug Analysis Verbose "Block source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    source_block_matchers ;
  L.debug Analysis Verbose "Field getters source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    source_field_getters_matchers ;
  L.debug Analysis Verbose "Field setters source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    source_field_setters_matchers ;
  L.debug Analysis Verbose "Procedure sink matchers:@\n %a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    sink_procedure_matchers ;
  L.debug Analysis Verbose "Field getters sink matchers:@\n %a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    sink_field_getters_matchers ;
  L.debug Analysis Verbose "Field setters sink matchers:@\n %a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    sink_field_setters_matchers ;
  L.debug Analysis Verbose "Sanitizer matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    sanitizer_matchers ;
  L.debug Analysis Verbose "Propagator matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    propagator_matchers


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
  L.d_with_indent "taint_sources" ~f:aux


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
  L.d_with_indent "taint_sanitizers" ~f:aux


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


let exclude_in_loc source_file exclude_in =
  match exclude_in with
  | Some exclude_in ->
      List.exists exclude_in ~f:(fun exclude_in ->
          String.is_substring (SourceFile.to_string source_file) ~substring:exclude_in )
  | _ ->
      false


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
  let* suspicious_source =
    List.find source.TaintItem.kinds ~f:(source_matches_sink_policy sink_kind sink_policy)
  in
  L.d_printfln ~color:Red "TAINTED: %a -> %a" Kind.pp suspicious_source Kind.pp sink_kind ;
  let matching_sanitizers =
    Attribute.TaintSanitizedSet.filter (source_is_sanitized source_times sink_policy) sanitizers
  in
  let {SinkPolicy.description; policy_id; privacy_effect; exclude_in} = sink_policy in
  if exclude_in_loc location.Location.file exclude_in then (
    L.d_printfln ~color:Green "...but location %a should be excluded from reporting" SourceFile.pp
      location.Location.file ;
    None )
  else if not (has_matching_taint_event_in_history source hist) then (
    L.d_printfln ~color:Green
      "...but value history doesn't contain matching taint event. Value history: %a" ValueHistory.pp
      hist ;
    (* No relevant taint events in value history -> skip reporting taint *)
    None )
  else if Attribute.TaintSanitizedSet.is_empty matching_sanitizers then
    Some (suspicious_source, sink_kind, description, policy_id, privacy_effect)
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


module TaintDependencies = struct
  module G = Graph.Imperative.Digraph.Concrete (struct
    type t = AbstractValue.t * (ValueHistory.t[@hash.ignore]) [@@deriving compare, equal, hash]
  end)

  type t = {root: G.vertex; graph: G.t}

  let create root =
    let graph = G.create () in
    G.add_vertex graph root ;
    {root; graph}


  let add_edge {graph} v1 v2 = G.add_edge graph v1 v2

  (* This iterates vertices with DFS order, but with additional stack'ed accumulation. *)
  let fold {root; graph} ~init ~stack_init ~f =
    let visited = ref AbstractValue.Set.empty in
    let rec visit acc stack ((addr, _) as v) =
      if AbstractValue.Set.mem addr !visited then Ok acc
      else (
        visited := AbstractValue.Set.add addr !visited ;
        let* acc, stack = f acc stack v in
        PulseResult.list_fold (G.succ graph v) ~init:acc ~f:(fun acc v -> visit acc stack v) )
    in
    visit init stack_init root
end

(** Preorder traversal of the tree formed by taint dependencies of [v] in [astate] *)
let gather_taint_dependencies addr_hist0 astate =
  let taint_dependencies = TaintDependencies.create addr_hist0 in
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
    | TakeAddress | Dereference ->
        false
  in
  let rec gather_taint_dependencies_aux ((from_addr, _) as from_addr_hist) =
    if not (AbstractValue.Set.mem from_addr !visited) then (
      visited := AbstractValue.Set.add from_addr !visited ;
      (* Collect propagated taint from attributes *)
      Option.iter (AbductiveDomain.AddressAttributes.get_propagate_taint_from from_addr astate)
        ~f:(fun taints_in ->
          List.iter taints_in ~f:(fun Attribute.{v= to_v; history= to_h} ->
              TaintDependencies.add_edge taint_dependencies from_addr_hist (to_v, to_h) ;
              gather_taint_dependencies_aux (to_v, to_h) ) ) ;
      (* Collect taint from memory [from_addr_hist] points to *)
      AbductiveDomain.Memory.fold_edges from_addr astate ~init:()
        ~f:(fun () (access, (dest_addr, _)) ->
          if should_follow_access access then
            Option.iter (AbductiveDomain.Memory.find_edge_opt dest_addr Dereference astate)
              ~f:(fun deref_dest ->
                TaintDependencies.add_edge taint_dependencies from_addr_hist deref_dest ;
                gather_taint_dependencies_aux deref_dest )
          else () ) )
  in
  gather_taint_dependencies_aux addr_hist0 ;
  taint_dependencies


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


let check_flows_wrt_sink ?(policy_violations_reported = IntSet.empty) path location
    ~sink:(sink_item, sink_trace) ~source astate =
  let source_value, _ = source in
  let source_expr = Decompiler.find source_value astate in
  let mk_reportable_error diagnostic = [ReportableError {astate; diagnostic}] in
  let check_tainted_flows policy_violations_reported sanitizers (v, hist) astate =
    L.d_printfln "Checking that %a is not tainted" AbstractValue.pp v ;
    let sources, new_sanitizers =
      AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers v astate
    in
    let sanitizers = Attribute.TaintSanitizedSet.union sanitizers new_sanitizers in
    Attribute.TaintedSet.fold
      (fun {source; time_trace= source_times; hist= source_hist; intra_procedural_only}
           policy_violations_reported_result ->
        let* policy_violations_reported = policy_violations_reported_result in
        L.d_printfln_escaped ~color:Red "Found source %a, checking policy..." TaintItem.pp source ;
        let potential_policy_violations =
          check_policies ~sink:sink_item ~source ~source_times ~intra_procedural_only ~hist
            ~sanitizers ~location
        in
        let report_policy_violation reported_so_far
            (source_kind, sink_kind, policy_description, violated_policy_id, policy_privacy_effect)
            =
          if IntSet.mem violated_policy_id reported_so_far then Ok reported_so_far
          else
            let flow_kind =
              if TaintConfig.Kind.is_data_flow_only source_kind then Diagnostic.FlowToSink
              else if TaintConfig.Kind.is_data_flow_only sink_kind then Diagnostic.FlowFromSource
              else Diagnostic.TaintedFlow
            in
            Recoverable
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
                     ; policy_privacy_effect } ) )
        in
        PulseResult.list_fold potential_policy_violations ~init:policy_violations_reported
          ~f:report_policy_violation )
      sources (Ok policy_violations_reported)
    |> PulseResult.map ~f:(fun res -> (res, sanitizers))
  in
  L.d_with_indent "check flows wrt sink from %a (%a)" AbstractValue.pp source_value
    DecompilerExpr.pp_with_abstract_value source_expr ~collapsible:true ~f:(fun () ->
      let taint_dependencies = gather_taint_dependencies source astate in
      TaintDependencies.fold taint_dependencies ~init:(policy_violations_reported, astate)
        ~stack_init:Attribute.TaintSanitizedSet.empty
        ~f:(fun (policy_violations_reported, astate) sanitizers ((addr, _) as addr_hist) ->
          let astate =
            AbductiveDomain.AddressAttributes.add_taint_sink path sink_item sink_trace addr astate
          in
          let+ policy_violations_reported, sanitizers =
            check_tainted_flows policy_violations_reported sanitizers addr_hist astate
          in
          ((policy_violations_reported, astate), sanitizers) ) )


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
          let open PulseResult.Let_syntax in
          let rec mark_sinked policy_violations_reported ?access ~(sink : TaintItem.t) v hist astate
              =
            let is_closure = Option.is_some (AddressAttributes.get_closure_proc_name v astate) in
            if AbstractValue.Set.mem v !visited || is_closure then
              Ok (policy_violations_reported, astate)
            else (
              visited := AbstractValue.Set.add v !visited ;
              let sink_value_tuple =
                match access with
                | Some (MemoryAccess.FieldAccess fieldname) ->
                    TaintItem.FieldOf
                      {name= Fieldname.get_field_name fieldname; value_tuple= sink.value_tuple}
                | Some MemoryAccess.Dereference ->
                    TaintItem.PointedToBy {value_tuple= sink.value_tuple}
                | _ ->
                    sink.value_tuple
              in
              let new_sink = {sink with value_tuple= sink_value_tuple} in
              let astate =
                AbductiveDomain.AddressAttributes.add_taint_sink path new_sink sink_trace v astate
              in
              let res =
                check_flows_wrt_sink ~policy_violations_reported path location
                  ~sink:(new_sink, sink_trace) ~source:(v, hist) astate
              in
              AbductiveDomain.Memory.fold_edges v astate ~init:res
                ~f:(fun res (access, (v, hist)) ->
                  match access with
                  | MemoryAccess.FieldAccess fieldname
                    when Fieldname.equal fieldname PulseOperations.ModeledField.internal_string
                         || Fieldname.equal fieldname
                              PulseOperations.ModeledField.internal_ref_count ->
                      res
                  | _ ->
                      let* policy_violations_reported, astate = res in
                      mark_sinked policy_violations_reported ~access ~sink:new_sink v hist astate )
              )
          in
          let+ _, astate = mark_sinked IntSet.empty ~sink v history astate in
          astate )
  in
  L.d_with_indent "taint_sinks" ~f:aux


let propagate_to path location value_origin values call astate =
  let v = ValueOrigin.value value_origin in
  let path_condition = astate.AbductiveDomain.path_condition in
  if PulseFormula.is_known_zero path_condition v then
    AbductiveDomain.AddressAttributes.remove_taint_attrs v astate
  else
    let astate =
      if not (List.is_empty values) then
        AbductiveDomain.AddressAttributes.add_one v
          (PropagateTaintFrom
             (List.map values ~f:(fun {FuncArg.arg_payload= value_origin} ->
                  let v, history = ValueOrigin.addr_hist value_origin in
                  Attribute.{v; history} ) ) )
          astate
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


let taint_propagators path location proc_name actuals tainted astate =
  let aux () =
    List.fold tainted ~init:astate ~f:(fun astate TaintItemMatcher.{value_origin} ->
        let v = ValueOrigin.value value_origin in
        let other_actuals =
          List.filter actuals ~f:(fun {FuncArg.arg_payload= actual_value_origin} ->
              let actual = ValueOrigin.value actual_value_origin in
              not (AbstractValue.equal v actual) )
        in
        propagate_to path location value_origin other_actuals (Call proc_name) astate )
  in
  L.d_with_indent "taint_propagators" ~f:aux


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
        propagate_to path location return_value_origin actuals call astate
    | _ ->
        astate
  in
  let astate =
    match actuals with
    | {FuncArg.arg_payload= this} :: other_actuals when propagate_to_receiver ->
        propagate_to path location this other_actuals call astate
    | _ ->
        astate
  in
  match List.rev actuals with
  | {FuncArg.arg_payload= last} :: other_actuals when propagate_to_last_actual ->
      propagate_to path location last other_actuals call astate
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
      {name_regex= Str.regexp "std::basic_string<.*>::basic_string"; exclude_in= None} ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint tenv ?proc_attributes proc_name =
  Option.exists proc_attributes ~f:(fun attrs -> attrs.ProcAttributes.is_cpp_implicit)
  && Procname.is_constructor proc_name
  || TaintItemMatcher.procedure_matches_any tenv proc_name proc_attributes
       pulse_models_to_treat_as_unknown_for_taint


let call tenv path location return ~call_was_unknown (call : _ Either.t)
    (actuals : ValueOrigin.t FuncArg.t list) astate =
  L.d_with_indent "taint operations -> call" ~f:(fun () ->
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
            (* This is a bit of a hack to achieve the following: be able to specify taint policies
               where the sink is 'any procedure except <these>'. <these> need to be marked as
               propagators. *)
            if List.is_empty propagator_matches then
              let astate, tainted = match_call sink_procedure_matchers astate in
              taint_sinks path location tainted astate
            else Ok astate
          in
          let astate, should_propagate_for_unknown =
            let new_state =
              taint_propagators path location proc_name actuals propagator_matches astate
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
