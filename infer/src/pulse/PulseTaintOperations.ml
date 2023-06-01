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
module TaintItemMatcher = PulseTaintItemMatcher

let fill_data_flow_kinds_from_config () =
  let open TaintConfig in
  Config.pulse_taint_config.data_flow_kinds
  |> List.iter ~f:(fun kind -> Kind.of_string kind |> Kind.mark_data_flow_only)


let fill_policies_from_config () =
  let open TaintConfig in
  Config.pulse_taint_config.policies
  |> List.iter
       ~f:(fun {Pulse_config_t.short_description= description; taint_flows; privacy_effect} ->
         let policy_id = SinkPolicy.next_policy_id () in
         List.iter taint_flows ~f:(fun {Pulse_config_t.source_kinds; sanitizer_kinds; sink_kinds} ->
             let source_kinds = List.map source_kinds ~f:Kind.of_string in
             let sanitizer_kinds = List.map sanitizer_kinds ~f:Kind.of_string in
             List.iter sink_kinds ~f:(fun sink_kind_s ->
                 let sink_kind = Kind.of_string sink_kind_s in
                 let flow =
                   {SinkPolicy.source_kinds; sanitizer_kinds; description; policy_id; privacy_effect}
                 in
                 Hashtbl.update SinkPolicy.sink_policies sink_kind ~f:(function
                   | None ->
                       [flow]
                   | Some flows ->
                       flow :: flows ) ) ) )


let () =
  let open TaintConfig in
  Hashtbl.add SinkPolicy.sink_policies ~key:Kind.simple_kind
    ~data:
      [ { SinkPolicy.description=
            "Built-in Simple taint kind, matching any Simple source with any Simple sink except if \
             any Simple sanitizer is in the way"
        ; source_kinds= [Kind.simple_kind]
        ; sanitizer_kinds= [Kind.simple_kind]
        ; policy_id= SinkPolicy.next_policy_id ()
        ; privacy_effect= None } ]
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
  (procedure_matchers, block_matchers, field_matchers)


let allocation_sources, source_procedure_matchers, source_block_matchers, source_field_matchers =
  let option_name = "--pulse-taint-sources" in
  let all_source_matchers =
    TaintItemMatcher.matcher_of_config ~default_taint_target:`ReturnValue ~option_name
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
  let procedure_matchers, block_matchers, field_matchers =
    get_procedure_field_matchers source_matchers ~field_matchers_allowed:true
      ~block_matchers_allowed:true ~option_name
  in
  (allocation_sources, procedure_matchers, block_matchers, field_matchers)


let sink_procedure_matchers, sink_field_matchers =
  let option_name = "--pulse-taint-sinks" in
  let sink_matchers =
    TaintItemMatcher.matcher_of_config ~default_taint_target:`AllArguments ~option_name
      Config.pulse_taint_config.sinks
  in
  let sink_procedure_matchers, _, sink_field_matchers =
    get_procedure_field_matchers sink_matchers ~field_matchers_allowed:true
      ~block_matchers_allowed:false ~option_name
  in
  (sink_procedure_matchers, sink_field_matchers)


let sanitizer_matchers =
  let option_name = "--pulse-taint-sanitizer" in
  let sink_matchers =
    TaintItemMatcher.matcher_of_config ~default_taint_target:`AllArguments ~option_name
      Config.pulse_taint_config.sanitizers
  in
  let procedure_matchers, _, _ =
    get_procedure_field_matchers sink_matchers ~field_matchers_allowed:false
      ~block_matchers_allowed:false ~option_name
  in
  procedure_matchers


let propagator_matchers =
  let option_name = "--pulse-taint-propagators" in
  let propagator_matchers =
    TaintItemMatcher.matcher_of_config ~default_taint_target:`ReturnValue ~option_name
      Config.pulse_taint_config.propagators
  in
  let procedure_matchers, _, _ =
    get_procedure_field_matchers propagator_matchers ~field_matchers_allowed:false
      ~block_matchers_allowed:false ~option_name
  in
  procedure_matchers


let log_taint_config () =
  let open TaintConfig in
  L.debug Analysis Verbose "@\nSink policies:@\n%a@." SinkPolicy.pp_sink_policies
    SinkPolicy.sink_policies ;
  L.debug Analysis Verbose "Procedure source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    source_procedure_matchers ;
  L.debug Analysis Verbose "Block source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    source_block_matchers ;
  L.debug Analysis Verbose "Field source matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    source_field_matchers ;
  L.debug Analysis Verbose "Sink procedure matchers:@\n %a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    sink_procedure_matchers ;
  L.debug Analysis Verbose "Sink field matchers:@\n %a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_field_unit)
    sink_field_matchers ;
  L.debug Analysis Verbose "Sanitizer matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    sanitizer_matchers ;
  L.debug Analysis Verbose "Propagator matchers:@\n%a@\n@."
    (Pp.seq ~sep:"\n" TaintConfig.Unit.pp_procedure_unit)
    propagator_matchers


let taint_allocation tenv path location ~typ_desc ~alloc_desc ~allocator v astate =
  (* Micro-optimisation: do not convert types to strings unless necessary *)
  if List.is_empty allocation_sources then astate
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
            let astate =
              List.fold matching_allocations ~init:astate ~f:(fun astate (_, kinds) ->
                  let source =
                    let proc_name = Procname.from_string_c_fun alloc_desc in
                    let value = TaintItem.TaintProcedure proc_name in
                    let origin = TaintItem.Allocation {typ= type_name} in
                    {TaintItem.kinds; value; origin}
                  in
                  let hist =
                    ValueHistory.singleton
                      (TaintSource (source, location, path.PathContext.timestamp))
                  in
                  let tainted =
                    let time_trace = Timestamp.trace0 path.PathContext.timestamp in
                    {Attribute.Tainted.source; hist; time_trace; intra_procedural_only= false}
                  in
                  AbductiveDomain.AddressAttributes.add_one v
                    (Tainted (Attribute.TaintedSet.singleton tainted))
                    astate )
            in
            Some astate
        in
        L.d_printfln "Checking allocation at %a for taint matching %a" Location.pp location
          Typ.Name.pp class_name ;
        let astate_opt = PatternMatch.supertype_find_map_opt tenv check_type_name class_name in
        Option.value astate_opt ~default:astate
    | _ ->
        astate


let taint_and_explore ~taint v astate =
  let rec aux (astate, visited) v =
    if AbstractValue.Set.mem v visited then (astate, visited)
    else
      let visited = AbstractValue.Set.add v visited in
      let astate = taint v astate in
      AbductiveDomain.Memory.fold_edges v astate ~init:(astate, visited)
        ~f:(fun astate_visited (_, (v, _)) -> aux astate_visited v)
  in
  aux (astate, AbstractValue.Set.empty) v |> fst


let taint_sources tenv path location ~intra_procedural_only return ~has_added_return_param
    potential_taint_value actuals astate =
  let astate, tainted =
    TaintItemMatcher.get_tainted tenv path location ~procedure_matchers:source_procedure_matchers
      ~block_matchers:source_block_matchers ~field_matchers:source_field_matchers return
      ~has_added_return_param potential_taint_value actuals astate
  in
  List.fold tainted ~init:astate ~f:(fun astate (source, ((v, _), _, _)) ->
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
      taint_and_explore v astate ~taint:(fun v astate ->
          let path_condition = astate.AbductiveDomain.path_condition in
          if not (PulseFormula.is_known_zero path_condition v) then
            AbductiveDomain.AddressAttributes.add_one v
              (Tainted (Attribute.TaintedSet.singleton tainted))
              astate
          else (
            L.d_printfln
              "Not adding Tainted attribute to the value %a because it is known to be zero"
              AbstractValue.pp v ;
            astate ) ) )


let taint_sanitizers tenv path return ~has_added_return_param ~location potential_taint_value
    actuals astate =
  let astate, tainted =
    TaintItemMatcher.get_tainted tenv path location ~procedure_matchers:sanitizer_matchers
      ~block_matchers:[] ~field_matchers:[] return ~has_added_return_param potential_taint_value
      actuals astate
  in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (sanitizer, ((v, history), _, _)) ->
        let trace = Trace.Immediate {location; history} in
        let taint_sanitized =
          Attribute.TaintSanitized.
            {sanitizer; time_trace= Timestamp.trace0 path.PathContext.timestamp; trace}
        in
        taint_and_explore v astate ~taint:(fun v astate ->
            AbductiveDomain.AddressAttributes.add_one v
              (TaintSanitized (Attribute.TaintSanitizedSet.singleton taint_sanitized))
              astate ) )
  in
  astate


let check_policies ~sink ~source ~source_times ~sanitizers =
  let open TaintConfig in
  List.fold sink.TaintItem.kinds ~init:[] ~f:(fun acc sink_kind ->
      let policies = Hashtbl.find_exn SinkPolicy.sink_policies sink_kind in
      List.fold policies ~init:acc
        ~f:(fun
             acc
             {SinkPolicy.source_kinds; sanitizer_kinds; description; policy_id; privacy_effect}
           ->
          match
            List.find source.TaintItem.kinds ~f:(fun source_kind ->
                (* We should ignore flows between data-flow-only sources and data-flow-only sinks *)
                (not (Kind.is_data_flow_only source_kind && Kind.is_data_flow_only sink_kind))
                && List.mem ~equal:Kind.equal source_kinds source_kind )
          with
          | None ->
              acc
          | Some suspicious_source ->
              L.d_printfln ~color:Red "TAINTED: %a -> %a" Kind.pp suspicious_source Kind.pp
                sink_kind ;
              let matching_sanitizers =
                Attribute.TaintSanitizedSet.filter
                  (fun {sanitizer; time_trace= sanitizer_times} ->
                    Timestamp.compare_trace source_times sanitizer_times <= 0
                    && List.exists sanitizer.TaintItem.kinds ~f:(fun sanitizer_kind ->
                           List.mem ~equal:Kind.equal sanitizer_kinds sanitizer_kind ) )
                  sanitizers
              in
              if Attribute.TaintSanitizedSet.is_empty matching_sanitizers then
                (suspicious_source, sink_kind, description, policy_id, privacy_effect) :: acc
              else (
                L.d_printfln ~color:Green "...but sanitized by %a" Attribute.TaintSanitizedSet.pp
                  matching_sanitizers ;
                acc ) ) )


module TaintDependencies = struct
  module G = Graph.Imperative.Digraph.Concrete (struct
    type t = AbstractValue.t [@@deriving compare, equal, hash]
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
    let rec visit acc stack v =
      if AbstractValue.Set.mem v !visited then Ok acc
      else (
        visited := AbstractValue.Set.add v !visited ;
        let* acc, stack = f acc stack v in
        PulseResult.list_fold (G.succ graph v) ~init:acc ~f:(fun acc v -> visit acc stack v) )
    in
    visit init stack_init root
end

(** Preorder traversal of the tree formed by taint dependencies of [v] in [astate] *)
let gather_taint_dependencies v astate =
  let taint_dependencies = TaintDependencies.create v in
  let visited = ref AbstractValue.Set.empty in
  let rec gather_taint_dependencies_aux v =
    if not (AbstractValue.Set.mem v !visited) then (
      visited := AbstractValue.Set.add v !visited ;
      Option.iter (AbductiveDomain.AddressAttributes.get_propagate_taint_from v astate)
        ~f:(fun taints_in ->
          List.iter taints_in ~f:(fun {Attribute.v= v'} ->
              TaintDependencies.add_edge taint_dependencies v v' ;
              gather_taint_dependencies_aux v' ) ) ;
      (* if the value is an array we propagate the check to the array elements *)
      (* NOTE: we could do the same for field accesses or really all accesses if we want the taint
          analysis to consider that the insides of objects are tainted whenever the object is. This
          might not be a very efficient way to do this though? *)
      AbductiveDomain.Memory.fold_edges v astate ~init:() ~f:(fun () (access, (dest, _hist)) ->
          match access with
          | ArrayAccess _ ->
              Option.iter (AbductiveDomain.Memory.find_edge_opt dest Dereference astate)
                ~f:(fun (dest_value, _) ->
                  TaintDependencies.add_edge taint_dependencies v dest_value )
          | FieldAccess _ | TakeAddress | Dereference ->
              () ) )
  in
  gather_taint_dependencies_aux v ;
  taint_dependencies


let check_flows_wrt_sink ?(policy_violations_reported = IntSet.empty) path location
    (sink, sink_trace) v astate =
  let source_expr = Decompiler.find v astate in
  let mk_reportable_error diagnostic = [ReportableError {astate; diagnostic}] in
  let check_tainted_flows policy_violations_reported sanitizers v astate =
    L.d_printfln "Checking that %a is not tainted" AbstractValue.pp v ;
    let sources, new_sanitizers =
      AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers v astate
    in
    let sanitizers = Attribute.TaintSanitizedSet.union sanitizers new_sanitizers in
    Attribute.TaintedSet.fold
      (fun {source; time_trace= source_times; hist= source_hist} policy_violations_reported_result ->
        let* policy_violations_reported = policy_violations_reported_result in
        L.d_printfln_escaped ~color:Red "Found source %a, checking policy..." TaintItem.pp source ;
        let potential_policy_violations = check_policies ~sink ~source ~source_times ~sanitizers in
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
                     ; sink= ({sink with kinds= [sink_kind]}, sink_trace)
                     ; flow_kind
                     ; policy_description
                     ; policy_privacy_effect } ) )
        in
        PulseResult.list_fold potential_policy_violations ~init:policy_violations_reported
          ~f:report_policy_violation )
      sources (Ok policy_violations_reported)
    |> PulseResult.map ~f:(fun res -> (res, sanitizers))
  in
  let taint_dependencies = gather_taint_dependencies v astate in
  TaintDependencies.fold taint_dependencies ~init:(policy_violations_reported, astate)
    ~stack_init:Attribute.TaintSanitizedSet.empty
    ~f:(fun (policy_violations_reported, astate) sanitizers v ->
      let astate = AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate in
      let+ policy_violations_reported, sanitizers =
        check_tainted_flows policy_violations_reported sanitizers v astate
      in
      ((policy_violations_reported, astate), sanitizers) )


let should_ignore_all_flows_to potential_taint_value =
  match potential_taint_value with
  | TaintItem.TaintProcedure proc_name ->
      Procname.is_objc_dealloc proc_name || BuiltinDecl.is_declared proc_name
  | _ ->
      false


let taint_sinks tenv path location return ~has_added_return_param potential_taint_value actuals
    astate =
  let astate, tainted =
    TaintItemMatcher.get_tainted tenv path location ~procedure_matchers:sink_procedure_matchers
      ~block_matchers:[] ~field_matchers:sink_field_matchers return ~has_added_return_param
      potential_taint_value actuals astate
  in
  PulseResult.list_fold tainted ~init:astate ~f:(fun astate (sink, ((v, history), _typ, _)) ->
      if should_ignore_all_flows_to potential_taint_value then Ok astate
      else
        let sink_trace = Trace.Immediate {location; history} in
        let visited = ref AbstractValue.Set.empty in
        let open PulseResult.Let_syntax in
        let rec mark_sinked policy_violations_reported v astate =
          if AbstractValue.Set.mem v !visited then Ok (policy_violations_reported, astate)
          else (
            visited := AbstractValue.Set.add v !visited ;
            let astate =
              AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate
            in
            let res =
              check_flows_wrt_sink ~policy_violations_reported path location (sink, sink_trace) v
                astate
            in
            AbductiveDomain.Memory.fold_edges v astate ~init:res ~f:(fun res (access, (v, _)) ->
                match access with
                | HilExp.Access.FieldAccess fieldname
                  when Fieldname.equal fieldname PulseOperations.ModeledField.internal_string
                       || Fieldname.equal fieldname PulseOperations.ModeledField.internal_ref_count
                  ->
                    res
                | _ ->
                    let* policy_violations_reported, astate = res in
                    mark_sinked policy_violations_reported v astate ) )
        in
        let+ _, astate = mark_sinked IntSet.empty v astate in
        astate )


let propagate_to path location v values call astate =
  let path_condition = astate.AbductiveDomain.path_condition in
  if PulseFormula.is_known_zero path_condition v then
    AbductiveDomain.AddressAttributes.remove_taint_attrs v astate
  else
    let astate =
      if not (List.is_empty values) then
        AbductiveDomain.AddressAttributes.add_one v
          (PropagateTaintFrom
             (List.map values ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= v, _hist} ->
                  {Attribute.v} ) ) )
          astate
      else astate
    in
    taint_and_explore v astate ~taint:(fun v astate ->
        List.fold values ~init:astate
          ~f:(fun astate {ProcnameDispatcher.Call.FuncArg.arg_payload= actual, _hist} ->
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
              AbductiveDomain.AddressAttributes.add_one v (Tainted tainted) astate
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


let taint_propagators tenv path location return ~has_added_return_param proc_name actuals astate =
  let astate, tainted =
    let potential_taint_value = TaintItem.TaintProcedure proc_name in
    TaintItemMatcher.get_tainted tenv path location ~procedure_matchers:propagator_matchers
      ~block_matchers:[] ~field_matchers:[] return ~has_added_return_param potential_taint_value
      actuals astate
  in
  List.fold tainted ~init:astate ~f:(fun astate (_propagator, ((v, _history), _, _)) ->
      let other_actuals =
        List.filter actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= actual, _hist} ->
            not (AbstractValue.equal v actual) )
      in
      propagate_to path location v other_actuals (Call proc_name) astate )


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
    , {ProcnameDispatcher.Call.FuncArg.typ= {desc= Tptr ({desc= Tstruct receiver_typename}, _)}}
      :: _ )
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
    | {ProcnameDispatcher.Call.FuncArg.arg_payload= this, _hist} :: _
      when is_cpp_assignment_operator proc_name_opt ->
        L.d_printfln "remove taint info of %a for cpp operator=" AbstractValue.pp this ;
        AddressAttributes.remove_taint_attrs this astate
    | _ ->
        astate
  in
  let astate =
    match Stack.find_opt return astate with
    | Some (return_value, _) when propagate_to_return ->
        propagate_to path location return_value actuals call astate
    | _ ->
        astate
  in
  let astate =
    match actuals with
    | {ProcnameDispatcher.Call.FuncArg.arg_payload= this, _hist} :: other_actuals
      when propagate_to_receiver ->
        propagate_to path location this other_actuals call astate
    | _ ->
        astate
  in
  match List.rev actuals with
  | {ProcnameDispatcher.Call.FuncArg.arg_payload= last, _hist} :: other_actuals
    when propagate_to_last_actual ->
      propagate_to path location last other_actuals call astate
  | _ ->
      astate


(* some pulse models are not a faithful reflection of the behaviour and should be treated as unknown
   wrt taint behaviour (i.e. propagate taint like an unknown function would) *)
let pulse_models_to_treat_as_unknown_for_taint =
  (* HACK: make a list of matchers just to reuse the matching code below *)
  let dummy_matcher_of_procedure_matcher procedure_matcher =
    let open TaintConfig in
    {Unit.procedure_matcher; arguments= []; kinds= []; procedure_target= ReturnValue}
  in
  [ ClassAndMethodNames
      { class_names= ["java.lang.StringBuilder"]
      ; method_names= ["append"; "delete"; "replace"; "setLength"] }
  ; ProcedureNameRegex {name_regex= Str.regexp "std::basic_string<.*>::basic_string"} ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint tenv proc_name =
  (* HACK: we already have a function for matching procedure names so just re-use it even though we
     don't need its full power *)
  Option.exists (IRAttributes.load proc_name) ~f:(fun attrs -> attrs.ProcAttributes.is_cpp_implicit)
  && Procname.is_constructor proc_name
  || TaintItemMatcher.procedure_matches tenv pulse_models_to_treat_as_unknown_for_taint proc_name []
     |> List.is_empty |> not


let call tenv path location return ~call_was_unknown (call : _ Either.t) actuals astate =
  match call with
  | First call_exp ->
      if call_was_unknown then
        Ok
          (propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param:false
             (SkippedUnknownCall call_exp) None actuals astate )
      else Ok astate
  | Second proc_name ->
      let potential_taint_value = TaintItem.TaintProcedure proc_name in
      let call_was_unknown = call_was_unknown || should_treat_as_unknown_for_taint tenv proc_name in
      let has_added_return_param =
        match IRAttributes.load proc_name with
        | Some attrs when attrs.ProcAttributes.has_added_return_param ->
            true
        | _ ->
            false
      in
      let astate =
        taint_sanitizers tenv path (Some return) ~has_added_return_param ~location
          potential_taint_value actuals astate
      in
      let astate =
        taint_sources tenv path location ~intra_procedural_only:false (Some return)
          ~has_added_return_param potential_taint_value actuals astate
      in
      let+ astate =
        taint_sinks tenv path location (Some return) ~has_added_return_param potential_taint_value
          actuals astate
      in
      let astate, call_was_unknown =
        let new_astate =
          taint_propagators tenv path location (Some return) ~has_added_return_param proc_name
            actuals astate
        in
        (new_astate, call_was_unknown && phys_equal astate new_astate)
      in
      if call_was_unknown then
        propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param
          (SkippedKnownCall proc_name) (Some proc_name) actuals astate
      else astate


let store tenv path location ~lhs:lhs_exp ~rhs:(rhs_exp, rhs_addr, typ) astate =
  match lhs_exp with
  | Exp.Lfield (_, field_name, _) ->
      let potential_taint_value = TaintItem.TaintField field_name in
      let rhs = {ProcnameDispatcher.Call.FuncArg.exp= rhs_exp; typ; arg_payload= rhs_addr} in
      taint_sinks tenv path location None ~has_added_return_param:false potential_taint_value [rhs]
        astate
  | _ ->
      Ok astate


let load procname tenv path location ~lhs:(lhs_id, typ) ~rhs:rhs_exp astate =
  match rhs_exp with
  | Exp.Lfield (_, field_name, _) when not (Procname.is_objc_dealloc procname) -> (
      let potential_taint_value = TaintItem.TaintField field_name in
      let lhs_exp = Exp.Var lhs_id in
      let result = PulseOperations.eval path NoAccess location lhs_exp astate in
      match PulseOperationResult.sat_ok result with
      | Some (astate, lhs_addr_hist) ->
          let lhs =
            {ProcnameDispatcher.Call.FuncArg.exp= lhs_exp; typ; arg_payload= lhs_addr_hist}
          in
          let astate =
            taint_sources tenv path location ~intra_procedural_only:false None
              ~has_added_return_param:false potential_taint_value [lhs] astate
          in
          taint_sinks tenv path location None ~has_added_return_param:false potential_taint_value
            [lhs] astate
      | None ->
          L.internal_error
            "could not add taint to the exp %a, got an error or an unsat state starting from %a"
            Exp.pp lhs_exp AbductiveDomain.pp astate ;
          Ok astate )
  | _ ->
      Ok astate


let taint_initial tenv proc_name (proc_attrs : ProcAttributes.t) astate0 =
  let result =
    let++ astate, rev_actuals =
      List.fold
        (ProcAttributes.get_pvar_formals proc_attrs)
        ~init:(Sat (Ok (astate0, [])))
        ~f:(fun result (pvar, typ) ->
          let** astate, rev_actuals = result in
          let++ astate, actual_value =
            PulseOperations.eval_deref PathContext.initial proc_attrs.loc (Lvar pvar) astate
          in
          ( astate
          , {ProcnameDispatcher.Call.FuncArg.exp= Lvar pvar; typ; arg_payload= actual_value}
            :: rev_actuals ) )
    in
    let block_passed_to =
      Option.map
        ~f:(fun ({passed_to} : ProcAttributes.block_as_arg_attributes) -> passed_to)
        proc_attrs.ProcAttributes.block_as_arg_attributes
    in
    let potential_taint_value =
      match block_passed_to with
      | Some proc_name ->
          TaintItem.TaintBlockPassedTo proc_name
      | None ->
          TaintItem.TaintProcedure proc_name
    in
    taint_sources tenv PathContext.initial proc_attrs.loc ~intra_procedural_only:true None
      ~has_added_return_param:false potential_taint_value (List.rev rev_actuals) astate
  in
  match PulseOperationResult.sat_ok result with
  | Some astate_tainted ->
      astate_tainted
  | None ->
      L.internal_error
        "could not add taint to the initial state for %a, got an error or an unsat state starting \
         from %a"
        Procname.pp proc_name AbductiveDomain.pp astate0 ;
      astate0
