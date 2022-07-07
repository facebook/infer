(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module IRAttributes = Attributes
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

let type_matches tenv actual_typ types =
  (* TODO: [Typ.Name.name] may not be the most intuitive representation of types here, also
     could be slow to generate. Maybe have more fine-grained matching for primitive types vs
     class names (with separate package names to avoid string building) *)
  match actual_typ with
  | {Typ.desc= Tptr ({desc= Tstruct actual_name}, _)} ->
      PatternMatch.supertype_exists tenv
        (fun type_name _struct ->
          List.exists types ~f:(fun typ ->
              String.is_substring ~substring:typ (Typ.Name.name type_name) ) )
        actual_name
  | _ ->
      false


let taint_target_matches tenv taint_target actual_index actual_typ =
  match taint_target with
  | `AllArguments ->
      true
  | `ArgumentPositions indices ->
      List.mem ~equal:Int.equal indices actual_index
  | `AllArgumentsButPositions indices ->
      not (List.mem ~equal:Int.equal indices actual_index)
  | `ArgumentsMatchingTypes types ->
      type_matches tenv actual_typ types


type procedure_matcher =
  | ProcedureName of {name: string}
  | ProcedureNameRegex of {name_regex: Str.regexp}
  | ClassAndMethodNames of {class_names: string list; method_names: string list}
  | OverridesOfClassWithAnnotation of {annotation: string}

type matcher =
  { procedure_matcher: procedure_matcher
  ; arguments: Pulse_config_t.argument_constraint list
  ; kinds: Taint.Kind.t list
  ; target: Pulse_config_t.taint_target }

type sink_policy =
  {source_kinds: Taint.Kind.t list; sanitizer_kinds: Taint.Kind.t list; description: string}
[@@deriving equal]

let sink_policies = Hashtbl.create (module Taint.Kind)

let fill_policies_from_config () =
  Config.pulse_taint_config.policies
  |> List.iter ~f:(function {Pulse_config_j.short_description= description; taint_flows} ->
         List.iter taint_flows ~f:(fun {Pulse_config_j.source_kinds; sanitizer_kinds; sink_kinds} ->
             let source_kinds = List.map source_kinds ~f:Taint.Kind.of_string in
             let sanitizer_kinds = List.map sanitizer_kinds ~f:Taint.Kind.of_string in
             List.iter sink_kinds ~f:(fun sink_kind_s ->
                 let sink_kind = Taint.Kind.of_string sink_kind_s in
                 let flow = {source_kinds; sanitizer_kinds; description} in
                 Hashtbl.update sink_policies sink_kind ~f:(function
                   | None ->
                       [flow]
                   | Some flows ->
                       flow :: flows ) ) ) )


let simple_kind = Taint.Kind.of_string "Simple"

let () =
  Hashtbl.add sink_policies ~key:simple_kind
    ~data:
      [ { description=
            "Built-in Simple taint kind, matching any Simple source with any Simple sink except if \
             any Simple sanitizer is in the way"
        ; source_kinds= [simple_kind]
        ; sanitizer_kinds= [simple_kind] } ]
  |> ignore ;
  fill_policies_from_config ()


let kinds_of_strings_opt = function
  | None ->
      [simple_kind]
  | Some kinds ->
      List.map kinds ~f:Taint.Kind.of_string


let matcher_of_config ~default_taint_target ~option_name matchers =
  List.map matchers ~f:(fun (matcher : Pulse_config_j.matcher) ->
      let procedure_matcher =
        match matcher with
        | { procedure= Some name
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None } ->
            ProcedureName {name}
        | { procedure= None
          ; procedure_regex= Some name_regex
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None } ->
            ProcedureNameRegex {name_regex= Str.regexp name_regex}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= Some class_names
          ; method_names= Some method_names
          ; overrides_of_class_with_annotation= None } ->
            ClassAndMethodNames {class_names; method_names}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= Some annotation } ->
            OverridesOfClassWithAnnotation {annotation}
        | _ ->
            L.die UserError
              "When parsing option %s: Unexpected JSON format: Exactly one of \"procedure\", \
               \"procedure_regex\" must be provided, or else \"class_names\" and \"method_names\" \
               must be provided, or else \"overrides_of_class_with_annotation\", but got \
               \"procedure\": %a, \"procedure_regex\": %a, \"class_names\": %a, \"method_names\": \
               %a, \"overrides_of_class_with_annotation\": %a"
              option_name (Pp.option F.pp_print_string) matcher.procedure
              (Pp.option F.pp_print_string) matcher.procedure_regex
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.class_names
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.method_names (Pp.option F.pp_print_string)
              matcher.overrides_of_class_with_annotation
      in
      { procedure_matcher
      ; arguments= matcher.argument_constraints
      ; kinds= kinds_of_strings_opt matcher.kinds
      ; target= Option.value ~default:default_taint_target matcher.taint_target } )


let source_matchers =
  matcher_of_config ~default_taint_target:`ReturnValue ~option_name:"--pulse-taint-sources"
    Config.pulse_taint_config.sources


let sink_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sinks"
    Config.pulse_taint_config.sinks


let sanitizer_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sanitizers"
    Config.pulse_taint_config.sanitizers


let procedure_matches tenv matchers proc_name actuals =
  List.find_map matchers ~f:(fun matcher ->
      let procedure_name_matches =
        match matcher.procedure_matcher with
        | ProcedureName {name} ->
            let proc_name_s = F.asprintf "%a" Procname.pp_unique_id proc_name in
            String.is_substring ~substring:name proc_name_s
        | ProcedureNameRegex {name_regex} -> (
            let proc_name_s = F.asprintf "%a" Procname.pp_unique_id proc_name in
            L.d_printfln "Matching regex wrt %s" proc_name_s ;
            match Str.search_forward name_regex proc_name_s 0 with
            | _ ->
                true
            | exception Caml.Not_found ->
                false )
        | ClassAndMethodNames {class_names; method_names} ->
            Option.exists (Procname.get_class_type_name proc_name) ~f:(fun class_name ->
                PatternMatch.supertype_exists tenv
                  (fun class_name _ ->
                    List.mem ~equal:String.equal class_names (Typ.Name.name class_name) )
                  class_name )
            && List.mem ~equal:String.equal method_names (Procname.get_method proc_name)
        | OverridesOfClassWithAnnotation {annotation} ->
            Option.exists (Procname.get_class_type_name proc_name) ~f:(fun procedure_class_name ->
                let method_name = Procname.get_method proc_name in
                PatternMatch.supertype_exists tenv
                  (fun class_name _ ->
                    Option.exists (Tenv.lookup tenv class_name) ~f:(fun procedure_superclass_type ->
                        Annotations.struct_typ_has_annot procedure_superclass_type
                          (fun annot_item -> Annotations.ia_ends_with annot_item annotation)
                        && PatternMatch.override_exists ~check_current_type:false
                             (fun superclass_pname ->
                               String.equal (Procname.get_method superclass_pname) method_name )
                             tenv proc_name ) )
                  procedure_class_name )
      in
      if procedure_name_matches then
        let actuals_match =
          List.for_all matcher.arguments ~f:(fun {Pulse_config_t.index; type_matches= types} ->
              List.nth actuals index
              |> Option.exists ~f:(fun {ProcnameDispatcher.Call.FuncArg.typ} ->
                     type_matches tenv typ types ) )
        in
        Option.some_if actuals_match matcher
      else None )


let get_tainted tenv matchers return_opt ~has_added_return_param proc_name actuals astate =
  match procedure_matches tenv matchers proc_name actuals with
  | None ->
      []
  | Some matcher -> (
      L.d_printfln "taint matches" ;
      let actuals =
        List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ} ->
            (arg_payload, typ) )
      in
      match matcher.target with
      | `ReturnValue -> (
          L.d_printf "matching return value... " ;
          match return_opt with
          | None ->
              L.d_printfln "no match" ;
              []
          | Some (return, return_typ) -> (
              L.d_printfln "match! tainting return value" ;
              let return_as_actual = if has_added_return_param then List.last actuals else None in
              match return_as_actual with
              | Some actual ->
                  let taint = {Taint.proc_name; origin= ReturnValue; kinds= matcher.kinds} in
                  [(taint, actual)]
              | None ->
                  let return = Var.of_id return in
                  Stack.find_opt return astate
                  |> Option.fold ~init:[] ~f:(fun tainted return_value ->
                         let taint = {Taint.proc_name; origin= ReturnValue; kinds= matcher.kinds} in
                         (taint, (return_value, return_typ)) :: tainted ) ) )
      | ( `AllArguments
        | `ArgumentPositions _
        | `AllArgumentsButPositions _
        | `ArgumentsMatchingTypes _ ) as taint_target ->
          L.d_printf "matching actuals... " ;
          List.foldi actuals ~init:[] ~f:(fun i tainted ((_, actual_typ) as actual_hist_and_typ) ->
              if taint_target_matches tenv taint_target i actual_typ then (
                L.d_printfln "match! tainting actual #%d with type %a" i (Typ.pp_full Pp.text)
                  actual_typ ;
                let taint = {Taint.proc_name; origin= Argument {index= i}; kinds= matcher.kinds} in
                (taint, actual_hist_and_typ) :: tainted )
              else (
                L.d_printfln "no match for #%d with type %a" i (Typ.pp_full Pp.text) actual_typ ;
                tainted ) ) )


let taint_sources tenv path location ~intra_procedural_only return ~has_added_return_param proc_name
    actuals astate =
  let tainted =
    get_tainted tenv source_matchers return ~has_added_return_param proc_name actuals astate
  in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (source, ((v, _), _)) ->
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
        let visited = ref AbstractValue.Set.empty in
        let rec mark_tainted v astate =
          if AbstractValue.Set.mem v !visited then astate
          else (
            visited := AbstractValue.Set.add v !visited ;
            let astate =
              AbductiveDomain.AddressAttributes.add_one v
                (Tainted (Attribute.TaintedSet.singleton tainted))
                astate
            in
            match AbductiveDomain.Memory.find_opt v astate with
            | None ->
                astate
            | Some edges ->
                BaseMemory.Edges.fold edges ~init:astate ~f:(fun astate (_, (v, _)) ->
                    mark_tainted v astate ) )
        in
        mark_tainted v astate )
  in
  (astate, not (List.is_empty tainted))


let taint_sanitizers tenv path return ~has_added_return_param ~location proc_name actuals astate =
  let tainted =
    get_tainted tenv sanitizer_matchers return ~has_added_return_param proc_name actuals astate
  in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (sanitizer, ((v, history), _)) ->
        let trace = Trace.Immediate {location; history} in
        let taint_sanitized =
          Attribute.TaintSanitized.
            {sanitizer; time_trace= Timestamp.trace0 path.PathContext.timestamp; trace}
        in
        AbductiveDomain.AddressAttributes.add_one v
          (TaintSanitized (Attribute.TaintSanitizedSet.singleton taint_sanitized))
          astate )
  in
  astate


let check_policies ~sink ~source ~source_times ~sanitizers =
  List.fold sink.Taint.kinds ~init:[] ~f:(fun acc sink_kind ->
      let policies = Hashtbl.find_exn sink_policies sink_kind in
      List.fold policies ~init:acc ~f:(fun acc ({source_kinds; sanitizer_kinds} as policy) ->
          match
            List.find source.Taint.kinds ~f:(fun source_kind ->
                List.mem ~equal:Taint.Kind.equal source_kinds source_kind )
          with
          | None ->
              acc
          | Some suspicious_source ->
              L.d_printfln ~color:Red "TAINTED: %a -> %a" Taint.Kind.pp suspicious_source
                Taint.Kind.pp sink_kind ;
              let matching_sanitizers =
                Attribute.TaintSanitizedSet.filter
                  (fun {sanitizer; time_trace= sanitizer_times} ->
                    Timestamp.compare_trace source_times sanitizer_times <= 0
                    && List.exists sanitizer.Taint.kinds ~f:(fun sanitizer_kind ->
                           List.mem ~equal:Taint.Kind.equal sanitizer_kinds sanitizer_kind ) )
                  sanitizers
              in
              if Attribute.TaintSanitizedSet.is_empty matching_sanitizers then
                (suspicious_source, sink_kind, policy) :: acc
              else (
                L.d_printfln ~color:Green "...but sanitized by %a" Attribute.TaintSanitizedSet.pp
                  matching_sanitizers ;
                acc ) ) )


(** Preorder traversal of the tree formed by taint dependencies of [v] in [astate] *)
let gather_taint_dependencies v astate =
  let visited = ref AbstractValue.Set.empty in
  let rec gather_taint_dependencies_aux acc v =
    if AbstractValue.Set.mem v !visited then acc
    else (
      visited := AbstractValue.Set.add v !visited ;
      let acc =
        match AbductiveDomain.AddressAttributes.get_propagate_taint_from v astate with
        | None ->
            acc
        | Some taints_in ->
            let acc =
              List.fold taints_in ~init:acc ~f:(fun acc {Attribute.v= v'} ->
                  gather_taint_dependencies_aux acc v' )
            in
            List.fold taints_in ~init:acc ~f:(fun acc {Attribute.v} -> v :: acc)
      in
      let acc =
        (* if the value is an array we propagate the check to the array elements *)
        (* NOTE: we could do the same for field accesses or really all accesses if we want the taint
            analysis to consider that the insides of objects are tainted whenever the object is. This
            might not be a very efficient way to do this though? *)
        match AbductiveDomain.Memory.find_opt v astate with
        | None ->
            acc
        | Some edges ->
            BaseMemory.Edges.fold edges ~init:acc ~f:(fun acc (access, (dest, _)) ->
                match access with
                | FieldAccess _ | TakeAddress | Dereference ->
                    acc
                | ArrayAccess _ -> (
                  match AbductiveDomain.Memory.find_edge_opt dest Dereference astate with
                  | None ->
                      acc
                  | Some (dest_value, _) ->
                      dest_value :: gather_taint_dependencies_aux acc dest_value ) )
      in
      v :: acc )
  in
  gather_taint_dependencies_aux [] v


let check_flows_wrt_sink path location (sink, sink_trace) v astate =
  let source_expr = Decompiler.find v astate in
  let mk_reportable_error diagnostic = [ReportableError {astate; diagnostic}] in
  let check_flows_to_taint_sink v astate =
    L.d_printfln "Checking for allocations flowing from %a to sink %a" AbstractValue.pp v Taint.pp
      sink ;
    match AbductiveDomain.AddressAttributes.get_allocation v astate with
    | None ->
        Ok ()
    | Some (allocator, history) ->
        L.d_printfln "Found allocation %a" Attribute.pp (Attribute.Allocated (allocator, history)) ;
        let sink_can_be_sanitized_by sink_kind ~sanitizer =
          let policies = Hashtbl.find_exn sink_policies sink_kind in
          List.exists policies ~f:(fun {sanitizer_kinds} ->
              List.exists sanitizer.Taint.kinds ~f:(fun sanitizer_kind ->
                  List.mem sanitizer_kinds sanitizer_kind ~equal:Taint.Kind.equal ) )
        in
        let sanitizers =
          let _, potential_sanitizers =
            AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers v astate
          in
          Attribute.TaintSanitizedSet.filter
            (fun {sanitizer} ->
              List.exists sink.Taint.kinds ~f:(sink_can_be_sanitized_by ~sanitizer) )
            potential_sanitizers
        in
        if not (Attribute.TaintSanitizedSet.is_empty sanitizers) then
          L.d_printfln ~color:Green "...but may be sanitized by %a" Attribute.TaintSanitizedSet.pp
            sanitizers ;
        Recoverable
          ( ()
          , mk_reportable_error
              (FlowToTaintSink
                 {source= (source_expr, history); sanitizers; sink= (sink, sink_trace); location} )
          )
  in
  let check_tainted_flows policy_violations_reported v astate =
    L.d_printfln "Checking that %a is not tainted" AbstractValue.pp v ;
    let sources, sanitizers =
      AbductiveDomain.AddressAttributes.get_taint_sources_and_sanitizers v astate
    in
    Attribute.TaintedSet.fold
      (fun {source; time_trace= source_times; hist= source_hist} policy_violations_reported_result ->
        let* policy_violations_reported = policy_violations_reported_result in
        L.d_printfln ~color:Red "Found source %a, checking policy..." Taint.pp source ;
        let potential_policy_violations = check_policies ~sink ~source ~source_times ~sanitizers in
        let report_policy_violation reported_so_far (source_kind, sink_kind, violated_policy) =
          (* HACK: compare by pointer as policies are fixed throughout execution and each policy
             record is different from all other policies; we could optimise this check by keeping
             a set of policies around instead of a list, eg assign an integer id to each policy
             (using a simple incrementing counter when reading the configuration) and comparing
             only that id. *)
          if List.mem ~equal:phys_equal reported_so_far violated_policy then Ok reported_so_far
          else
            Recoverable
              ( violated_policy :: reported_so_far
              , mk_reportable_error
                  (TaintFlow
                     { tainted= source_expr
                     ; location
                     ; source= ({source with kinds= [source_kind]}, source_hist)
                     ; sink= ({sink with kinds= [sink_kind]}, sink_trace) } ) )
        in
        PulseResult.list_fold potential_policy_violations ~init:policy_violations_reported
          ~f:report_policy_violation )
      sources (Ok policy_violations_reported)
  in
  let taint_dependencies = gather_taint_dependencies v astate in
  let+ _, astate =
    PulseResult.list_fold taint_dependencies ~init:([], astate)
      ~f:(fun (policy_violations_reported, astate) v ->
        let astate =
          AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate
        in
        let* () = check_flows_to_taint_sink v astate in
        let+ policy_violations_reported = check_tainted_flows policy_violations_reported v astate in
        (policy_violations_reported, astate) )
  in
  astate


let taint_sinks tenv path location return ~has_added_return_param proc_name actuals astate =
  let tainted =
    get_tainted tenv sink_matchers return ~has_added_return_param proc_name actuals astate
  in
  let+ astate =
    PulseResult.list_fold tainted ~init:astate ~f:(fun astate (sink, ((v, history), _typ)) ->
        let sink_trace = Trace.Immediate {location; history} in
        let astate =
          AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate
        in
        check_flows_wrt_sink path location (sink, sink_trace) v astate )
  in
  (astate, not (List.is_empty tainted))


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
  let propagate_to v values astate =
    let astate =
      if not (List.is_empty values) then
        AbductiveDomain.AddressAttributes.add_one v
          (PropagateTaintFrom
             (List.map values ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= v, _hist} ->
                  {Attribute.v} ) ) )
          astate
      else astate
    in
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
        astate )
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
        propagate_to return_value actuals astate
    | _ ->
        astate
  in
  let astate =
    match actuals with
    | {ProcnameDispatcher.Call.FuncArg.arg_payload= this, _hist} :: other_actuals
      when propagate_to_receiver ->
        propagate_to this other_actuals astate
    | _ ->
        astate
  in
  match List.rev actuals with
  | {ProcnameDispatcher.Call.FuncArg.arg_payload= last, _hist} :: other_actuals
    when propagate_to_last_actual ->
      propagate_to last other_actuals astate
  | _ ->
      astate


(* some pulse models are not a faithful reflection of the behaviour and should be treated as unknown
   wrt taint behaviour (i.e. propagate taint like an unknown function would) *)
let pulse_models_to_treat_as_unknown_for_taint =
  (* HACK: make a list of matchers just to reuse the matching code below *)
  let dummy_matcher_of_procedure_matcher procedure_matcher =
    {procedure_matcher; arguments= []; kinds= []; target= `ReturnValue}
  in
  [ ClassAndMethodNames
      { class_names= ["java.lang.StringBuilder"]
      ; method_names= ["append"; "delete"; "replace"; "setLength"] }
  ; ProcedureNameRegex {name_regex= Str.regexp "std::basic_string<.*>::basic_string"} ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint tenv proc_name =
  (* HACK: we already have a function for matching procedure names so just re-use it even though we
     don't need its full power *)
  Procname.is_implicit_ctor proc_name
  || procedure_matches tenv pulse_models_to_treat_as_unknown_for_taint proc_name []
     |> Option.is_some


let should_ignore_sensitive_data_flows_to proc_name =
  Procname.is_objc_dealloc proc_name || BuiltinDecl.is_declared proc_name


let call tenv path location return ~call_was_unknown (call : _ Either.t) actuals astate =
  match call with
  | First call_exp ->
      if call_was_unknown then
        Ok
          (propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param:false
             (SkippedUnknownCall call_exp) None actuals astate )
      else Ok astate
  | Second proc_name ->
      let call_was_unknown = call_was_unknown || should_treat_as_unknown_for_taint tenv proc_name in
      let has_added_return_param =
        match IRAttributes.load proc_name with
        | Some attrs when attrs.ProcAttributes.has_added_return_param ->
            true
        | _ ->
            false
      in
      let astate =
        taint_sanitizers tenv path (Some return) ~has_added_return_param ~location proc_name actuals
          astate
      in
      let astate, found_source_model =
        taint_sources tenv path location ~intra_procedural_only:false (Some return)
          ~has_added_return_param proc_name actuals astate
      in
      let+ astate, found_sink_model =
        taint_sinks tenv path location (Some return) ~has_added_return_param proc_name actuals
          astate
      in
      let astate =
        if should_ignore_sensitive_data_flows_to proc_name then astate
        else
          List.foldi actuals ~init:astate
            ~f:(fun index astate ProcnameDispatcher.Call.FuncArg.{arg_payload= v, history} ->
              let origin = Taint.Argument {index} in
              let trace = Trace.Immediate {location; history} in
              AbductiveDomain.AddressAttributes.add_taint_procedure path origin proc_name trace v
                astate )
      in
      (* NOTE: we don't care about sanitizers because we want to propagate taint source and sink
         information even if a procedure also happens to sanitize *some* of the sources *)
      if call_was_unknown && (not found_source_model) && not found_sink_model then
        propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param
          (SkippedKnownCall proc_name) (Some proc_name) actuals astate
      else astate


let taint_initial tenv proc_name (proc_attrs : ProcAttributes.t) astate =
  let astate, actuals =
    List.fold_map (ProcAttributes.get_pvar_formals proc_attrs) ~init:astate
      ~f:(fun astate (pvar, typ) ->
        let astate, actual_value =
          PulseOperations.eval_deref PathContext.initial proc_attrs.loc (Lvar pvar) astate
          |> PulseResult.ok_exn
        in
        (astate, {ProcnameDispatcher.Call.FuncArg.exp= Lvar pvar; typ; arg_payload= actual_value}) )
  in
  taint_sources tenv PathContext.initial proc_attrs.loc ~intra_procedural_only:true None
    ~has_added_return_param:false proc_name actuals astate
  |> fst
