(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

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
  | `Fields _ ->
      false


type procedure_matcher =
  | ProcedureName of {name: string}
  | ProcedureNameRegex of {name_regex: Str.regexp}
  | ClassAndMethodNames of {class_names: string list; method_names: string list}
  | OverridesOfClassWithAnnotation of {annotation: string}
  | MethodWithAnnotation of {annotation: string}
  | Allocation of {class_name: string}

type matcher =
  { procedure_matcher: procedure_matcher
  ; arguments: Pulse_config_t.argument_constraint list
  ; kinds: Taint.Kind.t list
  ; target: Pulse_config_t.taint_target
  ; match_objc_blocks: bool }

type sink_policy =
  { source_kinds: Taint.Kind.t list [@ignore]
  ; sanitizer_kinds: Taint.Kind.t list [@ignore]
  ; description: string [@ignore]
  ; policy_id: int
  ; privacy_effect: string option [@ignore] }
[@@deriving equal]

let sink_policies = Hashtbl.create (module Taint.Kind)

let next_policy_id =
  let policy_id_counter = ref 0 in
  fun () ->
    incr policy_id_counter ;
    !policy_id_counter


let fill_data_flow_kinds_from_config () =
  Config.pulse_taint_config.data_flow_kinds
  |> List.iter ~f:(fun kind -> Taint.Kind.of_string kind |> Taint.Kind.mark_data_flow_only)


let fill_policies_from_config () =
  Config.pulse_taint_config.policies
  |> List.iter
       ~f:(fun {Pulse_config_j.short_description= description; taint_flows; privacy_effect} ->
         let policy_id = next_policy_id () in
         List.iter taint_flows ~f:(fun {Pulse_config_j.source_kinds; sanitizer_kinds; sink_kinds} ->
             let source_kinds = List.map source_kinds ~f:Taint.Kind.of_string in
             let sanitizer_kinds = List.map sanitizer_kinds ~f:Taint.Kind.of_string in
             List.iter sink_kinds ~f:(fun sink_kind_s ->
                 let sink_kind = Taint.Kind.of_string sink_kind_s in
                 let flow =
                   {source_kinds; sanitizer_kinds; description; policy_id; privacy_effect}
                 in
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
        ; sanitizer_kinds= [simple_kind]
        ; policy_id= next_policy_id ()
        ; privacy_effect= None } ]
  |> ignore ;
  fill_data_flow_kinds_from_config () ;
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
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; allocation= None } ->
            ProcedureName {name}
        | { procedure= None
          ; procedure_regex= Some name_regex
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; allocation= None } ->
            ProcedureNameRegex {name_regex= Str.regexp name_regex}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= Some class_names
          ; method_names= Some method_names
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; allocation= None } ->
            ClassAndMethodNames {class_names; method_names}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= Some annotation
          ; method_with_annotation= None
          ; allocation= None } ->
            OverridesOfClassWithAnnotation {annotation}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= Some annotation
          ; allocation= None } ->
            MethodWithAnnotation {annotation}
        | { procedure= None
          ; procedure_regex= None
          ; class_names= None
          ; method_names= None
          ; overrides_of_class_with_annotation= None
          ; method_with_annotation= None
          ; allocation= Some class_name } ->
            Allocation {class_name}
        | _ ->
            L.die UserError
              "When parsing option %s: Unexpected JSON format: Exactly one of \"procedure\", \
               \"procedure_regex\", \"allocation\" must be provided, or else \"class_names\" and \
               \"method_names\" must be provided, or else \"overrides_of_class_with_annotation\", \
               but got \"procedure\": %a, \"procedure_regex\": %a, \"class_names\": %a, \
               \"method_names\": %a, \"overrides_of_class_with_annotation\": %a,\n\
              \               \"method_with_annotation\": %a \"allocation\": %a" option_name
              (Pp.option F.pp_print_string) matcher.procedure (Pp.option F.pp_print_string)
              matcher.procedure_regex
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.class_names
              (Pp.option (Pp.seq ~sep:"," F.pp_print_string))
              matcher.method_names (Pp.option F.pp_print_string)
              matcher.overrides_of_class_with_annotation (Pp.option F.pp_print_string)
              matcher.method_with_annotation (Pp.option F.pp_print_string) matcher.allocation
      in
      { procedure_matcher
      ; arguments= matcher.argument_constraints
      ; kinds= kinds_of_strings_opt matcher.kinds
      ; target= Option.value ~default:default_taint_target matcher.taint_target
      ; match_objc_blocks= matcher.match_objc_blocks } )


let allocation_sources, source_matchers =
  let all_source_matchers =
    matcher_of_config ~default_taint_target:`ReturnValue ~option_name:"--pulse-taint-sources"
      Config.pulse_taint_config.sources
  in
  List.partition_map all_source_matchers ~f:(fun ({procedure_matcher; kinds} as matcher) ->
      match procedure_matcher with
      | Allocation {class_name} ->
          Either.First (class_name, kinds)
      | _ ->
          Either.Second matcher )


let sink_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sinks"
    Config.pulse_taint_config.sinks


let sanitizer_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sanitizers"
    Config.pulse_taint_config.sanitizers


let propagator_matchers =
  matcher_of_config ~default_taint_target:`ReturnValue ~option_name:"--pulse-taint-propagators"
    Config.pulse_taint_config.propagators


let procedure_matches exe_env tenv matchers proc_name actuals =
  List.filter_map matchers ~f:(fun matcher ->
      let procedure_name_matches =
        match matcher.procedure_matcher with
        | ProcedureName {name} ->
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
            String.is_substring ~substring:name proc_name_s
        | ProcedureNameRegex {name_regex} -> (
            let proc_name_s = F.asprintf "%a" Procname.pp_verbose proc_name in
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
        | MethodWithAnnotation {annotation} ->
            Annotations.pname_has_return_annot exe_env proc_name (fun annot_item ->
                Annotations.ia_ends_with annot_item annotation )
        | Allocation _ ->
            false
      in
      if
        procedure_name_matches
        && Bool.equal (Procname.is_objc_block proc_name) matcher.match_objc_blocks
      then
        let actuals_match =
          List.for_all matcher.arguments ~f:(fun {Pulse_config_t.index; type_matches= types} ->
              List.nth actuals index
              |> Option.exists ~f:(fun {ProcnameDispatcher.Call.FuncArg.typ} ->
                     type_matches tenv typ types ) )
        in
        Option.some_if actuals_match matcher
      else None )


let get_tainted exe_env tenv path location matchers return_opt ~has_added_return_param proc_name
    actuals astate =
  let matches = procedure_matches exe_env tenv matchers proc_name actuals in
  if not (List.is_empty matches) then L.d_printfln "taint matches" ;
  List.fold matches ~init:(astate, []) ~f:(fun acc matcher ->
      let actuals =
        List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ} ->
            (arg_payload, typ) )
      in
      let {kinds} = matcher in
      let rec match_target acc = function
        | `ReturnValue -> (
            L.d_printf "matching return value... " ;
            match return_opt with
            | None ->
                L.d_printfln "no match" ;
                acc
            | Some (return, return_typ) -> (
                L.d_printfln "match! tainting return value" ;
                let return_as_actual = if has_added_return_param then List.last actuals else None in
                match return_as_actual with
                | Some actual ->
                    let astate, acc = acc in
                    let taint = {Taint.proc_name; origin= ReturnValue; kinds} in
                    (astate, (taint, actual) :: acc)
                | None ->
                    let return = Var.of_id return in
                    let astate =
                      if Stack.mem return astate then astate
                      else
                        let ret_val = AbstractValue.mk_fresh () in
                        Stack.add return (ret_val, ValueHistory.epoch) astate
                    in
                    Stack.find_opt return astate
                    |> Option.fold ~init:acc ~f:(fun (_, tainted) return_value ->
                           let taint = {Taint.proc_name; origin= ReturnValue; kinds} in
                           (astate, (taint, (return_value, return_typ)) :: tainted) ) ) )
        | ( `AllArguments
          | `ArgumentPositions _
          | `AllArgumentsButPositions _
          | `ArgumentsMatchingTypes _ ) as taint_target ->
            L.d_printf "matching actuals... " ;
            List.foldi actuals ~init:acc
              ~f:(fun i ((astate, tainted) as acc) ((_, actual_typ) as actual_hist_and_typ) ->
                if taint_target_matches tenv taint_target i actual_typ then (
                  L.d_printfln_escaped "match! tainting actual #%d with type %a" i
                    (Typ.pp_full Pp.text) actual_typ ;
                  let taint = {Taint.proc_name; origin= Argument {index= i}; kinds} in
                  (astate, (taint, actual_hist_and_typ) :: tainted) )
                else (
                  L.d_printfln_escaped "no match for #%d with type %a" i (Typ.pp_full Pp.text)
                    actual_typ ;
                  acc ) )
        | `Fields fields ->
            let type_check astate value typ fieldname =
              (* Dereference the value as much as possible and verify the result
                 is of structure type holding the expected fieldname. *)
              let open IOption.Let_syntax in
              let rec get_val_and_typ_name astate value typ =
                match typ.Typ.desc with
                | Tstruct typ_name | Tptr ({desc= Tstruct typ_name}, _) ->
                    Some (astate, value, typ_name)
                | Tptr (typ', _) ->
                    let* astate, value =
                      PulseOperations.eval_access path Read location value Dereference astate
                      |> PulseResult.ok
                    in
                    get_val_and_typ_name astate value typ'
                | _ ->
                    None
              in
              let* astate, value, typ_name = get_val_and_typ_name astate value typ in
              let+ field_typ =
                let* {Struct.fields} = Tenv.lookup tenv typ_name in
                List.find_map fields ~f:(fun (field, typ, _) ->
                    if String.equal fieldname (Fieldname.get_field_name field) then Some typ
                    else None )
              in
              (astate, value, typ_name, field_typ)
            in
            let move_taint_to_field ((astate, tainted) as acc) (taint, (value, typ)) fieldname =
              (* Move the taint from [value] to [value]'s field [fieldname] *)
              match type_check astate value typ fieldname with
              | None ->
                  (* The value cannot hold the expected field.
                     This is a type mismatch and we need to inform the user. *)
                  L.die UserError
                    "Type error in taint configuration: Model for `%a`:Type `%a` does not have a \
                     field `%s`"
                    Procname.pp proc_name (Typ.pp_full Pp.text) typ fieldname
              | Some (astate, value, typ_name, field_typ) ->
                  Option.value ~default:acc
                    (PulseResult.ok
                       (let open PulseResult.Let_syntax in
                       let* astate, ret_value =
                         PulseOperations.eval_access path Read location value
                           (FieldAccess (Fieldname.make typ_name fieldname))
                           astate
                       in
                       let+ astate, ret_value =
                         PulseOperations.eval_access path Read location ret_value Dereference astate
                       in
                       L.d_printfln "match! tainting field %s with type %a" fieldname
                         (Typ.pp_full Pp.text) field_typ ;
                       ( astate
                       , ( Taint.{taint with origin= Field {name= fieldname; origin= taint.origin}}
                         , (ret_value, field_typ) )
                         :: tainted )) )
            in
            List.fold fields ~init:acc ~f:(fun ((astate, tainted) as acc) (fieldname, origin) ->
                let astate', tainted' = match_target acc origin in
                let new_taints, _ =
                  List.split_n tainted' (List.length tainted' - List.length tainted)
                in
                let acc = if phys_equal astate' astate then acc else (astate', tainted) in
                List.fold new_taints ~init:acc ~f:(fun acc taint ->
                    move_taint_to_field acc taint fieldname ) )
      in
      match_target acc matcher.target )


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
          (* Micro-optimisation: do not allocate `alloc_desc` when no matching taint sources are found *)
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
                    let origin = Taint.Allocation {typ= type_name} in
                    {Taint.kinds; proc_name; origin}
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
  let visited = ref AbstractValue.Set.empty in
  let rec aux v astate =
    if AbstractValue.Set.mem v !visited then astate
    else (
      visited := AbstractValue.Set.add v !visited ;
      let astate = taint v astate in
      match AbductiveDomain.Memory.find_opt v astate with
      | None ->
          astate
      | Some edges ->
          BaseMemory.Edges.fold edges ~init:astate ~f:(fun astate (_, (v, _)) -> aux v astate) )
  in
  aux v astate


let taint_sources exe_env tenv path location ~intra_procedural_only return ~has_added_return_param
    proc_name actuals astate =
  let astate, tainted =
    get_tainted exe_env tenv path location source_matchers return ~has_added_return_param proc_name
      actuals astate
  in
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
      taint_and_explore v astate ~taint:(fun v astate ->
          AbductiveDomain.AddressAttributes.add_one v
            (Tainted (Attribute.TaintedSet.singleton tainted))
            astate ) )


let taint_sanitizers exe_env tenv path return ~has_added_return_param ~location proc_name actuals
    astate =
  let astate, tainted =
    get_tainted exe_env tenv path location sanitizer_matchers return ~has_added_return_param
      proc_name actuals astate
  in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (sanitizer, ((v, history), _)) ->
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
  List.fold sink.Taint.kinds ~init:[] ~f:(fun acc sink_kind ->
      let policies = Hashtbl.find_exn sink_policies sink_kind in
      List.fold policies ~init:acc
        ~f:(fun acc {source_kinds; sanitizer_kinds; description; policy_id; privacy_effect} ->
          match
            List.find source.Taint.kinds ~f:(fun source_kind ->
                (* We should ignore flows between data-flow-only sources and data-flow-only sinks *)
                (not
                   ( Taint.Kind.is_data_flow_only source_kind
                   && Taint.Kind.is_data_flow_only sink_kind ) )
                && List.mem ~equal:Taint.Kind.equal source_kinds source_kind )
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
                (suspicious_source, sink_kind, description, policy_id, privacy_effect) :: acc
              else (
                L.d_printfln ~color:Green "...but sanitized by %a" Attribute.TaintSanitizedSet.pp
                  matching_sanitizers ;
                acc ) ) )


module TaintDependencies = struct
  module G = Graph.Imperative.Digraph.Concrete (struct
    type t = AbstractValue.t [@@deriving compare, equal]

    let hash = Hashtbl.hash
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
      Option.iter (AbductiveDomain.Memory.find_opt v astate) ~f:(fun edges ->
          BaseMemory.Edges.iter edges ~f:(function
            | ArrayAccess _, (dest, _) ->
                Option.iter (AbductiveDomain.Memory.find_edge_opt dest Dereference astate)
                  ~f:(fun (dest_value, _) ->
                    TaintDependencies.add_edge taint_dependencies v dest_value )
            | (FieldAccess _ | TakeAddress | Dereference), _ ->
                () ) ) )
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
        L.d_printfln_escaped ~color:Red "Found source %a, checking policy..." Taint.pp source ;
        let potential_policy_violations = check_policies ~sink ~source ~source_times ~sanitizers in
        let report_policy_violation reported_so_far
            (source_kind, sink_kind, policy_description, violated_policy_id, policy_privacy_effect)
            =
          if IntSet.mem violated_policy_id reported_so_far then Ok reported_so_far
          else
            let flow_kind =
              if Taint.Kind.is_data_flow_only source_kind then Diagnostic.FlowToSink
              else if Taint.Kind.is_data_flow_only sink_kind then Diagnostic.FlowFromSource
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


let should_ignore_all_flows_to proc_name =
  Procname.is_objc_dealloc proc_name || BuiltinDecl.is_declared proc_name


let taint_sinks exe_env tenv path location return ~has_added_return_param proc_name actuals astate =
  let astate, tainted =
    get_tainted exe_env tenv path location sink_matchers return ~has_added_return_param proc_name
      actuals astate
  in
  PulseResult.list_fold tainted ~init:astate ~f:(fun astate (sink, ((v, history), _typ)) ->
      if should_ignore_all_flows_to proc_name then Ok astate
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
            let* policy_violations_reported, astate = res in
            match AbductiveDomain.Memory.find_opt v astate with
            | None ->
                Ok (policy_violations_reported, astate)
            | Some edges ->
                BaseMemory.Edges.fold edges ~init:res ~f:(fun res (access, (v, _)) ->
                    match access with
                    | HilExp.Access.FieldAccess fieldname
                      when Fieldname.equal fieldname PulseOperations.ModeledField.internal_string
                           || Fieldname.equal fieldname
                                PulseOperations.ModeledField.internal_ref_count ->
                        res
                    | _ ->
                        let* policy_violations_reported, astate = res in
                        mark_sinked policy_violations_reported v astate ) )
        in
        let+ _, astate = mark_sinked IntSet.empty v astate in
        astate )


let propagate_to path location v values call astate =
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


let taint_propagators exe_env tenv path location return ~has_added_return_param proc_name actuals
    astate =
  let astate, tainted =
    get_tainted exe_env tenv path location propagator_matchers return ~has_added_return_param
      proc_name actuals astate
  in
  List.fold tainted ~init:astate ~f:(fun astate (_propagator, ((v, _history), _)) ->
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
    {procedure_matcher; arguments= []; kinds= []; target= `ReturnValue; match_objc_blocks= false}
  in
  [ ClassAndMethodNames
      { class_names= ["java.lang.StringBuilder"]
      ; method_names= ["append"; "delete"; "replace"; "setLength"] }
  ; ProcedureNameRegex {name_regex= Str.regexp "std::basic_string<.*>::basic_string"} ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint exe_env tenv proc_name =
  (* HACK: we already have a function for matching procedure names so just re-use it even though we
     don't need its full power *)
  Option.exists (Exe_env.get_attributes exe_env proc_name) ~f:(fun attrs ->
      attrs.ProcAttributes.is_cpp_implicit )
  && Procname.is_constructor proc_name
  || procedure_matches exe_env tenv pulse_models_to_treat_as_unknown_for_taint proc_name []
     |> List.is_empty |> not


let call exe_env tenv path location return ~call_was_unknown (call : _ Either.t) actuals astate =
  match call with
  | First call_exp ->
      if call_was_unknown then
        Ok
          (propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param:false
             (SkippedUnknownCall call_exp) None actuals astate )
      else Ok astate
  | Second proc_name ->
      let call_was_unknown =
        call_was_unknown || should_treat_as_unknown_for_taint exe_env tenv proc_name
      in
      let has_added_return_param =
        match Exe_env.get_attributes exe_env proc_name with
        | Some attrs when attrs.ProcAttributes.has_added_return_param ->
            true
        | _ ->
            false
      in
      let astate =
        taint_sanitizers exe_env tenv path (Some return) ~has_added_return_param ~location proc_name
          actuals astate
      in
      let astate =
        taint_sources exe_env tenv path location ~intra_procedural_only:false (Some return)
          ~has_added_return_param proc_name actuals astate
      in
      let+ astate =
        taint_sinks exe_env tenv path location (Some return) ~has_added_return_param proc_name
          actuals astate
      in
      let astate, call_was_unknown =
        let new_astate =
          taint_propagators exe_env tenv path location (Some return) ~has_added_return_param
            proc_name actuals astate
        in
        (new_astate, call_was_unknown && phys_equal astate new_astate)
      in
      if call_was_unknown then
        propagate_taint_for_unknown_calls tenv path location return ~has_added_return_param
          (SkippedKnownCall proc_name) (Some proc_name) actuals astate
      else astate


let taint_initial exe_env tenv proc_name (proc_attrs : ProcAttributes.t) astate0 =
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
    taint_sources exe_env tenv PathContext.initial proc_attrs.loc ~intra_procedural_only:true None
      ~has_added_return_param:false proc_name (List.rev rev_actuals) astate
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
