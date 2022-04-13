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
  Pulse_config_j.taint_policies_of_string (Yojson.Basic.to_string Config.pulse_taint_policies)
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


let matcher_of_config ~default_taint_target ~option_name config =
  (* TODO: write our own json handling using [Yojson] directly as atdgen generated parsers ignore
     extra fields, meaning we won't report errors to users when they spell things wrong. *)
  let matchers = Pulse_config_j.matchers_of_string config in
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
    (Yojson.Basic.to_string Config.pulse_taint_sources)


let sink_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sinks"
    (Yojson.Basic.to_string Config.pulse_taint_sinks)


let sanitizer_matchers =
  matcher_of_config ~default_taint_target:`AllArguments ~option_name:"--pulse-taint-sanitizers"
    (Yojson.Basic.to_string Config.pulse_taint_sanitizers)


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


let get_tainted tenv matchers return_opt proc_name actuals astate =
  match procedure_matches tenv matchers proc_name actuals with
  | None ->
      []
  | Some matcher -> (
      L.d_printfln "taint matches" ;
      match matcher.target with
      | `ReturnValue -> (
          L.d_printf "matching return value... " ;
          match return_opt with
          | None ->
              L.d_printfln "no match" ;
              []
          | Some (return, return_typ) ->
              L.d_printfln "match! tainting return value" ;
              (* TODO: match values returned by reference by the frontend *)
              let return = Var.of_id return in
              Stack.find_opt return astate
              |> Option.fold ~init:[] ~f:(fun tainted return_value ->
                     let taint = {Taint.proc_name; origin= ReturnValue; kinds= matcher.kinds} in
                     (taint, (return_value, return_typ)) :: tainted ) )
      | ( `AllArguments
        | `ArgumentPositions _
        | `AllArgumentsButPositions _
        | `ArgumentsMatchingTypes _ ) as taint_target ->
          L.d_printf "matching actuals... " ;
          let actuals =
            List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ} ->
                (arg_payload, typ) )
          in
          List.foldi actuals ~init:[] ~f:(fun i tainted ((_, actual_typ) as actual_hist_and_typ) ->
              if taint_target_matches tenv taint_target i actual_typ then (
                L.d_printfln "match! tainting actual #%d with type %a" i (Typ.pp_full Pp.text)
                  actual_typ ;
                let taint = {Taint.proc_name; origin= Argument {index= i}; kinds= matcher.kinds} in
                (taint, actual_hist_and_typ) :: tainted )
              else (
                L.d_printfln "no match for #%d with type %a" i (Typ.pp_full Pp.text) actual_typ ;
                tainted ) ) )


let taint_sources tenv path location ~intra_procedural_only return proc_name actuals astate =
  let tainted = get_tainted tenv source_matchers return proc_name actuals astate in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (source, ((v, _), _)) ->
        let hist =
          ValueHistory.singleton (TaintSource (source, location, path.PathContext.timestamp))
        in
        AbductiveDomain.AddressAttributes.add_one v
          (Tainted {source; hist; intra_procedural_only})
          astate )
  in
  (astate, not (List.is_empty tainted))


let taint_sanitizers tenv return proc_name actuals astate =
  let tainted = get_tainted tenv sanitizer_matchers return proc_name actuals astate in
  let astate =
    List.fold tainted ~init:astate ~f:(fun astate (sanitizer, ((v, _), _)) ->
        AbductiveDomain.AddressAttributes.add_one v (TaintSanitized sanitizer) astate )
  in
  astate


let check_policies ~sink ~source ~sanitizer_opt =
  let sinks_policies =
    List.map sink.Taint.kinds ~f:(fun sink_kind ->
        (sink_kind, Hashtbl.find_exn sink_policies sink_kind) )
  in
  List.fold_result sinks_policies ~init:() ~f:(fun () (sink_kind, policies) ->
      List.fold_result policies ~init:() ~f:(fun () ({source_kinds; sanitizer_kinds} as policy) ->
          match
            List.find source.Taint.kinds ~f:(fun source_kind ->
                List.mem ~equal:Taint.Kind.equal source_kinds source_kind )
          with
          | None ->
              Ok ()
          | Some suspicious_source ->
              L.d_printfln ~color:Red "TAINTED: %a -> %a" Taint.Kind.pp suspicious_source
                Taint.Kind.pp sink_kind ;
              if
                Option.exists sanitizer_opt ~f:(fun sanitizer ->
                    List.exists sanitizer.Taint.kinds ~f:(fun sanitizer_kind ->
                        List.mem ~equal:Taint.Kind.equal sanitizer_kinds sanitizer_kind ) )
              then (
                L.d_printfln ~color:Green "...but sanitized by %a" Taint.pp
                  (Option.value_exn sanitizer_opt) ;
                Ok () )
              else
                (* TODO: we may want to collect *all* the policies that are violated instead of just
                   the first one *)
                Error (suspicious_source, sink_kind, policy) ) )


let check_not_tainted_wrt_sink path location (sink, sink_trace) v astate =
  let check_immediate policy_violations_reported v astate =
    L.d_printfln "Checking that %a is not tainted" AbstractValue.pp v ;
    match AbductiveDomain.AddressAttributes.get_taint_source_and_sanitizer v astate with
    | None ->
        Ok (policy_violations_reported, astate)
    | Some ((source, source_hist, _), sanitizer_opt) -> (
        L.d_printfln ~color:Red "Found source %a, checking policy..." Taint.pp source ;
        match check_policies ~sink ~source ~sanitizer_opt with
        | Ok () ->
            Ok (policy_violations_reported, astate)
        | Error (source_kind, sink_kind, policy) ->
            (* HACK: compare by pointer as policies are fixed throughout execution and each policy
               record is different from all other policies; we could optimise this check by keeping
               a set of policies around instead of a list, eg assign an integer id to each policy
               (using a simple incrementing counter when reading the configuration) and comparing
               only that id. *)
            if List.mem ~equal:phys_equal policy_violations_reported policy then
              Ok (policy_violations_reported, astate)
            else
              let tainted = Decompiler.find v astate in
              Recoverable
                ( (policy :: policy_violations_reported, astate)
                , [ ReportableError
                      { astate
                      ; diagnostic=
                          TaintFlow
                            { tainted
                            ; location
                            ; source= ({source with kinds= [source_kind]}, source_hist)
                            ; sink= ({sink with kinds= [sink_kind]}, sink_trace) } } ] ) )
  in
  let rec check_dependencies policy_violations_reported visited v astate =
    let* astate =
      match AbductiveDomain.AddressAttributes.get_propagate_taint_from v astate with
      | None ->
          Ok astate
      | Some taints_in ->
          PulseResult.list_fold taints_in ~init:astate ~f:(fun astate {Attribute.v= v'} ->
              check policy_violations_reported visited v' astate )
    in
    (* if the value is an array we propagate the check to the array elements *)
    (* NOTE: we could do the same for field accesses or really all accesses if we want the taint
       analysis to consider that the insides of objects are tainted whenever the object is. This
       might not be a very efficient way to do this though? *)
    match AbductiveDomain.Memory.find_opt v astate with
    | None ->
        Ok astate
    | Some edges ->
        BaseMemory.Edges.fold edges ~init:(Ok astate) ~f:(fun astate_result (access, (dest, _)) ->
            match (access : _ HilExp.Access.t) with
            | ArrayAccess _ -> (
              match AbductiveDomain.Memory.find_edge_opt dest Dereference astate with
              | None ->
                  astate_result
              | Some (dest_value, _) ->
                  let* astate = astate_result in
                  check policy_violations_reported visited dest_value astate )
            | FieldAccess _ | TakeAddress | Dereference ->
                astate_result )
  and check policy_violations_reported visited v astate =
    if AbstractValue.Set.mem v visited then Ok astate
    else
      let visited = AbstractValue.Set.add v visited in
      let astate = AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate in
      let* policy_violations_reported, astate =
        check_immediate policy_violations_reported v astate
      in
      check_dependencies policy_violations_reported visited v astate
  in
  check [] AbstractValue.Set.empty v astate


let taint_sinks tenv path location return proc_name actuals astate =
  let tainted = get_tainted tenv sink_matchers return proc_name actuals astate in
  let+ astate =
    PulseResult.list_fold tainted ~init:astate ~f:(fun astate (sink, ((v, history), _typ)) ->
        let sink_trace = Trace.Immediate {location; history} in
        let astate =
          AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate
        in
        check_not_tainted_wrt_sink path location (sink, sink_trace) v astate )
  in
  (astate, not (List.is_empty tainted))


(* merge all taint from actuals into the return value; NOTE: currently only one source and one
   sanitizer per value is supported so this may overwrite taint information *)
let propagate_taint_for_unknown_calls tenv path location (return, return_typ) call proc_name_opt
    actuals astate =
  (* TODO: match values returned by reference by the frontend *)
  L.d_printfln "propagating all taint for unknown call" ;
  let return = Var.of_id return in
  let propagate_to_return, propagate_to_receiver =
    let is_static =
      Option.exists proc_name_opt ~f:(fun proc_name ->
          Option.value ~default:true (Procname.is_static proc_name) )
    in
    match (proc_name_opt, return_typ, actuals) with
    | Some proc_name, _, _ when Procname.is_constructor proc_name ->
        L.d_printfln "unknown constructor, propagating taint to receiver" ;
        (false, true)
    | _, {Typ.desc= Tint _ | Tfloat _ | Tvoid}, _ when not is_static ->
        (* for instance methods with a non-Object return value, propagate the taint to the
               receiver *)
        L.d_printfln "non-object return type, propagating taint to receiver" ;
        (false, true)
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
        (true, true)
    | _, {Typ.desc= Tptr _ | Tstruct _}, _ ->
        L.d_printfln "object return type, propagating taint to return" ;
        (true, false)
    | _, _, _ ->
        L.d_printfln "not propagating taint" ;
        (false, false)
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
        match AbductiveDomain.AddressAttributes.get_taint_source_and_sanitizer actual astate with
        | None ->
            astate
        | Some ((source, source_hist, intra_procedural_only), sanitizer_opt) ->
            let hist =
              ValueHistory.sequence ~context:path.PathContext.conditions
                (Call
                   { f= call
                   ; location
                   ; timestamp= path.PathContext.timestamp
                   ; in_call= ValueHistory.epoch } )
                source_hist
            in
            L.d_printfln "tainting %a as source" AbstractValue.pp v ;
            let astate =
              AbductiveDomain.AddressAttributes.add_one v
                (Tainted {source; hist; intra_procedural_only})
                astate
            in
            Option.fold sanitizer_opt ~init:astate ~f:(fun astate sanitizer ->
                L.d_printfln "registering %a as sanitizer" AbstractValue.pp v ;
                AbductiveDomain.AddressAttributes.add_one v (TaintSanitized sanitizer) astate ) )
  in
  let astate =
    match Stack.find_opt return astate with
    | Some (return_value, _) when propagate_to_return ->
        propagate_to return_value actuals astate
    | _ ->
        astate
  in
  match actuals with
  | {ProcnameDispatcher.Call.FuncArg.arg_payload= this, _hist} :: other_actuals
    when propagate_to_receiver ->
      propagate_to this other_actuals astate
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
      ; method_names= ["append"; "delete"; "replace"; "setLength"] } ]
  |> List.map ~f:dummy_matcher_of_procedure_matcher


let should_treat_as_unknown_for_taint tenv proc_name =
  (* HACK: we already have a function for matching procedure names so just re-use it even though we
     don't need its full power *)
  procedure_matches tenv pulse_models_to_treat_as_unknown_for_taint proc_name [] |> Option.is_some


let call tenv path location return ~call_was_unknown (call : _ Either.t) actuals astate =
  match call with
  | First call_exp ->
      if call_was_unknown then
        Ok
          (propagate_taint_for_unknown_calls tenv path location return (SkippedUnknownCall call_exp)
             None actuals astate )
      else Ok astate
  | Second proc_name ->
      let call_was_unknown = call_was_unknown || should_treat_as_unknown_for_taint tenv proc_name in
      let astate = taint_sanitizers tenv (Some return) proc_name actuals astate in
      let astate, found_source_model =
        taint_sources tenv path location ~intra_procedural_only:false (Some return) proc_name
          actuals astate
      in
      let+ astate, found_sink_model =
        taint_sinks tenv path location (Some return) proc_name actuals astate
      in
      (* NOTE: we don't care about sanitizers because we want to propagate taint source and sink
         information even if a procedure also happens to sanitize *some* of the sources *)
      if call_was_unknown && (not found_source_model) && not found_sink_model then
        propagate_taint_for_unknown_calls tenv path location return (SkippedKnownCall proc_name)
          (Some proc_name) actuals astate
      else astate


let taint_initial tenv proc_desc astate =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let location = Procdesc.get_loc proc_desc in
  let astate, actuals =
    List.fold_map (Procdesc.get_pvar_formals proc_desc) ~init:astate ~f:(fun astate (pvar, typ) ->
        let astate, actual_value =
          PulseOperations.eval_deref PathContext.initial location (Lvar pvar) astate
          |> PulseResult.ok_exn
        in
        (astate, {ProcnameDispatcher.Call.FuncArg.exp= Lvar pvar; typ; arg_payload= actual_value}) )
  in
  taint_sources tenv PathContext.initial location ~intra_procedural_only:true None proc_name actuals
    astate
  |> fst
