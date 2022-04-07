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

type taint_target =
  | ArgumentPositions of Pulse_config_t.formal_matcher list
  | AllArguments
  | ReturnValue

let taint_target_of_list ~default_taint_target taint_target =
  if List.is_empty taint_target then default_taint_target else ArgumentPositions taint_target


let taint_target_matches taint_target actual_index actual_typ =
  let type_name_matches type_name_opt actual_typ =
    match type_name_opt with
    | None ->
        true
    | Some type_name ->
        String.is_substring ~substring:type_name (Typ.to_string actual_typ)
  in
  match taint_target with
  | ReturnValue ->
      false
  | AllArguments ->
      true
  | ArgumentPositions indices ->
      List.exists indices ~f:(fun {Pulse_config_t.index; type_name} ->
          Int.equal actual_index index && type_name_matches type_name actual_typ )


type simple_matcher =
  | ProcedureName of {name: string; kind: Taint.Kind.t; taint_target: taint_target}
  | ProcedureNameRegex of {name_regex: Str.regexp; kind: Taint.Kind.t; taint_target: taint_target}

let kind_of_matcher = function ProcedureName {kind} | ProcedureNameRegex {kind} -> kind

let taint_target_of_matcher = function
  | ProcedureName {taint_target} | ProcedureNameRegex {taint_target} ->
      taint_target


type sink_policy =
  {source_kinds: Taint.Kind.t list; sanitizer_kinds: Taint.Kind.t list; description: string}

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


let kind_of_string_opt = function None -> simple_kind | Some kind -> Taint.Kind.of_string kind

let matcher_of_config ~default_taint_target ~option_name config =
  (* TODO: write our own json handling using [Yojson] directly as atdgen generated parsers ignore
     extra fields, meaning we won't report errors to users when they spell things wrong. *)
  let matchers = Pulse_config_j.matchers_of_string config in
  List.map matchers ~f:(fun (matcher : Pulse_config_j.matcher) ->
      match matcher with
      | {procedure= None; procedure_regex= None} | {procedure= Some _; procedure_regex= Some _} ->
          L.die UserError
            "When parsing option %s: Unexpected JSON format: Exactly one of \"procedure\", \
             \"procedure_regex\" must be provided, but got \"procedure\": %a and \
             \"procedure_regex\": %a"
            option_name (Pp.option F.pp_print_string) matcher.procedure
            (Pp.option F.pp_print_string) matcher.procedure_regex
      | {procedure= Some name; procedure_regex= None; kind; formals} ->
          ProcedureName
            { name
            ; kind= kind_of_string_opt kind
            ; taint_target= taint_target_of_list ~default_taint_target formals }
      | {procedure= None; procedure_regex= Some name_regex; kind; formals} ->
          ProcedureNameRegex
            { name_regex= Str.regexp name_regex
            ; kind= kind_of_string_opt kind
            ; taint_target= taint_target_of_list ~default_taint_target formals } )


let source_matchers =
  matcher_of_config ~default_taint_target:ReturnValue ~option_name:"--pulse-taint-sources"
    (Yojson.Basic.to_string Config.pulse_taint_sources)


let sink_matchers =
  matcher_of_config ~default_taint_target:AllArguments ~option_name:"--pulse-taint-sinks"
    (Yojson.Basic.to_string Config.pulse_taint_sinks)


let sanitizer_matchers =
  matcher_of_config ~default_taint_target:AllArguments ~option_name:"--pulse-taint-sanitizers"
    (Yojson.Basic.to_string Config.pulse_taint_sanitizers)


let matches_simple matchers proc_name =
  let proc_name_s = Procname.to_string proc_name in
  (* handle [taint_target] *)
  List.find_map matchers ~f:(fun matcher ->
      let matches =
        match matcher with
        | ProcedureName {name} ->
            String.is_substring ~substring:name proc_name_s
        | ProcedureNameRegex {name_regex} -> (
          match Str.search_forward name_regex proc_name_s 0 with
          | _ ->
              true
          | exception Caml.Not_found ->
              false )
      in
      Option.some_if matches matcher )


let get_tainted matchers (return, return_typ) proc_name actuals astate =
  match matches_simple matchers proc_name with
  | None ->
      []
  | Some matcher -> (
    match taint_target_of_matcher matcher with
    | ReturnValue ->
        (* TODO: match values returned by reference by the frontend *)
        let return = Var.of_id return in
        Stack.find_opt return astate
        |> Option.fold ~init:[] ~f:(fun tainted return_value ->
               let taint = {Taint.proc_name; origin= ReturnValue; kind= kind_of_matcher matcher} in
               (taint, (return_value, return_typ)) :: tainted )
    | (AllArguments | ArgumentPositions _) as taint_target ->
        let actuals =
          List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload; typ} ->
              (arg_payload, typ) )
        in
        List.foldi actuals ~init:[] ~f:(fun i tainted ((_, actual_typ) as actual_hist_and_typ) ->
            if taint_target_matches taint_target i actual_typ then
              let taint =
                {Taint.proc_name; origin= Argument {index= i}; kind= kind_of_matcher matcher}
              in
              (taint, actual_hist_and_typ) :: tainted
            else tainted ) )


let taint_sources path location return proc_name actuals astate =
  let tainted = get_tainted source_matchers return proc_name actuals astate in
  List.fold tainted ~init:astate ~f:(fun astate (source, ((v, _), _)) ->
      let hist =
        ValueHistory.singleton (TaintSource (source, location, path.PathContext.timestamp))
      in
      AbductiveDomain.AddressAttributes.add_one v (Tainted (source, hist)) astate )


let taint_sanitizers return proc_name actuals astate =
  let tainted = get_tainted sanitizer_matchers return proc_name actuals astate in
  List.fold tainted ~init:astate ~f:(fun astate (sanitizer, ((v, _), _)) ->
      AbductiveDomain.AddressAttributes.add_one v (TaintSanitized sanitizer) astate )


let check_policy ~sink ~source ~sanitizer_opt =
  let policy = Hashtbl.find_exn sink_policies sink.Taint.kind in
  List.fold_result policy ~init:() ~f:(fun () {source_kinds; sanitizer_kinds} ->
      if List.mem ~equal:Taint.Kind.equal source_kinds source.Taint.kind then (
        L.d_printfln ~color:Red "TAINTED: %a" Taint.pp source ;
        if
          Option.exists sanitizer_opt ~f:(fun sanitizer ->
              List.mem ~equal:Taint.Kind.equal sanitizer_kinds sanitizer.Taint.kind )
        then (
          L.d_printfln ~color:Green "...but sanitized by %a" Taint.pp
            (Option.value_exn sanitizer_opt) ;
          Ok () )
        else Error () )
      else Ok () )


let check_not_tainted_wrt_sink location (sink, sink_trace) v astate : _ Result.t =
  L.d_printfln "Checking that %a is not tainted" AbstractValue.pp v ;
  match AbductiveDomain.AddressAttributes.get_taint_source_and_sanitizer v astate with
  | None ->
      Ok astate
  | Some ((source, source_hist), sanitizer_opt) -> (
      L.d_printfln ~color:Red "Found source %a, checking policy..." Taint.pp source ;
      match check_policy ~sink ~source ~sanitizer_opt with
      | Ok () ->
          Ok astate
      | Error () ->
          let tainted = Decompiler.find v astate in
          Error
            (ReportableError
               { astate
               ; diagnostic=
                   TaintFlow
                     {tainted; location; source= (source, source_hist); sink= (sink, sink_trace)} }
            ) )


let taint_sinks path location return proc_name actuals astate =
  let tainted = get_tainted sink_matchers return proc_name actuals astate in
  PulseResult.list_fold tainted ~init:astate ~f:(fun astate (sink, ((v, history), _typ)) ->
      let sink_trace = Trace.Immediate {location; history} in
      let astate = AbductiveDomain.AddressAttributes.add_taint_sink path sink sink_trace v astate in
      match check_not_tainted_wrt_sink location (sink, sink_trace) v astate with
      | Ok astate ->
          Ok astate
      | Error report ->
          Recoverable (astate, [report]) )


let call path location return proc_name actuals astate =
  taint_sanitizers return proc_name actuals astate
  |> taint_sources path location return proc_name actuals
  |> taint_sinks path location return proc_name actuals
