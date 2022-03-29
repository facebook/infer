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

type arg_indices = Positions of int list | Any

let arg_indices_of_list arg_indices =
  if List.is_empty arg_indices then Any else Positions arg_indices


type simple_matcher =
  | ProcedureName of {name: string; arg_indices: arg_indices}
  | ProcedureNameRegex of {name_regex: Str.regexp; arg_indices: arg_indices}

let simple_matcher_of_config ~can_have_arg_indices ~option_name config =
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
      | {arg_indices= _ :: _} when not can_have_arg_indices ->
          L.die UserError
            "When parsing option %s: Unexpected JSON format: Cannot have \"arg_indices\" in this \
             specification but got \"[%a]\"."
            option_name (Pp.seq ~sep:"," F.pp_print_int) matcher.arg_indices
      | {procedure= Some name; procedure_regex= None; arg_indices} ->
          ProcedureName {name; arg_indices= arg_indices_of_list arg_indices}
      | {procedure= None; procedure_regex= Some name_regex; arg_indices} ->
          ProcedureNameRegex
            {name_regex= Str.regexp name_regex; arg_indices= arg_indices_of_list arg_indices} )


let sources_matchers =
  simple_matcher_of_config ~can_have_arg_indices:false ~option_name:"--pulse-source-matchers"
    (Yojson.Basic.to_string Config.pulse_simple_sources)


let sinks_matchers =
  simple_matcher_of_config ~can_have_arg_indices:true ~option_name:"--pulse-sink-matchers"
    (Yojson.Basic.to_string Config.pulse_simple_sinks)


let matches_simple matchers proc_name =
  let proc_name_s = Procname.to_string proc_name in
  (* handle [arg_indices] *)
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
      Option.some_if matches (proc_name, matcher) )


let get_as_source proc_name_opt =
  Option.find_map proc_name_opt ~f:(fun proc_name -> matches_simple sources_matchers proc_name)


let get_as_sink proc_name_opt =
  Option.find_map proc_name_opt ~f:(fun proc_name -> matches_simple sinks_matchers proc_name)


let call_source path location (source_proc_name, _source_matcher) (return, _typ) exec_state_res =
  let source = Taint.ReturnValue source_proc_name in
  let return = Var.of_id return in
  let taint astate =
    Stack.find_opt return astate
    |> Option.fold ~init:astate ~f:(fun astate (return_val, _) ->
           let hist =
             ValueHistory.singleton (TaintSource (source, location, path.PathContext.timestamp))
           in
           AbductiveDomain.AddressAttributes.add_one return_val (Tainted (source, hist)) astate )
  in
  let one_exec_state (exec_state : ExecutionDomain.t) : ExecutionDomain.t =
    match exec_state with
    | ContinueProgram astate ->
        ContinueProgram (taint astate)
    | ISLLatentMemoryError _
    | AbortProgram _
    | LatentAbortProgram _
    | ExitProgram _
    | ExceptionRaised _
    | LatentInvalidAccess _ ->
        exec_state
  in
  List.map ~f:(PulseResult.map ~f:one_exec_state) exec_state_res


let call_sink path location (sink_proc_name, sink_matcher) actuals exec_state_res =
  let sink = Taint.PassedAsArgumentTo sink_proc_name in
  let actuals =
    List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload} -> arg_payload)
  in
  let taint astate =
    IList.foldi_result actuals ~init:astate ~f:(fun i astate (v, history) ->
        let matches_arg =
          match sink_matcher with
          | ProcedureName {arg_indices= Any} | ProcedureNameRegex {arg_indices= Any} ->
              true
          | ProcedureName {arg_indices= Positions indices}
          | ProcedureNameRegex {arg_indices= Positions indices} ->
              List.mem ~equal:Int.equal indices i
        in
        if matches_arg then
          let sink_trace = Trace.Immediate {location; history} in
          AbductiveDomain.AddressAttributes.check_not_tainted path sink sink_trace v astate
          |> Result.map_error ~f:(fun err -> (Decompiler.find v astate, sink_trace, err))
        else Ok astate )
  in
  let one_exec_state (exec_state : ExecutionDomain.t) : ExecutionDomain.t AccessResult.t =
    match exec_state with
    | ContinueProgram astate -> (
      match taint astate with
      | Ok astate ->
          Ok (ContinueProgram astate)
      | Error (tainted, sink_trace, source) ->
          Recoverable
            ( ContinueProgram astate
            , [ ReportableError
                  { astate
                  ; diagnostic= TaintFlow {tainted; location; source; sink= (sink, sink_trace)} } ]
            ) )
    | ISLLatentMemoryError _
    | AbortProgram _
    | LatentAbortProgram _
    | ExitProgram _
    | ExceptionRaised _
    | LatentInvalidAccess _ ->
        Ok exec_state
  in
  List.map exec_state_res ~f:(PulseResult.bind ~f:one_exec_state)


let call path location proc_name_opt actuals ret exec_state_res =
  let exec_state_res =
    match get_as_source proc_name_opt with
    | None ->
        exec_state_res
    | Some source ->
        call_source path location source ret exec_state_res
  in
  let exec_state_res =
    match get_as_sink proc_name_opt with
    | None ->
        exec_state_res
    | Some sink ->
        call_sink path location sink actuals exec_state_res
  in
  exec_state_res
