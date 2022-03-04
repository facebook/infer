(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

let get_as_source proc_name_opt =
  Option.find_map proc_name_opt ~f:(fun proc_name ->
      if String.is_substring ~substring:"inferSecretSource" (Procname.to_string proc_name) then
        Some (Taint.ReturnValue proc_name)
      else None )


let get_as_sink proc_name_opt =
  Option.find_map proc_name_opt ~f:(fun proc_name ->
      if String.is_substring ~substring:"inferSensitiveSink" (Procname.to_string proc_name) then
        Some (Taint.PassedAsArgumentTo proc_name)
      else None )


let call_source path location source (return, _typ) exec_state_res =
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


let call_sink path location sink actuals exec_state_res =
  let actuals =
    List.map actuals ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload} -> arg_payload)
  in
  let taint astate =
    List.fold_result actuals ~init:astate ~f:(fun astate (v, history) ->
        let sink_trace = Trace.Immediate {location; history} in
        AbductiveDomain.AddressAttributes.check_not_tainted path sink sink_trace v astate
        |> Result.map_error ~f:(fun err -> (Decompiler.find v astate, sink_trace, err)) )
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
