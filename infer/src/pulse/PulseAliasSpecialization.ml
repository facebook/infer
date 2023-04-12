(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module FuncArg = ProcnameDispatcher.Call.FuncArg
open PulseBasicInterface

let make_formals_set callee_pname actual_args actual_captured_vars (attributes : ProcAttributes.t) =
  let open IOption.Let_syntax in
  let make_addr_to_formals_map actuals pvar_formals =
    match
      List.fold2 actuals pvar_formals ~init:AbstractValue.Map.empty ~f:(fun map (addr, _) formal ->
          match AbstractValue.Map.find_opt addr map with
          | None ->
              AbstractValue.Map.add addr [formal] map
          | Some formals ->
              AbstractValue.Map.add addr (formal :: formals) map )
    with
    | Unequal_lengths ->
        None
    | Ok map ->
        Some map
  in
  let* arg_addr_to_formals =
    let pvar_formals =
      List.map attributes.formals ~f:(fun (mangled, _, _) -> Pvar.mk mangled callee_pname)
    in
    make_addr_to_formals_map actual_args pvar_formals
  in
  let+ captured_addr_to_formals =
    let pvar_captured = List.map attributes.captured ~f:(fun CapturedVar.{pvar} -> pvar) in
    make_addr_to_formals_map actual_captured_vars pvar_captured
  in
  let addr_to_formals_map =
    AbstractValue.Map.merge
      (fun _ arg_formals captured_formals ->
        match (arg_formals, captured_formals) with
        | None, formals | formals, None ->
            formals
        | Some arg_formals, Some captured_formals ->
            Some (arg_formals @ captured_formals) )
      arg_addr_to_formals captured_addr_to_formals
  in
  AbstractValue.Map.fold
    (fun _ formals set -> match formals with [] | [_] -> set | formals -> formals :: set)
    addr_to_formals_map []


let get_actual_captured_vars callee_pname call_kind captured actuals path call_loc astate =
  let captured_formals =
    List.map captured ~f:(fun CapturedVar.{pvar; typ; capture_mode} ->
        (Var.of_pvar pvar, capture_mode, typ) )
  in
  let astate_captured_vars =
    PulseOperations.get_captured_actuals callee_pname path call_loc ~captured_formals ~call_kind
      ~actuals astate
  in
  let open IOption.Let_syntax in
  let+ astate, captured_vars = PulseOperationResult.sat_ok astate_captured_vars in
  (astate, List.map captured_vars ~f:fst)


let make_specialization callee_pname actuals call_kind path call_loc astate =
  let open IOption.Let_syntax in
  let* callee_pdesc = Procdesc.load callee_pname in
  let attributes = Procdesc.get_attributes callee_pdesc in
  let* _, actual_captured_vars =
    get_actual_captured_vars callee_pname call_kind attributes.captured actuals path call_loc astate
  in
  let actual_args = List.map actuals ~f:fst in
  make_formals_set callee_pname actual_args actual_captured_vars attributes
