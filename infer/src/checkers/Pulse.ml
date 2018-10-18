(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open Result.Monad_infix

let check_error summary loc = function
  | Ok astate ->
      astate
  | Error (astate, diagnostic) ->
      let message = PulseDomain.Diagnostic.to_string diagnostic in
      Reporting.log_error summary ~loc IssueType.use_after_lifetime message ;
      astate


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PulseDomain

  type extras = Summary.t

  let dispatch_call ret (call : HilInstr.call) (actuals : HilExp.t list) _flags _call_loc astate =
    let model =
      match call with
      | Indirect _ ->
          (* function pointer, etc.: skip for now *)
          None
      | Direct callee_pname ->
          PulseModels.dispatch callee_pname
    in
    match model with None -> Ok astate | Some model -> model ~ret ~actuals astate


  let exec_instr (astate : PulseDomain.t) {ProcData.extras= summary} _cfg_node (instr : HilInstr.t)
      =
    match instr with
    | Assign (lhs_access, rhs_exp, loc) ->
        (* try to evaluate [rhs_exp] down to an abstract location or generate a fresh one *)
        let rhs_result =
          match rhs_exp with
          | AccessExpression rhs_access ->
              PulseDomain.read rhs_access astate
          | _ ->
              PulseDomain.read_all (HilExp.get_access_exprs rhs_exp) astate
              >>= fun astate -> Ok (astate, PulseDomain.AbstractLocation.mk_fresh ())
        in
        Result.bind rhs_result ~f:(fun (astate, rhs_value) ->
            PulseDomain.write lhs_access rhs_value astate )
        |> check_error summary loc
    | Assume (condition, _, _, loc) ->
        PulseDomain.read_all (HilExp.get_access_exprs condition) astate |> check_error summary loc
    | Call (ret, call, actuals, flags, loc) ->
        PulseDomain.read_all (List.concat_map actuals ~f:HilExp.get_access_exprs) astate
        >>= dispatch_call ret call actuals flags loc
        |> check_error summary loc


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  ignore (Analyzer.compute_post proc_data ~initial:PulseDomain.initial) ;
  summary
