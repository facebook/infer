(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
open Result.Monad_infix

let report summary diagnostic =
  let open PulseDomain.Diagnostic in
  Reporting.log_error summary ~loc:(get_location diagnostic) ~ltr:(get_trace diagnostic)
    (get_issue_type diagnostic) (get_message diagnostic)


let check_error summary = function
  | Ok astate ->
      astate
  | Error diagnostic ->
      report summary diagnostic ;
      (* We can also continue the analysis by returning {!PulseDomain.initial} here but there might
         be a risk we would get nonsense. This seems safer for now but TODO. *)
      raise_notrace AbstractDomain.Stop_analysis


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PulseDomain

  type extras = Summary.t

  let is_destructor = function
    | Typ.Procname.ObjC_Cpp clang_pname ->
        Typ.Procname.ObjC_Cpp.is_destructor clang_pname
        && not
             (* Our frontend generates synthetic inner destructors to model invocation of base class
           destructors correctly; see D5834555/D7189239 *)
             (Typ.Procname.ObjC_Cpp.is_inner_destructor clang_pname)
    | _ ->
        false


  let exec_call _ret (call : HilInstr.call) (actuals : HilExp.t list) _flags call_loc astate =
    match call with
    | Direct callee_pname when is_destructor callee_pname -> (
      match actuals with
      | [AccessExpression destroyed_access] ->
          PulseDomain.invalidate
            (CppDestructor (callee_pname, destroyed_access))
            call_loc destroyed_access astate
      | _ ->
          Ok astate )
    | _ ->
        PulseDomain.read_all call_loc (List.concat_map actuals ~f:HilExp.get_access_exprs) astate


  let dispatch_call ret (call : HilInstr.call) (actuals : HilExp.t list) _flags call_loc astate =
    let model =
      match call with
      | Indirect _ ->
          (* function pointer, etc.: skip for now *)
          None
      | Direct callee_pname ->
          PulseModels.dispatch callee_pname
    in
    match model with
    | None ->
        exec_call ret call actuals _flags call_loc astate >>| PulseDomain.havoc (fst ret)
    | Some model ->
        model call_loc ~ret ~actuals astate


  let exec_instr (astate : PulseDomain.t) {ProcData.extras= summary} _cfg_node (instr : HilInstr.t)
      =
    match instr with
    | Assign (lhs_access, rhs_exp, loc) ->
        (* try to evaluate [rhs_exp] down to an abstract address or generate a fresh one *)
        let rhs_result =
          match rhs_exp with
          | AccessExpression rhs_access ->
              PulseDomain.read loc rhs_access astate
          | _ ->
              PulseDomain.read_all loc (HilExp.get_access_exprs rhs_exp) astate
              >>= fun astate -> Ok (astate, PulseDomain.AbstractAddress.mk_fresh ())
        in
        Result.bind rhs_result ~f:(fun (astate, rhs_value) ->
            PulseDomain.write loc lhs_access rhs_value astate )
        |> check_error summary
    | Assume (condition, _, _, loc) ->
        PulseDomain.read_all loc (HilExp.get_access_exprs condition) astate |> check_error summary
    | Call (ret, call, actuals, flags, loc) ->
        dispatch_call ret call actuals flags loc astate |> check_error summary


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Exceptional) (TransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  ( try ignore (Analyzer.compute_post proc_data ~initial:PulseDomain.initial)
    with AbstractDomain.Stop_analysis -> () ) ;
  summary
