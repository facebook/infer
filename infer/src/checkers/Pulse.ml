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
  let open PulseDiagnostic in
  Reporting.log_error summary ~loc:(get_location diagnostic) ~ltr:(get_trace diagnostic)
    (get_issue_type diagnostic) (get_message diagnostic)


let check_error summary = function
  | Ok ok ->
      ok
  | Error diagnostic ->
      report summary diagnostic ;
      (* We can also continue the analysis by returning {!PulseDomain.initial} here but there might
         be a risk we would get nonsense. This seems safer for now but TODO. *)
      raise_notrace AbstractDomain.Stop_analysis


module PulseTransferFunctions (CFG : ProcCfg.S) = struct
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


  let rec exec_assign summary (lhs_access : HilExp.AccessExpression.t) (rhs_exp : HilExp.t) loc
      astate =
    (* try to evaluate [rhs_exp] down to an abstract address or generate a fresh one *)
    let crumb = PulseTrace.Assignment {lhs= lhs_access; location= loc} in
    match rhs_exp with
    | AccessExpression rhs_access -> (
        PulseOperations.read loc rhs_access astate
        >>= fun (astate, (rhs_addr, rhs_trace)) ->
        let return_addr_trace = (rhs_addr, crumb :: rhs_trace) in
        PulseOperations.write loc lhs_access return_addr_trace astate
        >>= fun astate ->
        match lhs_access with
        | Base (var, _) when Var.is_return var ->
            PulseOperations.check_address_of_local_variable summary.Summary.proc_desc rhs_addr
              astate
        | _ ->
            Ok astate )
    | Closure (pname, captured) ->
        PulseOperations.Closures.record loc lhs_access pname captured astate
    | Cast (_, e) ->
        exec_assign summary lhs_access e loc astate
    | _ ->
        PulseOperations.read_all loc (HilExp.get_access_exprs rhs_exp) astate
        >>= PulseOperations.havoc [crumb] loc lhs_access


  let exec_call summary _ret (call : HilInstr.call) (actuals : HilExp.t list) _flags call_loc
      astate =
    let read_all args astate =
      PulseOperations.read_all call_loc (List.concat_map args ~f:HilExp.get_access_exprs) astate
    in
    let crumb = PulseTrace.Call {f= `HilCall call; actuals; location= call_loc} in
    match call with
    | Direct callee_pname when is_destructor callee_pname -> (
      match actuals with
      | [AccessExpression (Base (destroyed_var, _))] when Var.is_this destroyed_var ->
          (* do not invalidate [this] when it is destroyed by calls to [this->~Obj()] *)
          Ok astate
      | [AccessExpression destroyed_access] ->
          let destroyed_object = HilExp.AccessExpression.dereference destroyed_access in
          PulseOperations.invalidate
            (CppDestructor (callee_pname, destroyed_object, call_loc))
            call_loc destroyed_object astate
      | _ ->
          Ok astate )
    | Direct callee_pname when Typ.Procname.is_constructor callee_pname -> (
      match actuals with
      | AccessExpression constructor_access :: rest ->
          let constructed_object = HilExp.AccessExpression.dereference constructor_access in
          PulseOperations.havoc [crumb] call_loc constructed_object astate >>= read_all rest
      | _ ->
          Ok astate )
    | Direct (Typ.Procname.ObjC_Cpp callee_pname)
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname -> (
      match actuals with
      (* We want to assign *lhs to *rhs when rhs is materialized temporary created in constructor *)
      | [AccessExpression lhs; HilExp.AccessExpression (AddressOf (Base rhs_base as rhs_exp))]
        when Var.is_cpp_temporary (fst rhs_base) ->
          let lhs_deref = HilExp.AccessExpression.dereference lhs in
          exec_assign summary lhs_deref (HilExp.AccessExpression rhs_exp) call_loc astate
      (* copy assignment *)
      | [AccessExpression lhs; HilExp.AccessExpression rhs] ->
          let lhs_deref = HilExp.AccessExpression.dereference lhs in
          let rhs_deref = HilExp.AccessExpression.dereference rhs in
          PulseOperations.havoc [crumb] call_loc lhs_deref astate
          >>= fun astate -> PulseOperations.read call_loc rhs_deref astate >>| fst
      | _ ->
          read_all actuals astate )
    | _ ->
        read_all actuals astate


  let dispatch_call summary ret (call : HilInstr.call) (actuals : HilExp.t list) flags call_loc
      astate =
    let model =
      match call with
      | Indirect _ ->
          (* function pointer, etc.: skip for now *)
          None
      | Direct callee_pname ->
          PulseModels.dispatch callee_pname flags
    in
    match model with
    | None ->
        exec_call summary ret call actuals flags call_loc astate
        >>| PulseOperations.havoc_var
              [PulseTrace.Call {f= `HilCall call; actuals; location= call_loc}]
              (fst ret)
    | Some model ->
        model call_loc ~ret ~actuals astate


  let exec_instr (astate : PulseDomain.t) {ProcData.extras= summary} _cfg_node (instr : HilInstr.t)
      =
    match instr with
    | Assign (lhs_access, rhs_exp, loc) ->
        exec_assign summary lhs_access rhs_exp loc astate |> check_error summary
    | Assume (condition, _, _, loc) ->
        PulseOperations.read_all loc (HilExp.get_access_exprs condition) astate
        |> check_error summary
    | Call (ret, call, actuals, flags, loc) ->
        dispatch_call summary ret call actuals flags loc astate |> check_error summary
    | ExitScope (vars, _) ->
        PulseOperations.remove_vars vars astate


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module HilConfig = LowerHil.DefaultConfig

module DisjunctiveTransferFunctions =
  TransferFunctions.MakeHILDisjunctive
    (PulseTransferFunctions
       (ProcCfg.Exceptional))
       (struct
         type domain_t = PulseDomain.t [@@deriving compare]

         let join_policy =
           match Config.pulse_max_disjuncts with 0 -> `NeverJoin | n -> `UnderApproximateAfter n


         let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
       end)

module DisjunctiveAnalyzer =
  LowerHil.MakeAbstractInterpreterWithConfig (AbstractInterpreter.MakeWTO) (HilConfig)
    (DisjunctiveTransferFunctions)

let checker {Callbacks.proc_desc; tenv; summary} =
  let proc_data = ProcData.make proc_desc tenv summary in
  PulseDomain.AbstractAddress.init () ;
  ( try
      ignore
        (DisjunctiveAnalyzer.compute_post proc_data
           ~initial:(DisjunctiveTransferFunctions.of_domain PulseDomain.initial))
    with AbstractDomain.Stop_analysis -> () ) ;
  summary
