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


  let rec exec_assign lhs_access (rhs_exp : HilExp.t) loc astate =
    (* try to evaluate [rhs_exp] down to an abstract address or generate a fresh one *)
    match rhs_exp with
    | AccessExpression rhs_access ->
        PulseDomain.read loc rhs_access astate
        >>= fun (astate, rhs_value) -> PulseDomain.write loc lhs_access rhs_value astate
    | Closure (pname, captured) ->
        PulseDomain.Closures.record loc lhs_access pname captured astate
    | Cast (_, e) ->
        exec_assign lhs_access e loc astate
    | _ ->
        PulseDomain.read_all loc (HilExp.get_access_exprs rhs_exp) astate
        >>= PulseDomain.havoc loc lhs_access


  let exec_call _ret (call : HilInstr.call) (actuals : HilExp.t list) _flags call_loc astate =
    let read_all args astate =
      PulseDomain.read_all call_loc (List.concat_map args ~f:HilExp.get_access_exprs) astate
    in
    match call with
    | Direct callee_pname when is_destructor callee_pname -> (
      match actuals with
      | [AccessExpression (Base (destroyed_var, _))] when Var.is_this destroyed_var ->
          (* do not invalidate [this] when it is destroyed by calls to [this->~Obj()] *)
          Ok astate
      | [AccessExpression destroyed_access] ->
          let destroyed_object = HilExp.AccessExpression.dereference destroyed_access in
          PulseDomain.invalidate
            (CppDestructor (callee_pname, destroyed_object, call_loc))
            call_loc destroyed_object astate
      | _ ->
          Ok astate )
    | Direct callee_pname when Typ.Procname.is_constructor callee_pname -> (
      match actuals with
      | AccessExpression constructor_access :: rest ->
          let constructed_object = HilExp.AccessExpression.dereference constructor_access in
          PulseDomain.havoc call_loc constructed_object astate >>= read_all rest
      | _ ->
          Ok astate )
    | Direct (Typ.Procname.ObjC_Cpp callee_pname)
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname -> (
      match actuals with
      (* We want to assign *lhs to *rhs when rhs is materialized temporary created in constructor *)
      | [AccessExpression lhs; (HilExp.AccessExpression (AddressOf (Base rhs_base)) as rhs_exp)]
        when Var.is_cpp_temporary (fst rhs_base) ->
          let lhs_deref = HilExp.AccessExpression.dereference lhs in
          exec_assign lhs_deref rhs_exp call_loc astate
      (* copy assignment *)
      | [AccessExpression lhs; HilExp.AccessExpression rhs] ->
          let lhs_deref = HilExp.AccessExpression.dereference lhs in
          let rhs_deref = HilExp.AccessExpression.dereference rhs in
          PulseDomain.havoc call_loc lhs_deref astate
          >>= fun astate -> PulseDomain.read call_loc rhs_deref astate >>| fst
      | _ ->
          read_all actuals astate )
    | _ ->
        read_all actuals astate


  let dispatch_call ret (call : HilInstr.call) (actuals : HilExp.t list) flags call_loc astate =
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
        exec_call ret call actuals flags call_loc astate >>| PulseDomain.havoc_var (fst ret)
    | Some model ->
        model call_loc ~ret ~actuals astate


  let exec_instr (astate : PulseDomain.t) {ProcData.extras= summary} _cfg_node (instr : HilInstr.t)
      =
    match instr with
    | Assign (lhs_access, rhs_exp, loc) ->
        exec_assign lhs_access rhs_exp loc astate |> check_error summary
    | Assume (condition, _, _, loc) ->
        PulseDomain.read_all loc (HilExp.get_access_exprs condition) astate |> check_error summary
    | Call (ret, call, actuals, flags, loc) ->
        dispatch_call ret call actuals flags loc astate |> check_error summary
    | ExitScope vars ->
        PulseDomain.remove_vars vars astate


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module HilConfig = LowerHil.DefaultConfig

module DisjunctiveTransferFunctions =
  TransferFunctions.MakeHILDisjunctive
    (PulseTransferFunctions
       (ProcCfg.Exceptional))
       (struct
         type domain_t = PulseDomain.t [@@deriving compare]

         let join_policy = `JoinAfter 1

         let widen_policy = `UnderApproximateAfterNumIterations 5
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
