(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open Result.Monad_infix
open PulseBasicInterface

let report summary diagnostic =
  let open Diagnostic in
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


let proc_name_of_call call_exp =
  match (call_exp : Exp.t) with
  | Const (Cfun proc_name) | Closure {name= proc_name} ->
      Some proc_name
  | _ ->
      None


module PulseTransferFunctions = struct
  module CFG = ProcCfg.Exceptional
  module Domain = PulseAbductiveDomain

  type extras = unit

  let exec_unknown_call reason ret call actuals _flags call_loc astate =
    let event = ValueHistory.Call {f= reason; location= call_loc; in_call= []} in
    let havoc_ret (ret, _) astate = PulseOperations.havoc_id ret [event] astate in
    match proc_name_of_call call with
    | Some callee_pname when Typ.Procname.is_constructor callee_pname -> (
        L.d_printfln "constructor call detected@." ;
        match actuals with
        | (object_ref, _) :: _ ->
            PulseOperations.havoc_deref call_loc object_ref [event] astate >>| havoc_ret ret
        | _ ->
            Ok (havoc_ret ret astate) )
    | Some (Typ.Procname.ObjC_Cpp callee_pname)
      when Typ.Procname.ObjC_Cpp.is_operator_equal callee_pname -> (
        L.d_printfln "operator= detected@." ;
        match actuals with
        (* copy assignment *)
        | [(lhs, _); _rhs] ->
            PulseOperations.havoc_deref call_loc lhs [event] astate >>| havoc_ret ret
        | _ ->
            Ok (havoc_ret ret astate) )
    | _ ->
        L.d_printfln "skipping unknown procedure@." ;
        Ok (havoc_ret ret astate)


  let interprocedural_call caller_summary ret call_exp actuals flags call_loc astate =
    match proc_name_of_call call_exp with
    | Some callee_pname ->
        PulseOperations.call ~caller_summary call_loc callee_pname ~ret ~actuals astate
    | None ->
        L.d_printfln "Indirect call %a@\n" Exp.pp call_exp ;
        exec_unknown_call (SkippedUnknownCall call_exp) ret call_exp actuals flags call_loc astate
        >>| List.return


  (** has an object just gone out of scope? *)
  let get_out_of_scope_object call_exp actuals (flags : CallFlags.t) =
    (* injected destructors are precisely inserted where an object goes out of scope *)
    if flags.cf_injected_destructor then
      match (proc_name_of_call call_exp, actuals) with
      | Some (Typ.Procname.ObjC_Cpp pname), [(Exp.Lvar pvar, typ)]
        when Pvar.is_local pvar && not (Typ.Procname.ObjC_Cpp.is_inner_destructor pname) ->
          (* ignore inner destructors, only trigger out of scope on the final destructor call *)
          Some (pvar, typ)
      | _ ->
          None
    else None


  (** [out_of_scope_access_expr] has just gone out of scope and in now invalid *)
  let exec_object_out_of_scope call_loc (pvar, typ) astate =
    let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
    (* invalidate both [&x] and [x]: reading either is now forbidden *)
    PulseOperations.eval call_loc (Exp.Lvar pvar) astate
    >>= fun (astate, out_of_scope_base) ->
    PulseOperations.invalidate_deref call_loc gone_out_of_scope out_of_scope_base astate
    >>= PulseOperations.invalidate call_loc gone_out_of_scope out_of_scope_base


  let dispatch_call tenv summary ret call_exp actuals call_loc flags astate =
    (* evaluate all actuals *)
    List.fold_result actuals ~init:(astate, [])
      ~f:(fun (astate, rev_actuals_evaled) (actual_exp, actual_typ) ->
        PulseOperations.eval call_loc actual_exp astate
        >>| fun (astate, actual_evaled) ->
        (astate, (actual_evaled, actual_typ) :: rev_actuals_evaled) )
    >>= fun (astate, rev_actuals_evaled) ->
    let actuals_evaled = List.rev rev_actuals_evaled in
    let model =
      match proc_name_of_call call_exp with
      | Some callee_pname ->
          PulseModels.dispatch tenv callee_pname flags
      | None ->
          (* function pointer, etc.: skip for now *)
          None
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let posts =
      match model with
      | Some model ->
          L.d_printfln "Found model for call@\n" ;
          model ~caller_summary:summary call_loc ~ret ~actuals:actuals_evaled astate
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let r = interprocedural_call summary ret call_exp actuals_evaled flags call_loc astate in
          PerfEvent.(log (fun logger -> log_end_event logger ())) ;
          r
    in
    match get_out_of_scope_object call_exp actuals flags with
    | Some pvar_typ ->
        L.d_printfln "%a is going out of scope" Pvar.pp_value (fst pvar_typ) ;
        posts
        >>= fun posts ->
        List.map posts ~f:(fun astate -> exec_object_out_of_scope call_loc pvar_typ astate)
        |> Result.all
    | None ->
        posts


  let exec_instr (astate : Domain.t) {tenv; ProcData.summary} _cfg_node (instr : Sil.instr) =
    match instr with
    | Load {id= lhs_id; e= rhs_exp; loc} ->
        (* [lhs_id := *rhs_exp] *)
        let result =
          PulseOperations.eval_deref loc rhs_exp astate
          >>| fun (astate, rhs_addr_hist) -> PulseOperations.write_id lhs_id rhs_addr_hist astate
        in
        [check_error summary result]
    | Store {e1= lhs_exp; e2= rhs_exp; loc} ->
        (* [*lhs_exp := rhs_exp] *)
        let event = ValueHistory.Assignment loc in
        let result =
          PulseOperations.eval loc rhs_exp astate
          >>= fun (astate, (rhs_addr, rhs_history)) ->
          PulseOperations.eval loc lhs_exp astate
          >>= fun (astate, lhs_addr_hist) ->
          PulseOperations.write_deref loc ~ref:lhs_addr_hist
            ~obj:(rhs_addr, event :: rhs_history)
            astate
          >>= fun astate ->
          match lhs_exp with
          | Lvar pvar when Pvar.is_return pvar ->
              PulseOperations.check_address_escape loc summary.Summary.proc_desc rhs_addr
                rhs_history astate
          | _ ->
              Ok astate
        in
        [check_error summary result]
    | Prune (condition, loc, is_then_branch, if_kind) ->
        let post, cond_satisfiable =
          PulseOperations.prune ~is_then_branch if_kind loc ~condition astate |> check_error summary
        in
        if cond_satisfiable then (* [condition] is true or unknown value: go into the branch *)
          [post]
        else (* [condition] is known to be unsatisfiable: prune path *) []
    | Call (ret, call_exp, actuals, loc, call_flags) ->
        dispatch_call tenv summary ret call_exp actuals loc call_flags astate |> check_error summary
    | Metadata (ExitScope (vars, location)) ->
        [PulseOperations.remove_vars vars location astate]
    | Metadata (VariableLifetimeBegins (pvar, _, location)) ->
        [PulseOperations.realloc_pvar pvar location astate]
    | Metadata (Abstract _ | Nullify _ | Skip) ->
        [astate]


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module DisjunctiveTransferFunctions =
  TransferFunctions.MakeDisjunctive
    (PulseTransferFunctions)
    (struct
      let join_policy =
        match Config.pulse_max_disjuncts with 0 -> `NeverJoin | n -> `UnderApproximateAfter n


      let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
    end)

module DisjunctiveAnalyzer = AbstractInterpreter.MakeWTO (DisjunctiveTransferFunctions)

let checker {Callbacks.exe_env; summary} =
  let tenv = Exe_env.get_tenv exe_env (Summary.get_proc_name summary) in
  let proc_data = ProcData.make summary tenv () in
  AbstractValue.init () ;
  let pdesc = Summary.get_proc_desc summary in
  let initial =
    DisjunctiveTransferFunctions.Disjuncts.singleton (PulseAbductiveDomain.mk_initial pdesc)
  in
  match DisjunctiveAnalyzer.compute_post proc_data ~initial with
  | Some posts ->
      PulsePayload.update_summary
        (PulseSummary.of_posts pdesc (DisjunctiveTransferFunctions.Disjuncts.elements posts))
        summary
  | None ->
      summary
  | exception AbstractDomain.Stop_analysis ->
      summary
