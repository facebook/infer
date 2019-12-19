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

  let interprocedural_call caller_summary ret call_exp actuals call_loc astate =
    match proc_name_of_call call_exp with
    | Some callee_pname ->
        PulseOperations.call ~caller_summary call_loc callee_pname ~ret ~actuals astate
    | None ->
        L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
        Ok [PulseOperations.unknown_call call_loc (SkippedUnknownCall call_exp) ~ret ~actuals astate]


  (** has an object just gone out of scope? *)
  let get_out_of_scope_object call_exp actuals (flags : CallFlags.t) =
    (* injected destructors are precisely inserted where an object goes out of scope *)
    if flags.cf_injected_destructor then
      match (proc_name_of_call call_exp, actuals) with
      | Some (Procname.ObjC_Cpp pname), [(Exp.Lvar pvar, typ)]
        when Pvar.is_local pvar && not (Procname.ObjC_Cpp.is_inner_destructor pname) ->
          (* ignore inner destructors, only trigger out of scope on the final destructor call *)
          Some (pvar, typ)
      | _ ->
          None
    else None


  (** [out_of_scope_access_expr] has just gone out of scope and in now invalid *)
  let exec_object_out_of_scope call_loc (pvar, typ) astate =
    let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
    (* invalidate [&x] *)
    PulseOperations.eval call_loc (Exp.Lvar pvar) astate
    >>= fun (astate, out_of_scope_base) ->
    PulseOperations.invalidate call_loc gone_out_of_scope out_of_scope_base astate


  let dispatch_call tenv summary ret call_exp actuals call_loc flags astate =
    (* evaluate all actuals *)
    List.fold_result actuals ~init:(astate, [])
      ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
        PulseOperations.eval call_loc actual_exp astate
        >>| fun (astate, actual_evaled) ->
        ( astate
        , ProcnameDispatcher.Call.FuncArg.
            {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
          :: rev_func_args ) )
    >>= fun (astate, rev_func_args) ->
    let func_args = List.rev rev_func_args in
    let model =
      match proc_name_of_call call_exp with
      | Some callee_pname ->
          PulseModels.dispatch tenv callee_pname func_args
      | None ->
          (* function pointer, etc.: skip for now *)
          None
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let posts =
      match model with
      | Some model ->
          L.d_printfln "Found model for call@\n" ;
          model ~caller_summary:summary call_loc ~ret astate
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let only_actuals_evaled =
            List.map
              ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} -> (arg_payload, typ))
              func_args
          in
          let r = interprocedural_call summary ret call_exp only_actuals_evaled call_loc astate in
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
