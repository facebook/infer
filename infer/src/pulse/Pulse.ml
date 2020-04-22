(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open IResult.Let_syntax
open PulseBasicInterface
open PulseDomainInterface

let report summary diagnostic =
  let open Diagnostic in
  Reporting.log_error summary ~loc:(get_location diagnostic) ~ltr:(get_trace diagnostic)
    (get_issue_type diagnostic) (get_message diagnostic)


let check_error_transform summary ~f = function
  | Ok astate ->
      f astate
  | Error (diagnostic, astate) ->
      if PulseArithmetic.is_unsat astate then []
      else (
        report summary diagnostic ;
        [ExecutionDomain.AbortProgram astate] )


let check_error_continue summary result =
  check_error_transform summary ~f:(fun astate -> [ExecutionDomain.ContinueProgram astate]) result


let proc_name_of_call call_exp =
  match (call_exp : Exp.t) with
  | Const (Cfun proc_name) | Closure {name= proc_name} ->
      Some proc_name
  | _ ->
      None


type get_formals = Procname.t -> (Pvar.t * Typ.t) list option

module PulseTransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = ExecutionDomain

  type extras = get_formals

  let interprocedural_call caller_summary ret call_exp actuals call_loc get_formals astate =
    match proc_name_of_call call_exp with
    | Some callee_pname when not Config.pulse_intraprocedural_only ->
        let formals_opt = get_formals callee_pname in
        PulseOperations.call ~caller_summary call_loc callee_pname ~ret ~actuals ~formals_opt astate
    | _ ->
        L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
        PulseOperations.unknown_call call_loc (SkippedUnknownCall call_exp) ~ret ~actuals
          ~formals_opt:None astate
        |> PulseOperations.ok_continue


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
  let exec_object_out_of_scope call_loc (pvar, typ) exec_state =
    match exec_state with
    | ExecutionDomain.ContinueProgram astate ->
        let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
        let* astate, out_of_scope_base = PulseOperations.eval call_loc (Exp.Lvar pvar) astate in
        (* invalidate [&x] *)
        PulseOperations.invalidate call_loc gone_out_of_scope out_of_scope_base astate
        >>| ExecutionDomain.continue
    | ExecutionDomain.AbortProgram _ | ExecutionDomain.ExitProgram _ ->
        Ok exec_state


  let dispatch_call tenv summary ret call_exp actuals call_loc flags get_formals astate =
    (* evaluate all actuals *)
    let* astate, rev_func_args =
      List.fold_result actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let+ astate, actual_evaled = PulseOperations.eval call_loc actual_exp astate in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    let func_args = List.rev rev_func_args in
    let model =
      match proc_name_of_call call_exp with
      | Some callee_pname ->
          PulseModels.dispatch tenv callee_pname func_args
          |> Option.map ~f:(fun model -> (model, callee_pname))
      | None ->
          (* function pointer, etc.: skip for now *)
          None
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let exec_state_res =
      match model with
      | Some (model, callee_procname) ->
          L.d_printfln "Found model for call@\n" ;
          model ~caller_summary:summary ~callee_procname call_loc ~ret astate
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let only_actuals_evaled =
            List.map
              ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} -> (arg_payload, typ))
              func_args
          in
          let r =
            interprocedural_call summary ret call_exp only_actuals_evaled call_loc get_formals
              astate
          in
          PerfEvent.(log (fun logger -> log_end_event logger ())) ;
          r
    in
    match get_out_of_scope_object call_exp actuals flags with
    | Some pvar_typ ->
        L.d_printfln "%a is going out of scope" Pvar.pp_value (fst pvar_typ) ;
        let* exec_states = exec_state_res in
        List.map exec_states ~f:(fun exec_state ->
            exec_object_out_of_scope call_loc pvar_typ exec_state )
        |> Result.all
    | None ->
        exec_state_res


  let exec_instr (astate : Domain.t) {tenv; ProcData.summary; extras= get_formals} _cfg_node
      (instr : Sil.instr) : Domain.t list =
    match astate with
    | AbortProgram _ ->
        (* We can also continue the analysis with the error state here
           but there might be a risk we would get nonsense. *)
        [astate]
    | ExitProgram _ ->
        (* program already exited, simply propagate the exited state upwards  *)
        [astate]
    | ContinueProgram astate -> (
      match instr with
      | Load {id= lhs_id; e= rhs_exp; loc} ->
          (* [lhs_id := *rhs_exp] *)
          let result =
            let+ astate, rhs_addr_hist = PulseOperations.eval_deref loc rhs_exp astate in
            PulseOperations.write_id lhs_id rhs_addr_hist astate
          in
          check_error_continue summary result
      | Store {e1= lhs_exp; e2= rhs_exp; loc} ->
          (* [*lhs_exp := rhs_exp] *)
          let event = ValueHistory.Assignment loc in
          let result =
            let* astate, (rhs_addr, rhs_history) = PulseOperations.eval loc rhs_exp astate in
            let* astate, lhs_addr_hist = PulseOperations.eval loc lhs_exp astate in
            let* astate =
              PulseOperations.write_deref loc ~ref:lhs_addr_hist
                ~obj:(rhs_addr, event :: rhs_history)
                astate
            in
            match lhs_exp with
            | Lvar pvar when Pvar.is_return pvar ->
                PulseOperations.check_address_escape loc summary.Summary.proc_desc rhs_addr
                  rhs_history astate
            | _ ->
                Ok astate
          in
          check_error_continue summary result
      | Prune (condition, loc, is_then_branch, if_kind) ->
          PulseOperations.prune ~is_then_branch if_kind loc ~condition astate
          |> check_error_transform summary ~f:(fun (exec_state, cond_satisfiable) ->
                 if cond_satisfiable then
                   (* [condition] is true or unknown value: go into the branch *)
                   [Domain.ContinueProgram exec_state]
                 else (* [condition] is known to be unsatisfiable: prune path *)
                   [] )
      | Call (ret, call_exp, actuals, loc, call_flags) ->
          dispatch_call tenv summary ret call_exp actuals loc call_flags get_formals astate
          |> check_error_transform summary ~f:(fun id -> id)
      | Metadata (ExitScope (vars, location)) ->
          let astate = PulseOperations.remove_vars vars location astate in
          check_error_continue summary astate
      | Metadata (VariableLifetimeBegins (pvar, _, location)) ->
          [PulseOperations.realloc_pvar pvar location astate |> Domain.continue]
      | Metadata (Abstract _ | Nullify _ | Skip) ->
          [Domain.ContinueProgram astate] )


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module DisjunctiveAnalyzer =
  AbstractInterpreter.MakeDisjunctive
    (PulseTransferFunctions)
    (struct
      let join_policy =
        match Config.pulse_max_disjuncts with 0 -> `NeverJoin | n -> `UnderApproximateAfter n


      let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
    end)

(* Output cases that sledge was unhappy with in files for later replay or inclusion as sledge test
   cases. We create one file for each PID to avoid all analysis processes racing on writing to the same
   file. *)
let sledge_test_fmt =
  lazy
    (let sledge_test_output =
       Out_channel.create
         ( ResultsDir.get_path Debug
         ^/ Printf.sprintf "sledge_test-%d.ml" (Pid.to_int (Unix.getpid ())) )
     in
     let f = F.formatter_of_out_channel sledge_test_output in
     Epilogues.register ~description:"closing sledge debug fd" ~f:(fun () ->
         F.pp_print_flush f () ;
         Out_channel.close sledge_test_output ) ;
     f )


let checker {Callbacks.exe_env; summary} =
  let tenv = Exe_env.get_tenv exe_env (Summary.get_proc_name summary) in
  AbstractValue.State.reset () ;
  let pdesc = Summary.get_proc_desc summary in
  let initial = [ExecutionDomain.mk_initial pdesc] in
  let get_formals callee_pname =
    Ondemand.get_proc_desc callee_pname |> Option.map ~f:Procdesc.get_pvar_formals
  in
  let proc_data = ProcData.make summary tenv get_formals in
  match DisjunctiveAnalyzer.compute_post proc_data ~initial with
  | Some posts ->
      PulsePayload.update_summary (PulseSummary.of_posts pdesc posts) summary
  | None ->
      summary
  | exception exn ->
      (* output sledge replay tests, see comment on [sledge_test_fmt] *)
      IExn.reraise_if exn ~f:(fun () ->
          match Exn.sexp_of_t exn with
          | List [exn; replay] ->
              let exn = Error.t_of_sexp exn in
              L.internal_error "Analysis of %a FAILED:@\n@[%a@]@\n" Procname.pp
                (Procdesc.get_proc_name pdesc) Error.pp exn ;
              F.fprintf (Lazy.force sledge_test_fmt)
                "@\n\
                \    let%%expect_test _ =@\n\
                \      Equality.replay@\n\
                \        {|%a|} ;@\n\
                \      [%%expect {| |}]@\n\
                 @\n\
                 %!"
                Sexp.pp_hum replay ;
              false
          | _ | (exception _) ->
              (* re-raise original exception *)
              true ) ;
      summary


let () = Sledge.Timer.enabled := Config.sledge_timers
