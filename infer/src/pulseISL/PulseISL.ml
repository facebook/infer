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
open PulseISLBasicInterface
open PulseISLDomainInterface

module Operations = PulseISLOperations

let report {InterproceduralAnalysis.proc_desc; err_log} diagnostic =
  let open Diagnostic in
  Reporting.log_issue proc_desc err_log ~loc:(get_location diagnostic) ~ltr:(get_trace diagnostic)
    Pil (get_issue_type diagnostic) (get_message diagnostic)


let check_error_transform ?(rep=true) analysis_data ~f = function
  | Ok astate ->
      f astate
  | Error (diagnostic, astate) ->
      if PulseISLArithmetic.is_unsat_expensive astate then []
      else (
        if rep then
        report analysis_data diagnostic ;
        [ExecutionDomain.AbortProgram astate] )


let check_error_continue ?(rep=true) analysis_data result =
  check_error_transform ~rep:rep analysis_data
    ~f:(fun astate -> List.map ~f:(fun ast -> ExecutionDomain.ContinueProgram ast) astate)
    result


let proc_name_of_call call_exp =
  match (call_exp : Exp.t) with
  | Const (Cfun proc_name) | Closure {name= proc_name} ->
      Some proc_name
  | _ ->
      None


module PulseISLTransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = ExecutionDomain

  type analysis_data = PulseISLSummary.t InterproceduralAnalysis.t

  let interprocedural_call {InterproceduralAnalysis.analyze_dependency} ret call_exp actuals
      call_loc astate =
    match proc_name_of_call call_exp with
    | Some callee_pname when not Config.pil_intraprocedural_only ->
        let formals_opt =
          AnalysisCallbacks.get_proc_desc callee_pname |> Option.map ~f:Procdesc.get_pvar_formals
        in
        let n = AbstractValue.get_next () in
        (* FIXME: somehow, the following cmd dec the next addr value. TEMP Fix *)
        let callee_data = analyze_dependency callee_pname in
        let () =  AbstractValue.set_next n in
        Operations.call ~callee_data call_loc callee_pname ~ret ~actuals ~formals_opt astate
    | _ ->
        L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
        Operations.unknown_call call_loc (SkippedUnknownCall call_exp) ~ret ~actuals
          ~formals_opt:None astate
        |> Operations.ok_continue


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
    L.d_printfln "exec_object_out_of_scope@\n" ;
    match exec_state with
    | ExecutionDomain.ContinueProgram astate ->
        let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
        let* astate, out_of_scope_base = Operations.eval call_loc (Exp.Lvar pvar) astate in
        (* invalidate [&x] *)
        Operations.invalidate_post call_loc gone_out_of_scope out_of_scope_base astate
        >>| ExecutionDomain.continue
    | ExecutionDomain.AbortProgram _ | ExecutionDomain.ExitProgram _ ->
        Ok exec_state


  let dispatch_call ({InterproceduralAnalysis.tenv} as analysis_data) ret call_exp actuals call_loc
      flags astate =
    (* evaluate all actuals *)
    let* astate, rev_func_args =
      List.fold_result actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let+ astate, actual_evaled = Operations.eval call_loc actual_exp astate in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    let func_args = List.rev rev_func_args in
    let model =
      match proc_name_of_call call_exp with
      | Some callee_pname ->
          PulseISLModels.dispatch tenv callee_pname func_args
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
          model analysis_data ~callee_procname call_loc ~ret astate
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulseISL interproc call" ())) ;
          let only_actuals_evaled =
            List.map func_args ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
                (arg_payload, typ) )
          in
          let r =
            interprocedural_call analysis_data ret call_exp only_actuals_evaled call_loc astate
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

(* [get_dealloc_from_dynamic_types vars_types loc] returns a dealloc procname and vars and
     type needed to execute a call to dealloc for the given variables for which the dynamic type
     is an Objective-C class. *)
  let get_dealloc_from_dynamic_types dynamic_types_unreachable =
    let get_dealloc (var, name) =
      let cls_typ = Typ.mk (Typ.Tstruct name) in
      match Var.get_ident var with
      | Some id when Typ.is_objc_class cls_typ ->
          let ret_id = Ident.create_fresh Ident.knormal in
          let dealloc = Procname.make_objc_dealloc name in
          let typ = Typ.mk_ptr cls_typ in
          Some (ret_id, id, typ, dealloc)
      | _ ->
          None
    in
    List.filter_map ~f:get_dealloc dynamic_types_unreachable


  (* In the case of variables that point to Objective-C classes for which we have a dynamic type, we
     add and execute calls to dealloc. The main advantage of adding this calls
     is that some memory could be freed in dealloc, and we would be reporting a leak on it if we
     didn't call it. *)
  let execute_injected_dealloc_calls analysis_data vars astate location =
    let used_ids = Stack.keys astate |> List.filter_map ~f:(fun var -> Var.get_ident var) in
    Ident.update_name_generator used_ids ;
    let call_dealloc (astate_list : Domain.t list) (ret_id, id, typ, dealloc) =
      let ret = (ret_id, Typ.void) in
      let call_flags = CallFlags.default in
      let call_exp = Exp.Const (Cfun dealloc) in
      let actuals = [(Exp.Var id, typ)] in
      let call_instr = Sil.Call (ret, call_exp, actuals, location, call_flags) in
      L.d_printfln ~color:Pp.Orange "@\nExecuting injected instr:%a@\n@."
        (Sil.pp_instr Pp.text ~print_types:true)
        call_instr ;
      List.fold
        ~f:(fun astates (astate : Domain.t) ->
          let astate =
            match astate with
            | AbortProgram _ | ExitProgram _ ->
                [astate]
            | ContinueProgram astate ->
                dispatch_call analysis_data ret call_exp actuals location call_flags astate
                |> check_error_transform analysis_data ~f:Fn.id
          in
          List.rev_append astate astates )
        ~init:[] astate_list
    in
    let dynamic_types_unreachable =
      Operations.get_dynamic_type_unreachable_values vars astate
    in
    let dealloc_data = get_dealloc_from_dynamic_types dynamic_types_unreachable in
    let ret_vars = List.map ~f:(fun (ret_id, _, _, _) -> Var.of_id ret_id) dealloc_data in
    L.d_printfln ~color:Pp.Orange
      "Executing injected call to dealloc for vars (%a) that are exiting the scope@."
      (Pp.seq ~sep:"," Var.pp) vars ;
    let astates = List.fold ~f:call_dealloc dealloc_data ~init:[Domain.ContinueProgram astate] in
    (astates, ret_vars)

 
  let exec_instr (astate : Domain.t) ({InterproceduralAnalysis.proc_desc} as analysis_data)
          _cfg_node (instr : Sil.instr) : Domain.t list =
    let procname = Procdesc.get_proc_name proc_desc in
    match astate with
    | AbortProgram _ ->
        (* We can also continue the analysis with the error state here
           but there might be a risk we would get nonsense. *)
        [astate]
    | ExitProgram _ ->
        (* program already exited, simply propagate the exited state upwards  *)
        [astate]
    | ContinueProgram astate -> (
        match astate.status with
          | PostStatus.Er -> [ContinueProgram astate]
          | PostStatus.Ok -> (
      match instr with
        | Load {id= lhs_id; e= rhs_exp; loc} ->
            (* [lhs_id := *rhs_exp] *)
           let result =
             let+ ls_astate_rhs_addr_hist = Operations.eval_deref_biad proc_desc loc rhs_exp astate in
             (*as captured vars do not work, HACK through local*)
             let ls_astate_rhs_addr_hist =
               match rhs_exp with
                 | Lvar pvar when Pvar.is_local pvar && not (Var.Set.mem (Var.of_pvar pvar) astate.AbductiveDomain.local_vars)->
                    let var = (Var.of_pvar pvar) in
                    List.map ls_astate_rhs_addr_hist ~f:(fun (astate, b) ->
                            (AbductiveDomain.add_abducible_var var astate, b))
                 | _ -> ls_astate_rhs_addr_hist
             in
             Operations.write_id_list lhs_id ls_astate_rhs_addr_hist
           in
           check_error_continue analysis_data result
        | Store {e1= lhs_exp; e2= rhs_exp; loc} -> (
            (* [*lhs_exp := rhs_exp] *)
            let event = ValueHistory.Assignment loc in
            let result =
              let* astate, (rhs_addr, rhs_history) = Operations.eval loc rhs_exp astate in
              let* is_biad, ls_astate_lhs_addr_hist = Operations.eval_structure proc_desc loc lhs_exp astate in
              let* astates =
                List.fold ls_astate_lhs_addr_hist ~init:(Ok [])
                    ~f:(fun acc (astate, lhs_addr_hist) ->
                        match acc with
                        | Ok  acc_astates ->
                           (match astate.AbductiveDomain.status with
                            | Ok ->
                               let+ astates =
                                 if is_biad then
                                     Operations.write_deref_biad procname loc ~ref:lhs_addr_hist Dereference ~obj:(rhs_addr, event :: rhs_history) astate
                                 else
                                     let+ astate = Operations.write_deref loc ~ref:lhs_addr_hist ~obj:(rhs_addr, event :: rhs_history) astate in
                                     [astate]
                               in
                               acc_astates@astates
                            | Er -> Ok (acc_astates@[astate])
                           )
                        | Error _ as a -> a)
              in
              match lhs_exp with
              | Lvar pvar when Pvar.is_return pvar ->
                 Operations.check_address_escape_list loc proc_desc rhs_addr rhs_history astates
              | _ ->
                 Ok astates
            in
            check_error_continue analysis_data result )
        | Prune (condition, loc, _is_then_branch, _if_kind) ->
           Operations.prune loc ~condition astate
           |> check_error_transform analysis_data ~f:(fun astate ->
                   if PulseISLArithmetic.is_unsat_cheap astate then
                     (* [condition] is known to be unsatisfiable: prune path *)
                     []
                   else
                     (* [condition] is true or unknown value: go into the branch *)
                     [Domain.ContinueProgram astate] )
        | Call (ret, call_exp, actuals, loc, call_flags) ->
           dispatch_call analysis_data ret call_exp actuals loc call_flags astate
           |> check_error_transform analysis_data ~f:(fun id -> id)
        | Metadata (ExitScope (vars, location)) ->
           let remove_vars vars astates =
             List.fold
                 ~f:(fun astates (astate : Domain.t) ->
                     match astate with
                     | AbortProgram _ | ExitProgram _ ->
                        [astate]
                     | ContinueProgram astate ->
                        let astate =
                          Operations.remove_vars (* proc_desc *) vars location astate
                          |> check_error_continue analysis_data
                        in
                        List.rev_append astate astates )
                 ~init:[] astates
           in
           (* temporal not remove as the EXIT scope has not been properly
            *  let astate = AbductiveDomain.remove_abducible_vars vars astate in *)
           if Procname.is_java (Procdesc.get_proc_name proc_desc) then
             remove_vars vars [Domain.ContinueProgram astate]
           else
             (* Here we add and execute calls to dealloc for Objective-C objects
                before removing the variables *)
             let astates, ret_vars =
               execute_injected_dealloc_calls analysis_data vars astate location
             in
             (* OPTIM: avoid re-allocating [vars] when [ret_vars] is empty
                (in p     articular if no ObjC objects are involved), but otherwise
                assume [ret_vars] is potentially larger than [vars] and so
                append [vars] to [ret_vars]. *)
             let vars_to_remove =
               if List.is_empty ret_vars then vars else List.rev_append vars ret_vars
             in
             remove_vars vars_to_remove astates
        | Metadata (VariableLifetimeBegins (pvar, (* typ *) _, location)) when not (Pvar.is_global pvar)  ->
           [Operations.realloc_pvar pvar location astate |> Domain.continue]
        | Metadata (Abstract _ |  VariableLifetimeBegins _ | Nullify _ | Skip) ->
           [Domain.ContinueProgram astate]
    ))


  let pp_session_name _node fmt = F.pp_print_string fmt "Pil"
end

module DisjunctiveAnalyzer =
  AbstractInterpreter.MakeDisjunctive
    (PulseISLTransferFunctions)
    (struct
      let join_policy =
        match Config.pil_max_disjuncts with 0 -> `NeverJoin | n -> `UnderApproximateAfter n


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


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  AbstractValue.State.reset () ;
  let initial = [ExecutionDomain.mk_initial proc_desc] in
  match DisjunctiveAnalyzer.compute_post analysis_data ~initial proc_desc with
    | Some posts -> (
        let simpl_posts = List.fold posts ~init:[] ~f:(fun r post ->
          match post with
            | ExecutionDomain.ContinueProgram astate -> (
                let post = ExecutionDomain.ContinueProgram (AbductiveDomain.set_max_logvar astate) in
                match astate.AbductiveDomain.status with
                  | AbductiveDomain.PostStatus.Er when (BaseDomain.is_empty (astate.pre :> BaseDomain.t)) -> r
                  | _ -> r@[post] )
            | ExecutionDomain.ExitProgram astate -> r@[ExecutionDomain.ExitProgram (AbductiveDomain.set_max_logvar astate)]
            | ExecutionDomain.AbortProgram astate -> r@[ExecutionDomain.AbortProgram (AbductiveDomain.set_max_logvar astate)] )
        in
        Some (PulseISLSummary.of_posts proc_desc simpl_posts)
  )
  | None ->
      None
  | exception exn ->
      (* output sledge replay tests, see comment on [sledge_test_fmt] *)
      IExn.reraise_if exn ~f:(fun () ->
          match Exn.sexp_of_t exn with
          | List [exn; replay] ->
              let exn = Error.t_of_sexp exn in
              L.internal_error "Analysis of %a FAILED:@\n@[%a@]@\n" Procname.pp
                (Procdesc.get_proc_name proc_desc)
                Error.pp exn ;
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
      None


let () = NS.Timer.enabled := Config.sledge_timers
