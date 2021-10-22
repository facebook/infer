(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

let report_topl_errors proc_desc err_log (summary, _) =
  let f = function
    | ContinueProgram astate ->
        PulseTopl.report_errors proc_desc err_log (AbductiveDomain.Topl.get astate)
    | _ ->
        ()
  in
  List.iter ~f summary


let report_unnecessary_copies proc_desc err_log non_disj_astate =
  PulseNonDisjunctiveDomain.get_copied non_disj_astate
  |> List.iter ~f:(fun (var, location) ->
         let diagnostic = Diagnostic.UnnecessaryCopy {variable= var; location} in
         PulseReport.report_non_disj_error proc_desc err_log diagnostic )


module PulseTransferFunctions = struct
  module CFG = ProcCfg.Normal
  module DisjDomain = AbstractDomain.PairDisjunct (ExecutionDomain) (PathContext)
  module NonDisjDomain = NonDisjDomain

  type analysis_data = PulseSummary.t InterproceduralAnalysis.t

  let get_pvar_formals pname = IRAttributes.load pname |> Option.map ~f:Pvar.get_pvar_formals

  let interprocedural_call {InterproceduralAnalysis.analyze_dependency; tenv; proc_desc} path ret
      callee_pname call_exp actuals call_loc (flags : CallFlags.t) astate =
    match callee_pname with
    | Some callee_pname when not Config.pulse_intraprocedural_only ->
        let formals_opt = get_pvar_formals callee_pname in
        let callee_data = analyze_dependency callee_pname in
        PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data call_loc
          callee_pname ~ret ~actuals ~formals_opt astate
    | _ ->
        (* dereference call expression to catch nil issues *)
        let<*> astate, _ =
          if flags.cf_is_objc_block then
            PulseOperations.eval_deref path ~must_be_valid_reason:BlockCall call_loc call_exp astate
          else PulseOperations.eval_deref path call_loc call_exp astate
        in
        L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
        let astate =
          let arg_values = List.map actuals ~f:(fun ((value, _), _) -> value) in
          PulseCallOperations.conservatively_initialize_args arg_values astate
        in
        let<+> astate =
          PulseCallOperations.unknown_call path call_loc (SkippedUnknownCall call_exp) ~ret ~actuals
            ~formals_opt:None astate
        in
        astate


  (** has an object just gone out of scope? *)
  let get_out_of_scope_object callee_pname actuals (flags : CallFlags.t) =
    (* injected destructors are precisely inserted where an object goes out of scope *)
    if flags.cf_injected_destructor then
      match (callee_pname, actuals) with
      | Some (Procname.ObjC_Cpp pname), [(Exp.Lvar pvar, typ)]
        when Pvar.is_local pvar && not (Procname.ObjC_Cpp.is_inner_destructor pname) ->
          (* ignore inner destructors, only trigger out of scope on the final destructor call *)
          Some (pvar, typ)
      | _ ->
          None
    else None


  (** [out_of_scope_access_expr] has just gone out of scope and in now invalid *)
  let exec_object_out_of_scope path call_loc (pvar, typ) exec_state =
    match (exec_state : ExecutionDomain.t) with
    | ContinueProgram astate ->
        let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
        let* astate, out_of_scope_base =
          PulseOperations.eval path NoAccess call_loc (Exp.Lvar pvar) astate
        in
        (* invalidate [&x] *)
        PulseOperations.invalidate path
          (StackAddress (Var.of_pvar pvar, []))
          call_loc gone_out_of_scope out_of_scope_base astate
        >>| ExecutionDomain.continue
    | ISLLatentMemoryError _
    | AbortProgram _
    | ExitProgram _
    | LatentAbortProgram _
    | LatentInvalidAccess _ ->
        Ok exec_state


  let topl_small_step loc procname arguments (return, _typ) exec_state_res =
    let arguments =
      List.map arguments ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload} -> fst arg_payload)
    in
    let return = Var.of_id return in
    let do_astate astate =
      let return = Option.map ~f:fst (Stack.find_opt return astate) in
      let topl_event = PulseTopl.Call {return; arguments; procname} in
      AbductiveDomain.Topl.small_step loc topl_event astate
    in
    let do_one_exec_state (exec_state : ExecutionDomain.t) : ExecutionDomain.t =
      match exec_state with
      | ContinueProgram astate ->
          ContinueProgram (do_astate astate)
      | ISLLatentMemoryError _
      | AbortProgram _
      | LatentAbortProgram _
      | ExitProgram _
      | LatentInvalidAccess _ ->
          exec_state
    in
    List.map ~f:(Result.map ~f:do_one_exec_state) exec_state_res


  let topl_store_step path loc ~lhs ~rhs:_ astate =
    match (lhs : Exp.t) with
    | Lindex (arr, index) ->
        (let open IResult.Let_syntax in
        let* _astate, (aw_array, _history) = PulseOperations.eval path Read loc arr astate in
        let+ _astate, (aw_index, _history) = PulseOperations.eval path Read loc index astate in
        let topl_event = PulseTopl.ArrayWrite {aw_array; aw_index} in
        AbductiveDomain.Topl.small_step loc topl_event astate)
        |> Result.ok (* don't emit Topl event if evals fail *) |> Option.value ~default:astate
    | _ ->
        astate


  let dispatch_call ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path ret
      call_exp actuals call_loc flags astate =
    let<*> astate, callee_pname = PulseOperations.eval_proc_name path call_loc call_exp astate in
    (* special case for objc dispatch models *)
    let callee_pname, actuals =
      match callee_pname with
      | Some callee_pname when ObjCDispatchModels.is_model callee_pname -> (
        match ObjCDispatchModels.get_dispatch_closure_opt actuals with
        | Some (block_name, args) ->
            (Some block_name, args)
        | None ->
            (Some callee_pname, actuals) )
      | _ ->
          (callee_pname, actuals)
    in
    (* evaluate all actuals *)
    let<*> astate, rev_func_args =
      List.fold_result actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let+ astate, actual_evaled = PulseOperations.eval path Read call_loc actual_exp astate in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    let func_args = List.rev rev_func_args in
    let model =
      match callee_pname with
      | Some callee_pname ->
          PulseModels.dispatch tenv callee_pname func_args
          |> Option.map ~f:(fun model -> (model, callee_pname))
      | None ->
          (* unresolved function pointer, etc.: skip *)
          None
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let exec_states_res =
      match model with
      | Some (model, callee_procname) ->
          L.d_printfln "Found model for call@\n" ;
          let astate =
            let arg_values =
              List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= value, _} ->
                  value )
            in
            PulseCallOperations.conservatively_initialize_args arg_values astate
          in
          model {analysis_data; path; callee_procname; location= call_loc; ret} astate
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let only_actuals_evaled =
            List.map func_args ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
                (arg_payload, typ) )
          in
          let r =
            interprocedural_call analysis_data path ret callee_pname call_exp only_actuals_evaled
              call_loc flags astate
          in
          PerfEvent.(log (fun logger -> log_end_event logger ())) ;
          r
    in
    let exec_states_res =
      if Topl.is_active () then
        match callee_pname with
        | Some callee_pname ->
            topl_small_step call_loc callee_pname func_args ret exec_states_res
        | None ->
            (* skip, as above for non-topl *) exec_states_res
      else exec_states_res
    in
    let exec_states_res =
      match get_out_of_scope_object callee_pname actuals flags with
      | Some pvar_typ ->
          L.d_printfln "%a is going out of scope" Pvar.pp_value (fst pvar_typ) ;
          List.map exec_states_res ~f:(fun exec_state ->
              exec_state >>= exec_object_out_of_scope path call_loc pvar_typ )
      | None ->
          exec_states_res
    in
    if Option.exists callee_pname ~f:IRAttributes.is_no_return then
      List.filter_map exec_states_res ~f:(fun exec_state_res ->
          match exec_state_res with
          | Error _ as err ->
              Some err
          | Ok exec_state ->
              PulseSummary.force_exit_program tenv proc_desc err_log call_loc exec_state
              |> Option.map ~f:(fun exec_state -> Ok exec_state) )
    else exec_states_res


  (* [get_dealloc_from_dynamic_types vars_types loc] returns a dealloc procname and vars and
     type needed to execute a call to dealloc for the given variables for which the dynamic type
     is an Objective-C class. *)
  let get_dealloc_from_dynamic_types dynamic_types_unreachable =
    let get_dealloc (var, typ) =
      Typ.name typ
      |> Option.bind ~f:(fun name ->
             let cls_typ = Typ.mk (Typ.Tstruct name) in
             match Var.get_ident var with
             | Some id when Typ.is_objc_class cls_typ ->
                 let ret_id = Ident.create_fresh Ident.knormal in
                 let dealloc = Procname.make_objc_dealloc name in
                 let typ = Typ.mk_ptr cls_typ in
                 Some (ret_id, id, typ, dealloc)
             | _ ->
                 None )
    in
    List.filter_map ~f:get_dealloc dynamic_types_unreachable


  (* In the case of variables that point to Objective-C classes for which we have a dynamic type, we
     add and execute calls to dealloc. The main advantage of adding this calls
     is that some memory could be freed in dealloc, and we would be reporting a leak on it if we
     didn't call it. *)
  let execute_injected_dealloc_calls
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path vars astate
      location =
    let used_ids = Stack.keys astate |> List.filter_map ~f:(fun var -> Var.get_ident var) in
    Ident.update_name_generator used_ids ;
    let call_dealloc (astate_list : ExecutionDomain.t list) (ret_id, id, typ, dealloc) =
      let ret = (ret_id, StdTyp.void) in
      let call_flags = CallFlags.default in
      let call_exp = Exp.Const (Cfun dealloc) in
      let actuals = [(Exp.Var id, typ)] in
      let call_instr = Sil.Call (ret, call_exp, actuals, location, call_flags) in
      L.d_printfln ~color:Pp.Orange "@\nExecuting injected instr:%a@\n@."
        (Sil.pp_instr Pp.text ~print_types:true)
        call_instr ;
      List.concat_map astate_list ~f:(fun (astate : ExecutionDomain.t) ->
          match astate with
          | ISLLatentMemoryError _
          | AbortProgram _
          | ExitProgram _
          | LatentAbortProgram _
          | LatentInvalidAccess _ ->
              [astate]
          | ContinueProgram astate ->
              dispatch_call analysis_data path ret call_exp actuals location call_flags astate
              |> PulseReport.report_exec_results tenv proc_desc err_log location )
    in
    let dynamic_types_unreachable =
      PulseOperations.get_dynamic_type_unreachable_values vars astate
    in
    let dealloc_data = get_dealloc_from_dynamic_types dynamic_types_unreachable in
    let ret_vars = List.map ~f:(fun (ret_id, _, _, _) -> Var.of_id ret_id) dealloc_data in
    L.d_printfln ~color:Pp.Orange
      "Executing injected call to dealloc for vars (%a) that are exiting the scope@."
      (Pp.seq ~sep:"," Var.pp) vars ;
    let astates = List.fold ~f:call_dealloc dealloc_data ~init:[ContinueProgram astate] in
    (astates, ret_vars)


  let exec_instr_aux path (astate : ExecutionDomain.t) (astate_n : NonDisjDomain.t)
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) _cfg_node
      (instr : Sil.instr) : ExecutionDomain.t list * NonDisjDomain.t =
    match astate with
    | AbortProgram _ | ISLLatentMemoryError _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
        ([astate], astate_n)
    | ExitProgram _ ->
        (* program already exited, simply propagate the exited state upwards  *)
        ([astate], astate_n)
    | ContinueProgram astate -> (
      match instr with
      | Load {id= lhs_id; e= rhs_exp; loc} ->
          (* [lhs_id := *rhs_exp] *)
          let deref_rhs astate =
            let results =
              if Config.pulse_isl then
                PulseOperations.eval_deref_isl path loc rhs_exp astate
                |> List.map ~f:(fun result ->
                       let+ astate, rhs_addr_hist = result in
                       PulseOperations.write_id lhs_id rhs_addr_hist astate )
              else
                [ (let+ astate, rhs_addr_hist = PulseOperations.eval_deref path loc rhs_exp astate in
                   PulseOperations.write_id lhs_id rhs_addr_hist astate ) ]
            in
            PulseReport.report_results tenv proc_desc err_log loc results
          in
          let set_global_astates =
            match rhs_exp with
            | Lvar pvar when Pvar.(is_global pvar && (is_const pvar || is_compile_constant pvar))
              -> (
              (* Inline initializers of global constants when they are being used.
                 This addresses nullptr false positives by pruning infeasable paths global_var != global_constant_value,
                 where global_constant_value is the value of global_var *)
              (* TODO: Initial global constants only once *)
              match Pvar.get_initializer_pname pvar with
              | Some proc_name ->
                  L.d_printfln_escaped "Found initializer for %a" (Pvar.pp Pp.text) pvar ;
                  let call_flags = CallFlags.default in
                  let ret_id_void = (Ident.create_fresh Ident.knormal, StdTyp.void) in
                  let no_error_states =
                    dispatch_call analysis_data path ret_id_void (Const (Cfun proc_name)) [] loc
                      call_flags astate
                    |> List.filter_map ~f:(function
                         | Ok (ContinueProgram astate) ->
                             Some astate
                         | _ ->
                             (* ignore errors in global initializers *)
                             None )
                  in
                  if List.is_empty no_error_states then [astate] else no_error_states
              | None ->
                  [astate] )
            | _ ->
                [astate]
          in
          (List.concat_map set_global_astates ~f:deref_rhs, astate_n)
      | Store {e1= lhs_exp; e2= rhs_exp; loc} ->
          (* [*lhs_exp := rhs_exp] *)
          let event =
            match lhs_exp with
            | Lvar v when Pvar.is_return v ->
                ValueHistory.Returned loc
            | _ ->
                ValueHistory.Assignment loc
          in
          let result =
            let<*> astate, (rhs_addr, rhs_history) =
              PulseOperations.eval path NoAccess loc rhs_exp astate
            in
            let<*> is_structured, ls_astate_lhs_addr_hist =
              if Config.pulse_isl then
                PulseOperations.eval_structure_isl path Write loc lhs_exp astate
              else
                let+ astate, lhs_addr_hist = PulseOperations.eval path Write loc lhs_exp astate in
                (false, [Ok (astate, lhs_addr_hist)])
            in
            let write_function lhs_addr_hist astate =
              if is_structured then
                PulseOperations.write_deref_biad_isl path loc ~ref:lhs_addr_hist Dereference
                  ~obj:(rhs_addr, event :: rhs_history)
                  astate
              else
                [ PulseOperations.write_deref path loc ~ref:lhs_addr_hist
                    ~obj:(rhs_addr, event :: rhs_history)
                    astate ]
            in
            let astates =
              List.concat_map ls_astate_lhs_addr_hist ~f:(fun result ->
                  let<*> astate, lhs_addr_hist = result in
                  write_function lhs_addr_hist astate )
            in
            let astates =
              if Topl.is_active () then
                List.map astates ~f:(fun result ->
                    let+ astate = result in
                    topl_store_step path loc ~lhs:lhs_exp ~rhs:rhs_exp astate )
              else astates
            in
            match lhs_exp with
            | Lvar pvar when Pvar.is_return pvar ->
                List.map astates ~f:(fun result ->
                    let* astate = result in
                    PulseOperations.check_address_escape loc proc_desc rhs_addr rhs_history astate )
            | _ ->
                astates
          in
          (PulseReport.report_results tenv proc_desc err_log loc result, astate_n)
      | Prune (condition, loc, _is_then_branch, _if_kind) ->
          ( (let<*> astate = PulseOperations.prune path loc ~condition astate in
             if PulseArithmetic.is_unsat_cheap astate then
               (* [condition] is known to be unsatisfiable: prune path *)
               []
             else
               (* [condition] is true or unknown value: go into the branch *)
               [Ok (ContinueProgram astate)] )
            |> PulseReport.report_exec_results tenv proc_desc err_log loc
          , astate_n )
      | Call (ret, call_exp, actuals, loc, call_flags) ->
          let astates =
            dispatch_call analysis_data path ret call_exp actuals loc call_flags astate
            |> PulseReport.report_exec_results tenv proc_desc err_log loc
          in
          ( astates
          , PulseNonDisjunctiveOperations.add_copies loc call_exp actuals call_flags astates
              astate_n )
      | Metadata (ExitScope (vars, location)) ->
          let remove_vars vars astates =
            List.map astates ~f:(fun (exec_state : ExecutionDomain.t) ->
                match exec_state with
                | ISLLatentMemoryError _
                | AbortProgram _
                | ExitProgram _
                | LatentAbortProgram _
                | LatentInvalidAccess _ ->
                    exec_state
                | ContinueProgram astate ->
                    ContinueProgram (PulseOperations.remove_vars vars location astate) )
          in
          if Procname.is_java (Procdesc.get_proc_name proc_desc) then
            (remove_vars vars [ContinueProgram astate], astate_n)
          else
            (* Here we add and execute calls to dealloc for Objective-C objects
               before removing the variables *)
            let astates, ret_vars =
              execute_injected_dealloc_calls analysis_data path vars astate location
            in
            (* OPTIM: avoid re-allocating [vars] when [ret_vars] is empty
               (in particular if no ObjC objects are involved), but otherwise
               assume [ret_vars] is potentially larger than [vars] and so
               append [vars] to [ret_vars]. *)
            let vars_to_remove =
              if List.is_empty ret_vars then vars else List.rev_append vars ret_vars
            in
            ( remove_vars vars_to_remove astates
            , PulseNonDisjunctiveOperations.mark_modified_copies vars astates astate_n )
      | Metadata (VariableLifetimeBegins (pvar, typ, location)) when not (Pvar.is_global pvar) ->
          ( [PulseOperations.realloc_pvar tenv pvar typ location astate |> ExecutionDomain.continue]
          , astate_n )
      | Metadata
          ( Abstract _
          | CatchEntry _
          | Nullify _
          | Skip
          | TryEntry _
          | TryExit _
          | VariableLifetimeBegins _ ) ->
          ([ContinueProgram astate], astate_n) )


  let exec_instr ((astate, path), astate_n) analysis_data cfg_node instr :
      DisjDomain.t list * NonDisjDomain.t =
    (* Sometimes instead of stopping on contradictions a false path condition is recorded
       instead. Prune these early here so they don't spuriously count towards the disjunct limit. *)
    let astates, astate_n = exec_instr_aux path astate astate_n analysis_data cfg_node instr in
    ( List.filter_map astates ~f:(fun exec_state ->
          if ExecutionDomain.is_unsat_cheap exec_state then None
          else Some (exec_state, PathContext.post_exec_instr path) )
    , astate_n )


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module DisjunctiveAnalyzer =
  AbstractInterpreter.MakeDisjunctive
    (PulseTransferFunctions)
    (struct
      let join_policy = `UnderApproximateAfter Config.pulse_max_disjuncts

      let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
    end)

let with_debug_exit_node proc_desc ~f =
  AnalysisCallbacks.html_debug_new_node_session
    (Procdesc.get_exit_node proc_desc)
    ~pp_name:(fun fmt -> F.pp_print_string fmt "pulse summary creation")
    ~f


let initial tenv proc_desc =
  [ ( ContinueProgram (PulseObjectiveCSummary.mk_initial_with_positive_self tenv proc_desc)
    , PathContext.initial ) ]


let checker ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) =
  AbstractValue.State.reset () ;
  match
    DisjunctiveAnalyzer.compute_post analysis_data
      ~initial:(initial tenv proc_desc, NonDisjDomain.bottom)
      proc_desc
  with
  | Some (posts, non_disj_astate) ->
      (* forget path contexts, we don't propagate them across functions *)
      let posts = List.map ~f:fst posts in
      with_debug_exit_node proc_desc ~f:(fun () ->
          let objc_nil_summary = PulseObjectiveCSummary.mk_nil_messaging_summary tenv proc_desc in
          let summary =
            PulseSummary.of_posts tenv proc_desc err_log
              (Procdesc.get_exit_node proc_desc |> Procdesc.Node.get_loc)
              (Option.to_list objc_nil_summary @ posts)
              non_disj_astate
          in
          report_topl_errors proc_desc err_log summary ;
          report_unnecessary_copies proc_desc err_log non_disj_astate ;
          Some summary )
  | None ->
      None
