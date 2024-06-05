(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module BasicCost = CostDomain.BasicCost
module BasicCostWithReason = CostDomain.BasicCostWithReason

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module Node = ProcCfg.DefaultNode

type extras_WorstCaseCost =
  { inferbo_invariant_map: BufferOverrunAnalysis.invariant_map
  ; integer_type_widths: IntegerWidths.t
  ; inferbo_get_summary: BufferOverrunAnalysisSummary.get_summary
  ; get_node_nb_exec: Node.t -> BasicCost.t
  ; get_summary: Procname.t -> CostDomain.summary option
  ; get_formals: Procname.t -> (Pvar.t * Typ.t) list option }

let instantiate_cost ?get_closure_callee_cost ~default_closure_cost integer_type_widths
    ~inferbo_caller_mem ~callee_pname ~callee_formals ~args ~captured_vars ~callee_cost ~loc =
  let {BufferOverrunDomain.eval_sym; eval_func_ptrs} =
    BufferOverrunSemantics.mk_eval_sym_cost integer_type_widths callee_formals args captured_vars
      inferbo_caller_mem
  in
  let get_closure_callee_cost pname =
    Option.bind get_closure_callee_cost ~f:(fun get_closure_callee_cost ->
        get_closure_callee_cost pname )
  in
  BasicCostWithReason.subst callee_pname loc callee_cost eval_sym eval_func_ptrs
    get_closure_callee_cost ~default_closure_cost


module InstrBasicCostWithReason = struct
  (*
    Compute the cost for an instruction.
    For example for basic operation we set it to 1 and for function call we take it from the spec of the function.
  *)

  let get_modeled_cost_unless_top ~default modeled_cost =
    if BasicCost.is_top modeled_cost then
      (* If we get Top modeled cost, this could cause
         Top-poisoning up the call chain, so instead, we keep
         underestimating the cost as we do with the case where we
         have no summary below.*)
      default
    else BasicCostWithReason.of_basic_cost modeled_cost


  let dispatch_operation tenv callee_pname callee_cost_opt fun_arg_list get_summary model_env ret
      inferbo_mem =
    match CostModels.Call.dispatch tenv callee_pname fun_arg_list with
    | Some model ->
        let modeled_cost = model {get_summary; model_env= Lazy.force model_env} ~ret inferbo_mem in
        get_modeled_cost_unless_top ~default:BasicCostWithReason.one modeled_cost
    | None -> (
      match callee_cost_opt with
      | Some callee_cost when not (Procname.is_hack_builtins callee_pname) ->
          callee_cost
      | _ ->
          if Config.cost_log_unknown_calls then
            ScubaLogging.log_message ~label:"unmodeled_function_operation_cost"
              ~message:
                (F.asprintf "[Operation Cost] Unmodeled Function: %a" Procname.pp_without_templates
                   callee_pname ) ;
          BasicCostWithReason.one )


  let dispatch_allocation tenv callee_pname callee_cost_opt : BasicCostWithReason.t =
    CostAllocationModels.ProcName.dispatch tenv callee_pname
    |> Option.value_map ~default:(Option.value callee_cost_opt ~default:BasicCostWithReason.zero)
         ~f:(fun modeled_cost ->
           get_modeled_cost_unless_top ~default:BasicCostWithReason.zero modeled_cost )


  let dispatch_func_ptr_call {inferbo_invariant_map; integer_type_widths} instr_node fun_exp =
    BufferOverrunAnalysis.extract_pre (InstrCFG.Node.id instr_node) inferbo_invariant_map
    |> Option.bind ~f:(fun inferbo_mem ->
           let func_ptrs =
             BufferOverrunSemantics.eval integer_type_widths fun_exp inferbo_mem
             |> BufferOverrunDomain.Val.get_func_ptrs
           in
           match FuncPtr.Set.is_singleton_or_more func_ptrs with
           | Singleton (Path path) ->
               let symbolic_cost =
                 BasicCost.of_func_ptr path |> BasicCostWithReason.of_basic_cost
               in
               Some (CostDomain.construct ~f:(fun _ -> symbolic_cost))
           | _ ->
               None )
    |> Option.value ~default:CostDomain.unit_cost_atomic_operation


  let get_call_cost_record tenv
      {inferbo_invariant_map; integer_type_widths; inferbo_get_summary; get_summary; get_formals}
      instr_node callee_pname ret args captured_vars location =
    let fun_arg_list =
      List.map args ~f:(fun (exp, typ) ->
          ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
    in
    let inferbo_mem_opt =
      BufferOverrunAnalysis.extract_pre (InstrCFG.Node.id instr_node) inferbo_invariant_map
    in
    let model_env =
      lazy
        (let node_hash = InstrCFG.Node.hash instr_node in
         BufferOverrunUtils.ModelEnv.mk_model_env callee_pname ~node_hash location tenv
           integer_type_widths inferbo_get_summary )
    in
    let get_callee_cost_opt kind inferbo_mem =
      let default_closure_cost =
        match (kind : CostKind.t) with
        | OperationCost ->
            Ints.NonNegativeInt.one
        | AllocationCost ->
            Ints.NonNegativeInt.zero
      in
      match (get_summary callee_pname, get_formals callee_pname) with
      | Some {CostDomain.post= callee_cost_record}, Some callee_formals ->
          CostDomain.find_opt kind callee_cost_record
          |> Option.map ~f:(fun callee_cost ->
                 let get_closure_callee_cost pname =
                   get_summary pname
                   |> Option.map ~f:(fun {CostDomain.post} -> CostDomain.get_cost_kind kind post)
                 in
                 instantiate_cost ~get_closure_callee_cost ~default_closure_cost integer_type_widths
                   ~inferbo_caller_mem:inferbo_mem ~callee_pname ~callee_formals ~args
                   ~captured_vars ~callee_cost ~loc:location )
      | _ ->
          None
    in
    match inferbo_mem_opt with
    | None ->
        CostDomain.unit_cost_atomic_operation
    | Some inferbo_mem ->
        CostDomain.construct ~f:(fun kind ->
            let callee_cost_opt = get_callee_cost_opt kind inferbo_mem in
            match kind with
            | OperationCost ->
                dispatch_operation tenv callee_pname callee_cost_opt fun_arg_list get_summary
                  model_env ret inferbo_mem
            | AllocationCost ->
                dispatch_allocation tenv callee_pname callee_cost_opt )


  let get_instr_cost_record tenv extras instr_node instr =
    match instr with
    | Sil.Call (ret, Const (Cfun callee_pname), args, location, _) when Config.inclusive_cost ->
        get_call_cost_record tenv extras instr_node callee_pname ret args [] location
    | Sil.Call (ret, Closure {name= callee_pname; captured_vars}, args, location, _)
      when Config.inclusive_cost ->
        get_call_cost_record tenv extras instr_node callee_pname ret args captured_vars location
    | Sil.Call (_, (Const (Cfun _) | Closure _), _, _, _) ->
        CostDomain.zero_record
    | Sil.Call (_, fun_exp, _, _, _) ->
        dispatch_func_ptr_call extras instr_node fun_exp
    | Sil.Load {id= lhs_id} when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a step. In
           JDK 11, dummy deref disappears and causes cost differences
           otherwise. *)
        CostDomain.zero_record
    | Sil.Load _ | Sil.Store _ | Sil.Prune _ ->
        CostDomain.unit_cost_atomic_operation
    | Sil.Metadata
        ( Abstract _
        | CatchEntry _
        | EndBranches
        | ExitScope _
        | Nullify _
        | Skip
        | TryEntry _
        | TryExit _
        | VariableLifetimeBegins _ ) ->
        CostDomain.zero_record


  let get_instr_node_cost_record tenv extras instr_node =
    let instrs = InstrCFG.instrs instr_node in
    let instr =
      match IContainer.singleton_or_more instrs ~fold:Instrs.fold with
      | Empty ->
          Sil.skip_instr
      | Singleton instr ->
          instr
      | More ->
          assert false
    in
    let cost = get_instr_cost_record tenv extras instr_node instr in
    let operation_cost = CostDomain.get_operation_cost cost in
    let log_msg top_or_bottom =
      L.d_printfln_escaped "Statement's operation cost became %s at %a (%a)." top_or_bottom
        InstrCFG.Node.pp_id (InstrCFG.Node.id instr_node)
        (Sil.pp_instr ~print_types:false Pp.text)
        instr
    in
    if BasicCostWithReason.is_top operation_cost then log_msg "top"
    else if BasicCostWithReason.is_unreachable operation_cost then log_msg "unreachable" ;
    cost
end

let compute_errlog_extras cost =
  { Jsonbug_t.cost_polynomial= Some (Format.asprintf "%a" BasicCostWithReason.pp_hum cost)
  ; cost_degree= BasicCostWithReason.degree cost |> Option.map ~f:Polynomials.Degree.encode_to_int
  ; copy_type= None
  ; config_usage_extra= None
  ; taint_extra= None
  ; transitive_callees= []
  ; transitive_missed_captures= [] }


(** Calculate the final Worst Case Cost of the cfg. It is the dot product of the symbolic cost of
    the node and how many times it is executed. *)
module WorstCaseCost = struct
  (** We don't report when the cost is Top as it corresponds to subsequent 'don't know's. Instead,
      we report Top cost only at the top level per function. *)
  let exec_node tenv extras instr_node =
    let {get_node_nb_exec} = extras in
    let instr_cost_record =
      InstrBasicCostWithReason.get_instr_node_cost_record tenv extras instr_node
    in
    let node = InstrCFG.Node.underlying_node instr_node in
    let nb_exec = get_node_nb_exec node in
    if BasicCost.is_top nb_exec then
      L.d_printfln_escaped "Node %a is analyzed to visit infinite (top) times." Node.pp_id
        (Node.id node) ;
    CostDomain.mult_by instr_cost_record ~nb_exec


  let compute tenv extras cfg =
    let init = CostDomain.zero_record in
    let cost =
      InstrCFG.fold_nodes cfg ~init ~f:(fun acc pair ->
          let cost = exec_node tenv extras pair in
          CostDomain.plus acc cost )
    in
    Option.iter (CostDomain.get_operation_cost cost).top_pname_opt ~f:(fun top_pname ->
        if Config.cost_log_unknown_calls then
          ScubaLogging.log_message ~label:"unmodeled_function_top_cost"
            ~message:
              (F.asprintf "[Top Cost] Unmodeled Function: %a" Procname.pp_without_templates
                 top_pname ) ) ;
    cost
end

let is_report_suppressed pname =
  Procname.is_java_access_method pname
  || Procname.is_java_anonymous_inner_class_method pname
  || Procname.is_java_autogen_method pname


module Check = struct
  let mk_report proc_desc pname err_log loc ~name cost =
    let message suffix = F.asprintf "%s of the function `%a` %s" name Procname.pp pname suffix in
    fun issue suffix ->
      Reporting.log_issue proc_desc err_log ~loc
        ~ltr:(BasicCostWithReason.polynomial_traces cost)
        ~extras:(compute_errlog_extras cost) Cost issue (message suffix)


  let report_top_and_unreachable ~report ~unreachable_issue ~infinite_issue cost =
    if BasicCostWithReason.is_top cost then report infinite_issue "cannot be computed"
    else if BasicCostWithReason.is_unreachable cost then
      report unreachable_issue
        "cannot be computed since the program's exit state is never reachable"


  let report_expensive ~report ~expensive_issue cost =
    Option.iter (BasicCostWithReason.degree cost) ~f:(fun degree ->
        if not (Polynomials.Degree.is_constant degree) then
          report expensive_issue "has non-constant and non-top cost" )


  let check_and_report {InterproceduralAnalysis.proc_desc; err_log} cost =
    let pname = Procdesc.get_proc_name proc_desc in
    if not (is_report_suppressed pname) then
      CostIssues.CostKindMap.iter2 CostIssues.enabled_cost_map cost
        ~f:(fun
            _kind
            CostIssues.
              { name
              ; unreachable_issue
              ; infinite_issue
              ; expensive_issue
              ; top_and_unreachable
              ; expensive }
            cost
          ->
          let report = mk_report proc_desc pname err_log (Procdesc.get_loc proc_desc) ~name cost in
          if top_and_unreachable then
            report_top_and_unreachable ~report ~unreachable_issue ~infinite_issue cost ;
          if expensive then report_expensive ~report ~expensive_issue cost )
end

type bound_map = BasicCost.t Node.IdMap.t

type get_node_nb_exec = Node.t -> BasicCost.t

let compute_bound_map tenv proc_desc node_cfg inferbo_invariant_map analyze_dependency : bound_map =
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map = ReachingDefs.compute_invariant_map proc_desc in
  (* collect all prune nodes that occur in loop guards, needed for ControlDepAnalyzer *)
  let loop_head_to_source_nodes = Loop_control.get_loop_head_to_source_nodes node_cfg in
  let control_maps = Loop_control.get_loop_control_maps loop_head_to_source_nodes in
  (* computes the control dependencies: node -> var set *)
  let control_dep_invariant_map = Control.compute_invariant_map proc_desc control_maps in
  (* compute loop invariant map for control var analysis *)
  let loop_head_to_loop_nodes =
    Loop_control.get_loop_head_to_loop_nodes loop_head_to_source_nodes
  in
  let loop_inv_map =
    let get_callee_purity callee_pname =
      match analyze_dependency callee_pname with Ok (_, _, purity) -> purity | Error _ -> None
    in
    LoopInvariant.get_loop_inv_var_map tenv get_callee_purity reaching_defs_invariant_map
      loop_head_to_loop_nodes
  in
  BoundMap.compute_upperbound_map node_cfg inferbo_invariant_map control_dep_invariant_map
    loop_inv_map


let compute_get_node_nb_exec node_cfg bound_map : get_node_nb_exec =
  let debug =
    if Config.write_html then
      let f fmt = L.d_printfln fmt in
      {ConstraintSolver.f}
    else
      let f fmt = L.(debug Analysis Verbose) fmt in
      {ConstraintSolver.f}
  in
  let start_node = NodeCFG.start_node node_cfg in
  AnalysisCallbacks.html_debug_new_node_session start_node
    ~pp_name:(fun fmt -> F.pp_print_string fmt "cost(constraints)")
    ~f:(fun () ->
      let equalities = ConstraintSolver.collect_constraints ~debug node_cfg in
      let () = ConstraintSolver.compute_costs ~debug bound_map equalities in
      ConstraintSolver.get_node_nb_exec equalities )


let get_cost_summary ~is_on_ui_thread astate = {CostDomain.post= astate; is_on_ui_thread}

let just_throws_exception proc_desc =
  Procdesc.get_nodes proc_desc |> List.length <= 5
  && Procdesc.fold_instrs proc_desc ~init:false ~f:(fun acc _node instr ->
         match instr with Sil.Store {e1= Lvar pvar; e2= Exn _} -> Pvar.is_return pvar | _ -> acc )


let checker ({InterproceduralAnalysis.proc_desc; exe_env; analyze_dependency; tenv} as analysis_data)
    =
  let open IOption.Let_syntax in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let+ inferbo_invariant_map =
    BufferOverrunAnalysis.cached_compute_invariant_map
      (InterproceduralAnalysis.bind_payload_opt ~f:snd3 analysis_data)
  in
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  (* given the semantics computes the upper bound on the number of times a node could be executed *)
  let bound_map =
    compute_bound_map tenv proc_desc node_cfg inferbo_invariant_map analyze_dependency
  in
  let is_on_ui_thread =
    (not (Procname.is_objc_method proc_name)) && ConcurrencyModels.runs_on_ui_thread tenv proc_name
  in
  let get_node_nb_exec = compute_get_node_nb_exec node_cfg bound_map in
  let astate =
    let get_summary callee_pname =
      let* cost_summary, _inferbo_summary, _ =
        analyze_dependency callee_pname |> AnalysisResult.to_option
      in
      cost_summary
    in
    let inferbo_get_summary callee_pname =
      let* _cost_summary, inferbo_summary, _ =
        analyze_dependency callee_pname |> AnalysisResult.to_option
      in
      inferbo_summary
    in
    let get_formals callee_pname =
      Attributes.load callee_pname >>| ProcAttributes.get_pvar_formals
    in
    let instr_cfg = InstrCFG.from_pdesc proc_desc in
    let extras =
      { inferbo_invariant_map
      ; inferbo_get_summary
      ; integer_type_widths
      ; get_node_nb_exec
      ; get_summary
      ; get_formals }
    in
    AnalysisCallbacks.html_debug_new_node_session (NodeCFG.start_node node_cfg)
      ~pp_name:(fun f -> F.pp_print_string f "cost(worst-case)")
      ~f:(fun () -> WorstCaseCost.compute tenv extras instr_cfg)
  in
  let () =
    L.(debug Analysis Verbose)
      "@\n[COST ANALYSIS] PROCEDURE '%a' FINAL COST = %a @\n" Procname.pp proc_name
      CostDomain.VariantCostMap.pp astate
  in
  let astate =
    (* Heuristic: if the original function simply throws an exception,
       we don't want to report any execution time complexity increase
       on it to prevent noisy complexity increases. Hence, we set its
       operation cost to 0 which is filtered in differential mode *)
    if just_throws_exception proc_desc then CostDomain.set_operation_cost_zero astate else astate
  in
  Check.check_and_report analysis_data astate ;
  get_cost_summary ~is_on_ui_thread astate
