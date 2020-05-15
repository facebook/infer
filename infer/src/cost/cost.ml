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

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module Node = ProcCfg.DefaultNode

type extras_WorstCaseCost =
  { inferbo_invariant_map: BufferOverrunAnalysis.invariant_map
  ; integer_type_widths: Typ.IntegerWidths.t
  ; get_node_nb_exec: Node.id -> BasicCost.t
  ; get_summary: Procname.t -> CostDomain.summary option
  ; get_formals: Procname.t -> (Pvar.t * Typ.t) list option }

let instantiate_cost integer_type_widths ~inferbo_caller_mem ~callee_pname ~callee_formals ~params
    ~callee_cost ~loc =
  let eval_sym =
    BufferOverrunSemantics.mk_eval_sym_cost integer_type_widths callee_formals params
      inferbo_caller_mem
  in
  BasicCost.subst callee_pname loc callee_cost eval_sym


module InstrBasicCost = struct
  (*
    Compute the cost for an instruction.
    For example for basic operation we set it to 1 and for function call we take it from the spec of the function.
  *)

  let allocation_functions =
    [ BuiltinDecl.__new
    ; BuiltinDecl.__new_array
    ; BuiltinDecl.__objc_alloc_no_fail
    ; BuiltinDecl.malloc
    ; BuiltinDecl.malloc_no_fail ]


  let is_allocation_function callee_pname =
    List.exists allocation_functions ~f:(fun f -> Procname.equal callee_pname f)


  let get_instr_cost_record tenv extras instr_node instr =
    match instr with
    | Sil.Call (ret, Exp.Const (Const.Cfun callee_pname), params, _, _) when Config.inclusive_cost
      ->
        let {inferbo_invariant_map; integer_type_widths; get_summary; get_formals} = extras in
        let operation_cost =
          match
            BufferOverrunAnalysis.extract_pre (InstrCFG.Node.id instr_node) inferbo_invariant_map
          with
          | None ->
              CostDomain.unit_cost_atomic_operation
          | Some inferbo_mem -> (
              let loc = InstrCFG.Node.loc instr_node in
              let fun_arg_list =
                List.map params ~f:(fun (exp, typ) ->
                    ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
              in
              match CostModels.Call.dispatch tenv callee_pname fun_arg_list with
              | Some model ->
                  let node_hash = InstrCFG.Node.hash instr_node in
                  let model_env =
                    BufferOverrunUtils.ModelEnv.mk_model_env callee_pname ~node_hash loc tenv
                      integer_type_widths
                  in
                  CostDomain.of_operation_cost (model model_env ~ret inferbo_mem)
              | None -> (
                match Tenv.get_summary_formals tenv ~get_summary ~get_formals callee_pname with
                | `Found ({CostDomain.post= callee_cost_record}, callee_formals) ->
                    CostDomain.map callee_cost_record ~f:(fun callee_cost ->
                        instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
                          ~callee_pname ~callee_formals ~params ~callee_cost ~loc )
                | `FoundFromSubclass
                    (callee_pname, {CostDomain.post= callee_cost_record}, callee_formals) ->
                    (* Note: It ignores top cost of subclass to avoid its propagations. *)
                    let instantiated_cost =
                      CostDomain.map callee_cost_record ~f:(fun callee_cost ->
                          instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
                            ~callee_pname ~callee_formals ~params ~callee_cost ~loc )
                    in
                    if BasicCost.is_top (CostDomain.get_operation_cost instantiated_cost) then
                      CostDomain.unit_cost_atomic_operation
                    else instantiated_cost
                | `NotFound ->
                    CostDomain.unit_cost_atomic_operation ) )
        in
        if is_allocation_function callee_pname then
          CostDomain.plus CostDomain.unit_cost_allocation operation_cost
        else operation_cost
    | Sil.Call (_, Exp.Const (Const.Cfun _), _, _, _) ->
        CostDomain.zero_record
    | Sil.Load {id= lhs_id} when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a step. In
           JDK 11, dummy deref disappears and causes cost differences
           otherwise. *)
        CostDomain.zero_record
    | Sil.Load _ | Sil.Store _ | Sil.Call _ | Sil.Prune _ ->
        CostDomain.unit_cost_atomic_operation
    | Sil.Metadata Skip -> (
      match InstrCFG.Node.kind instr_node with
      | Procdesc.Node.Start_node ->
          CostDomain.unit_cost_atomic_operation
      | _ ->
          CostDomain.zero_record )
    | Sil.Metadata (Abstract _ | ExitScope _ | Nullify _ | VariableLifetimeBegins _) ->
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
      Logging.d_printfln_escaped "Statement cost became %s at %a (%a)." top_or_bottom
        InstrCFG.Node.pp_id (InstrCFG.Node.id instr_node)
        (Sil.pp_instr ~print_types:false Pp.text)
        instr
    in
    if BasicCost.is_top operation_cost then log_msg "top"
    else if BasicCost.is_unreachable operation_cost then log_msg "unreachable" ;
    cost
end

let compute_errlog_extras cost =
  { Jsonbug_t.cost_polynomial= Some (Format.asprintf "%a" BasicCost.pp_hum cost)
  ; cost_degree= BasicCost.degree cost |> Option.map ~f:Polynomials.Degree.encode_to_int
  ; nullsafe_extra= None }


module ThresholdReports = struct
  type threshold_or_report =
    | Threshold of BasicCost.t
    | ReportOn of {location: Location.t; cost: BasicCost.t}
    | NoReport

  type t = threshold_or_report CostIssues.CostKindMap.t

  let none : t = CostIssues.CostKindMap.empty

  let config =
    CostIssues.CostKindMap.fold
      (fun kind kind_spec acc ->
        match kind_spec with
        | CostIssues.{threshold= Some threshold} ->
            CostIssues.CostKindMap.add kind (Threshold (BasicCost.of_int_exn threshold)) acc
        | _ ->
            acc )
      CostIssues.enabled_cost_map none
end

(** Calculate the final Worst Case Cost predicted for each cost field and each WTO component. It is
    the dot product of the symbolic cost of the node and how many times it is executed. *)
module WorstCaseCost = struct
  type astate = {costs: CostDomain.t; reports: ThresholdReports.t}

  (** We don't report when the cost is Top as it corresponds to subsequent 'don't know's. Instead,
      we report Top cost only at the top level per function. *)
  let exec_node tenv {costs; reports} extras instr_node =
    let {get_node_nb_exec} = extras in
    let node_cost =
      let instr_cost_record = InstrBasicCost.get_instr_node_cost_record tenv extras instr_node in
      let node_id = InstrCFG.Node.underlying_node instr_node |> Node.id in
      let nb_exec = get_node_nb_exec node_id in
      if BasicCost.is_top nb_exec then
        Logging.d_printfln_escaped "Node %a is analyzed to visit infinite (top) times." Node.pp_id
          node_id ;
      CostDomain.mult_by instr_cost_record ~nb_exec
    in
    let costs = CostDomain.plus costs node_cost in
    let reports =
      CostIssues.CostKindMap.merge
        (fun _kind threshold_or_report_opt cost_opt ->
          match (threshold_or_report_opt, cost_opt) with
          | Some ThresholdReports.NoReport, _ ->
              threshold_or_report_opt
          | Some ThresholdReports.(Threshold _ | ReportOn _), Some cost when BasicCost.is_top cost
            ->
              Some ThresholdReports.NoReport
          | Some (ThresholdReports.Threshold threshold), Some cost
            when not (BasicCost.leq ~lhs:cost ~rhs:threshold) ->
              Some (ThresholdReports.ReportOn {location= InstrCFG.Node.loc instr_node; cost})
          | Some (ThresholdReports.ReportOn {cost= prev}), Some cost
            when BasicCost.compare_by_degree prev cost < 0 ->
              Some (ThresholdReports.ReportOn {location= InstrCFG.Node.loc instr_node; cost})
          | _ ->
              threshold_or_report_opt )
        reports costs
    in
    {costs; reports}


  let rec exec_partition tenv astate extras
      (partition : InstrCFG.Node.t WeakTopologicalOrder.Partition.t) =
    match partition with
    | Empty ->
        astate
    | Node {node; next} ->
        let astate = exec_node tenv astate extras node in
        exec_partition tenv astate extras next
    | Component {head; rest; next} ->
        let {costs; reports} = astate in
        let {costs} = exec_partition tenv {costs; reports= ThresholdReports.none} extras rest in
        (* Execute head after the loop body to always report at loop head *)
        let astate = exec_node tenv {costs; reports} extras head in
        exec_partition tenv astate extras next


  let compute tenv extras instr_cfg_wto =
    let initial = {costs= CostDomain.zero_record; reports= ThresholdReports.config} in
    exec_partition tenv initial extras instr_cfg_wto
end

let is_report_suppressed pname =
  Procname.is_java_access_method pname
  || Procname.is_java_anonymous_inner_class_method pname
  || Procname.is_java_autogen_method pname


module Check = struct
  let report_threshold pname proc_desc err_log ~name ~location ~cost CostIssues.{expensive_issue}
      ~threshold ~is_on_ui_thread =
    let report_issue_type =
      L.(debug Analysis Medium) "@\n\n++++++ Checking error type for %a **** @\n" Procname.pp pname ;
      expensive_issue ~is_on_ui_thread
    in
    let bigO_str =
      Format.asprintf ", %a"
        (BasicCost.pp_degree ~only_bigO:true)
        (BasicCost.get_degree_with_term cost)
    in
    let degree_str = BasicCost.degree_str cost in
    let message =
      F.asprintf
        "%s from the beginning of the function up to this program point is likely above the \
         acceptable threshold of %d (estimated cost %a%s)"
        name threshold BasicCost.pp_hum cost degree_str
    in
    let cost_trace_elem =
      let cost_desc =
        F.asprintf "with estimated cost %a%s%s" BasicCost.pp_hum cost bigO_str degree_str
      in
      Errlog.make_trace_element 0 location cost_desc []
    in
    Reporting.log_error proc_desc err_log ~loc:location
      ~ltr:(cost_trace_elem :: BasicCost.polynomial_traces cost)
      ~extras:(compute_errlog_extras cost) report_issue_type message


  let report_top_and_unreachable pname proc_desc err_log loc ~name ~cost
      {CostIssues.unreachable_issue; infinite_issue} =
    let report issue suffix =
      let message = F.asprintf "%s of the function %a %s" name Procname.pp pname suffix in
      Reporting.log_error proc_desc err_log ~loc
        ~ltr:(BasicCost.polynomial_traces cost)
        ~extras:(compute_errlog_extras cost) issue message
    in
    if BasicCost.is_top cost then report infinite_issue "cannot be computed"
    else if BasicCost.is_unreachable cost then
      report unreachable_issue
        "cannot be computed since the program's exit state is never reachable"


  let check_and_report {InterproceduralAnalysis.proc_desc; err_log} ~is_on_ui_thread
      {WorstCaseCost.costs; reports} =
    let pname = Procdesc.get_proc_name proc_desc in
    let proc_loc = Procdesc.get_loc proc_desc in
    if not (is_report_suppressed pname) then (
      CostIssues.CostKindMap.iter2 CostIssues.enabled_cost_map reports
        ~f:(fun _kind (CostIssues.{name; threshold} as kind_spec) -> function
        | ThresholdReports.Threshold _ | ThresholdReports.NoReport ->
            ()
        | ThresholdReports.ReportOn {location; cost} ->
            report_threshold pname proc_desc err_log ~name ~location ~cost kind_spec
              ~threshold:(Option.value_exn threshold) ~is_on_ui_thread ) ;
      CostIssues.CostKindMap.iter2 CostIssues.enabled_cost_map costs
        ~f:(fun _kind (CostIssues.{name; top_and_unreachable} as issue_spec) cost ->
          if top_and_unreachable then
            report_top_and_unreachable pname proc_desc err_log proc_loc ~name ~cost issue_spec ) )
end

type bound_map = BasicCost.t Node.IdMap.t

type get_node_nb_exec = Node.id -> BasicCost.t

let compute_bound_map node_cfg inferbo_invariant_map control_dep_invariant_map loop_invmap :
    bound_map =
  BoundMap.compute_upperbound_map node_cfg inferbo_invariant_map control_dep_invariant_map
    loop_invmap


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


let compute_worst_case_cost tenv integer_type_widths get_summary get_formals instr_cfg_wto
    inferbo_invariant_map get_node_nb_exec =
  let extras =
    {inferbo_invariant_map; integer_type_widths; get_node_nb_exec; get_summary; get_formals}
  in
  WorstCaseCost.compute tenv extras instr_cfg_wto


let get_cost_summary ~is_on_ui_thread astate =
  {CostDomain.post= astate.WorstCaseCost.costs; is_on_ui_thread}


let checker ({InterproceduralAnalysis.proc_desc; exe_env; analyze_dependency} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let inferbo_invariant_map =
    BufferOverrunAnalysis.cached_compute_invariant_map
      (InterproceduralAnalysis.bind_payload ~f:snd3 analysis_data)
  in
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map = ReachingDefs.compute_invariant_map proc_desc in
  (* collect all prune nodes that occur in loop guards, needed for ControlDepAnalyzer *)
  let control_maps, loop_head_to_loop_nodes = Loop_control.get_loop_control_maps node_cfg in
  (* computes the control dependencies: node -> var set *)
  let control_dep_invariant_map = Control.compute_invariant_map proc_desc control_maps in
  (* compute loop invariant map for control var analysis *)
  let loop_inv_map =
    let get_callee_purity callee_pname =
      match analyze_dependency callee_pname with Some (_, (_, _, purity)) -> purity | _ -> None
    in
    LoopInvariant.get_loop_inv_var_map tenv get_callee_purity reaching_defs_invariant_map
      loop_head_to_loop_nodes
  in
  (* given the semantics computes the upper bound on the number of times a node could be executed *)
  let bound_map =
    compute_bound_map node_cfg inferbo_invariant_map control_dep_invariant_map loop_inv_map
  in
  let is_on_ui_thread = ConcurrencyModels.runs_on_ui_thread tenv proc_name in
  let get_node_nb_exec = compute_get_node_nb_exec node_cfg bound_map in
  let astate =
    let get_summary callee_pname =
      match analyze_dependency callee_pname with
      | Some (_, (payload, _, _)) ->
          payload
      | None ->
          None
    in
    let get_formals callee_pname =
      AnalysisCallbacks.get_proc_desc callee_pname |> Option.map ~f:Procdesc.get_pvar_formals
    in
    let instr_cfg = InstrCFG.from_pdesc proc_desc in
    let instr_cfg_wto = InstrCFG.wto instr_cfg in
    compute_worst_case_cost tenv integer_type_widths get_summary get_formals instr_cfg_wto
      inferbo_invariant_map get_node_nb_exec
  in
  let () =
    let exit_cost_record = astate.WorstCaseCost.costs in
    L.(debug Analysis Verbose)
      "@\n[COST ANALYSIS] PROCEDURE '%a' |CFG| = %i FINAL COST = %a @\n" Procname.pp proc_name
      (Container.length ~fold:NodeCFG.fold_nodes node_cfg)
      CostDomain.VariantCostMap.pp exit_cost_record
  in
  Check.check_and_report analysis_data ~is_on_ui_thread astate ;
  Some (get_cost_summary ~is_on_ui_thread astate)
