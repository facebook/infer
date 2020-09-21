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
  ; integer_type_widths: Typ.IntegerWidths.t
  ; inferbo_get_summary: BufferOverrunAnalysisSummary.get_summary
  ; get_node_nb_exec: Node.t -> BasicCost.t
  ; get_summary: Procname.t -> CostDomain.summary option
  ; get_formals: Procname.t -> (Pvar.t * Typ.t) list option
  ; proc_resolve_attributes: Procname.t -> ProcAttributes.t option }

let instantiate_cost integer_type_widths ~inferbo_caller_mem ~callee_pname ~callee_formals ~params
    ~callee_cost ~loc =
  let eval_sym =
    BufferOverrunSemantics.mk_eval_sym_cost integer_type_widths callee_formals params
      inferbo_caller_mem
  in
  BasicCostWithReason.subst callee_pname loc callee_cost eval_sym


module InstrBasicCostWithReason = struct
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


  (** The methods whose name start with one of the prefixes return an object that is owned by the
      caller. Therefore ARC will not add any objects they return into an autorelease pool. *)
  let return_object_owned_by_caller =
    let prefixes = ["alloc"; "new"; "copy"; "mutableCopy"] in
    fun callee_pname ->
      let method_name = Procname.get_method callee_pname in
      List.exists prefixes ~f:(fun prefix -> String.is_prefix method_name ~prefix)


  let is_objc_call_from_no_arc_to_arc {proc_resolve_attributes} caller_pdesc callee_pname =
    (not (Procdesc.is_objc_arc_on caller_pdesc))
    && Option.exists (proc_resolve_attributes callee_pname)
         ~f:(fun {ProcAttributes.is_objc_arc_on} -> is_objc_arc_on)


  let get_instr_cost_record tenv extras cfg instr_node instr =
    match instr with
    | Sil.Call (((_, ret_typ) as ret), Exp.Const (Const.Cfun callee_pname), params, location, _)
      when Config.inclusive_cost -> (
        let { inferbo_invariant_map
            ; integer_type_widths
            ; inferbo_get_summary
            ; get_summary
            ; get_formals } =
          extras
        in
        let fun_arg_list =
          List.map params ~f:(fun (exp, typ) ->
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
        let cost =
          match inferbo_mem_opt with
          | None ->
              CostDomain.unit_cost_atomic_operation
          | Some inferbo_mem -> (
            match CostModels.Call.dispatch tenv callee_pname fun_arg_list with
            | Some model ->
                CostDomain.of_operation_cost (model (Lazy.force model_env) ~ret inferbo_mem)
            | None -> (
              match (get_summary callee_pname, get_formals callee_pname) with
              | Some {CostDomain.post= callee_cost_record}, Some callee_formals -> (
                  let instantiated_cost =
                    CostDomain.map callee_cost_record ~f:(fun callee_cost ->
                        instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
                          ~callee_pname ~callee_formals ~params ~callee_cost ~loc:location )
                  in
                  match CostDomain.get_operation_cost callee_cost_record with
                  | {cost; top_pname_opt= None} when BasicCost.is_top cost ->
                      CostDomain.add_top_pname_opt CostKind.OperationCost instantiated_cost
                        (Some callee_pname)
                  | _ ->
                      instantiated_cost )
              | _, _ ->
                  ScubaLogging.cost_log_message ~label:"unmodeled_function_cost_analysis"
                    ~message:
                      (F.asprintf "Unmodeled Function[Cost Analysis] : %a" Procname.pp callee_pname) ;
                  CostDomain.unit_cost_atomic_operation ) )
        in
        let cost =
          if is_allocation_function callee_pname then
            CostDomain.plus CostDomain.unit_cost_allocation cost
          else cost
        in
        match
          (inferbo_mem_opt, CostAutoreleaseModels.Call.dispatch tenv callee_pname fun_arg_list)
        with
        | Some inferbo_mem, Some model ->
            let autoreleasepool_size = model (Lazy.force model_env) ~ret inferbo_mem in
            CostDomain.plus_autoreleasepool_size autoreleasepool_size cost
        | _, _ ->
            if
              is_objc_call_from_no_arc_to_arc extras cfg callee_pname
              && Typ.is_pointer_to_objc_class ret_typ
              && not (return_object_owned_by_caller callee_pname)
            then
              let autoreleasepool_trace =
                Bounds.BoundTrace.of_arc_from_non_arc (Procname.to_string callee_pname) location
              in
              CostDomain.plus cost
                (CostDomain.unit_cost_autoreleasepool_size ~autoreleasepool_trace)
            else cost )
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


  let get_instr_node_cost_record tenv extras cfg instr_node =
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
    let cost = get_instr_cost_record tenv extras cfg instr_node instr in
    let operation_cost = CostDomain.get_operation_cost cost in
    let log_msg top_or_bottom =
      Logging.d_printfln_escaped "Statement cost became %s at %a (%a)." top_or_bottom
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
  ; nullsafe_extra= None }


(** Calculate the final Worst Case Cost of the cfg. It is the dot product of the symbolic cost of
    the node and how many times it is executed. *)
module WorstCaseCost = struct
  (** We don't report when the cost is Top as it corresponds to subsequent 'don't know's. Instead,
      we report Top cost only at the top level per function. *)
  let exec_node tenv extras cfg instr_node =
    let {get_node_nb_exec} = extras in
    let instr_cost_record =
      InstrBasicCostWithReason.get_instr_node_cost_record tenv extras cfg instr_node
    in
    let node = InstrCFG.Node.underlying_node instr_node in
    let nb_exec = get_node_nb_exec node in
    if BasicCost.is_top nb_exec then
      Logging.d_printfln_escaped "Node %a is analyzed to visit infinite (top) times." Node.pp_id
        (Node.id node) ;
    CostDomain.mult_by instr_cost_record ~nb_exec


  let compute tenv extras cfg =
    let init = CostDomain.zero_record in
    let cost =
      let nodes_in_autoreleasepool = CostUtils.get_nodes_in_autoreleasepool cfg in
      InstrCFG.fold_nodes cfg ~init ~f:(fun acc ((node, _) as pair) ->
          let cost = exec_node tenv extras cfg pair in
          let cost =
            if Procdesc.NodeSet.mem node nodes_in_autoreleasepool then
              CostDomain.set_autoreleasepool_size_zero cost
            else cost
          in
          CostDomain.plus acc cost )
    in
    Option.iter (CostDomain.get_operation_cost cost).top_pname_opt ~f:(fun top_pname ->
        ScubaLogging.cost_log_message ~label:"unmodeled_function_top_cost"
          ~message:(F.asprintf "Unmodeled Function[Top Cost] : %a" Procname.pp top_pname) ;
        Logging.(debug Analysis Verbose)
          "@ Unmodeled Function[Top Cost]: %a@\n" Procname.pp top_pname ) ;
    cost
end

let is_report_suppressed pname =
  Procname.is_java_access_method pname
  || Procname.is_java_anonymous_inner_class_method pname
  || Procname.is_java_autogen_method pname


module Check = struct
  let report_top_and_unreachable kind pname proc_desc err_log loc ~name ~cost
      {CostIssues.unreachable_issue; infinite_issue} =
    let report issue suffix =
      let is_autoreleasepool_trace =
        match (kind : CostKind.t) with
        | AutoreleasepoolSize ->
            true
        | OperationCost | AllocationCost ->
            false
      in
      let message = F.asprintf "%s of the function %a %s" name Procname.pp pname suffix in
      Reporting.log_issue proc_desc err_log ~loc
        ~ltr:(BasicCostWithReason.polynomial_traces ~is_autoreleasepool_trace cost)
        ~extras:(compute_errlog_extras cost) Cost issue message
    in
    if BasicCostWithReason.is_top cost then report infinite_issue "cannot be computed"
    else if BasicCostWithReason.is_unreachable cost then
      report unreachable_issue
        "cannot be computed since the program's exit state is never reachable"


  let check_and_report {InterproceduralAnalysis.proc_desc; err_log} cost =
    let pname = Procdesc.get_proc_name proc_desc in
    let proc_loc = Procdesc.get_loc proc_desc in
    if not (is_report_suppressed pname) then
      CostIssues.CostKindMap.iter2 CostIssues.enabled_cost_map cost
        ~f:(fun kind (CostIssues.{name; top_and_unreachable} as issue_spec) cost ->
          if top_and_unreachable then
            report_top_and_unreachable kind pname proc_desc err_log proc_loc ~name ~cost issue_spec )
end

type bound_map = BasicCost.t Node.IdMap.t

type get_node_nb_exec = Node.t -> BasicCost.t

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


let get_cost_summary ~is_on_ui_thread astate = {CostDomain.post= astate; is_on_ui_thread}

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
  let is_on_ui_thread =
    (not (Procname.is_objc_method proc_name)) && ConcurrencyModels.runs_on_ui_thread tenv proc_name
  in
  let get_node_nb_exec = compute_get_node_nb_exec node_cfg bound_map in
  let astate =
    let open IOption.Let_syntax in
    let get_summary_common callee_pname =
      let+ _, summaries = analyze_dependency callee_pname in
      summaries
    in
    let get_summary callee_pname =
      let* cost_summary, _inferbo_summary, _ = get_summary_common callee_pname in
      cost_summary
    in
    let inferbo_get_summary callee_pname =
      let* _cost_summary, inferbo_summary, _ = get_summary_common callee_pname in
      inferbo_summary
    in
    let get_formals callee_pname =
      AnalysisCallbacks.proc_resolve_attributes callee_pname >>| ProcAttributes.get_pvar_formals
    in
    let instr_cfg = InstrCFG.from_pdesc proc_desc in
    let extras =
      { inferbo_invariant_map
      ; inferbo_get_summary
      ; integer_type_widths
      ; get_node_nb_exec
      ; get_summary
      ; get_formals
      ; proc_resolve_attributes= AnalysisCallbacks.proc_resolve_attributes }
    in
    AnalysisCallbacks.html_debug_new_node_session (NodeCFG.start_node node_cfg)
      ~pp_name:(fun f -> F.pp_print_string f "cost(worst-case)")
      ~f:(fun () -> WorstCaseCost.compute tenv extras instr_cfg)
  in
  let () =
    L.(debug Analysis Verbose)
      "@\n[COST ANALYSIS] PROCEDURE '%a' |CFG| = %i FINAL COST = %a @\n" Procname.pp proc_name
      (Container.length ~fold:NodeCFG.fold_nodes node_cfg)
      CostDomain.VariantCostMap.pp astate
  in
  Check.check_and_report analysis_data astate ;
  Some (get_cost_summary ~is_on_ui_thread astate)
