(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module InstrCFG = ProcCfg.NormalOneInstrPerNode

module Call = struct
  type t =
    { instr: Sil.instr
    ; loc: Location.t
    ; pname: Procname.t
    ; node: Procdesc.Node.t
    ; args: (Exp.t * Typ.t) list
    ; ret: Ident.t * Typ.t }
  [@@deriving compare]

  let pp fmt {pname; loc} =
    F.fprintf fmt "loop-invariant call to %a, at %a " Procname.pp pname Location.pp loc
end

type cost_args =
  { tenv: Tenv.t
  ; integer_type_widths: Typ.IntegerWidths.t
  ; get_callee_cost_summary_and_formals:
      Procname.t -> (CostDomain.summary * (Pvar.t * Typ.t) list) option
  ; inferbo_invariant_map: BufferOverrunAnalysis.invariant_map
  ; inferbo_get_summary: BufferOverrunAnalysisSummary.get_summary
  ; call: Call.t }

type 'a interproc_analysis =
  (BufferOverrunAnalysisSummary.t option * 'a * CostDomain.summary option) InterproceduralAnalysis.t

let get_symbolic_cost
    { tenv
    ; integer_type_widths
    ; get_callee_cost_summary_and_formals
    ; inferbo_invariant_map
    ; inferbo_get_summary
    ; call= Call.{instr; pname; node; ret; args} } =
  let last_node = Option.value_exn (InstrCFG.of_instr_opt node instr) in
  let inferbo_mem =
    let instr_node_id = InstrCFG.Node.id last_node in
    Option.value_exn (BufferOverrunAnalysis.extract_pre instr_node_id inferbo_invariant_map)
  in
  let loc = InstrCFG.Node.loc last_node in
  let get_symbolic cost =
    if CostDomain.BasicCost.is_symbolic cost then `SymbolicCost cost else `Cheap
  in
  let get_summary pname = Option.map ~f:fst (get_callee_cost_summary_and_formals pname) in
  match get_callee_cost_summary_and_formals pname with
  | Some (CostDomain.{post= cost_record}, callee_formals) ->
      let callee_cost = CostDomain.get_operation_cost cost_record in
      if CostDomain.BasicCost.is_symbolic callee_cost.cost then
        (Cost.instantiate_cost ~default_closure_cost:Ints.NonNegativeInt.one integer_type_widths
           ~inferbo_caller_mem:inferbo_mem ~callee_pname:pname ~callee_formals ~args ~callee_cost
           ~loc)
          .cost |> get_symbolic
      else `Cheap
  | None ->
      let fun_arg_list =
        List.map args ~f:(fun (exp, typ) ->
            ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )
      in
      CostModels.Call.dispatch tenv pname fun_arg_list
      |> Option.value_map ~default:`NoModel ~f:(fun model ->
             let model_env =
               let node_hash = InstrCFG.Node.hash last_node in
               BufferOverrunUtils.ModelEnv.mk_model_env pname ~node_hash loc tenv
                 integer_type_widths inferbo_get_summary
             in
             model CostUtils.CostModelEnv.{get_summary; model_env} ~ret inferbo_mem |> get_symbolic )


let prepare_call_args
    ({InterproceduralAnalysis.proc_desc; exe_env; analyze_dependency} as analysis_data) call =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let tenv = Exe_env.get_proc_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let inferbo_invariant_map =
    BufferOverrunAnalysis.cached_compute_invariant_map
      (InterproceduralAnalysis.bind_payload ~f:fst3 analysis_data)
  in
  let open IOption.Let_syntax in
  let get_callee_cost_summary_and_formals callee_pname =
    let* callee_pdesc, (_inferbo, _, callee_costs_summary) = analyze_dependency callee_pname in
    let+ callee_costs_summary = callee_costs_summary in
    (callee_costs_summary, Procdesc.get_pvar_formals callee_pdesc)
  in
  let inferbo_get_summary callee_pname =
    let* _callee_pdesc, (inferbo, _purity, _callee_costs_summary) =
      analyze_dependency callee_pname
    in
    inferbo
  in
  { tenv
  ; integer_type_widths
  ; get_callee_cost_summary_and_formals
  ; inferbo_invariant_map
  ; inferbo_get_summary
  ; call }


let get_cost_if_expensive analysis_data call =
  match prepare_call_args analysis_data call |> get_symbolic_cost with
  | `SymbolicCost cost ->
      Some cost
  | `Cheap | `NoModel ->
      None


let get_is_cheap_call analysis_data call =
  match prepare_call_args analysis_data call |> get_symbolic_cost with
  | `SymbolicCost _ ->
      (* symbolic costs (e.g. 4n+5 or log(n) are considered expensive) *)
      false
  | `Cheap ->
      true
  | `NoModel ->
      (* unmodeled calls are considered expensive *)
      false
