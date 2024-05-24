(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbstractDomain.Types
module F = Format
module L = Logging
module ModifiedVarSet = PrettyPrintable.MakePPSet (Var)
module InstrCFG = ProcCfg.NormalOneInstrPerNode

let debug fmt = L.debug Analysis Verbose fmt

(* A simple purity checker *)

type analysis_data =
  { tenv: Tenv.t
  ; inferbo_invariant_map: BufferOverrunAnalysis.invariant_map
  ; formals: Var.t list
  ; get_callee_summary: Procname.t -> PurityDomain.summary option }

module TransferFunctions = struct
  module CFG = ProcCfg.Normal
  module Domain = PurityDomain

  type nonrec analysis_data = analysis_data

  let get_alias_set inferbo_mem var =
    let default = ModifiedVarSet.empty in
    let alias_v = BufferOverrunDomain.Mem.find (AbsLoc.Loc.of_var var) inferbo_mem in
    let pow_locs = BufferOverrunDomain.Val.get_all_locs alias_v in
    AbsLoc.PowLoc.fold
      (fun loc modified_acc ->
        AbsLoc.Loc.get_path loc
        |> Option.value_map ~default:modified_acc ~f:(fun path ->
               Symb.SymbolPath.get_pvar path
               |> Option.value_map ~default:modified_acc ~f:(fun pvar ->
                      debug "Add alias of %a -> %a " Var.pp var (Pvar.pp Pp.text) pvar ;
                      ModifiedVarSet.add (Var.of_pvar pvar) modified_acc ) ) )
      pow_locs default


  let get_modified_params formals ~f =
    List.foldi ~init:Domain.ModifiedParamIndices.empty
      ~f:(fun i modified_acc var ->
        if f var then Domain.ModifiedParamIndices.add i modified_acc else modified_acc )
      formals


  (* given a heap access to ae, find which parameter indices of pdesc
     it modifies *)
  let track_modified_params inferbo_mem formals ae =
    let base_var, _ = HilExp.AccessExpression.get_base ae in
    (* treat writes to global (static) variables separately since they
       are not considered to be explicit parameters. *)
    if Var.is_global base_var then Domain.impure_global
    else
      let alias_set = lazy (get_alias_set inferbo_mem base_var) in
      get_modified_params formals ~f:(fun var ->
          Var.equal var base_var || ModifiedVarSet.mem var (Lazy.force alias_set) )
      |> Domain.impure_params


  let rec is_heap_access ae =
    match (ae : HilExp.AccessExpression.t) with
    | FieldOffset _ | ArrayOffset _ ->
        true
    | Dereference ae | AddressOf ae ->
        is_heap_access ae
    | Base _ ->
        false


  exception Modified_Global

  (* find all the variables occurring in the given modified argument exp,
     unless the variable is a global (which makes all other function
     calls impure since they may read from that modified global var ) *)
  let get_modified_vars_unless_global inferbo_mem modified_acc modified_arg_exp =
    debug "Argument %a is modified.\n" HilExp.pp modified_arg_exp ;
    let ae_list = HilExp.get_access_exprs modified_arg_exp in
    List.fold ae_list ~init:modified_acc ~f:(fun modified_acc ae ->
        let base_var, typ = HilExp.AccessExpression.get_base ae in
        let alias_set = get_alias_set inferbo_mem base_var in
        debug "Alias set for %a : %a \n\n\n" Var.pp base_var ModifiedVarSet.pp alias_set ;
        if Var.is_global base_var || ModifiedVarSet.exists Var.is_global alias_set then
          raise Modified_Global
        else
          let modified_acc' =
            if Typ.is_pointer typ then ModifiedVarSet.add base_var modified_acc else modified_acc
          in
          ModifiedVarSet.union modified_acc' alias_set )


  (* given the modified parameters and the args of the callee, find
     parameter indices of the current procedure that match, i.e have
     been modified by the callee. Note that index counting starts from
     0, reserved for the implicit parameter (formal) this .

     E.g. : for the below call to 'impure_fun' in 'foo', we return 2
     (i.e. index of a wrt. foo's formals).

     void foo (int x, Object a, Object b){
     for (...){
     impure_fun(b, 10, a); // modifies only 3rd argument, i.e. a
     }
     }
  *)
  let find_params_matching_modified_args inferbo_mem formals callee_args callee_modified_params =
    try
      let vars_of_modified_args =
        List.foldi ~init:ModifiedVarSet.empty
          ~f:(fun i acc arg_exp ->
            if Domain.ModifiedParamIndices.mem i callee_modified_params then
              get_modified_vars_unless_global inferbo_mem acc arg_exp
            else acc )
          callee_args
      in
      (* find the respective parameter of the caller, matching the modified vars *)
      let caller_modified_params =
        get_modified_params formals ~f:(fun formal_var ->
            ModifiedVarSet.mem formal_var vars_of_modified_args )
      in
      Domain.impure_params caller_modified_params
    with Modified_Global -> Domain.impure_global


  (* if the callee is impure, find the parameters that have been modified by the callee *)
  let find_modified_if_impure inferbo_mem formals args callee_summary =
    match callee_summary with
    | Top ->
        Domain.impure_global
    | NonTop callee_modified_params ->
        Domain.(
          debug "Callee modified params %a \n" ModifiedParamIndices.pp callee_modified_params ;
          if ModifiedParamIndices.is_empty callee_modified_params then pure
          else find_params_matching_modified_args inferbo_mem formals args callee_modified_params )


  let modified_global ae = HilExp.AccessExpression.get_base ae |> fst |> Var.is_global

  let exec_instr (astate : Domain.t) {tenv; inferbo_invariant_map; formals; get_callee_summary}
      (node : CFG.Node.t) idx (instr : HilInstr.t) =
    let (node_id : InstrCFG.Node.id) = CFG.Node.to_instr idx node |> ProcCfg.InstrNode.id in
    let inferbo_mem = BufferOverrunAnalysis.extract_post node_id inferbo_invariant_map in
    if Option.is_none inferbo_mem then
      debug "Inferbo memory at %a was not found\n" InstrCFG.Node.pp_id node_id ;
    match (instr, inferbo_mem) with
    | Assign (ae, _, _), Some inferbo_mem
      when is_heap_access ae || (Language.curr_language_is Clang && modified_global ae) ->
        track_modified_params inferbo_mem formals ae |> Domain.join astate
    | Call (_, Direct called_pname, args, _, _), Some inferbo_mem ->
        Domain.join astate
          ( match PurityModels.ProcName.dispatch tenv called_pname with
          | Some callee_summary ->
              find_modified_if_impure inferbo_mem formals args callee_summary
          | None -> (
            match get_callee_summary called_pname with
            | Some callee_summary ->
                debug "Reading from %a \n" Procname.pp called_pname ;
                find_modified_if_impure inferbo_mem formals args callee_summary
            | None ->
                if Procname.is_constructor called_pname then Domain.pure else Domain.impure_global )
          )
    | Call (_, Indirect _, _, _, _), _ ->
        (* This should never happen in Java *)
        debug "Unexpected indirect call %a" HilInstr.pp instr ;
        Top
    | _, _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "purity checker"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions)

let compute_summary {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency}
    inferbo_invariant_map =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let formals =
    Procdesc.get_formals proc_desc
    |> List.map ~f:(fun (mname, _, _) -> Var.of_pvar (Pvar.mk mname proc_name))
  in
  let get_callee_summary callee_pname =
    analyze_dependency callee_pname |> AnalysisResult.to_option |> Option.bind ~f:fst
  in
  let analysis_data = {tenv; inferbo_invariant_map; formals; get_callee_summary} in
  Analyzer.compute_post analysis_data ~initial:PurityDomain.pure proc_desc


let checker analysis_data =
  let open IOption.Let_syntax in
  let* inferbo_invariant_map =
    BufferOverrunAnalysis.cached_compute_invariant_map
      (InterproceduralAnalysis.bind_payload_opt ~f:snd analysis_data)
  in
  let+ astate = compute_summary analysis_data inferbo_invariant_map in
  debug "Purity summary :%a \n" PurityDomain.pp astate ;
  astate
