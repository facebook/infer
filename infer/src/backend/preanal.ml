(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** add instructions to perform abstraction *)
let add_abstraction_instructions pdesc =
  let open Procdesc in
  (* true if there is a succ node s.t.: it is an exit node, or the succ of >1 nodes *)
  let converging_node node =
    let is_exit node = match Node.get_kind node with Node.Exit_node _ -> true | _ -> false in
    let succ_nodes = Node.get_succs node in
    if List.exists ~f:is_exit succ_nodes then true
    else
      match succ_nodes with [] -> false | [h] -> List.length (Node.get_preds h) > 1 | _ -> false
  in
  let node_requires_abstraction node =
    match Node.get_kind node with
    | Node.Start_node _ | Node.Join_node ->
        false
    | Node.Exit_node _ | Node.Stmt_node _ | Node.Prune_node _ | Node.Skip_node _ ->
        converging_node node
  in
  let do_node node =
    let loc = Node.get_last_loc node in
    if node_requires_abstraction node then Node.append_instrs node [Sil.Abstract loc]
  in
  Procdesc.iter_nodes do_node pdesc


module BackwardCfg = ProcCfg.Backward (ProcCfg.Exceptional)
module LivenessAnalysis = AbstractInterpreter.Make (BackwardCfg) (Liveness.TransferFunctions)
module VarDomain = Liveness.Domain

(** computes the non-nullified reaching definitions at the end of each node by building on the
    results of a liveness analysis to be precise, what we want to compute is:
    to_nullify := (live_before U non_nullifed_reaching_defs) - live_after
    non_nullified_reaching_defs := non_nullified_reaching_defs - to_nullify
    Note that this can't be done with by combining the results of reaching definitions and liveness
    after the fact, nor can it be done with liveness alone. We will insert nullify instructions for
    each pvar in to_nullify afer we finish the analysis. Nullify instructions speed up the analysis
    by enabling it to GC state that will no longer be read. *)
module NullifyTransferFunctions = struct
  (* (reaching non-nullified vars) * (vars to nullify) *)
  module Domain = AbstractDomain.Pair (VarDomain) (VarDomain)
  module CFG = ProcCfg.Exceptional

  type extras = LivenessAnalysis.invariant_map

  let postprocess ((reaching_defs, _) as astate) node {ProcData.extras} =
    let node_id = Procdesc.Node.get_id (CFG.Node.underlying_node node) in
    match LivenessAnalysis.extract_state node_id extras with
    (* note: because the analysis is backward, post and pre are reversed *)
    | Some {AbstractInterpreter.post= live_before; pre= live_after} ->
        let to_nullify = VarDomain.diff (VarDomain.union live_before reaching_defs) live_after in
        let reaching_defs' = VarDomain.diff reaching_defs to_nullify in
        (reaching_defs', to_nullify)
    | None ->
        astate


  let cache_node = ref (Procdesc.Node.dummy None)

  let cache_instr = ref Sil.skip_instr

  let last_instr_in_node node =
    let get_last_instr () =
      CFG.instrs node |> Instrs.last |> Option.value ~default:Sil.skip_instr
    in
    if phys_equal node !cache_node then !cache_instr
    else
      let last_instr = get_last_instr () in
      cache_node := node ;
      cache_instr := last_instr ;
      last_instr


  let is_last_instr_in_node instr node = phys_equal (last_instr_in_node node) instr

  let exec_instr ((active_defs, to_nullify) as astate) extras node instr =
    let astate' =
      match instr with
      | Sil.Load (lhs_id, _, _, _) ->
          (VarDomain.add (Var.of_id lhs_id) active_defs, to_nullify)
      | Sil.Call ((id, _), _, _, _, _) ->
          let active_defs' = VarDomain.add (Var.of_id id) active_defs in
          (active_defs', to_nullify)
      | Sil.Store (Exp.Lvar lhs_pvar, _, _, _) ->
          (VarDomain.add (Var.of_pvar lhs_pvar) active_defs, to_nullify)
      | Sil.Store _ | Prune _ | Remove_temps _ | Abstract _ ->
          astate
      | Sil.Nullify _ ->
          L.(die InternalError)
            "Should not add nullify instructions before running nullify analysis!"
    in
    if is_last_instr_in_node instr node then postprocess astate' node extras else astate'


  let pp_session_name _node fmt = Format.pp_print_string fmt "nullify"
end

module NullifyAnalysis =
  AbstractInterpreter.MakeNoCFG
    (Scheduler.ReversePostorder (ProcCfg.Exceptional)) (NullifyTransferFunctions)

let add_nullify_instrs pdesc tenv liveness_inv_map =
  let address_taken_vars =
    if Typ.Procname.is_java (Procdesc.get_proc_name pdesc) then AddressTaken.Domain.empty
      (* can't take the address of a variable in Java *)
    else
      let initial = AddressTaken.Domain.empty in
      match AddressTaken.Analyzer.compute_post (ProcData.make_default pdesc tenv) ~initial with
      | Some post ->
          post
      | None ->
          AddressTaken.Domain.empty
  in
  let nullify_proc_cfg = ProcCfg.Exceptional.from_pdesc pdesc in
  let nullify_proc_data = ProcData.make pdesc tenv liveness_inv_map in
  let initial = (VarDomain.empty, VarDomain.empty) in
  let nullify_inv_map =
    NullifyAnalysis.exec_cfg nullify_proc_cfg nullify_proc_data ~initial ~debug:false
  in
  (* only nullify pvars that are local; don't nullify those that can escape *)
  let is_local pvar = not (Pvar.is_return pvar || Pvar.is_global pvar) in
  let node_nullify_instructions loc pvars =
    List.rev_filter_map pvars ~f:(fun pvar ->
        if is_local pvar then Some (Sil.Nullify (pvar, loc)) else None )
  in
  let node_removetmps_instruction loc ids =
    if ids <> [] then Some (Sil.Remove_temps (List.rev ids, loc)) else None
  in
  Container.iter nullify_proc_cfg ~fold:ProcCfg.Exceptional.fold_nodes ~f:(fun node ->
      match NullifyAnalysis.extract_post (ProcCfg.Exceptional.Node.id node) nullify_inv_map with
      | Some (_, to_nullify) ->
          let pvars_to_nullify, ids_to_remove =
            VarDomain.fold
              (fun var (pvars_acc, ids_acc) ->
                match Var.to_exp var with
                (* we nullify all address taken variables at the end of the procedure *)
                | Exp.Lvar pvar
                  when not (AddressTaken.Domain.mem pvar address_taken_vars) ->
                    (pvar :: pvars_acc, ids_acc)
                | Exp.Var id ->
                    (pvars_acc, id :: ids_acc)
                | _ ->
                    (pvars_acc, ids_acc) )
              to_nullify ([], [])
          in
          let loc = Procdesc.Node.get_last_loc node in
          node_nullify_instructions loc pvars_to_nullify
          |> IList.opt_cons (node_removetmps_instruction loc ids_to_remove)
          |> Procdesc.Node.append_instrs node
      | None ->
          () ) ;
  (* nullify all address taken variables *)
  if not (AddressTaken.Domain.is_empty address_taken_vars) then
    let exit_node = ProcCfg.Exceptional.exit_node nullify_proc_cfg in
    let exit_loc = Procdesc.Node.get_last_loc exit_node in
    node_nullify_instructions exit_loc (AddressTaken.Domain.elements address_taken_vars)
    |> Procdesc.Node.append_instrs exit_node


(** perform liveness analysis and insert Nullify/Remove_temps instructions into the IR to make it
    easy for analyses to do abstract garbage collection *)
let do_liveness pdesc tenv =
  let liveness_proc_cfg = BackwardCfg.from_pdesc pdesc in
  let initial = Liveness.Domain.empty in
  let liveness_inv_map =
    LivenessAnalysis.exec_cfg liveness_proc_cfg
      (ProcData.make_default pdesc tenv)
      ~initial ~debug:false
  in
  add_nullify_instrs pdesc tenv liveness_inv_map ;
  Procdesc.signal_did_preanalysis pdesc


(** add Abstract instructions into the IR to give hints about when abstraction should be
    performed *)
let do_abstraction pdesc =
  add_abstraction_instructions pdesc ;
  Procdesc.signal_did_preanalysis pdesc


let do_preanalysis pdesc tenv =
  if not (Procdesc.did_preanalysis pdesc) then ( do_liveness pdesc tenv ; do_abstraction pdesc )
