(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module BasicCost = CostDomain.BasicCost

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module Node = ProcCfg.DefaultNode

let print_upper_bound_map bound_map =
  L.(debug Analysis Medium)
    "@\n\n******* Bound Map : [node -> bound] ITV **** @\n %a @\n"
    (Node.IdMap.pp ~pp_value:BasicCost.pp)
    bound_map ;
  L.(debug Analysis Medium) "@\n******* END Bound Map ITV **** @\n\n"


let filter_loc vars_to_keep = function
  | BufferOverrunField.Prim (AbsLoc.Loc.Var (Var.LogicalVar _)) ->
      None
  | BufferOverrunField.Prim (AbsLoc.Loc.Var var) ->
      Control.ControlMap.find_opt var vars_to_keep
  | _ ->
      None


let compute_upperbound_map node_cfg inferbo_invariant_map control_invariant_map loop_inv_map =
  let compute_node_upper_bound bound_map node =
    let node_id = NodeCFG.Node.id node in
    match Procdesc.Node.get_kind node with
    | Procdesc.Node.Exit_node ->
        Node.IdMap.add node_id BasicCost.one bound_map
    | _ -> (
        let exit_state_opt =
          let instr_node_id = InstrCFG.last_of_underlying_node node |> InstrCFG.Node.id in
          BufferOverrunAnalysis.extract_post instr_node_id inferbo_invariant_map
        in
        match exit_state_opt with
        | Some entry_mem ->
            (* compute control vars, i.e. set of variables that affect the execution count *)
            let control_map =
              Control.compute_control_vars control_invariant_map loop_inv_map node
            in
            L.(debug Analysis Medium)
              "@\n>>> All dependencies for node = %a : %a  @\n\n" Procdesc.Node.pp node
              (Control.ControlMap.pp ~pp_value:Location.pp)
              control_map ;
            (* bound = env(v1) *... * env(vn) *)
            let bound =
              match entry_mem with
              | Unreachable ->
                  let node_loc = NodeCFG.Node.loc node in
                  L.debug Analysis Medium
                    "@\n[COST ANALYSIS INTERNAL WARNING:] This location is unreachable \n" ;
                  BasicCost.of_unreachable node_loc
              | ExcRaised ->
                  BasicCost.one
              | Reachable mem ->
                  let cost =
                    BufferOverrunDomain.MemReach.range ~filter_loc:(filter_loc control_map) ~node_id
                      mem
                  in
                  (* The zero number of executions for a node
                     (corresponding to getting the range of bottom
                     values) does not make sense especially when the
                     abstract memory is non-bottom. This is a source
                     of unsoundness in the analysis. *)
                  if BasicCost.is_zero cost then BasicCost.one else cost
            in
            L.(debug Analysis Medium)
              "@\n>>>Setting bound for node = %a  to %a@\n\n" Node.pp_id node_id BasicCost.pp bound ;
            Node.IdMap.add node_id bound bound_map
        | _ ->
            Node.IdMap.add node_id BasicCost.zero bound_map )
  in
  let bound_map = NodeCFG.fold_nodes node_cfg ~f:compute_node_upper_bound ~init:Node.IdMap.empty in
  print_upper_bound_map bound_map ;
  bound_map


let lookup_upperbound bound_map nid =
  match Node.IdMap.find_opt nid bound_map with
  | Some bound ->
      bound
  | None ->
      L.(debug Analysis Medium)
        "@\n\n[WARNING] Bound not found for node %a, returning Top @\n" Node.pp_id nid ;
      BasicCost.top
