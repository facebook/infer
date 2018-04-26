(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

module Summary = Summary.Make (struct
  type payload = CostDomain.summary

  let update_payload sum (summary: Specs.summary) =
    {summary with payload= {summary.payload with cost= Some sum}}


  let read_payload (summary: Specs.summary) = summary.payload.cost
end)

(* We use this treshold to give error if the cost is above it.
   Currently it's set randomly to 200. *)
let expensive_threshold = Itv.Bound.of_int 200

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module InstrCFGScheduler = Scheduler.ReversePostorder (InstrCFG)

module Node = struct
  include ProcCfg.DefaultNode

  let equal_id = [%compare.equal : id]
end

module NodesBasicCostDomain = struct
  include AbstractDomain.Pair (BufferOverrunDomain.Mem) (CostDomain.NodeInstructionToCostMap)

  let init = (BufferOverrunDomain.Mem.init, CostDomain.NodeInstructionToCostMap.empty)
end

(* Compute a map (node,instruction) -> basic_cost, where basic_cost is the
   cost known for a certain operation. For example for basic operation we
   set it to 1 and for function call we take it from the spec of the function.
   The nodes in the domain of the map are those in the path reaching the current node.
*)
module TransferFunctionsNodesBasicCost = struct
  module CFG = InstrCFG
  module InferboTransferFunctions = BufferOverrunChecker.TransferFunctions (CFG)
  module Domain = NodesBasicCostDomain

  type extras = InferboTransferFunctions.extras

  let cost_atomic_instruction = Itv.Bound.one

  let exec_instr_cost _inferbo_mem (astate: CostDomain.NodeInstructionToCostMap.astate)
      {ProcData.pdesc} (node: CFG.node) instr : CostDomain.NodeInstructionToCostMap.astate =
    let key = CFG.id node in
    let astate' =
      match instr with
      | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), _, _, _) -> (
        match Summary.read_summary pdesc callee_pname with
        | Some {post= cost_callee} ->
            CostDomain.NodeInstructionToCostMap.add key cost_callee astate
        | None ->
            CostDomain.NodeInstructionToCostMap.add key cost_atomic_instruction astate )
      | Sil.Load _ | Sil.Store _ | Sil.Call _ | Sil.Prune _ ->
          CostDomain.NodeInstructionToCostMap.add key cost_atomic_instruction astate
      | _ ->
          astate
    in
    L.(debug Analysis Medium)
      "@\n>>>Instr: %a   Cost: %a@\n" (Sil.pp_instr Pp.text) instr
      CostDomain.NodeInstructionToCostMap.pp astate' ;
    astate'


  let exec_instr (inferbo_mem, costmap) pdata node instr =
    let inferbo_mem = InferboTransferFunctions.exec_instr inferbo_mem pdata node instr in
    let costmap = exec_instr_cost inferbo_mem costmap pdata node instr in
    (inferbo_mem, costmap)


  let pp_session_name node fmt = F.fprintf fmt "cost(basic) %a" CFG.pp_id (CFG.id node)
end

module AnalyzerNodesBasicCost =
  AbstractInterpreter.MakeNoCFG (InstrCFGScheduler) (TransferFunctionsNodesBasicCost)

(* Map associating to each node a bound on the number of times it can be executed.
   This bound is computed using environments (map: val -> values), using the following
   observation: the number of environments associated with a program point is an upperbound
   of the number of times the program point can be executed in any execution.
   The size of an environment env is computed as:
     |env| = |env(v1)| * ... * |env(n_k)|

   where |env(v)| is the size of the interval associated to v by env.

    Reference: see Stefan Bygde PhD thesis, 2010

*)
module BoundMap = struct
  type t = Itv.Bound.t Node.IdMap.t

  let print_upper_bound_map bound_map =
    L.(debug Analysis Medium) "@\n\n******* Bound Map ITV **** @\n" ;
    Node.IdMap.iter
      (fun nid b ->
        L.(debug Analysis Medium) "@\n node: %a --> bound = %a @\n" Node.pp_id nid Itv.Bound.pp b
        )
      bound_map ;
    L.(debug Analysis Medium) "@\n******* END Bound Map ITV **** @\n\n"


  let convert (mem: BufferOverrunDomain.Mem.astate) : CostDomain.EnvDomainBO.astate =
    let open AbstractDomain.Types in
    match mem with
    | Bottom ->
        L.internal_error
          "@\n\
           [COST ANALYSIS INTERNAL WARNING:] Found Bottom when converting mem, returning No env \n" ;
        Bottom
    | NonBottom {BufferOverrunDomain.MemReach.heap} ->
        let env =
          BufferOverrunDomain.Heap.fold
            (fun loc data acc ->
              match loc with
              | AbsLoc.Loc.Var (Var.LogicalVar id) ->
                  let key = Exp.Var id in
                  CostDomain.EnvMap.add key (BufferOverrunDomain.Val.get_itv data) acc
              | AbsLoc.Loc.Var (Var.ProgramVar v) ->
                  let key = Exp.Lvar v in
                  CostDomain.EnvMap.add key (BufferOverrunDomain.Val.get_itv data) acc
              | _ ->
                  acc )
            heap CostDomain.EnvMap.empty
        in
        NonBottom env


  let compute_upperbound_map node_cfg invariant_map_NodesBasicCost data_invariant_map
      control_invariant_map =
    let fparam = Procdesc.get_formals node_cfg in
    let pname = Procdesc.get_proc_name node_cfg in
    let fparam' = List.map ~f:(fun (m, _) -> Exp.Lvar (Pvar.mk m pname)) fparam in
    let compute_node_upper_bound bound_map node =
      let node_id = NodeCFG.id node in
      match Procdesc.Node.get_kind node with
      | Procdesc.Node.Exit_node _ ->
          Node.IdMap.add node_id Itv.Bound.one bound_map
      | _ ->
          let entry_state_opt =
            let instr_node_id = InstrCFG.of_underlying_node node |> InstrCFG.id in
            AnalyzerNodesBasicCost.extract_pre instr_node_id invariant_map_NodesBasicCost
          in
          match entry_state_opt with
          | Some (entry_mem, _) ->
              (* compute all the dependencies, i.e. set of variables that affect the control flow upto the node *)
              let all_deps =
                Control.compute_all_deps data_invariant_map control_invariant_map node
              in
              L.(debug Analysis Medium)
                "@\n>>> All dependencies for node = %a : %a  @\n\n" Procdesc.Node.pp node
                Control.VarSet.pp all_deps ;
              (* bound = env(v1) *... * env(vn) *)
              let bound =
                match convert entry_mem with
                | Bottom ->
                    (* We get None by converting Bottom *)
                    L.internal_error
                      "@\n\
                       [COST ANALYSIS INTERNAL WARNING:] No 'env' found. This location is \
                       unreachable returning cost 0 \n" ;
                    Itv.Bound.zero
                | NonBottom env ->
                    CostDomain.EnvMap.fold
                      (fun exp itv acc ->
                        let itv' =
                          match exp with
                          | Exp.Var _ ->
                              Itv.one
                          (* For temp var we give [1,1] so it doesn't count*)
                          | Exp.Lvar _
                            when List.mem fparam' exp ~equal:Exp.equal ->
                              Itv.one
                          | Exp.Lvar pvar when Control.VarSet.mem (Var.of_pvar pvar) all_deps ->
                              itv
                          | Exp.Lvar _ ->
                              (* For a var that doesn't affect control flow directly or indirectly, we give [1,1] so it doesn't count *)
                              Itv.one
                          | _ ->
                              assert false
                        in
                        let range = Itv.range itv' in
                        L.(debug Analysis Medium)
                          "@\n>>>For node = %a :  exp=%a  itv=%a   range =%a @\n\n" Node.pp_id
                          node_id Exp.pp exp Itv.pp itv' Itv.Bound.pp range ;
                        Itv.Bound.mult acc range )
                      env Itv.Bound.one
              in
              L.(debug Analysis Medium)
                "@\n>>>Setting bound for node = %a  to %a@\n\n" Node.pp_id node_id Itv.Bound.pp
                bound ;
              Node.IdMap.add node_id bound bound_map
          | _ ->
              Node.IdMap.add node_id Itv.Bound.zero bound_map
    in
    let bound_map =
      List.fold (NodeCFG.nodes node_cfg) ~f:compute_node_upper_bound ~init:Node.IdMap.empty
    in
    print_upper_bound_map bound_map ; bound_map


  let upperbound bound_map nid =
    match Node.IdMap.find_opt nid bound_map with
    | Some bound ->
        bound
    | None ->
        L.(debug Analysis Medium)
          "@\n\n[WARNING] Bound not found for node %a, returning Top @\n" Node.pp_id nid ;
        Itv.Bound.pinf
end

(* Structural Constraints are expressions of the kind:
     n <= n1 +...+ nk

   The informal meaning is: the number of times node n can be executed is less or
   equal to the sum of the number of times nodes n1,..., nk can be executed.
*)
module StructuralConstraints = struct
  type rhs = Single of Node.id | Sum of Node.IdSet.t

  type t = {lhs: Node.id; rhs: rhs}

  let is_single ~lhs:expected_lhs = function
    | {lhs; rhs= Single single} when Node.equal_id lhs expected_lhs ->
        Some single
    | _ ->
        None


  let is_sum ~lhs:expected_lhs = function
    | {lhs; rhs= Sum sum} when Node.equal_id lhs expected_lhs ->
        Some sum
    | _ ->
        None


  let pp_rhs fmt = function
    | Single nid ->
        Node.pp_id fmt nid
    | Sum nidset ->
        Pp.seq ~sep:" + " Node.pp_id fmt (Node.IdSet.elements nidset)


  let pp fmt {lhs; rhs} = F.fprintf fmt "%a <= %a" Node.pp_id lhs pp_rhs rhs

  let print_constraint_list constraints =
    L.(debug Analysis Medium) "@\n\n******* Structural Constraints **** @\n" ;
    List.iter ~f:(fun c -> L.(debug Analysis Medium) "@\n    %a   @\n" pp c) constraints ;
    L.(debug Analysis Medium) "@\n******* END Structural Constraints **** @\n\n"


  (*  for each program point return a set of contraints of the kind

     i<=Sum_{j \in Predecessors(i) } j
     i<=Sum_{j \in Successors(i)} j
*)
  let compute_structural_constraints node_cfg =
    let compute_node_constraints acc node =
      let constraints_append node get_nodes tail =
        match get_nodes node with
        | [] ->
            tail
        | [single] ->
            {lhs= NodeCFG.id node; rhs= Single (NodeCFG.id single)} :: tail
        | nodes ->
            let sum =
              List.fold nodes ~init:Node.IdSet.empty ~f:(fun idset node ->
                  Node.IdSet.add (NodeCFG.id node) idset )
            in
            {lhs= NodeCFG.id node; rhs= Sum sum} :: tail
      in
      acc |> constraints_append node Procdesc.Node.get_preds
      |> constraints_append node Procdesc.Node.get_succs
    in
    let constraints = List.fold (NodeCFG.nodes node_cfg) ~f:compute_node_constraints ~init:[] in
    print_constraint_list constraints ; constraints
end

(* MinTree is used to compute:

    \max (\Sum_{n \in Nodes} c_n * x_n )

   given a set of contraints on x_n. The constraints involve the contro flow
    of the program.

*)
module MinTree = struct
  type mt_node = Leaf of (Node.id * Itv.Bound.t) | Min of mt_node list | Plus of mt_node list

  let add_leaf node nid leaf =
    let leaf' = Leaf (nid, leaf) in
    match node with Min l -> Min (leaf' :: l) | Plus l -> Plus (leaf' :: l) | _ -> assert false


  let plus_seq pp f l = Pp.seq ~sep:" + " pp f l

  let rec pp fmt node =
    match node with
    | Leaf (nid, c) ->
        F.fprintf fmt "%a:%a" Node.pp_id nid Itv.Bound.pp c
    | Min l ->
        F.fprintf fmt "Min(%a)" (Pp.comma_seq pp) l
    | Plus l ->
        F.fprintf fmt "(%a)" (plus_seq pp) l


  let add_child node child =
    match child with
    | Plus [] | Min [] ->
        node (* if it's a dummy child, don't add it *)
    | _ ->
      match node with Plus l -> Plus (child :: l) | Min l -> Min (child :: l) | _ -> assert false


  (* finds the subset of constraints of the form x_k <= x_j *)
  let get_k_single_constraints constraints k =
    List.filter_map constraints ~f:(StructuralConstraints.is_single ~lhs:k)


  (* finds the subset of constraints of the form x_k <= x_j1 +...+ x_jn and
return the addends of the sum x_j1+x_j2+..+x_j_n*)
  let get_k_sum_constraints constraints k =
    List.filter_map constraints ~f:(StructuralConstraints.is_sum ~lhs:k)


  let rec evaluate_tree t =
    match t with
    | Leaf (_, c) ->
        c
    | Min l ->
        evaluate_operator Itv.Bound.min l
    | Plus l ->
        evaluate_operator Itv.Bound.plus_u l


  and evaluate_operator op l =
    match l with
    | [] ->
        assert false
    | [c] ->
        evaluate_tree c
    | c :: l' ->
        let res_c = evaluate_tree c in
        let res_l' = evaluate_operator op l' in
        op res_c res_l'


  (*  a plus node is well formed if has at least two addends *)
  let is_well_formed_plus_node plus_node =
    match plus_node with Plus (_ :: _ :: _) -> true | _ -> false


  module SetOfSetsOfNodes = Caml.Set.Make (Node.IdSet)

  module BuiltTreeMap = Caml.Map.Make (struct
    type t = Node.id * Node.IdSet.t [@@deriving compare]
  end)

  let rec minimum_propagation (bound_map: BoundMap.t) (q: Node.id) (visited: Node.IdSet.t)
      (constraints: StructuralConstraints.t list) global_built_tree_map =
    let rec build_min node branch visited_acc worklist =
      match worklist with
      | [] ->
          (node, branch, visited_acc)
      | k :: rest ->
          if Node.IdSet.mem k visited_acc then build_min node branch visited_acc rest
          else
            let visited_acc' = Node.IdSet.add k visited_acc in
            let node = add_leaf node k (BoundMap.upperbound bound_map k) in
            let k_constraints_upperbound = get_k_single_constraints constraints k in
            let worklist' =
              List.fold k_constraints_upperbound ~init:rest ~f:(fun acc ub_id ->
                  if Node.IdSet.mem ub_id visited_acc' then acc else ub_id :: acc )
            in
            let k_sum_constraints = get_k_sum_constraints constraints k in
            let branch =
              List.fold_left
                ~f:(fun branch set_addend ->
                  if Node.IdSet.is_empty (Node.IdSet.inter set_addend visited_acc') then
                    SetOfSetsOfNodes.add set_addend branch
                  else branch )
                ~init:branch k_sum_constraints
            in
            build_min node branch visited_acc' worklist'
    in
    match BuiltTreeMap.find_opt (q, visited) !global_built_tree_map with
    | Some tree ->
        tree
    | None ->
        let node, branch, visited_res = build_min (Min []) SetOfSetsOfNodes.empty visited [q] in
        SetOfSetsOfNodes.fold
          (fun addend i_node ->
            if Node.IdSet.cardinal addend < 2 then assert false
            else (
              L.(debug Analysis Medium) "@\n\n|Set addends| = %i  {" (Node.IdSet.cardinal addend) ;
              Node.IdSet.iter (fun e -> L.(debug Analysis Medium) " %a, " Node.pp_id e) addend ;
              L.(debug Analysis Medium) " }@\n " ) ;
            let plus_node =
              Node.IdSet.fold
                (fun n acc ->
                  let child =
                    minimum_propagation bound_map n visited_res constraints global_built_tree_map
                  in
                  global_built_tree_map :=
                    BuiltTreeMap.add (n, visited_res) child !global_built_tree_map ;
                  add_child acc child )
                addend (Plus [])
            in
            (* without this check it would add plus node with just one child, and give wrong results *)
            if is_well_formed_plus_node plus_node then add_child i_node plus_node else i_node )
          branch node


  let compute_trees_from_contraints bound_map node_cfg constraints =
    (* a map used for bookkeeping of the min trees that we have already built *)
    let global_built_tree_map : mt_node BuiltTreeMap.t ref = ref BuiltTreeMap.empty in
    let min_trees =
      List.fold
        ~f:(fun acc node ->
          let nid = Node.id node in
          let tree =
            minimum_propagation bound_map nid Node.IdSet.empty constraints global_built_tree_map
          in
          (nid, tree) :: acc )
        ~init:[] (NodeCFG.nodes node_cfg)
    in
    List.iter
      ~f:(fun (nid, t) -> L.(debug Analysis Medium) "@\n node %a = %a @\n" Node.pp_id nid pp t)
      min_trees ;
    min_trees
end

module ReportedOnNodes = AbstractDomain.FiniteSetOfPPSet (Node.IdSet)

type extras_TransferFunctionsWCET =
  { basic_cost_map: AnalyzerNodesBasicCost.invariant_map
  ; min_trees_map: Itv.Bound.t Node.IdMap.t
  ; summary: Specs.summary }

(* Calculate the final Worst Case Execution Time predicted for each node.
   It uses the basic cost of the nodes (computed previously by AnalyzerNodesBasicCost)
   and MinTrees which give an upperbound on the number of times a node can be executed
*)
module TransferFunctionsWCET = struct
  module CFG = InstrCFG
  module Domain = AbstractDomain.Pair (Itv.Bound) (ReportedOnNodes)

  type extras = extras_TransferFunctionsWCET

  (* We don't report when the cost is Top as it corresponds to subsequent 'don't know's.
   Instead, we report Top cost only at the top level per function when `report_infinity` is set to true *)
  let report_cost summary instr (cost: Itv.Bound.t) instr_node reported_so_far =
    let mk_message () =
      F.asprintf
        "The execution time from the beginning of the function up to this program point is likely \
         above the acceptable threshold of %a (estimated cost %a)"
        Itv.Bound.pp expensive_threshold Itv.Bound.pp cost
    in
    match cost with
    | b when Itv.Bound.is_not_infty b
      -> (
        let above_expensive_threshold = not (Itv.Bound.le cost expensive_threshold) in
        let cost_desc = F.asprintf "with estimated cost %a" Itv.Bound.pp cost in
        match instr with
        | Sil.Call (_, _, _, loc, _) when above_expensive_threshold ->
            let ltr = [Errlog.make_trace_element 0 loc cost_desc []] in
            let exn =
              Exceptions.Checkers
                (IssueType.expensive_execution_time_call, Localise.verbatim_desc (mk_message ()))
            in
            Reporting.log_error summary ~loc ~ltr exn ;
            let nid = instr_node |> CFG.underlying_node |> Procdesc.Node.get_id in
            (cost, ReportedOnNodes.add nid reported_so_far)
        | Sil.Load (_, _, _, loc)
        | Sil.Store (_, _, _, loc)
        | Sil.Call (_, _, _, loc, _)
        | Sil.Prune (_, loc, _, _)
          when above_expensive_threshold ->
            let ltr = [Errlog.make_trace_element 0 loc cost_desc []] in
            let exn =
              Exceptions.Checkers
                (IssueType.expensive_execution_time_call, Localise.verbatim_desc (mk_message ()))
            in
            Reporting.log_error summary ~loc ~ltr exn ;
            let nid = instr_node |> CFG.underlying_node |> Procdesc.Node.get_id in
            (cost, ReportedOnNodes.add nid reported_so_far)
        | _ ->
            (cost, reported_so_far) )
    | _ ->
        (cost, reported_so_far)


  (* get a list of nodes and check if we have already reported for at
     least one of them. In that case no need to report again. *)
  let should_report preds reported_so_far =
    List.for_all
      ~f:(fun node ->
        let nid = Procdesc.Node.get_id node in
        not (ReportedOnNodes.mem nid reported_so_far) )
      preds


  let map_cost trees m : Itv.Bound.t =
    CostDomain.NodeInstructionToCostMap.fold
      (fun ((node_id, _) as instr_node_id) c acc ->
        let t = Node.IdMap.find node_id trees in
        let c_node = Itv.Bound.mult c t in
        let c_node' = Itv.Bound.plus_u acc c_node in
        L.(debug Analysis Medium)
          "@\n  [AnalyzerWCET] Adding cost: (%a) --> c =%a  t = %a @\n" InstrCFG.pp_id
          instr_node_id Itv.Bound.pp c Itv.Bound.pp t ;
        L.(debug Analysis Medium)
          "@\n  [AnalyzerWCET] Adding cost: (%a) --> c_node=%a  cost = %a @\n" InstrCFG.pp_id
          instr_node_id Itv.Bound.pp c_node Itv.Bound.pp c_node' ;
        c_node' )
      m Itv.Bound.zero


  let exec_instr ((_, reported_so_far): Domain.astate) {ProcData.extras} (node: CFG.node) instr
      : Domain.astate =
    let {basic_cost_map= invariant_map_cost; min_trees_map= trees; summary} = extras in
    let cost_node =
      let instr_node_id = CFG.id node in
      match AnalyzerNodesBasicCost.extract_post instr_node_id invariant_map_cost with
      | Some (_, node_map) ->
          L.(debug Analysis Medium)
            "@\n AnalyzerWCET] Final map for node: %a @\n" CFG.pp_id instr_node_id ;
          map_cost trees node_map
      | _ ->
          assert false
    in
    L.(debug Analysis Medium)
      "@\n>>>AnalyzerWCET] Instr: %a   Cost: %a@\n" (Sil.pp_instr Pp.text) instr Itv.Bound.pp
      cost_node ;
    let astate' =
      let und_node = CFG.underlying_node node in
      let preds = Procdesc.Node.get_preds und_node in
      if should_report (und_node :: preds) reported_so_far then
        report_cost summary instr cost_node node reported_so_far
      else (cost_node, reported_so_far)
    in
    astate'


  let pp_session_name _node fmt = F.pp_print_string fmt "cost(wcet)"
end

module AnalyzerWCET = AbstractInterpreter.MakeNoCFG (InstrCFGScheduler) (TransferFunctionsWCET)

let check_and_report_infinity cost proc_desc summary =
  if not (Itv.Bound.is_not_infty cost) then
    let loc = Procdesc.get_start_node proc_desc |> Procdesc.Node.get_loc in
    let message =
      F.asprintf "The execution time of the function %a cannot be computed" Typ.Procname.pp
        (Procdesc.get_proc_name proc_desc)
    in
    let exn =
      Exceptions.Checkers (IssueType.infinite_execution_time_call, Localise.verbatim_desc message)
    in
    Reporting.log_error ~loc summary exn


let checker {Callbacks.tenv; summary; proc_desc} : Specs.summary =
  Preanal.do_preanalysis proc_desc tenv ;
  let proc_data = ProcData.make_default proc_desc tenv in
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  (* computes the data dependencies: node -> (var -> var set) *)
  let data_dep_invariant_map =
    Control.DataDepAnalyzer.exec_cfg node_cfg proc_data ~initial:Control.DataDepMap.empty
      ~debug:true
  in
  (* computes the control dependencies: node -> var set *)
  let control_dep_invariant_map =
    Control.ControlDepAnalyzer.exec_cfg node_cfg proc_data ~initial:Control.ControlDepSet.empty
      ~debug:true
  in
  let instr_cfg = InstrCFG.from_pdesc proc_desc in
  let invariant_map_NodesBasicCost =
    (*compute_WCET cfg invariant_map min_trees in *)
    AnalyzerNodesBasicCost.exec_cfg instr_cfg proc_data ~initial:NodesBasicCostDomain.init
      ~debug:true
  in
  (* given the semantics computes the upper bound on the number of times a node could be executed *)
  let bound_map =
    BoundMap.compute_upperbound_map node_cfg invariant_map_NodesBasicCost data_dep_invariant_map
      control_dep_invariant_map
  in
  let constraints = StructuralConstraints.compute_structural_constraints node_cfg in
  L.internal_error "@\n[COST ANALYSIS] PROCESSING MIN_TREE for PROCEDURE '%a' |CFG| = %i "
    Typ.Procname.pp
    (Procdesc.get_proc_name proc_desc)
    (List.length (NodeCFG.nodes node_cfg)) ;
  let min_trees = MinTree.compute_trees_from_contraints bound_map node_cfg constraints in
  let trees_valuation =
    List.fold
      ~f:(fun acc (nid, t) ->
        let res = MinTree.evaluate_tree t in
        L.(debug Analysis Medium) "@\n   Tree %a eval to %a @\n" Node.pp_id nid Itv.Bound.pp res ;
        Node.IdMap.add nid res acc )
      ~init:Node.IdMap.empty min_trees
  in
  let initWCET = (Itv.Bound.zero, ReportedOnNodes.empty) in
  let invariant_map_WCETFinal =
    (* Final map with nodes cost *)
    AnalyzerWCET.exec_cfg instr_cfg
      (ProcData.make proc_desc tenv
         {basic_cost_map= invariant_map_NodesBasicCost; min_trees_map= trees_valuation; summary})
      ~initial:initWCET ~debug:true
  in
  match
    AnalyzerWCET.extract_post (InstrCFG.id (InstrCFG.exit_node instr_cfg)) invariant_map_WCETFinal
  with
  | Some (exit_cost, _) ->
      L.internal_error "  PROCEDURE COST = %a @\n" Itv.Bound.pp exit_cost ;
      check_and_report_infinity exit_cost proc_desc summary ;
      Summary.update_summary {post= exit_cost} summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Failed to compute final cost for function %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary
