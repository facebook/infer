(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module BasicCost = CostDomain.BasicCost
module NodesBasicCostDomain = CostDomain.NodeInstructionToCostMap

module Payload = SummaryPayload.Make (struct
  type t = CostDomain.summary

  let update_payloads sum (payloads : Payloads.t) = {payloads with cost= Some sum}

  let of_payloads (payloads : Payloads.t) = payloads.cost
end)

(* We use this treshold to give error if the cost is above it.
   Currently it's set randomly to 200. *)
let expensive_threshold = BasicCost.of_int_exn 200

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module Node = ProcCfg.DefaultNode

(* Compute a map (node,instruction) -> basic_cost, where basic_cost is the
   cost known for a certain operation. For example for basic operation we
   set it to 1 and for function call we take it from the spec of the function.
   The nodes in the domain of the map are those in the path reaching the current node.
*)

let instantiate_cost integer_type_widths ~inferbo_caller_mem ~callee_pname ~params ~callee_cost =
  match Ondemand.get_proc_desc callee_pname with
  | None ->
      L.(die InternalError)
        "Can't instantiate symbolic cost %a from call to %a (can't get procdesc)" BasicCost.pp
        callee_cost Typ.Procname.pp callee_pname
  | Some callee_pdesc ->
      let eval_sym =
        BufferOverrunSemantics.mk_eval_sym integer_type_widths callee_pdesc params
          inferbo_caller_mem
      in
      BasicCost.subst callee_cost eval_sym


module TransferFunctionsNodesBasicCost = struct
  module CFG = InstrCFG
  module Domain = NodesBasicCostDomain

  type extras =
    { inferbo_invariant_map: BufferOverrunChecker.invariant_map
    ; integer_type_widths: Typ.IntegerWidths.t }

  let cost_atomic_instruction = BasicCost.one

  let exec_instr_cost integer_type_widths inferbo_mem
      (astate : CostDomain.NodeInstructionToCostMap.t) {ProcData.pdesc} (node : CFG.Node.t) instr :
      CostDomain.NodeInstructionToCostMap.t =
    let key = CFG.Node.id node in
    let astate' =
      match instr with
      | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), params, _, _) ->
          let callee_cost =
            match CostModels.Call.dispatch () callee_pname params with
            | Some model ->
                model inferbo_mem
            | None -> (
              match Payload.read pdesc callee_pname with
              | Some {post= callee_cost} ->
                  if BasicCost.is_symbolic callee_cost then
                    instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
                      ~callee_pname ~params ~callee_cost
                  else callee_cost
              | None ->
                  cost_atomic_instruction )
          in
          CostDomain.NodeInstructionToCostMap.add key callee_cost astate
      | Sil.Load _ | Sil.Store _ | Sil.Call _ | Sil.Prune _ ->
          CostDomain.NodeInstructionToCostMap.add key cost_atomic_instruction astate
      | Sil.ExitScope _ -> (
        match CFG.Node.kind node with
        | Procdesc.Node.Start_node ->
            CostDomain.NodeInstructionToCostMap.add key cost_atomic_instruction astate
        | _ ->
            astate )
      | _ ->
          astate
    in
    L.(debug Analysis Medium)
      "@\n>>>Instr: %a   Cost: %a@\n"
      (Sil.pp_instr ~print_types:false Pp.text)
      instr CostDomain.NodeInstructionToCostMap.pp astate' ;
    astate'


  let exec_instr costmap ({ProcData.extras= {inferbo_invariant_map; integer_type_widths}} as pdata)
      node instr =
    let inferbo_mem =
      Option.value_exn (BufferOverrunChecker.extract_pre (CFG.Node.id node) inferbo_invariant_map)
    in
    let costmap = exec_instr_cost integer_type_widths inferbo_mem costmap pdata node instr in
    costmap


  let pp_session_name node fmt = F.fprintf fmt "cost(basic) %a" CFG.Node.pp_id (CFG.Node.id node)
end

module AnalyzerNodesBasicCost = AbstractInterpreter.MakeRPO (TransferFunctionsNodesBasicCost)

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
  let print_upper_bound_map bound_map =
    L.(debug Analysis Medium)
      "@\n\n******* Bound Map : [node -> bound] ITV **** @\n %a @\n"
      (Node.IdMap.pp ~pp_value:BasicCost.pp)
      bound_map ;
    L.(debug Analysis Medium) "@\n******* END Bound Map ITV **** @\n\n"


  let filter_loc vars_to_keep = function
    | AbsLoc.Loc.Var (Var.LogicalVar _) ->
        false
    | AbsLoc.Loc.Var var when Control.VarSet.mem var vars_to_keep ->
        true
    | _ ->
        false


  let compute_upperbound_map node_cfg inferbo_invariant_map control_invariant_map loop_inv_map =
    let compute_node_upper_bound bound_map node =
      let node_id = NodeCFG.Node.id node in
      match Procdesc.Node.get_kind node with
      | Procdesc.Node.Exit_node ->
          Node.IdMap.add node_id BasicCost.one bound_map
      | _ -> (
          let exit_state_opt =
            let instr_node_id = InstrCFG.last_of_underlying_node node |> InstrCFG.Node.id in
            BufferOverrunChecker.extract_post instr_node_id inferbo_invariant_map
          in
          match exit_state_opt with
          | Some entry_mem ->
              (* compute control vars, i.e. set of variables that affect the execution count *)
              let control_vars =
                Control.compute_control_vars control_invariant_map loop_inv_map node
              in
              L.(debug Analysis Medium)
                "@\n>>> All dependencies for node = %a : %a  @\n\n" Procdesc.Node.pp node
                Control.VarSet.pp control_vars ;
              (* bound = env(v1) *... * env(vn) *)
              let bound =
                match entry_mem with
                | Bottom ->
                    L.internal_error
                      "@\n\
                       [COST ANALYSIS INTERNAL WARNING:] No 'env' found. This location is \
                       unreachable returning cost 0 \n" ;
                    BasicCost.zero
                | NonBottom mem ->
                    BufferOverrunDomain.MemReach.range ~filter_loc:(filter_loc control_vars) mem
              in
              L.(debug Analysis Medium)
                "@\n>>>Setting bound for node = %a  to %a@\n\n" Node.pp_id node_id BasicCost.pp
                bound ;
              Node.IdMap.add node_id bound bound_map
          | _ ->
              Node.IdMap.add node_id BasicCost.zero bound_map )
    in
    let bound_map =
      NodeCFG.fold_nodes node_cfg ~f:compute_node_upper_bound ~init:Node.IdMap.empty
    in
    print_upper_bound_map bound_map ; bound_map


  let upperbound bound_map nid =
    match Node.IdMap.find_opt nid bound_map with
    | Some bound ->
        bound
    | None ->
        L.(debug Analysis Medium)
          "@\n\n[WARNING] Bound not found for node %a, returning Top @\n" Node.pp_id nid ;
        BasicCost.top
end

module ControlFlowCost = struct
  (* A Control-flow cost represents the number of times the flow of control can go through a certain CFG item (a node or an edge),
  or a sum of such things *)

  module Item = struct
    type t = [`Node of Node.id | `Edge of Node.id * Node.id]

    let compare : t -> t -> int =
     fun x y ->
      match (x, y) with
      | `Node id1, `Node id2 ->
          Node.compare_id id1 id2
      | `Node _, `Edge _ ->
          -1
      | `Edge _, `Node _ ->
          1
      | `Edge (f1, t1), `Edge (f2, t2) ->
          [%compare: Node.id * Node.id] (f1, t1) (f2, t2)


    let equal = [%compare.equal: t]

    let pp : F.formatter -> t -> unit =
     fun fmt -> function
      | `Node id ->
          F.fprintf fmt "Node(%a)" Node.pp_id id
      | `Edge (f, t) ->
          F.fprintf fmt "Edge(%a -> %a)" Node.pp_id f Node.pp_id t


    let normalize ~(normalizer : t -> [> t]) (x : t) : t =
      match normalizer x with #t as x -> x | _ -> assert false
  end

  module Sum = struct
    type 'a set = (* non-empty sorted list *) 'a list

    type t = [`Sum of int * Item.t set]

    let of_list l =
      let length = List.length l in
      let set = List.sort ~compare:Item.compare l in
      `Sum (length, set)


    let compare : t -> t -> int =
     fun (`Sum (l1, s1)) (`Sum (l2, s2)) -> [%compare: int * Item.t list] (l1, s1) (l2, s2)


    let pp : F.formatter -> t -> unit =
     fun fmt (`Sum (_, set)) -> Pp.seq ~sep:" + " Item.pp fmt set


    let items (`Sum (_, l)) = l

    let normalized_items ~normalizer (`Sum (_, l)) =
      let normalizer = (normalizer :> Item.t -> [> Item.t]) in
      l |> List.rev_map ~f:(Item.normalize ~normalizer)


    let normalize ~normalizer sum = sum |> normalized_items ~normalizer |> of_list

    (* Given a sum and an item, remove one occurence of the item in the sum. Returns [None] if the item is not present in the sum.
      [remove_one_item ~item:A (A + B)] = B
      [remove_one_item ~item:A (A + B + C)] = B + C
      [remove_one_item ~item:A (A + A + B)] = A + B
      [remove_one_item ~item:A (B + C)] = None
      *)
    let remove_one_item ~item (`Sum (len, l)) =
      match IList.remove_first l ~f:(Item.equal item) with
      | None ->
          None
      | Some [e] ->
          Some (e :> [Item.t | t])
      | Some l ->
          Some (`Sum (len - 1, l))


    let cost ~of_item (`Sum (_, l)) =
      List.fold l ~init:BasicCost.zero ~f:(fun cost item -> BasicCost.plus cost (of_item item))
  end

  type t = [Item.t | Sum.t]

  let compare : t -> t -> int =
   fun x y ->
    match (x, y) with
    | (#Item.t as x), (#Item.t as y) ->
        Item.compare x y
    | #Item.t, #Sum.t ->
        -1
    | #Sum.t, #Item.t ->
        1
    | (#Sum.t as x), (#Sum.t as y) ->
        Sum.compare x y


  let make_node node = `Node node

  let make_pred_edge succ pred = `Edge (pred, succ)

  let make_succ_edge pred succ = `Edge (pred, succ)

  let pp : F.formatter -> t -> unit =
   fun fmt -> function #Item.t as item -> Item.pp fmt item | #Sum.t as sum -> Sum.pp fmt sum


  let sum : Item.t list -> t = function [] -> assert false | [e] -> (e :> t) | l -> Sum.of_list l

  module Set = struct
    type elt = t [@@deriving compare]

    type t =
      { mutable size: int
      ; mutable items: Item.t ARList.t
      ; mutable sums: Sum.t ARList.t
      ; mutable cost: BasicCost.t }

    let create e =
      let items, sums =
        match e with
        | #Item.t as item ->
            (ARList.singleton item, ARList.empty)
        | #Sum.t as sum ->
            (ARList.empty, ARList.singleton sum)
      in
      {size= 1; items; sums; cost= BasicCost.top}


    let compare_size {size= size1} {size= size2} = Int.compare size1 size2

    (* Invalidation is just a sanity check, union-find already takes care of it. *)
    let is_valid {size} = size >= 1

    let cost {cost} = cost

    (* move semantics, should not be called with aliases *)
    let merge ~from ~to_ =
      assert (not (phys_equal from to_)) ;
      assert (is_valid from) ;
      assert (is_valid to_) ;
      to_.size <- to_.size + from.size ;
      to_.items <- ARList.append to_.items from.items ;
      to_.sums <- ARList.append to_.sums from.sums ;
      from.size <- 0


    let pp_equalities fmt t =
      ARList.append (t.items :> elt ARList.t) (t.sums :> elt ARList.t)
      |> IContainer.to_rev_list ~fold:ARList.fold_unordered
      |> List.sort ~compare |> Pp.seq ~sep:" = " pp fmt


    let normalize_sums : normalizer:(elt -> elt) -> t -> unit =
     fun ~normalizer t ->
      t.sums
      <- t.sums
         |> IContainer.rev_map_to_list ~fold:ARList.fold_unordered ~f:(Sum.normalize ~normalizer)
         |> List.dedup_and_sort ~compare:Sum.compare
         |> ARList.of_list


    let infer_equalities_by_removing_item ~on_infer t item =
      t.sums
      |> IContainer.rev_filter_map_to_list ~fold:ARList.fold_unordered
           ~f:(Sum.remove_one_item ~item)
      |> IContainer.iter_consecutive ~fold:List.fold ~f:on_infer


    let sum_items t =
      t.sums
      |> ARList.fold_unordered ~init:ARList.empty ~f:(fun acc sum ->
             sum |> Sum.items |> ARList.of_list |> ARList.append acc )
      |> IContainer.to_rev_list ~fold:ARList.fold_unordered
      |> List.dedup_and_sort ~compare:Item.compare


    let infer_equalities_from_sums :
        on_infer:(elt -> elt -> unit) -> normalizer:(elt -> elt) -> t -> unit =
     fun ~on_infer ~normalizer t ->
      normalize_sums ~normalizer t ;
      (* Keep in mind that [on_infer] can modify [t].
        It happens only if we merge a node while infering equalities from it, i.e. in the case an item appears in an equality class both alone and in two sums, i.e. X = A + X = A + B.
        This is not a problem here (we could stop if it happens but it is not necessary as existing equalities still remain true after merges) *)
      (* Also keep in mind that the current version, in the worst-case scenario, is quadratic-ish in the size of the CFG *)
      sum_items t |> List.iter ~f:(fun item -> infer_equalities_by_removing_item ~on_infer t item)


    let init_cost : of_node:(Node.id -> BasicCost.t) -> t -> unit =
     fun ~of_node t ->
      let min_if_node cost item =
        match item with `Node node -> BasicCost.min_default_left cost (of_node node) | _ -> cost
      in
      t.cost <- ARList.fold_unordered t.items ~init:t.cost ~f:min_if_node


    let improve_cost_from_sums :
           on_improve:(Sum.t -> BasicCost.t -> BasicCost.t -> unit)
        -> of_item:(Item.t -> BasicCost.t)
        -> t
        -> unit =
     fun ~on_improve ~of_item t ->
      let f sum =
        let cost_of_sum = Sum.cost ~of_item sum in
        let new_cost = BasicCost.min_default_left t.cost cost_of_sum in
        if not (BasicCost.( <= ) ~lhs:t.cost ~rhs:new_cost) then (
          on_improve sum cost_of_sum new_cost ;
          t.cost <- new_cost )
      in
      Container.iter t.sums ~fold:ARList.fold_unordered ~f


    let improve_cost_with t cost' =
      let old_cost = t.cost in
      let new_cost = BasicCost.min_default_left old_cost cost' in
      if not (BasicCost.( <= ) ~lhs:old_cost ~rhs:new_cost) then (
        t.cost <- new_cost ;
        Some old_cost )
      else None
  end
end

module ConstraintSolver = struct
  type debug = {f: 'a. ('a, F.formatter, unit, unit) format4 -> 'a} [@@unboxed]

  module Equalities = struct
    include ImperativeUnionFind.Make (ControlFlowCost.Set)

    let normalizer equalities e = (find equalities e :> ControlFlowCost.t)

    let pp_repr fmt (repr : Repr.t) = ControlFlowCost.pp fmt (repr :> ControlFlowCost.t)

    let pp_equalities fmt equalities =
      let pp_item fmt (repr, set) =
        F.fprintf fmt "%a --> %a" pp_repr repr ControlFlowCost.Set.pp_equalities set
      in
      IContainer.pp_collection ~fold:fold_sets ~pp_item fmt equalities


    let pp_costs fmt equalities =
      let pp_item fmt (repr, set) =
        F.fprintf fmt "%a --> %a" pp_repr repr BasicCost.pp (ControlFlowCost.Set.cost set)
      in
      IContainer.pp_collection ~fold:fold_sets ~pp_item fmt equalities


    let log_union ~debug equalities e1 e2 =
      match union equalities e1 e2 with
      | None ->
          debug.f "[UF] Preexisting %a = %a@\n" ControlFlowCost.pp e1 ControlFlowCost.pp e2 ;
          false
      | Some (e1, e2) ->
          debug.f "[UF] Union %a into %a@\n" ControlFlowCost.pp e1 ControlFlowCost.pp e2 ;
          true


    let try_to_improve ~debug ~on_improve ~f equalities ~max =
      let f did_improve repr_set =
        if did_improve then (
          f ~did_improve:(fun () -> ()) repr_set ;
          true )
        else
          let did_improve = ref false in
          f ~did_improve:(fun () -> did_improve := true) repr_set ;
          !did_improve
      in
      let rec loop max =
        if fold_sets equalities ~init:false ~f then (
          on_improve () ;
          if max > 0 then loop (max - 1)
          else debug.f "[ConstraintSolver] Maximum number of iterations reached@\n" )
      in
      loop max


    (**
    Infer equalities from sums, like this:
      (1) A + sum1 = A + sum2  =>  sum1 = sum2

    It does not try to saturate
      (2) A = B + C  /\  B = D + E  =>  A = C + D + E
    Nor combine more than 2 equations
      (3) A = B + C  /\  B = D + E  /\  F = C + D + E  =>  A = F
      ((3) is implied by (1) /\ (2))

    Its complexity is unknown but I think it is bounded by nbNodes x nbEdges x max.
  *)
    let infer_equalities_from_sums ~debug equalities ~max =
      let normalizer = normalizer equalities in
      let f ~did_improve (_repr, set) =
        let on_infer e1 e2 = if log_union equalities ~debug e1 e2 then did_improve () in
        ControlFlowCost.Set.infer_equalities_from_sums ~on_infer ~normalizer set
      in
      let on_improve () = debug.f "[ConstraintSolver][EInfe] %a@\n" pp_equalities equalities in
      try_to_improve ~debug ~on_improve ~f equalities ~max


    let normalize_sums equalities =
      let normalizer = normalizer equalities in
      Container.iter ~fold:fold_sets equalities ~f:(fun (_repr, set) ->
          ControlFlowCost.Set.normalize_sums ~normalizer set )


    let union ~debug equalities e1 e2 =
      let _ : bool = log_union ~debug equalities e1 e2 in
      ()


    let init_costs bound_map equalities =
      let of_node node_id = BoundMap.upperbound bound_map node_id in
      Container.iter equalities ~fold:fold_sets ~f:(fun (_repr, set) ->
          ControlFlowCost.Set.init_cost ~of_node set )


    (**
      From sums: if A = B + C, do cost(A) = min(cost(A), cost(B) + cost(C))
      From inequalities: if A = B + C, then B <= A, do cost(B) = min(cost(B), cost(A))
    *)
    let improve_costs ~debug equalities ~max =
      let of_item (item : ControlFlowCost.Item.t) =
        (item :> ControlFlowCost.t)
        |> find equalities |> find_set equalities
        |> Option.value_map ~f:ControlFlowCost.Set.cost ~default:BasicCost.top
      in
      let f ~did_improve (repr, set) =
        let on_improve sum cost_of_sum new_cost =
          debug.f
            "[ConstraintSolver][CImpr] Improved cost of %a using %a (cost: %a), from %a to %a@\n"
            pp_repr repr ControlFlowCost.Sum.pp sum BasicCost.pp cost_of_sum BasicCost.pp
            (ControlFlowCost.Set.cost set) BasicCost.pp new_cost ;
          did_improve ()
        in
        ControlFlowCost.Set.improve_cost_from_sums ~on_improve ~of_item set ;
        let try_from_inequality (sum_item : ControlFlowCost.Item.t) =
          let sum_item_set =
            (sum_item :> ControlFlowCost.t) |> find equalities |> find_create_set equalities
          in
          match
            ControlFlowCost.Set.improve_cost_with sum_item_set (ControlFlowCost.Set.cost set)
          with
          | Some previous_cost ->
              debug.f
                "[ConstraintSolver][CImpr] Improved cost of %a <= %a (cost: %a), from %a to %a@\n"
                ControlFlowCost.Item.pp sum_item pp_repr repr BasicCost.pp
                (ControlFlowCost.Set.cost set) BasicCost.pp previous_cost BasicCost.pp
                (ControlFlowCost.Set.cost sum_item_set) ;
              did_improve ()
          | None ->
              ()
        in
        ControlFlowCost.Set.sum_items set |> List.iter ~f:try_from_inequality
      in
      let on_improve () = debug.f "[ConstraintSolver][CImpr] %a@\n" pp_costs equalities in
      try_to_improve ~debug ~on_improve ~f equalities ~max
  end

  let add_constraints ~debug equalities node get_nodes make =
    match get_nodes node with
    | [] ->
        (* either start/exit node or dead node (broken CFG) *)
        ()
    | nodes ->
        let node_id = Node.id node in
        let edges = List.rev_map nodes ~f:(fun other -> make node_id (Node.id other)) in
        let sum = ControlFlowCost.sum edges in
        Equalities.union ~debug equalities (ControlFlowCost.make_node node_id) sum


  let collect_on_node ~debug equalities node =
    add_constraints ~debug equalities node Procdesc.Node.get_preds ControlFlowCost.make_pred_edge ;
    add_constraints ~debug equalities node Procdesc.Node.get_succs ControlFlowCost.make_succ_edge


  let collect_constraints ~debug node_cfg =
    let equalities = Equalities.create () in
    Container.iter node_cfg ~fold:NodeCFG.fold_nodes ~f:(collect_on_node ~debug equalities) ;
    debug.f "[ConstraintSolver] Procedure %a @@ %a@\n" Typ.Procname.pp
      (Procdesc.get_proc_name node_cfg) Location.pp_file_pos (Procdesc.get_loc node_cfg) ;
    debug.f "[ConstraintSolver][EInit] %a@\n" Equalities.pp_equalities equalities ;
    Equalities.normalize_sums equalities ;
    debug.f "[ConstraintSolver][ENorm] %a@\n" Equalities.pp_equalities equalities ;
    Equalities.infer_equalities_from_sums equalities ~debug ~max:10 ;
    debug.f "[ConstraintSolver][EInfe] %a@\n" Equalities.pp_equalities equalities ;
    equalities


  let compute_costs ~debug bound_map equalities =
    Equalities.init_costs bound_map equalities ;
    debug.f "[ConstraintSolver][CInit] %a@\n" Equalities.pp_costs equalities ;
    Equalities.improve_costs equalities ~debug ~max:10 ;
    debug.f "[ConstraintSolver][CImpr] %a@\n" Equalities.pp_costs equalities


  let get_node_nb_exec equalities node_id =
    let set =
      node_id |> ControlFlowCost.make_node |> Equalities.find equalities
      |> Equalities.find_set equalities
    in
    Option.value_exn set |> ControlFlowCost.Set.cost
end

module ReportedOnNodes = AbstractDomain.FiniteSetOfPPSet (Node.IdSet)

type extras_TransferFunctionsWCET =
  { basic_cost_map: AnalyzerNodesBasicCost.invariant_map
  ; get_node_nb_exec: Node.id -> BasicCost.t
  ; summary: Summary.t }

let compute_errlog_extras cost =
  { Jsonbug_t.cost_polynomial= Some (Format.asprintf "%a" BasicCost.pp cost)
  ; cost_degree= BasicCost.degree cost |> Option.map ~f:Polynomials.Degree.encode_to_int }


(* Calculate the final Worst Case Execution Time predicted for each node.
   It uses the basic cost of the nodes (computed previously by AnalyzerNodesBasicCost)
   and MinTrees which give an upperbound on the number of times a node can be executed
*)
module TransferFunctionsWCET = struct
  module CFG = InstrCFG
  module Domain = AbstractDomain.Pair (BasicCost) (ReportedOnNodes)

  type extras = extras_TransferFunctionsWCET

  let should_report_on_instr = function
    | Sil.Call _ | Load _ | Prune _ | Store _ ->
        true
    | Sil.Abstract _ | Nullify _ | ExitScope _ ->
        false


  (* We don't report when the cost is Top as it corresponds to subsequent 'don't know's.
   Instead, we report Top cost only at the top level per function when `report_infinity` is set to true *)
  let should_report_cost cost =
    Config.use_cost_threshold
    && (not (BasicCost.is_top cost))
    && not (BasicCost.( <= ) ~lhs:cost ~rhs:expensive_threshold)


  let do_report summary loc cost =
    let degree_str =
      match BasicCost.degree cost with
      | Some degree ->
          Format.asprintf ", degree = %a" Polynomials.Degree.pp degree
      | None ->
          ""
    in
    let ltr =
      let cost_desc = F.asprintf "with estimated cost %a%s" BasicCost.pp cost degree_str in
      [Errlog.make_trace_element 0 loc cost_desc []]
    in
    let message =
      F.asprintf
        "The execution time from the beginning of the function up to this program point is likely \
         above the acceptable threshold of %a (estimated cost %a%s)"
        BasicCost.pp expensive_threshold BasicCost.pp cost degree_str
    in
    Reporting.log_error summary ~loc ~ltr ~extras:(compute_errlog_extras cost)
      IssueType.expensive_execution_time_call message


  (* get a list of nodes and check if we have already reported for at
     least one of them. In that case no need to report again. *)
  let should_report_on_node preds reported_so_far =
    List.for_all
      ~f:(fun node ->
        let nid = Procdesc.Node.get_id node in
        not (ReportedOnNodes.mem nid reported_so_far) )
      preds


  let map_cost get_node_nb_exec m : BasicCost.t =
    CostDomain.NodeInstructionToCostMap.fold
      (fun ((node_id, _) as instr_node_id) c acc ->
        let t = get_node_nb_exec node_id in
        let c_node = BasicCost.mult c t in
        let c_node' = BasicCost.plus acc c_node in
        L.(debug Analysis Medium)
          "@\n  [AnalyzerWCET] Adding cost: (%a) --> c =%a  t = %a @\n" InstrCFG.Node.pp_id
          instr_node_id BasicCost.pp c BasicCost.pp t ;
        L.(debug Analysis Medium)
          "@\n  [AnalyzerWCET] Adding cost: (%a) --> c_node=%a  cost = %a @\n" InstrCFG.Node.pp_id
          instr_node_id BasicCost.pp c_node BasicCost.pp c_node' ;
        c_node' )
      m BasicCost.zero


  let exec_instr ((_, reported_so_far) : Domain.t) {ProcData.extras} (node : CFG.Node.t) instr :
      Domain.t =
    let {basic_cost_map= invariant_map_cost; get_node_nb_exec; summary} = extras in
    let cost_node =
      let instr_node_id = CFG.Node.id node in
      match AnalyzerNodesBasicCost.extract_post instr_node_id invariant_map_cost with
      | Some node_map ->
          L.(debug Analysis Medium)
            "@\n [AnalyzerWCET] Final map for node: %a @\n" CFG.Node.pp_id instr_node_id ;
          map_cost get_node_nb_exec node_map
      | _ ->
          assert false
    in
    L.(debug Analysis Medium)
      "@\n[>>>AnalyzerWCET] Instr: %a   Cost: %a@\n"
      (Sil.pp_instr ~print_types:false Pp.text)
      instr BasicCost.pp cost_node ;
    let astate' =
      let und_node = CFG.Node.underlying_node node in
      let preds = Procdesc.Node.get_preds und_node in
      let reported_so_far =
        if
          should_report_on_instr instr
          && should_report_on_node (und_node :: preds) reported_so_far
          && should_report_cost cost_node
        then (
          do_report summary (Sil.instr_get_loc instr) cost_node ;
          let nid = Procdesc.Node.get_id und_node in
          ReportedOnNodes.add nid reported_so_far )
        else reported_so_far
      in
      (cost_node, reported_so_far)
    in
    astate'


  let pp_session_name _node fmt = F.pp_print_string fmt "cost(wcet)"
end

module AnalyzerWCET = AbstractInterpreter.MakeRPO (TransferFunctionsWCET)

let check_and_report_top_and_bottom cost proc_desc summary =
  let report issue suffix =
    let message =
      F.asprintf "The execution time of the function %a %s" Typ.Procname.pp
        (Procdesc.get_proc_name proc_desc)
        suffix
    in
    let loc = Procdesc.get_start_node proc_desc |> Procdesc.Node.get_loc in
    Reporting.log_error ~loc ~extras:(compute_errlog_extras cost) summary issue message
  in
  if BasicCost.is_top cost then report IssueType.infinite_execution_time_call "cannot be computed"
  else if BasicCost.is_zero cost then report IssueType.zero_execution_time_call "is zero"


let checker ({Callbacks.tenv; proc_desc; integer_type_widths} as callback_args) : Summary.t =
  let inferbo_invariant_map, summary =
    BufferOverrunChecker.compute_invariant_map_and_check callback_args
  in
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  let proc_data = ProcData.make_default proc_desc tenv in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map =
    ReachingDefs.Analyzer.exec_cfg node_cfg proc_data
      ~initial:(ReachingDefs.init_reaching_defs_with_formals proc_desc)
  in
  (* collect all prune nodes that occur in loop guards, needed for ControlDepAnalyzer *)
  let control_maps, loop_head_to_loop_nodes = Loop_control.get_control_maps node_cfg in
  (* computes the control dependencies: node -> var set *)
  let control_dep_invariant_map =
    let proc_data = ProcData.make proc_desc tenv control_maps in
    Control.ControlDepAnalyzer.exec_cfg node_cfg proc_data ~initial:Control.ControlDepSet.empty
  in
  let instr_cfg = InstrCFG.from_pdesc proc_desc in
  let invariant_map_NodesBasicCost =
    let proc_data =
      ProcData.make proc_desc tenv
        TransferFunctionsNodesBasicCost.{inferbo_invariant_map; integer_type_widths}
    in
    (*compute_WCET cfg invariant_map min_trees in *)
    AnalyzerNodesBasicCost.exec_cfg instr_cfg proc_data ~initial:NodesBasicCostDomain.empty
  in
  (* compute loop invariant map for control var analysis *)
  let loop_inv_map =
    LoopInvariant.get_loop_inv_var_map tenv reaching_defs_invariant_map loop_head_to_loop_nodes
  in
  (* given the semantics computes the upper bound on the number of times a node could be executed *)
  let bound_map =
    BoundMap.compute_upperbound_map node_cfg inferbo_invariant_map control_dep_invariant_map
      loop_inv_map
  in
  let get_node_nb_exec =
    let debug =
      if Config.write_html then
        let f fmt = L.d_printfln fmt in
        {ConstraintSolver.f}
      else
        let f fmt = L.(debug Analysis Verbose) fmt in
        {ConstraintSolver.f}
    in
    let start_node = NodeCFG.start_node node_cfg in
    ( if Config.write_html then
      let pp_name fmt = F.pp_print_string fmt "cost(constraints)" in
      NodePrinter.start_session ~pp_name start_node ) ;
    let equalities = ConstraintSolver.collect_constraints ~debug node_cfg in
    let () = ConstraintSolver.compute_costs ~debug bound_map equalities in
    if Config.write_html then NodePrinter.finish_session start_node ;
    ConstraintSolver.get_node_nb_exec equalities
  in
  let initWCET = (BasicCost.zero, ReportedOnNodes.empty) in
  match
    AnalyzerWCET.compute_post ~initial:initWCET
      (ProcData.make proc_desc tenv
         {basic_cost_map= invariant_map_NodesBasicCost; get_node_nb_exec; summary})
  with
  | Some (exit_cost, _) ->
      L.(debug Analysis Verbose)
        "@\n[COST ANALYSIS] PROCEDURE '%a' |CFG| = %i FINAL COST = %a @\n" Typ.Procname.pp
        (Procdesc.get_proc_name proc_desc)
        (Container.length ~fold:NodeCFG.fold_nodes node_cfg)
        BasicCost.pp exit_cost ;
      check_and_report_top_and_bottom exit_cost proc_desc summary ;
      Payload.update_summary {post= exit_cost} summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Failed to compute final cost for function %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary
