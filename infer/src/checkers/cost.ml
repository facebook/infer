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

  let update_payloads sum (payloads: Payloads.t) = {payloads with cost= Some sum}

  let of_payloads (payloads: Payloads.t) = payloads.cost
end)

(* We use this treshold to give error if the cost is above it.
   Currently it's set randomly to 200. *)
let expensive_threshold = BasicCost.of_int_exn 200

(* CFG modules used in several other modules  *)
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module NodeCFG = ProcCfg.Normal
module InstrCFGScheduler = Scheduler.ReversePostorder (InstrCFG)
module Node = ProcCfg.DefaultNode

(* Compute a map (node,instruction) -> basic_cost, where basic_cost is the
   cost known for a certain operation. For example for basic operation we
   set it to 1 and for function call we take it from the spec of the function.
   The nodes in the domain of the map are those in the path reaching the current node.
*)
module TransferFunctionsNodesBasicCost = struct
  module CFG = InstrCFG
  module Domain = NodesBasicCostDomain

  type extras = BufferOverrunChecker.invariant_map

  let cost_atomic_instruction = BasicCost.one

  let instantiate_cost ~tenv ~caller_pdesc ~inferbo_caller_mem ~callee_pname ~params ~callee_cost =
    match Ondemand.get_proc_desc callee_pname with
    | None ->
        L.(die InternalError)
          "Can't instantiate symbolic cost %a from call to %a (can't get procdesc)" BasicCost.pp
          callee_cost Typ.Procname.pp callee_pname
    | Some callee_pdesc ->
      match BufferOverrunChecker.Payload.read caller_pdesc callee_pname with
      | None ->
          L.(die InternalError)
            "Can't instantiate symbolic cost %a from call to %a (can't get summary)" BasicCost.pp
            callee_cost Typ.Procname.pp callee_pname
      | Some inferbo_summary ->
          let inferbo_caller_mem = Option.value_exn inferbo_caller_mem in
          let callee_entry_mem = BufferOverrunDomain.Summary.get_input inferbo_summary in
          let callee_exit_mem = BufferOverrunDomain.Summary.get_output inferbo_summary in
          let callee_ret_alias = BufferOverrunDomain.Mem.find_ret_alias callee_exit_mem in
          let (subst_map, _), _ =
            BufferOverrunSemantics.get_subst_map tenv callee_pdesc params inferbo_caller_mem
              callee_entry_mem ~callee_ret_alias
          in
          BasicCost.subst callee_cost subst_map


  let exec_instr_cost inferbo_mem (astate: CostDomain.NodeInstructionToCostMap.astate)
      {ProcData.pdesc; tenv} (node: CFG.Node.t) instr : CostDomain.NodeInstructionToCostMap.astate =
    let key = CFG.Node.id node in
    let astate' =
      match instr with
      | Sil.Call (_, Exp.Const (Const.Cfun callee_pname), params, _, _) ->
          let callee_cost =
            match Payload.read pdesc callee_pname with
            | Some {post= callee_cost} ->
                if BasicCost.is_symbolic callee_cost then
                  instantiate_cost ~tenv ~caller_pdesc:pdesc ~inferbo_caller_mem:inferbo_mem
                    ~callee_pname ~params ~callee_cost
                else callee_cost
            | None ->
                cost_atomic_instruction
          in
          CostDomain.NodeInstructionToCostMap.add key callee_cost astate
      | Sil.Load _ | Sil.Store _ | Sil.Call _ | Sil.Prune _ ->
          CostDomain.NodeInstructionToCostMap.add key cost_atomic_instruction astate
      | _ ->
          astate
    in
    L.(debug Analysis Medium)
      "@\n>>>Instr: %a   Cost: %a@\n" (Sil.pp_instr Pp.text) instr
      CostDomain.NodeInstructionToCostMap.pp astate' ;
    astate'


  let exec_instr costmap ({ProcData.extras= inferbo_invariant_map} as pdata) node instr =
    let inferbo_mem = BufferOverrunChecker.extract_pre (CFG.Node.id node) inferbo_invariant_map in
    let costmap = exec_instr_cost inferbo_mem costmap pdata node instr in
    costmap


  let pp_session_name node fmt = F.fprintf fmt "cost(basic) %a" CFG.Node.pp_id (CFG.Node.id node)
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
  type t = BasicCost.astate Node.IdMap.t

  let print_upper_bound_map bound_map =
    L.(debug Analysis Medium) "@\n\n******* Bound Map ITV **** @\n" ;
    Node.IdMap.iter
      (fun nid b ->
        L.(debug Analysis Medium) "@\n node: %a --> bound = %a @\n" Node.pp_id nid BasicCost.pp b
        )
      bound_map ;
    L.(debug Analysis Medium) "@\n******* END Bound Map ITV **** @\n\n"


  let filter_loc formal_pvars vars_to_keep = function
    | AbsLoc.Loc.Var (Var.LogicalVar _) ->
        false
    | AbsLoc.Loc.Var (Var.ProgramVar pvar) when List.mem formal_pvars pvar ~equal:Pvar.equal ->
        false
    | AbsLoc.Loc.Var var when Control.VarSet.mem var vars_to_keep ->
        true
    | _ ->
        false


  let compute_upperbound_map node_cfg inferbo_invariant_map data_invariant_map
      control_invariant_map =
    let pname = Procdesc.get_proc_name node_cfg in
    let formal_pvars =
      Procdesc.get_formals node_cfg |> List.map ~f:(fun (m, _) -> Pvar.mk m pname)
    in
    let compute_node_upper_bound bound_map node =
      let node_id = NodeCFG.Node.id node in
      match Procdesc.Node.get_kind node with
      | Procdesc.Node.Exit_node _ ->
          Node.IdMap.add node_id BasicCost.one bound_map
      | _ ->
          let exit_state_opt =
            let instr_node_id = InstrCFG.last_of_underlying_node node |> InstrCFG.Node.id in
            BufferOverrunChecker.extract_post instr_node_id inferbo_invariant_map
          in
          match exit_state_opt with
          | Some entry_mem ->
              (* compute all the dependencies, i.e. set of variables that affect the control flow upto the node *)
              let all_deps =
                Control.compute_all_deps data_invariant_map control_invariant_map node
              in
              L.(debug Analysis Medium)
                "@\n>>> All dependencies for node = %a : %a  @\n\n" Procdesc.Node.pp node
                Control.VarSet.pp all_deps ;
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
                    BufferOverrunDomain.MemReach.heap_range
                      ~filter_loc:(filter_loc formal_pvars all_deps)
                      mem
              in
              L.(debug Analysis Medium)
                "@\n>>>Setting bound for node = %a  to %a@\n\n" Node.pp_id node_id BasicCost.pp
                bound ;
              Node.IdMap.add node_id bound bound_map
          | _ ->
              Node.IdMap.add node_id BasicCost.zero bound_map
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
          [%compare : Node.id * Node.id] (f1, t1) (f2, t2)


    let equal = [%compare.equal : t]

    let pp : F.formatter -> t -> unit =
     fun fmt -> function
      | `Node id ->
          F.fprintf fmt "Node(%a)" Node.pp_id id
      | `Edge (f, t) ->
          F.fprintf fmt "Edge(%a -> %a)" Node.pp_id f Node.pp_id t


    let normalize ~(normalizer: t -> [> t]) (x: t) : t =
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
     fun (`Sum (l1, s1)) (`Sum (l2, s2)) -> [%compare : int * Item.t list] (l1, s1) (l2, s2)


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


  let equal = [%compare.equal : t]

  let make_node node = `Node node

  let make_pred_edge succ pred = `Edge (pred, succ)

  let make_succ_edge pred succ = `Edge (pred, succ)

  let pp : F.formatter -> t -> unit =
   fun fmt -> function #Item.t as item -> Item.pp fmt item | #Sum.t as sum -> Sum.pp fmt sum


  let sum : Item.t list -> t = function [] -> assert false | [e] -> (e :> t) | l -> Sum.of_list l

  module Set = struct
    type elt = t

    type t = {mutable size: int; mutable items: Item.t ARList.t; mutable sums: Sum.t ARList.t}

    let create e =
      let items, sums =
        match e with
        | #Item.t as item ->
            (ARList.singleton item, ARList.empty)
        | #Sum.t as sum ->
            (ARList.empty, ARList.singleton sum)
      in
      {size= 1; items; sums}


    (* Because we are modifying things on which we are iterating, we want to invalidate them first before removing them from hashtables, to avoid iterator invalidation. *)
    let is_valid {size} = size >= 1

    let size {size} = size

    (* move semantics, should not be called with aliases *)
    let merge_invalidate ~from ~to_ =
      assert (not (phys_equal from to_)) ;
      assert (is_valid from) ;
      assert (is_valid to_) ;
      to_.size <- to_.size + from.size ;
      to_.items <- ARList.append to_.items from.items ;
      to_.sums <- ARList.append to_.sums from.sums ;
      from.size <- 0


    let pp_equalities fmt t =
      if is_valid t then
        ARList.append (t.items :> elt ARList.t) (t.sums :> elt ARList.t)
        |> IContainer.to_rev_list ~fold:ARList.fold_unordered |> List.sort ~compare
        |> Pp.seq ~sep:" = " pp fmt


    let normalize_sums : normalizer:(elt -> elt) -> t -> unit =
     fun ~normalizer t ->
      assert (is_valid t) ;
      t.sums
      <- t.sums
         |> IContainer.rev_map_to_list ~fold:ARList.fold_unordered ~f:(Sum.normalize ~normalizer)
         |> List.dedup_and_sort ~compare:Sum.compare |> ARList.of_list


    let infer_equalities_by_removing_item ~on_infer t item =
      t.sums
      |> IContainer.rev_filter_map_to_list ~fold:ARList.fold_unordered
           ~f:(Sum.remove_one_item ~item)
      |> IContainer.iter_consecutive ~fold:List.fold ~f:on_infer


    let infer_equalities_from_sums
        : on_infer:(elt -> elt -> unit) -> normalizer:(elt -> elt) -> t -> unit =
     fun ~on_infer ~normalizer t ->
      if is_valid t then (
        normalize_sums ~normalizer t ;
        let all_items =
          t.sums
          |> ARList.fold_unordered ~init:ARList.empty ~f:(fun acc sum ->
                 sum |> Sum.items |> ARList.of_list |> ARList.append acc )
          |> IContainer.to_rev_list ~fold:ARList.fold_unordered
          |> List.dedup_and_sort ~compare:Item.compare
        in
        (* Keep in mind that [on_infer] can modify (and possibly invalidate) [t].
        It happens only if we merge a node while infer equalities from it, i.e. in the case an item appears in an equality class both alone and in two sums, i.e. X = A + X = A + B.
        This is not a problem here (we could stop if it happens but it is not necessary as existing equalities still remain true after merges) *)
        (* Also keep in mind that the current version, in the worst-case scenario, is quadratic-ish in the size of the CFG *)
        List.iter all_items ~f:(fun item -> infer_equalities_by_removing_item ~on_infer t item) )
  end
end

module ImperativeUnionFind (E : sig
  type t [@@deriving compare]

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit

  module Set : sig
    type elt = t

    type t

    val create : elt -> t

    val size : t -> int

    val merge_invalidate : from:t -> to_:t -> unit

    val pp_equalities : F.formatter -> t -> unit

    val normalize_sums : normalizer:(elt -> elt) -> t -> unit

    val infer_equalities_from_sums :
      on_infer:(elt -> elt -> unit) -> normalizer:(elt -> elt) -> t -> unit
  end
end) =
struct
  module H = struct
    include Caml.Hashtbl

    let caml_fold = fold

    let fold : (('a, 'b) t, 'a * 'b, 'accum) Container.fold =
     fun h ~init ~f ->
      let f' k v accum = f accum (k, v) in
      caml_fold f' h init


    let pp_bindings pp_key pp_value fmt h =
      let pp_item fmt (k, v) = F.fprintf fmt "%a --> %a" pp_key k pp_value v in
      IContainer.pp_collection ~fold ~pp_item fmt h


    let[@warning "-32"] pp_values pp_value fmt h =
      let pp_item fmt (_, v) = pp_value fmt v in
      IContainer.pp_collection ~fold ~pp_item fmt h
  end

  module Repr : sig
    (* Sort-of abstracting away the fact that a representative is just an element itself.
      This ensures that the [Sets] hashtable is accessed with representative and not just elements. *)

    type t = private E.t

    val equal : t -> t -> bool

    val pp : F.formatter -> t -> unit

    val of_e : E.t -> t

    val is_simpler_than : t -> t -> bool
  end = struct
    include E

    let of_e e = e

    let is_simpler_than r1 r2 = compare r1 r2 <= 0
  end

  module Reprs = struct
    type t = (E.t, Repr.t) H.t

    let create () = H.create 1

    let rec find (t: t) e : Repr.t =
      match H.find_opt t e with
      | None ->
          Repr.of_e e
      | Some r ->
          let r' = find t (r :> E.t) in
          if not (phys_equal r r') then H.replace t e r' ;
          r'


    let merge (t: t) ~(from: Repr.t) ~(to_: Repr.t) = H.replace t (from :> E.t) to_

    let normalizer t e = (find t e :> E.t)
  end

  module Set = E.Set

  module Sets = struct
    type t = (Repr.t, Set.t) H.t

    let create () = H.create 1

    let find t (r: Repr.t) =
      match H.find_opt t r with
      | Some set ->
          set
      | None ->
          let set = Set.create (r :> E.t) in
          H.replace t r set ; set


    let merge_no_remove _t ~from:(from_r, from_set) ~to_:(_, to_set) =
      Set.merge_invalidate ~from:from_set ~to_:to_set ;
      from_r


    let merge t ~from:(from_r, from_set) ~to_:(_, to_set) =
      H.remove t from_r ;
      Set.merge_invalidate ~from:from_set ~to_:to_set


    let pp_equalities fmt t = H.pp_bindings Repr.pp Set.pp_equalities fmt t

    let normalize_sums ~normalizer t =
      H.iter (fun _repr set -> Set.normalize_sums ~normalizer set) t


    let infer_equalities_from_sums ~on_infer ~normalizer t =
      H.iter (fun _repr set -> Set.infer_equalities_from_sums ~on_infer ~normalizer set) t


    let remove_list t rs = List.iter rs ~f:(H.remove t)
  end

  (**
    Data-structure for disjoint sets.
    [reprs] is the mapping element -> representative
    [sets] is the mapping representative -> set

    It implements path-compression and union by size, hence find and union are amortized O(1)-ish.
  *)
  type t = {reprs: Reprs.t; sets: Sets.t}

  let create () = {reprs= Reprs.create (); sets= Sets.create ()}

  let do_merge t ~from ~to_ ~merge_sets =
    let to_r, _ = to_ in
    let from_r, _ = from in
    Reprs.merge t.reprs ~from:from_r ~to_:to_r ;
    merge_sets ~from ~to_


  let union_with_merge t e1 e2 ~merge_sets =
    let repr1 = Reprs.find t.reprs e1 in
    let repr2 = Reprs.find t.reprs e2 in
    if Repr.equal repr1 repr2 then None
    else
      let set1 = Sets.find t.sets repr1 in
      let set2 = Sets.find t.sets repr2 in
      let size1 = Set.size set1 in
      let size2 = Set.size set2 in
      if size1 < size2 || (Int.equal size1 size2 && Repr.is_simpler_than repr2 repr1) then (
        (* A desired side-effect of using [is_simpler_than] is that the representative for a set will always be a [`Node]. For now. *)
        do_merge t ~from:(repr1, set1) ~to_:(repr2, set2) ~merge_sets ;
        Some (e1, e2) )
      else (
        do_merge t ~from:(repr2, set2) ~to_:(repr1, set1) ~merge_sets ;
        Some (e2, e1) )


  let union_log t e1 e2 ~merge_sets =
    match union_with_merge t e1 e2 ~merge_sets with
    | None ->
        L.(debug Analysis Verbose) "[UF] Preexisting %a = %a@\n" E.pp e1 E.pp e2
    | Some (e1, e2) ->
        L.(debug Analysis Verbose) "[UF] Union %a into %a@\n" E.pp e1 E.pp e2


  let union t e1 e2 = union_log t e1 e2 ~merge_sets:(Sets.merge t.sets)

  let union_defer_remove t e1 e2 =
    let res = ref None in
    let merge_sets ~from ~to_ = res := Some (Sets.merge_no_remove t.sets ~from ~to_) in
    union_log t e1 e2 ~merge_sets ; !res


  let pp_equalities fmt t = Sets.pp_equalities fmt t.sets

  let normalizer t = Reprs.normalizer t.reprs

  let normalize_sums t = Sets.normalize_sums ~normalizer:(normalizer t) t.sets

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
  let infer_equalities_from_sums t ~max =
    let normalizer = normalizer t in
    let on_infer sets_to_remove e1 e2 =
      (* need to defer removes to avoid iterator invalidation *)
      union_defer_remove t e1 e2
      |> Option.iter ~f:(fun set_to_remove -> sets_to_remove := set_to_remove :: !sets_to_remove)
    in
    let rec loop max =
      let sets_to_remove = ref [] in
      let on_infer = on_infer sets_to_remove in
      Sets.infer_equalities_from_sums ~on_infer ~normalizer t.sets ;
      if not (List.is_empty !sets_to_remove) then (
        Sets.remove_list t.sets !sets_to_remove ;
        L.(debug Analysis Verbose) "[ConstraintSolver] %a@\n" pp_equalities t ;
        if max > 0 then loop (max - 1)
        else
          L.(debug Analysis Verbose) "[ConstraintSolver] Maximum number of iterations reached@\n" )
    in
    loop max
end

module ConstraintSolver = struct
  module Equalities = ImperativeUnionFind (ControlFlowCost)

  let add_constraints equalities node get_nodes make =
    match get_nodes node with
    | [] ->
        (* either start/exit node or dead node (broken CFG) *)
        ()
    | nodes ->
        let node_id = Node.id node in
        let edges = List.rev_map nodes ~f:(fun other -> make node_id (Node.id other)) in
        let sum = ControlFlowCost.sum edges in
        Equalities.union equalities (ControlFlowCost.make_node node_id) sum


  let collect_on_node equalities node =
    add_constraints equalities node Procdesc.Node.get_preds ControlFlowCost.make_pred_edge ;
    add_constraints equalities node Procdesc.Node.get_succs ControlFlowCost.make_succ_edge


  let collect_constraints node_cfg =
    let equalities = Equalities.create () in
    Container.iter node_cfg ~fold:NodeCFG.fold_nodes ~f:(collect_on_node equalities) ;
    L.(debug Analysis Verbose)
      "[ConstraintSolver] Procedure %a @@ %a@\n" Typ.Procname.pp (Procdesc.get_proc_name node_cfg)
      Location.pp_file_pos (Procdesc.get_loc node_cfg) ;
    L.(debug Analysis Verbose) "[ConstraintSolver] %a@\n" Equalities.pp_equalities equalities ;
    Equalities.normalize_sums equalities ;
    L.(debug Analysis Verbose) "[ConstraintSolver] %a@\n" Equalities.pp_equalities equalities ;
    Equalities.infer_equalities_from_sums equalities ~max:10 ;
    L.(debug Analysis Verbose) "[ConstraintSolver] %a@\n" Equalities.pp_equalities equalities ;
    equalities
end

(* Structural Constraints are expressions of the kind:
     n <= n1 +...+ nk

   The informal meaning is: the number of times node n can be executed is less or
   equal to the sum of the number of times nodes n1,..., nk can be executed.
*)
module StructuralConstraints = struct
  type t = {single: Node.id list; sum: Node.IdSet.t list}

  (*
  Finds subset of constraints of node k.
  It returns a pair (single_constraints, sum_constraints) where single constraints are
  of the form 'x_k <= x_j' and  sum constraints are of the form 'x_k <= x_j1 +...+ x_jn'.
  *)
  let get_constraints_of_node constraints k =
    let c = Node.IdMap.find_opt k constraints in
    match c with Some c -> c | _ -> {single= []; sum= []}


  let print_constraints_map constraints =
    let pp_nidset fmt nidset = Pp.seq ~sep:" + " Node.pp_id fmt (Node.IdSet.elements nidset) in
    L.(debug Analysis Medium)
      "@\n\n******* Structural Constraints size = %i **** @\n" (Node.IdMap.cardinal constraints) ;
    Node.IdMap.iter
      (fun n {single; sum} ->
        List.iter
          ~f:(fun s -> L.(debug Analysis Medium) "@\n    %a <= %a @\n" Node.pp_id n Node.pp_id s)
          single ;
        List.iter
          ~f:(fun s -> L.(debug Analysis Medium) "@\n    %a <= %a @\n" Node.pp_id n pp_nidset s)
          sum )
      constraints ;
    L.(debug Analysis Medium) "@\n******* END Structural Constraints **** @\n\n"


  (*  for each program point return a set of contraints of the kind

     i<=Sum_{j \in Predecessors(i) } j
     i<=Sum_{j \in Successors(i)} j
*)
  let compute_structural_constraints node_cfg =
    let compute_node_constraints acc node =
      let constraints_add node get_nodes =
        match get_nodes node with
        | [] ->
            {single= []; sum= []}
        | [single] ->
            {single= [NodeCFG.Node.id single]; sum= []}
        | nodes ->
            let sum =
              List.fold nodes ~init:Node.IdSet.empty ~f:(fun idset node ->
                  Node.IdSet.add (NodeCFG.Node.id node) idset )
            in
            {single= []; sum= [sum]}
      in
      let preds = constraints_add node Procdesc.Node.get_preds in
      let succs = constraints_add node Procdesc.Node.get_succs in
      Node.IdMap.add (NodeCFG.Node.id node)
        {single= List.append preds.single succs.single; sum= List.append preds.sum succs.sum} acc
    in
    let constraints =
      NodeCFG.fold_nodes node_cfg ~f:compute_node_constraints ~init:Node.IdMap.empty
    in
    print_constraints_map constraints ; constraints
end

(* MinTree is used to compute:

    \max (\Sum_{n \in Nodes} c_n * x_n )

   given a set of contraints on x_n. The constraints involve the contro flow
    of the program.

*)
module MinTree = struct
  type mt_node =
    | Leaf of (Node.id * BasicCost.astate)
    | Min of mt_node list
    | Plus of mt_node list

  let add_leaf node nid leaf =
    let leaf' = Leaf (nid, leaf) in
    match node with Min l -> Min (leaf' :: l) | Plus l -> Plus (leaf' :: l) | _ -> assert false


  let plus_seq pp f l = Pp.seq ~sep:" + " pp f l

  let rec pp fmt node =
    match node with
    | Leaf (nid, c) ->
        F.fprintf fmt "%a:%a" Node.pp_id nid BasicCost.pp c
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


  let rec evaluate_tree t =
    match t with
    | Leaf (_, c) ->
        c
    | Min l ->
        evaluate_operator BasicCost.min l
    | Plus l ->
        evaluate_operator BasicCost.plus l


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

  let minimum_propagation (bound_map: BoundMap.t)
      (constraints: StructuralConstraints.t Node.IdMap.t) self
      ((q, visited): Node.id * Node.IdSet.t) =
    let rec build_min node branch visited_acc worklist =
      match worklist with
      | [] ->
          (node, branch, visited_acc)
      | k :: rest ->
          if Node.IdSet.mem k visited_acc then build_min node branch visited_acc rest
          else
            let visited_acc' = Node.IdSet.add k visited_acc in
            let node = add_leaf node k (BoundMap.upperbound bound_map k) in
            let k_constraints = StructuralConstraints.get_constraints_of_node constraints k in
            let worklist' =
              List.fold k_constraints.single ~init:rest ~f:(fun acc ub_id ->
                  if Node.IdSet.mem ub_id visited_acc' then acc else ub_id :: acc )
            in
            let branch =
              List.fold_left
                ~f:(fun branch set_addend ->
                  if Node.IdSet.is_empty (Node.IdSet.inter set_addend visited_acc') then
                    SetOfSetsOfNodes.add set_addend branch
                  else branch )
                ~init:branch k_constraints.sum
            in
            build_min node branch visited_acc' worklist'
    in
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
              let child = self (n, visited_res) in
              add_child acc child )
            addend (Plus [])
        in
        (* without this check it would add plus node with just one child, and give wrong results *)
        if is_well_formed_plus_node plus_node then add_child i_node plus_node else i_node )
      branch node


  let with_cache f =
    (* a map used for bookkeeping of the min trees that we have already built *)
    let global_built_tree_map : mt_node BuiltTreeMap.t ref = ref BuiltTreeMap.empty in
    let rec f_with_cache x =
      match BuiltTreeMap.find_opt x !global_built_tree_map with
      | Some v ->
          v
      | None ->
          let v = f f_with_cache x in
          global_built_tree_map := BuiltTreeMap.add x v !global_built_tree_map ;
          v
    in
    Staged.stage f_with_cache


  let compute_trees_from_contraints bound_map node_cfg constraints =
    let minimum_propagation =
      with_cache (minimum_propagation bound_map constraints) |> Staged.unstage
    in
    let min_trees =
      NodeCFG.fold_nodes node_cfg
        ~f:(fun acc node ->
          let nid = Node.id node in
          let tree = minimum_propagation (nid, Node.IdSet.empty) in
          (nid, tree) :: acc )
        ~init:[]
    in
    List.iter
      ~f:(fun (nid, t) -> L.(debug Analysis Medium) "@\n node %a = %a @\n" Node.pp_id nid pp t)
      min_trees ;
    min_trees
end

module ReportedOnNodes = AbstractDomain.FiniteSetOfPPSet (Node.IdSet)

type extras_TransferFunctionsWCET =
  { basic_cost_map: AnalyzerNodesBasicCost.invariant_map
  ; min_trees_map: BasicCost.astate Node.IdMap.t
  ; summary: Summary.t }

(* Calculate the final Worst Case Execution Time predicted for each node.
   It uses the basic cost of the nodes (computed previously by AnalyzerNodesBasicCost)
   and MinTrees which give an upperbound on the number of times a node can be executed
*)
module TransferFunctionsWCET = struct
  module CFG = InstrCFG
  module Domain = AbstractDomain.Pair (BasicCost) (ReportedOnNodes)

  type extras = extras_TransferFunctionsWCET

  let should_report_on_instr = function
    | Sil.Call _ | Sil.Load _ | Sil.Prune _ | Sil.Store _ ->
        true
    | Sil.Abstract _ | Sil.Declare_locals _ | Sil.Nullify _ | Sil.Remove_temps _ ->
        false


  (* We don't report when the cost is Top as it corresponds to subsequent 'don't know's.
   Instead, we report Top cost only at the top level per function when `report_infinity` is set to true *)
  let should_report_cost cost =
    not (BasicCost.is_top cost) && not (BasicCost.( <= ) ~lhs:cost ~rhs:expensive_threshold)


  let do_report summary loc cost =
    let ltr =
      let cost_desc = F.asprintf "with estimated cost %a" BasicCost.pp cost in
      [Errlog.make_trace_element 0 loc cost_desc []]
    in
    let exn =
      let message =
        F.asprintf
          "The execution time from the beginning of the function up to this program point is \
           likely above the acceptable threshold of %a (estimated cost %a)"
          BasicCost.pp expensive_threshold BasicCost.pp cost
      in
      Exceptions.Checkers (IssueType.expensive_execution_time_call, Localise.verbatim_desc message)
    in
    Reporting.log_error summary ~loc ~ltr exn


  (* get a list of nodes and check if we have already reported for at
     least one of them. In that case no need to report again. *)
  let should_report_on_node preds reported_so_far =
    List.for_all
      ~f:(fun node ->
        let nid = Procdesc.Node.get_id node in
        not (ReportedOnNodes.mem nid reported_so_far) )
      preds


  let map_cost trees m : BasicCost.astate =
    CostDomain.NodeInstructionToCostMap.fold
      (fun ((node_id, _) as instr_node_id) c acc ->
        let t = Node.IdMap.find node_id trees in
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


  let exec_instr ((_, reported_so_far): Domain.astate) {ProcData.extras} (node: CFG.Node.t) instr
      : Domain.astate =
    let {basic_cost_map= invariant_map_cost; min_trees_map= trees; summary} = extras in
    let cost_node =
      let instr_node_id = CFG.Node.id node in
      match AnalyzerNodesBasicCost.extract_post instr_node_id invariant_map_cost with
      | Some node_map ->
          L.(debug Analysis Medium)
            "@\n [AnalyzerWCET] Final map for node: %a @\n" CFG.Node.pp_id instr_node_id ;
          map_cost trees node_map
      | _ ->
          assert false
    in
    L.(debug Analysis Medium)
      "@\n[>>>AnalyzerWCET] Instr: %a   Cost: %a@\n" (Sil.pp_instr Pp.text) instr BasicCost.pp
      cost_node ;
    let astate' =
      let und_node = CFG.Node.underlying_node node in
      let preds = Procdesc.Node.get_preds und_node in
      let reported_so_far =
        if
          should_report_on_instr instr && should_report_on_node (und_node :: preds) reported_so_far
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

module AnalyzerWCET = AbstractInterpreter.MakeNoCFG (InstrCFGScheduler) (TransferFunctionsWCET)

let check_and_report_infinity cost proc_desc summary =
  if BasicCost.is_top cost then
    let loc = Procdesc.get_start_node proc_desc |> Procdesc.Node.get_loc in
    let message =
      F.asprintf "The execution time of the function %a cannot be computed" Typ.Procname.pp
        (Procdesc.get_proc_name proc_desc)
    in
    let exn =
      Exceptions.Checkers (IssueType.infinite_execution_time_call, Localise.verbatim_desc message)
    in
    Reporting.log_error ~loc summary exn


let checker ({Callbacks.tenv; proc_desc} as callback_args) : Summary.t =
  let inferbo_invariant_map, summary =
    BufferOverrunChecker.compute_invariant_map_and_check callback_args
  in
  let proc_data = ProcData.make_default proc_desc tenv in
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  (* collect all prune nodes that occur in loop guards, needed for ControlDepAnalyzer *)
  let control_maps = Loop_control.get_control_maps node_cfg in
  (* computes the data dependencies: node -> (var -> var set) *)
  let data_dep_invariant_map =
    Control.DataDepAnalyzer.exec_cfg node_cfg proc_data ~initial:Control.DataDepMap.empty
      ~debug:true
  in
  (* computes the control dependencies: node -> var set *)
  let control_dep_invariant_map =
    let proc_data = ProcData.make proc_desc tenv control_maps in
    Control.ControlDepAnalyzer.exec_cfg node_cfg proc_data ~initial:Control.ControlDepSet.empty
      ~debug:true
  in
  let instr_cfg = InstrCFG.from_pdesc proc_desc in
  let invariant_map_NodesBasicCost =
    let proc_data = ProcData.make proc_desc tenv inferbo_invariant_map in
    (*compute_WCET cfg invariant_map min_trees in *)
    AnalyzerNodesBasicCost.exec_cfg instr_cfg proc_data ~initial:NodesBasicCostDomain.empty
      ~debug:true
  in
  (* given the semantics computes the upper bound on the number of times a node could be executed *)
  let bound_map =
    BoundMap.compute_upperbound_map node_cfg inferbo_invariant_map data_dep_invariant_map
      control_dep_invariant_map
  in
  let _ = ConstraintSolver.collect_constraints node_cfg in
  let constraints = StructuralConstraints.compute_structural_constraints node_cfg in
  let min_trees = MinTree.compute_trees_from_contraints bound_map node_cfg constraints in
  let trees_valuation =
    List.fold
      ~f:(fun acc (nid, t) ->
        let res = MinTree.evaluate_tree t in
        L.(debug Analysis Medium) "@\n   Tree %a eval to %a @\n" Node.pp_id nid BasicCost.pp res ;
        Node.IdMap.add nid res acc )
      ~init:Node.IdMap.empty min_trees
  in
  let initWCET = (BasicCost.zero, ReportedOnNodes.empty) in
  match
    AnalyzerWCET.compute_post
      (ProcData.make proc_desc tenv
         {basic_cost_map= invariant_map_NodesBasicCost; min_trees_map= trees_valuation; summary})
      ~debug:true ~initial:initWCET
  with
  | Some (exit_cost, _) ->
      L.internal_error
        "@\n[COST ANALYSIS] PROCESSING MIN_TREE for PROCEDURE '%a' |CFG| = %i FINAL COST = %a @\n"
        Typ.Procname.pp
        (Procdesc.get_proc_name proc_desc)
        (Container.length ~fold:NodeCFG.fold_nodes node_cfg)
        BasicCost.pp exit_cost ;
      check_and_report_infinity exit_cost proc_desc summary ;
      Payload.update_summary {post= exit_cost} summary
  | None ->
      if Procdesc.Node.get_succs (Procdesc.get_start_node proc_desc) <> [] then (
        L.internal_error "Failed to compute final cost for function %a" Typ.Procname.pp
          (Procdesc.get_proc_name proc_desc) ;
        summary )
      else summary
