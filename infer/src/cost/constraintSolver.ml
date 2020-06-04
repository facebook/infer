(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BasicCost = CostDomain.BasicCost
module Node = ProcCfg.DefaultNode
module NodeCFG = ProcCfg.Normal

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


  (** Infer equalities from sums, like this:

      (1) A + sum1 = A + sum2 => sum1 = sum2

      It does not try to saturate

      (2) A = B + C /\ B = D + E => A = C + D + E

      Nor combine more than 2 equations

      (3) A = B + C /\ B = D + E /\ F = C + D + E => A = F

      ((3) is implied by (1) /\ (2))

      Its complexity is unknown but I think it is bounded by nbNodes x nbEdges x max. *)
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
    let (_ : bool) = log_union ~debug equalities e1 e2 in
    ()


  let init_costs bound_map equalities =
    let of_node node_id = BoundMap.lookup_upperbound bound_map node_id in
    Container.iter equalities ~fold:fold_sets ~f:(fun (_repr, set) ->
        ControlFlowCost.Set.init_cost ~of_node set )


  (** From sums: if A = B + C, do cost(A) = min(cost(A), cost(B) + cost(C))

      From inequalities: if A = B + C, then B <= A, do cost(B) = min(cost(B), cost(A)) *)
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
        match ControlFlowCost.Set.improve_cost_with sum_item_set (ControlFlowCost.Set.cost set) with
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
  debug.f "[ConstraintSolver] Procedure %a @@ %a@\n" Procname.pp (Procdesc.get_proc_name node_cfg)
    Location.pp_file_pos (Procdesc.get_loc node_cfg) ;
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


let get_node_nb_exec equalities node =
  let nb_exec_opt =
    Node.id node |> ControlFlowCost.make_node |> Equalities.find equalities
    |> Equalities.find_set equalities
  in
  match nb_exec_opt with
  | Some nb_exec ->
      ControlFlowCost.Set.cost nb_exec
  | None ->
      (* a dangling node with no incoming or outgoing edges is unreachable *)
      BasicCost.of_unreachable (Node.loc node)
