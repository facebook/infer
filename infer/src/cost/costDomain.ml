(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module BasicCost = struct
  include Polynomials.NonNegativePolynomial

  (* NOTE: Increment the version number if you changed the [t] type.  This is for avoiding
     demarshalling failure of cost analysis results in running infer-reportdiff. *)
  let version = 8
end

module BasicCostWithReason = struct
  type t = {cost: BasicCost.t; top_pname_opt: Procname.t option}

  let is_top {cost} = BasicCost.is_top cost

  let is_unreachable {cost} = BasicCost.is_unreachable cost

  let zero = {cost= BasicCost.zero; top_pname_opt= None}

  let one ?autoreleasepool_trace () =
    {cost= BasicCost.one ?autoreleasepool_trace (); top_pname_opt= None}


  let subst callee_pname location record eval_sym =
    {record with cost= BasicCost.subst callee_pname location record.cost eval_sym}


  (* When we fold the nodes while traversing the cfg,
     make sure we only keep the first top cost callee we see *)
  let plus record1 record2 =
    { cost= BasicCost.plus record1.cost record2.cost
    ; top_pname_opt= Option.first_some record1.top_pname_opt record2.top_pname_opt }


  let degree {cost} = BasicCost.degree cost

  let mult_unreachable cost record = {record with cost= BasicCost.mult_unreachable cost record.cost}

  let polynomial_traces ~is_autoreleasepool_trace {cost} =
    BasicCost.polynomial_traces ~is_autoreleasepool_trace cost


  let pp format {cost} = BasicCost.pp format cost

  let pp_hum = pp
end

(** Module to simulate a record [{OperationCost:BasicCost.t; AllocationCost: BasicCost.t}] with a
    map [{OperationCost, AllocationCost} -> BasicCost.t] *)
module VariantCostMap = struct
  include PrettyPrintable.PPMonoMapOfPPMap (CostIssues.CostKindMap) (BasicCostWithReason)

  let[@warning "-32"] add _ = Logging.die InternalError "Don't call me"

  let get kind record = find_opt kind record |> Option.value ~default:BasicCostWithReason.zero

  let increase_by kind cost_to_add record =
    update kind
      (function
        | None ->
            Some cost_to_add
        | Some existing ->
            Some (BasicCostWithReason.plus cost_to_add existing) )
      record


  let increment ?autoreleasepool_trace kind record =
    increase_by kind (BasicCostWithReason.one ?autoreleasepool_trace ()) record
end

type t = VariantCostMap.t

type summary = {post: t; is_on_ui_thread: bool}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" VariantCostMap.pp post

let get_cost_kind kind cost_record = VariantCostMap.get kind cost_record

let add_top_pname_opt kind cost_record top_pname_opt =
  VariantCostMap.update kind
    (function Some cost_with_reason -> Some {cost_with_reason with top_pname_opt} | _ -> None)
    cost_record


let get_operation_cost cost_record = get_cost_kind CostKind.OperationCost cost_record

let set_autoreleasepool_size_zero cost_record =
  VariantCostMap.remove CostKind.AutoreleasepoolSize cost_record


let map ~f cost_record = VariantCostMap.map f cost_record

let zero_record = VariantCostMap.empty

(** If nb_exec is unreachable, we map to unreachable, not 0 *)
let mult_by cost_record ~nb_exec = map cost_record ~f:(BasicCostWithReason.mult_unreachable nb_exec)

(** "zero+unreachable" is defined as unreachable in the operation cost, but as zero in the other
    costs. This is because "zero+unreachable" is a bit weird in the operation cost: it means that no
    statment is analyzed yet, but at the same time the a program point is analyzed as unreachable.
    For debugging purpose, we define it to return the more specific unreachable cost. *)
let plus cost_record1 cost_record2 =
  VariantCostMap.merge
    (fun kind cost1 cost2 ->
      match (kind, cost1, cost2) with
      | OperationCost, Some cost, None | OperationCost, None, Some cost ->
          Some cost
      | (OperationCost | AllocationCost | AutoreleasepoolSize), _, _ ->
          let cost1 = Option.value cost1 ~default:BasicCostWithReason.zero in
          let cost2 = Option.value cost2 ~default:BasicCostWithReason.zero in
          Some (BasicCostWithReason.plus cost1 cost2) )
    cost_record1 cost_record2


let plus_autoreleasepool_size autoreleasepool_size cost =
  VariantCostMap.update AutoreleasepoolSize
    (function
      | None ->
          Some {BasicCostWithReason.cost= autoreleasepool_size; top_pname_opt= None}
      | Some prev ->
          Some {prev with cost= BasicCost.plus prev.cost autoreleasepool_size} )
    cost


let unit_cost_atomic_operation = VariantCostMap.increment CostKind.OperationCost zero_record

let unit_cost_allocation = VariantCostMap.increment CostKind.AllocationCost zero_record

let unit_cost_autoreleasepool_size ~autoreleasepool_trace =
  VariantCostMap.increment ~autoreleasepool_trace CostKind.AutoreleasepoolSize zero_record


let of_operation_cost operation_cost =
  VariantCostMap.increase_by CostKind.OperationCost
    {cost= operation_cost; top_pname_opt= None}
    zero_record
