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
  let version = 1
end

(**
  Module to simulate a record 
    {OperationCost:BasicCost.t; AllocationCost: BasicCost.t; IOCost:BasicCost.t} with a map
    {OperationCost, AllocationCost, IOCost} -> BasicCost.t
*)
module VariantCostMap = struct
  include PrettyPrintable.PPMonoMapOfPPMap (CostIssues.CostKindMap) (BasicCost)

  let[@warning "-32"] add _ = Logging.die InternalError "Don't call me"

  let get kind record = find_opt kind record |> Option.value ~default:BasicCost.zero

  let increase_by kind cost_to_add record =
    update kind
      (function
        | None -> Some cost_to_add | Some existing -> Some (BasicCost.plus cost_to_add existing) )
      record


  let increment kind record = increase_by kind BasicCost.one record
end

type t = VariantCostMap.t

type summary = {post: t; is_on_ui_thread: bool}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" VariantCostMap.pp post

let get_cost_kind kind cost_record = VariantCostMap.get kind cost_record

let get_operation_cost cost_record = get_cost_kind CostKind.OperationCost cost_record

let map ~f cost_record = VariantCostMap.map f cost_record

(* Map representing cost record {OperationCost:0; AllocationCost:0; IOCost:0} *)
let zero_record = VariantCostMap.empty

let mult_by_scalar cost_record scalar = map cost_record ~f:(BasicCost.mult scalar)

let plus cost_record1 cost_record2 =
  VariantCostMap.union
    (fun _kind cost1 cost2 -> Some (BasicCost.plus cost1 cost2))
    cost_record1 cost_record2


(* Map representing cost record {OperationCost:1; AllocationCost:0; IOCost:0} *)
let unit_cost_atomic_operation = VariantCostMap.increment CostKind.OperationCost zero_record

(* Map representing cost record {OperationCost:0; AllocationCost:1; IOCost:0} *)
let unit_cost_allocation = VariantCostMap.increment CostKind.AllocationCost zero_record

(* Map representing cost record {OperationCost:operation_cost; AllocationCost:0; IOCost:0} *)
let of_operation_cost operation_cost =
  VariantCostMap.increase_by CostKind.OperationCost operation_cost zero_record
