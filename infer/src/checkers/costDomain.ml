(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BasicCost = Polynomials.NonNegativePolynomial

type cost_kind = OperationCost | AllocationCost | IOCost [@@deriving compare]

module CostKind : PrettyPrintable.PrintableOrderedType with type t = cost_kind = struct
  type t = cost_kind [@@deriving compare]

  let pp f k =
    let k_str =
      match k with
      | OperationCost ->
          "OperationCost"
      | AllocationCost ->
          "AllocationCost"
      | IOCost ->
          "IOCost"
    in
    F.pp_print_string f k_str
end

module CostKindMap = struct
  include PrettyPrintable.MakePPMap (CostKind)

  type no_value = |

  let iter2 map1 map2 ~f =
    let (_ : no_value t) =
      merge
        (fun k v1_opt v2_opt ->
          (match (v1_opt, v2_opt) with Some v1, Some v2 -> f k v1 v2 | _ -> ()) ;
          None )
        map1 map2
    in
    ()
end

(**
  Module to simulate a record 
    {OperationCost:BasicCost.t; AllocationCost: BasicCost.t; IOCost:BasicCost.t} with a map
    {OperationCost, AllocationCost, IOCost} -> BasicCost.t
*)
module VariantCostMap = struct
  include PrettyPrintable.PPMonoMapOfPPMap (CostKindMap) (BasicCost)

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

type summary = {post: t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" VariantCostMap.pp post

let get_cost_kind kind cost_record = VariantCostMap.get kind cost_record

let get_operation_cost cost_record = get_cost_kind OperationCost cost_record

let map ~f cost_record = VariantCostMap.map f cost_record

(* Map representing cost record {OperationCost:0; AllocationCost:0; IOCost:0} *)
let zero_record = VariantCostMap.empty

let mult_by_scalar cost_record scalar = map cost_record ~f:(BasicCost.mult scalar)

let plus cost_record1 cost_record2 =
  VariantCostMap.union
    (fun _kind cost1 cost2 -> Some (BasicCost.plus cost1 cost2))
    cost_record1 cost_record2


(* Map representing cost record {OperationCost:1; AllocationCost:0; IOCost:0} *)
let unit_cost_atomic_operation = VariantCostMap.increment OperationCost zero_record

(* Map representing cost record {OperationCost:0; AllocationCost:1; IOCost:0} *)
let unit_cost_allocation = VariantCostMap.increment AllocationCost zero_record

(* Map representing cost record {OperationCost:operation_cost; AllocationCost:0; IOCost:0} *)
let of_operation_cost operation_cost =
  VariantCostMap.increase_by OperationCost operation_cost zero_record
