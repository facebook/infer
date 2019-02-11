(*
 * Copyright (c) 2017-present, Facebook, Inc.
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

(* Module to simulate a record 
    {OperationCost:BasicCost.t; AllocationCost: BasicCost.t; IOCost:BasicCost.t} with a map
    {OperationCost, AllocationCost, IOCost} -> BasicCost.t
*)

module VariantCostMap = struct
  include AbstractDomain.Map (CostKind) (BasicCost)

  let replace k v map = add k v map

  let[@warning "-32"] add _ = Logging.die InternalError "Don't call me"

  let increase_by kind basic_cost record =
    let existing = match find_opt kind record with Some c -> c | None -> BasicCost.zero in
    replace kind (BasicCost.plus basic_cost existing) record


  let[@warning "-32"] increment kind record = increase_by kind BasicCost.one record
end

type summary = {post: VariantCostMap.t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" VariantCostMap.pp post

(** Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap =
  AbstractDomain.MapOfPPMap (ProcCfg.InstrNode.IdMap) (VariantCostMap)

let get_cost_kind kind cost_record =
  try VariantCostMap.find kind cost_record with _ ->
    Logging.(die InternalError)
      "Can't find %a for Cost Record %a" CostKind.pp kind VariantCostMap.pp cost_record


let get_operation_cost cost_record = get_cost_kind OperationCost cost_record

let get_allocation_cost cost_record = get_cost_kind AllocationCost cost_record

let get_IO_cost cost_record = get_cost_kind IOCost cost_record

let mk_cost_record ~operation_cost:oc ~allocation_cost:ac ~io_cost:ioc =
  let r1 = VariantCostMap.replace OperationCost oc VariantCostMap.empty in
  let r2 = VariantCostMap.replace AllocationCost ac r1 in
  VariantCostMap.replace IOCost ioc r2


(* Map representing cost record {OperationCost:0; AllocationCost:0; IOCost:0} *)
let zero_record =
  mk_cost_record ~operation_cost:BasicCost.zero ~allocation_cost:BasicCost.zero
    ~io_cost:BasicCost.zero


(* Map representing cost record {OperationCost:1; AllocationCost:0; IOCost:0} *)
let unit_cost_atomic_operation = VariantCostMap.replace OperationCost BasicCost.one zero_record

(* Map representing cost record {OperationCost:operation_cost; AllocationCost:0; IOCost:0} *)
let set_operation_cost operation_cost map = VariantCostMap.replace OperationCost operation_cost map
