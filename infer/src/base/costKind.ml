(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = OperationCost | AllocationCost [@@deriving compare]

let to_issue_string = function OperationCost -> "EXECUTION_TIME" | AllocationCost -> "ALLOCATION"

let to_complexity_string = function
  | AllocationCost ->
      "Allocation complexity"
  | OperationCost ->
      "Time complexity"


let pp f k =
  let k_str =
    match k with OperationCost -> "Execution Cost" | AllocationCost -> "Allocation Cost"
  in
  F.pp_print_string f k_str


let to_json_cost_info c = function
  | OperationCost ->
      c.Jsonbug_t.exec_cost
  | AllocationCost ->
      assert false


type kind_spec = {kind: t; (* for non-diff analysis *) top_and_unreachable: bool}

let enabled_cost_kinds = [{kind= OperationCost; top_and_unreachable= true}]
