(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = OperationCost | AllocationCost | IOCost [@@deriving compare]

let to_issue_string = function
  | OperationCost ->
      "EXECUTION_TIME"
  | AllocationCost ->
      "ALLOCATION"
  | IOCost ->
      "IO"


let to_complexity_string = function
  | AllocationCost ->
      "Allocation complexity"
  | OperationCost ->
      "Time complexity"
  | IOCost ->
      "IO complexity"


let pp f k =
  let k_str =
    match k with
    | OperationCost ->
        "Execution Cost"
    | AllocationCost ->
        "Allocation Cost"
    | IOCost ->
        "IO Cost"
  in
  F.pp_print_string f k_str


let to_json_cost_info c = function
  | OperationCost ->
      c.Jsonbug_t.exec_cost
  | AllocationCost ->
      c.Jsonbug_t.alloc_cost
  | IOCost ->
      assert false


(* We use this threshold to give error if the cost is above it.
   Currently it's set randomly to 200 for OperationCost and 3 for AllocationCost. *)
let to_threshold = function OperationCost -> Some 200 | AllocationCost -> Some 3 | IOCost -> None

type kind_spec = {kind: t; (* for non-diff analysis *) top_and_bottom: bool}

let enabled_cost_kinds =
  [{kind= OperationCost; top_and_bottom= true}; {kind= AllocationCost; top_and_bottom= false}]
