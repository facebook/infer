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


let enabled_cost_kinds =
  [ ((fun c -> c.Jsonbug_t.exec_cost), OperationCost)
  ; ((fun c -> c.Jsonbug_t.alloc_cost), AllocationCost) ]
