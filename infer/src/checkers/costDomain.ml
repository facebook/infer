(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BasicCost = Polynomials.NonNegativePolynomial

(** Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap = AbstractDomain.MapOfPPMap (ProcCfg.InstrNode.IdMap) (BasicCost)

type cost_record = {basic_operation_cost: BasicCost.t}

type summary = {post: cost_record}

let pp_summary fmt {post} =
  F.fprintf fmt "@\n Post: {basic_operatio_cost: %a} @\n" BasicCost.pp post.basic_operation_cost
