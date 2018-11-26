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

type summary = {post: BasicCost.t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" BasicCost.pp post
