(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module BasicCost = Itv.NonNegativeBound

(** Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap = AbstractDomain.MapOfPPMap (ProcCfg.InstrNode.IdMap) (BasicCost)

type summary = {post: BasicCost.astate}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" BasicCost.pp post
