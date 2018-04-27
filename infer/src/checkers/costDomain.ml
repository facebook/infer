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
(* Map (node,instr) -> basic cost  *)
module NodeInstructionToCostMap = AbstractDomain.MapOfPPMap (ProcCfg.InstrNode.IdMap) (Itv.Bound)
module EnvMap = AbstractDomain.Map (Exp) (Itv)
module EnvDomainBO = AbstractDomain.BottomLifted (EnvMap)

type summary = {post: Itv.Bound.t}

let pp_summary fmt {post} = F.fprintf fmt "@\n Post: %a @\n" Itv.Bound.pp post
