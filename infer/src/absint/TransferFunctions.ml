(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type S = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S

  type analysis_data

  type instr

  val exec_instr :
    Domain.t -> analysis_data -> CFG.Node.t -> ProcCfg.InstrNode.instr_index -> instr -> Domain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type SIL = sig
  include S with type instr := Sil.instr
end

module type HIL = sig
  include S with type instr := HilInstr.t
end

module type MakeHIL = functor (C : ProcCfg.S) -> sig
  include HIL with module CFG = C
end

module type DisjunctiveConfig = sig
  val join_policy : [`UnderApproximateAfter of int]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module DisjDomain : AbstractDomain.Disjunct

  module NonDisjDomain : AbstractDomain.WithBottom

  type analysis_data

  val exec_instr :
       DisjDomain.t * NonDisjDomain.t
    -> analysis_data
    -> CFG.Node.t
    -> Sil.instr
    -> DisjDomain.t list * NonDisjDomain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end
