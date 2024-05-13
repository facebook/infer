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

type join_policy_t = UnderApproximateAfter of int

type widen_policy_t = UnderApproximateAfterNumIterations of int

module type DisjunctiveConfig = sig
  val join_policy : join_policy_t

  val widen_policy : widen_policy_t
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module DisjDomain : AbstractDomain.Disjunct

  module NonDisjDomain : AbstractDomain.WithBottomTop

  type analysis_data

  val exec_instr :
       DisjDomain.t * NonDisjDomain.t
    -> analysis_data
    -> CFG.Node.t
    -> Sil.instr
    -> DisjDomain.t list * NonDisjDomain.t

  val remember_dropped_disjuncts : DisjDomain.t list -> NonDisjDomain.t -> NonDisjDomain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit

  val pp_disjunct : Pp.print_kind -> Format.formatter -> DisjDomain.t -> unit
end
