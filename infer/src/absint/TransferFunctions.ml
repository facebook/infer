(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type S = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S

  type extras

  type instr

  val exec_instr : Domain.astate -> extras ProcData.t -> CFG.node -> instr -> Domain.astate
end

module type SIL = sig
  include S with type instr := Sil.instr
end

module type HIL = sig
  include S with type instr := HilInstr.t
end

module type MakeSIL = functor (C : ProcCfg.S) -> sig
  include SIL with module CFG = C
end

module type MakeHIL = functor (C : ProcCfg.S) -> sig
  include HIL with module CFG = C
end
