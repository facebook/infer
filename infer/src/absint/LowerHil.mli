(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Functor for turning HIL transfer functions into SIL transfer functions *)
module Make (MakeTransferFunctions : TransferFunctions.MakeHIL) (CFG : ProcCfg.S) : sig
  module TransferFunctions : module type of MakeTransferFunctions (CFG)

  module CFG : module type of TransferFunctions.CFG

  module Domain :
      module type of AbstractDomain.Pair (TransferFunctions.Domain) (IdAccessPathMapDomain)

  type extras = TransferFunctions.extras

  val exec_instr : Domain.astate -> extras ProcData.t -> CFG.node -> Sil.instr -> Domain.astate
end
