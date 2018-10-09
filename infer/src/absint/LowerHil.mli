(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type HilConfig = sig
  val include_array_indexes : bool
  (** if true, array index expressions will appear in access paths *)
end

module DefaultConfig : HilConfig

(** Functor for turning HIL transfer functions into SIL transfer functions *)
module Make
    (MakeTransferFunctions : TransferFunctions.MakeHIL)
    (HilConfig : HilConfig)
    (CFG : ProcCfg.S) : sig
  module TransferFunctions : module type of MakeTransferFunctions (CFG)

  module CFG : module type of TransferFunctions.CFG

  module Domain :
      module type of AbstractDomain.Pair (TransferFunctions.Domain) (IdAccessPathMapDomain)

  type extras = TransferFunctions.extras

  val exec_instr : Domain.astate -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.astate

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

(** Wrapper around Interpreter to prevent clients from having to deal with IdAccessPathMapDomain *)
module MakeAbstractInterpreterWithConfig
    (HilConfig : HilConfig)
    (CFG : ProcCfg.S)
    (MakeTransferFunctions : TransferFunctions.MakeHIL) : sig
  module Interpreter :
      module type of AbstractInterpreter.MakeRPO (Make (MakeTransferFunctions) (HilConfig) (CFG))

  val compute_post :
       Interpreter.TransferFunctions.extras ProcData.t
    -> initial:MakeTransferFunctions(CFG).Domain.astate
    -> MakeTransferFunctions(CFG).Domain.astate option
  (** compute and return the postcondition for the given procedure starting from [initial]. If
      [debug] is true, print html debugging output. *)
end

(** Simpler version of the above wrapper that uses the default HIL config *)
module MakeAbstractInterpreter
    (CFG : ProcCfg.S)
    (MakeTransferFunctions : TransferFunctions.MakeHIL) : sig
  include module type of
    MakeAbstractInterpreterWithConfig (DefaultConfig) (CFG) (MakeTransferFunctions)
end
