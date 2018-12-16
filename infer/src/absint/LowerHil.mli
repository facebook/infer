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
module Make (TransferFunctions : TransferFunctions.HIL) (HilConfig : HilConfig) : sig
  module CFG :
    ProcCfg.S
    with type t = TransferFunctions.CFG.t
     and type instrs_dir = TransferFunctions.CFG.instrs_dir
     and type Node.t = TransferFunctions.CFG.Node.t
     and type Node.id = TransferFunctions.CFG.Node.id
     and module Node.IdMap = TransferFunctions.CFG.Node.IdMap
     and module Node.IdSet = TransferFunctions.CFG.Node.IdSet

  module Domain : module type of AbstractDomain.Pair (TransferFunctions.Domain) (Bindings)

  type extras = TransferFunctions.extras

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

module type S = sig
  module Interpreter : AbstractInterpreter.S

  type domain

  val compute_post :
    Interpreter.TransferFunctions.extras ProcData.t -> initial:domain -> domain option
  (** compute and return the postcondition for the given procedure starting from [initial]. *)
end

(** Wrapper around Interpreter to prevent clients from having to deal with IdAccessPathMapDomain *)
module MakeAbstractInterpreterWithConfig
    (MakeAbstractInterpreter : AbstractInterpreter.Make)
    (HilConfig : HilConfig)
    (TransferFunctions : TransferFunctions.HIL) :
  S
  with type domain = TransferFunctions.Domain.t
   and module Interpreter = MakeAbstractInterpreter(Make(TransferFunctions)(HilConfig))

(** Simpler version of the above wrapper that uses the default HIL config *)
module MakeAbstractInterpreter (TransferFunctions : TransferFunctions.HIL) : sig
  include module type of
    MakeAbstractInterpreterWithConfig (AbstractInterpreter.MakeRPO) (DefaultConfig)
      (TransferFunctions)
end
