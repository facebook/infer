(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Transfer functions that push abstract states across instructions. A typical client should
    implement the Make signature to allow the transfer functions to be used with any kind of CFG. *)

module type S = sig
  module CFG : ProcCfg.S

  (** abstract domain whose state we propagate *)
  module Domain : AbstractDomain.S

  (** read-only extra state (results of previous analyses, globals, etc.) *)
  type extras

  (** type of the instructions the transfer functions operate on *)
  type instr

  val exec_instr : Domain.astate -> extras ProcData.t -> CFG.Node.t -> instr -> Domain.astate
  (** [A] instr [A']. [node] is the node of the current instruction *)

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
  (** print session name for HTML debug *)
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
