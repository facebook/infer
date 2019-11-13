(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Transfer functions that push abstract states across instructions. A typical client should
    implement the Make signature to allow the transfer functions to be used with any kind of CFG. *)

module type S = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.S
  (** abstract domain whose state we propagate *)

  (** read-only extra state (results of previous analyses, globals, etc.) *)
  type extras

  (** type of the instructions the transfer functions operate on *)
  type instr

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> instr -> Domain.t
  (** {A} instr {A'}. [node] is the node of the current instruction *)

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
  (** print session name for HTML debug *)
end

module type SIL = sig
  include S with type instr := Sil.instr
end

module type HIL = sig
  include S with type instr := HilInstr.t
end

module type DisjunctiveConfig = sig
  val join_policy :
    [ `UnderApproximateAfter of int
      (** When the set of disjuncts gets bigger than [n] then just stop adding new states to it,
         drop any further states on the floor. This corresponds to an under-approximation/bounded
         approach. *)
    | `NeverJoin  (** keep accumaluting states *) ]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

module type DisjReady = sig
  module CFG : ProcCfg.S

  module Domain : AbstractDomain.NoJoin

  type extras

  val exec_instr : Domain.t -> extras ProcData.t -> CFG.Node.t -> Sil.instr -> Domain.t list

  val pp_session_name : CFG.Node.t -> Format.formatter -> unit
end

(** In the disjunctive interpreter, the domain is a set of abstract states representing a
   disjunction between these states. The transfer functions are executed on each state in the
   disjunct independently. The join on the disjunctive state is governed by the policy described in
   [DConfig]. *)
module MakeDisjunctive (TransferFunctions : DisjReady) (DConfig : DisjunctiveConfig) : sig
  module Disjuncts : sig
    type t

    val singleton : TransferFunctions.Domain.t -> t

    val elements : t -> TransferFunctions.Domain.t list [@@warning "-32"]
  end

  include
    SIL
      with type extras = TransferFunctions.extras
       and module CFG = TransferFunctions.CFG
       and type Domain.t = Disjuncts.t
end
