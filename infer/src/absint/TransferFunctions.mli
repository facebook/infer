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
  (** the underlying domain *)
  type domain_t [@@deriving compare]

  val join_policy :
    [ `JoinAfter of int
      (** when the set of disjuncts gets bigger than [n] the underlying domain's join is called to
       collapse them into one state *)
    | `UnderApproximateAfter of int
      (** When the set of disjuncts gets bigger than [n] then just stop adding new states to it,
         drop any further states on the floor. This corresponds to an under-approximation/bounded
         approach. *)
    | `NeverJoin  (** keep accumaluting states *) ]

  val widen_policy : [`UnderApproximateAfterNumIterations of int]
end

(** In the disjunctive interpreter, the domain is a set of abstract states representing a
   disjunction between these states. The transfer functions are executed on each state in the
   disjunct independently. The join on the disjunctive state is governed by the policy described in
   [DConfig]. *)
module MakeHILDisjunctive
    (TransferFunctions : HIL)
    (DConfig : DisjunctiveConfig with type domain_t = TransferFunctions.Domain.t) : sig
  include HIL with type extras = TransferFunctions.extras and module CFG = TransferFunctions.CFG

  val of_domain : DConfig.domain_t -> Domain.t
end
