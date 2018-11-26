(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module VisitCount : sig
  type t
end

module State : sig
  type 'a t = {pre: 'a; post: 'a; visit_count: VisitCount.t}
end

(** type of an intraprocedural abstract interpreter *)
module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap = TransferFunctions.CFG.Node.IdMap

  (** invariant map from node id -> state representing postcondition for node id *)
  type invariant_map = TransferFunctions.Domain.t State.t InvariantMap.t

  val compute_post :
       ?do_narrowing:bool
    -> ?pp_instr:(TransferFunctions.Domain.t -> Sil.instr -> (Format.formatter -> unit) option)
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
    -> TransferFunctions.Domain.t option
  (** compute and return the postcondition for the given procedure starting from [initial].
      [pp_instr] is used for the debug HTML and passed as a hook to handle both SIL and HIL CFGs. *)

  val exec_cfg :
       ?do_narrowing:bool
    -> TransferFunctions.CFG.t
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
    -> invariant_map
  (** compute and return invariant map for the given CFG/procedure starting from [initial]. *)

  val exec_pdesc :
       ?do_narrowing:bool
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
    -> invariant_map
  (** compute and return invariant map for the given procedure starting from [initial] *)

  val extract_post : InvariantMap.key -> 'a State.t InvariantMap.t -> 'a option
  (** extract the postcondition for a node id from the given invariant map *)

  val extract_pre : InvariantMap.key -> 'a State.t InvariantMap.t -> 'a option
  (** extract the precondition for a node id from the given invariant map *)

  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
  (** extract the state for a node id from the given invariant map *)
end

module type Make = functor (TransferFunctions : TransferFunctions.SIL) -> S
                                                                          with module TransferFunctions = TransferFunctions

(** create an intraprocedural abstract interpreter from transfer functions using the reverse post-order scheduler *)
module MakeRPO : Make

(** create an intraprocedural abstract interpreter from transfer functions using Bourdoncle's strongly connected component weak topological order *)
module MakeWTO : Make
