(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type 'a state = {pre: 'a; post: 'a; visit_count: int}

(** type of an intraprocedural abstract interpreter *)
module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap : Caml.Map.S with type key = TransferFunctions.CFG.id

  (** invariant map from node id -> state representing postcondition for node id *)
  type invariant_map = TransferFunctions.Domain.astate state InvariantMap.t

  val compute_post :
    ?debug:bool -> TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate
    -> TransferFunctions.Domain.astate option
  (** compute and return the postcondition for the given procedure starting from [initial]. If
      [debug] is true, print html debugging output. *)

  val exec_cfg :
    TransferFunctions.CFG.t -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate -> debug:bool -> invariant_map
  (** compute and return invariant map for the given CFG/procedure starting from [initial]. if
      [debug] is true, print html debugging output. *)

  val exec_pdesc :
    TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate -> invariant_map
  (** compute and return invariant map for the given procedure starting from [initial] *)

  val extract_post : InvariantMap.key -> 'a state InvariantMap.t -> 'a option
  (** extract the postcondition for a node id from the given invariant map *)

  val extract_pre : InvariantMap.key -> 'a state InvariantMap.t -> 'a option
  (** extract the precondition for a node id from the given invariant map *)

  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
  (** extract the state for a node id from the given invariant map *)
end

(** create an intraprocedural abstract interpreter from a scheduler and transfer functions *)
module MakeNoCFG
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.SIL with module CFG = Scheduler.CFG) :
  S with module TransferFunctions = TransferFunctions and module InvariantMap = Scheduler.CFG.IdMap

(** create an intraprocedural abstract interpreter from a CFG and functors for creating a scheduler/
    transfer functions from a CFG *)
module Make (CFG : ProcCfg.S) (MakeTransferFunctions : TransferFunctions.MakeSIL) :
  S with module TransferFunctions = MakeTransferFunctions(CFG) and module InvariantMap = CFG.IdMap
