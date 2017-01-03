(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type 'a state = { pre: 'a; post: 'a; visit_count: int; }

(** type of an intraprocedural abstract interpreter *)
module type S = sig
  module TransferFunctions : TransferFunctions.S

  module InvariantMap : Caml.Map.S with type key = TransferFunctions.CFG.id

  (** invariant map from node id -> state representing postcondition for node id *)
  type invariant_map = TransferFunctions.Domain.astate state InvariantMap.t

  (** compute and return the postcondition for the given procedure starting from [initial] *)
  val compute_post :
    TransferFunctions.extras ProcData.t ->
    initial:TransferFunctions.Domain.astate ->
    TransferFunctions.Domain.astate option

  (** compute and return invariant map for the given CFG/procedure starting from [initial] *)
  val exec_cfg :
    TransferFunctions.CFG.t ->
    TransferFunctions.extras ProcData.t ->
    initial:TransferFunctions.Domain.astate ->
    invariant_map

  (** compute and return invariant map for the given procedure starting from [initial] *)
  val exec_pdesc :
    TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate -> invariant_map

  (** extract the postcondition for a node id from the given invariant map *)
  val extract_post : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  (** extract the precondition for a node id from the given invariant map *)
  val extract_pre : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  (** extract the state for a node id from the given invariant map *)
  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
end

(** create an intraprocedural abstract interpreter from a scheduler and transfer functions *)
module MakeNoCFG
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.S with module CFG = Scheduler.CFG) :
  S with module TransferFunctions = TransferFunctions

(** create an intraprocedural abstract interpreter from a CFG and functors for creating a scheduler/
    transfer functions from a CFG *)
module Make
    (CFG : ProcCfg.S)
    (MakeScheduler : Scheduler.Make)
    (MakeTransferFunctions : TransferFunctions.Make) :
  S with module TransferFunctions = MakeTransferFunctions(CFG)

(** create an interprocedural abstract interpreter given logic for handling summaries *)
module Interprocedural (Summary : Summary.S) : sig

  (** compute and return the summary for the given procedure and store it on disk using
      [compute_post]. *)
  val compute_and_store_post :
    compute_post: ('a ProcData.t -> Summary.summary option) ->
    make_extras : (Procdesc.t -> 'a) ->
    Callbacks.proc_callback_args ->
    Summary.summary option
end
