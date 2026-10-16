(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface

val widen_interrupted_loop :
     Procdesc.Node.t
  -> prev:(ExecutionDomain.t * PathContext.t) list
  -> next:(ExecutionDomain.t * PathContext.t) list
  -> (ExecutionDomain.t * PathContext.t) list
(** When the widening threshold stops the exploration of the loop headed by the given node, return
    copies of the states about to be dropped in which what the loop body may modify is havoced (see
    {!TransferFunctions.DisjReady.widen_interrupted_loop}). They are marked with
    [PathContext.loop_exit_only] so that they are only used to exit the loop. *)

val is_in_loop : Procdesc.Node.id -> Procdesc.Node.t -> bool
(** whether the node belongs to the (natural) loop headed by the node with the given id *)
