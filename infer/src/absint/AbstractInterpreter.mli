(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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
    -> TransferFunctions.analysis_data
    -> initial:TransferFunctions.Domain.t
    -> Procdesc.t
    -> TransferFunctions.Domain.t option
  (** compute and return the postcondition for the given {!IR.Procdesc.t} starting from [initial].
      [pp_instr] is used for the debug HTML and passed as a hook to handle both SIL and HIL CFGs. *)

  val exec_cfg :
       ?do_narrowing:bool
    -> TransferFunctions.CFG.t
    -> TransferFunctions.analysis_data
    -> initial:TransferFunctions.Domain.t
    -> invariant_map
  (** compute and return invariant map for the given CFG/procedure starting from [initial]. *)

  val exec_pdesc :
       ?do_narrowing:bool
    -> TransferFunctions.analysis_data
    -> initial:TransferFunctions.Domain.t
    -> Procdesc.t
    -> invariant_map
  (** compute and return invariant map for the given procedure starting from [initial] *)

  val extract_post : TransferFunctions.CFG.Node.id -> 'a State.t InvariantMap.t -> 'a option
  (** extract the postcondition for a node id from the given invariant map *)

  val extract_pre : TransferFunctions.CFG.Node.id -> 'a State.t InvariantMap.t -> 'a option
  (** extract the precondition for a node id from the given invariant map *)

  val extract_state : TransferFunctions.CFG.Node.id -> 'a InvariantMap.t -> 'a option
  (** extract the state for a node id from the given invariant map *)
end

module type Make = functor (TransferFunctions : TransferFunctions.SIL) ->
  S with module TransferFunctions = TransferFunctions

(** create an intraprocedural abstract interpreter from transfer functions using the reverse
    post-order scheduler *)
module MakeRPO : Make

(** create an intraprocedural abstract interpreter from transfer functions using Bourdoncle's
    strongly connected component weak topological order *)
module MakeWTO : Make

(** used internally to compute various metrics related to [MakeDisjunctive] analyses; this can be
    queried with [get_cfg_metadata] below at the end of the analysis of each procedure *)
module DisjunctiveMetadata : sig
  type t

  val pp : F.formatter -> t -> unit
end

(** In the disjunctive interpreter, the domain is a set of abstract states representing a
    disjunction between these states. The transfer functions are executed on each state in the
    disjunct independently. The join on the disjunctive state is governed by the policy described in
    [DConfig]. *)
module MakeDisjunctive
    (T : TransferFunctions.DisjReady)
    (DConfig : TransferFunctions.DisjunctiveConfig) : sig
  include
    S
      with type TransferFunctions.analysis_data = T.analysis_data
       and module TransferFunctions.CFG = T.CFG
       and type TransferFunctions.Domain.t = T.DisjDomain.t list * T.NonDisjDomain.t

  val get_cfg_metadata : unit -> DisjunctiveMetadata.t
  (** return CFG-wide metadata about the analysis *)
end

module type TransferFunctions = sig
  include TransferFunctions.SIL

  val join_all : Domain.t list -> into:Domain.t option -> Domain.t option
  (** Joins the abstract states from predecessors. It returns [None] when the given list is empty
      and [into] is [None]. *)

  val filter_normal : Domain.t -> Domain.t
  (** Refines the abstract state to select non-exceptional concrete states. Should return bottom if
      no such states exist *)

  val filter_exceptional : Domain.t -> Domain.t
  (** Refines the abstract state to select exceptional concrete states. Should return bottom if no
      such states exist *)

  val transform_on_exceptional_edge : Domain.t -> Domain.t
  (** Change the nature normal/exceptional when flowing through an exceptional edge. For a forward
      analysis, it should turn an exceptional state into normal. For a backward analysis, it should
      turn an normal state into exceptional. *)
end

module type MakeExceptional = functor (T : TransferFunctions) -> S with module TransferFunctions = T

(* Create an intraprocedural backward abstract interpreter from transfer functions using the reverse
   post-order scheduler. Dispatch properly exceptional flows backward. *)
module MakeBackwardRPO : MakeExceptional

(* Create an intraprocedural backward abstract interpreter from transfer functions using the weak
   topological order scheduler. Dispatch properly exceptional flows backward. *)
module MakeBackwardWTO : MakeExceptional
