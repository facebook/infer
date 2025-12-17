(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** State of symbolic execution *)

type t =
  { mutable last_instr: Sil.instr option  (** Last instruction seen *)
  ; mutable last_node: Procdesc.Node.t option  (** Last node seen *)
  ; mutable last_session: int  (** Last session seen *)
  ; mutable remaining_disjuncts: int option
        (** Number of remaining disjuncts in the transfer function for Pulse *)
  ; mutable active_loops: Procdesc.IdSet.t
        (** Set of loop headers for which symbolic execution has already been initiated *) }

let initial () =
  { last_instr= None
  ; last_node= None
  ; last_session= 0
  ; remaining_disjuncts= None
  ; active_loops= Procdesc.IdSet.empty }


(** Global state *)
let gs = AnalysisGlobalState.make_dls ~init:initial

let set_instr instr = (DLS.get gs).last_instr <- Some instr

let get_node () = (DLS.get gs).last_node

let set_node (node : Procdesc.Node.t) =
  let gs = DLS.get gs in
  gs.last_instr <- None ;
  gs.last_node <- Some node


let reset_active_loops () =
  let gs = DLS.get gs in
  gs.active_loops <- Procdesc.IdSet.empty


let is_active_loop id =
  let gs = DLS.get gs in
  Procdesc.IdSet.mem id gs.active_loops


let set_active_loops set =
  let gs = DLS.get gs in
  gs.active_loops <- set


let set_active_loop id =
  let gs = DLS.get gs in
  gs.active_loops <- Procdesc.IdSet.add id gs.active_loops


let set_session (session : int) = (DLS.get gs).last_session <- session

let get_remaining_disjuncts () = (DLS.get gs).remaining_disjuncts

let set_remaining_disjuncts remaining_disjuncts =
  (DLS.get gs).remaining_disjuncts <- Some remaining_disjuncts
