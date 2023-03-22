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
        (** Number of remaining disjuncts in the transfer function for Pulse *) }

let initial () = {last_instr= None; last_node= None; last_session= 0; remaining_disjuncts= None}

(** Global state *)
let gs = ref (initial ())

let () = AnalysisGlobalState.register_ref gs ~init:initial

let get_instr () = !gs.last_instr

let set_instr instr = !gs.last_instr <- Some instr

let get_node_exn () = Option.value_exn !gs.last_node

let get_node () = !gs.last_node

let set_node (node : Procdesc.Node.t) =
  !gs.last_instr <- None ;
  !gs.last_node <- Some node


let get_session () = !gs.last_session

let set_session (session : int) = !gs.last_session <- session

let get_loc_exn () =
  match !gs.last_instr with
  | Some instr ->
      Sil.location_of_instr instr
  | None ->
      get_node_exn () |> Procdesc.Node.get_loc


let get_loc () =
  match !gs.last_instr with Some instr -> Some (Sil.location_of_instr instr) | None -> None


let get_remaining_disjuncts () = !gs.remaining_disjuncts

let set_remaining_disjuncts remaining_disjuncts =
  !gs.remaining_disjuncts <- Some remaining_disjuncts
