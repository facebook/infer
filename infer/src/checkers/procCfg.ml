(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

(** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). *)

module type Base = sig
  type t
  type node
  type node_id

  val node_id : node -> node_id
  val node_id_compare : node_id -> node_id -> int
  (** all successors (normal and exceptional) *)
  val succs : t -> node -> node list
  (** all predecessors (normal and exceptional) *)
  val preds : t -> node -> node list
end

(** Wrapper that allows us to do tricks like turn a forward cfg to into a backward one *)
module type Wrapper = sig
  include Base

  (** non-exceptional successors *)
  val normal_succs : t -> node -> node list
  (** non-exceptional predecessors *)
  val normal_preds : t -> node -> node list
  (** exceptional successors *)
  val exceptional_succs : t -> node -> node list
  (** exceptional predescessors *)
  val exceptional_preds : t -> node -> node list
  val start_node : t -> node
  val exit_node : t -> node
  val instrs : node -> Sil.instr list
  val kind : node -> Cfg.Node.nodekind
  val proc_desc : t -> Cfg.Procdesc.t
  val nodes : t -> node list

  val from_pdesc : Cfg.Procdesc.t -> t

  val pp_node : F.formatter -> node -> unit
  val pp_node_id : F.formatter -> node_id -> unit
end

(** Forward CFG with no exceptional control-flow *)
module Normal = struct
  type t = Cfg.Procdesc.t
  type node = Cfg.node
  type node_id = Cfg.Node.id

  let node_id = Cfg.Node.get_id
  let normal_succs _ n = Cfg.Node.get_succs n
  let normal_preds _ n = Cfg.Node.get_preds n
  (* prune away exceptional control flow *)
  let exceptional_succs _ _ = []
  let exceptional_preds _ _ = []
  let succs = normal_succs
  let preds = normal_preds
  let start_node = Cfg.Procdesc.get_start_node
  let exit_node = Cfg.Procdesc.get_exit_node
  let instrs = Cfg.Node.get_instrs
  let kind = Cfg.Node.get_kind
  let proc_desc t = t
  let nodes = Cfg.Procdesc.get_nodes

  let from_pdesc pdesc = pdesc

  let node_id_compare = Cfg.Node.id_compare

  let pp_node = Cfg.Node.pp

  let pp_node_id fmt (n : Cfg.Node.id) = F.fprintf fmt "%d" (n :> int)
end

(** Forward CFG with exceptional control-flow *)
module Exceptional : Wrapper with type node = Cfg.node = struct

  module NodeIdMap = Map.Make(struct
      type t = Cfg.Node.id
      let compare = Cfg.Node.id_compare
    end)

  type node = Cfg.node
  type node_id = Cfg.Node.id
  type id_node_map = node list NodeIdMap.t
  type t = Cfg.Procdesc.t * id_node_map

  let from_pdesc pdesc =
    (* map from a node to its exceptional predecessors *)
    let add_exn_preds exn_preds_acc n =
      let add_exn_pred exn_preds_acc exn_succ_node =
        let exn_succ_node_id = Cfg.Node.get_id exn_succ_node in
        let existing_exn_preds =
          try NodeIdMap.find exn_succ_node_id exn_preds_acc
          with Not_found -> [] in
        if not (IList.mem Cfg.Node.equal n existing_exn_preds)
        then (* don't add duplicates *)
          NodeIdMap.add exn_succ_node_id (n :: existing_exn_preds) exn_preds_acc
        else
          exn_preds_acc in
      IList.fold_left add_exn_pred exn_preds_acc (Cfg.Node.get_exn n) in
    let exceptional_preds =
      IList.fold_left add_exn_preds NodeIdMap.empty (Cfg.Procdesc.get_nodes pdesc) in
    pdesc, exceptional_preds

  let nodes (t, _) = Cfg.Procdesc.get_nodes t

  let normal_succs _ n = Cfg.Node.get_succs n

  let normal_preds _ n = Cfg.Node.get_preds n

  let exceptional_succs _ n = Cfg.Node.get_exn n

  let exceptional_preds (_, exn_pred_map) n =
    try NodeIdMap.find (Cfg.Node.get_id n) exn_pred_map
    with Not_found -> []

  (** get all normal and exceptional successors of [n]. *)
  let succs t n =
    let normal_succs = normal_succs t n in
    match exceptional_succs t n with
    | [] ->
        normal_succs
    | exceptional_succs ->
        normal_succs @ exceptional_succs
        |> IList.sort Cfg.Node.compare
        |> IList.remove_duplicates Cfg.Node.compare

  (** get all normal and exceptional predecessors of [n]. *)
  let preds t n =
    let normal_preds = normal_preds t n in
    match exceptional_preds t n with
    | [] ->
        normal_preds
    | exceptional_preds ->
        normal_preds @ exceptional_preds
        |> IList.sort Cfg.Node.compare
        |> IList.remove_duplicates Cfg.Node.compare

  let proc_desc (pdesc, _) = pdesc
  let start_node (pdesc, _) = Cfg.Procdesc.get_start_node pdesc
  let exit_node (pdesc, _) = Cfg.Procdesc.get_exit_node pdesc
  let instrs = Cfg.Node.get_instrs
  let node_id = Cfg.Node.get_id
  let node_id_compare = Cfg.Node.id_compare
  let pp_node = Cfg.Node.pp
  let pp_node_id fmt (n : Cfg.Node.id) = F.fprintf fmt "%d" (n :> int)
  let kind = Cfg.Node.get_kind
end

(** Turn a forward CFG into a backward cfg *)
module Backward (W : Wrapper) = struct
  include W

  let succs = W.preds
  let preds = W.succs
  let start_node = W.exit_node
  let exit_node = W.start_node
  let instrs t = IList.rev (W.instrs t)
  let normal_succs = W.normal_preds
  let normal_preds = W.normal_succs
  let exceptional_succs = W.exceptional_preds
  let exceptional_preds = W.exceptional_succs
end

module NodeIdMap (B : Base) = Map.Make(struct
    type t = B.node_id
    let compare = B.node_id_compare
  end)

module NodeIdSet (B : Base) = Set.Make(struct
    type t = B.node_id
    let compare = B.node_id_compare
  end)
