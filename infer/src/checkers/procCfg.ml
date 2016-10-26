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
    file). Defines useful wrappers that allows us to do tricks like turn a forward cfg into a
    backward one, or view a cfg as having a single instruction per node. *)

type index = Node_index | Instr_index of int

module type Node = sig
  type t
  type id

  val kind : t -> Cfg.Node.nodekind
  val id : t -> id
  val loc : t -> Location.t
  val underlying_id : t -> Cfg.Node.id
  val id_compare : id -> id -> int
  val pp_id : F.formatter -> id -> unit
end

module DefaultNode = struct
  type t = Cfg.Node.t
  type id = Cfg.Node.id

  let kind = Cfg.Node.get_kind
  let id = Cfg.Node.get_id
  let loc = Cfg.Node.get_loc
  let underlying_id = id
  let id_compare = Cfg.Node.id_compare
  let pp_id = Cfg.Node.pp_id
end

module InstrNode = struct
  type t = Cfg.Node.t
  type id = Cfg.Node.id * index

  let kind = Cfg.Node.get_kind

  let underlying_id t = Cfg.Node.get_id t

  let id t = underlying_id t, Node_index

  let loc t = Cfg.Node.get_loc t

  let index_compare index1 index2 = match index1, index2 with
    | Node_index, Node_index -> 0
    | Instr_index i1, Instr_index i2 -> int_compare i1 i2
    | Node_index, Instr_index _ -> 1
    | Instr_index _, Node_index -> -1

  let id_compare (id1, index1) (id2, index2) =
    let n = Cfg.Node.id_compare id1 id2 in
    if n <> 0 then n
    else index_compare index1 index2

  let pp_id fmt (id, index) = match index with
    | Node_index -> Cfg.Node.pp_id fmt id
    | Instr_index i -> F.fprintf fmt "(%a: %d)" Cfg.Node.pp_id id i
end

module type S = sig
  type t
  type node
  include (Node with type t := node)

  (** get the instructions from a node *)
  val instrs : node -> Sil.instr list

  (** explode a block into its instructions and an optional id for the instruction. the purpose of
      this is to specify a policy for fine-grained storage of invariants by the abstract
      interpreter. the interpreter will forget invariants at program points where the id is None,
      and remember them otherwise *)
  val instr_ids : node -> (Sil.instr * id option) list

  val succs : t -> node -> node list

  (** all predecessors (normal and exceptional) *)
  val preds : t -> node -> node list

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

  val proc_desc : t -> Cfg.Procdesc.t

  val nodes : t -> node list

  val from_pdesc : Cfg.Procdesc.t -> t
end

(** Forward CFG with no exceptional control-flow *)
module Normal = struct
  type t = Cfg.Procdesc.t
  type node = DefaultNode.t
  include (DefaultNode : module type of DefaultNode with type t := node)

  let instrs = Cfg.Node.get_instrs
  let instr_ids n = IList.map (fun i -> i, None) (instrs n)
  let normal_succs _ n = Cfg.Node.get_succs n
  let normal_preds _ n = Cfg.Node.get_preds n
  (* prune away exceptional control flow *)
  let exceptional_succs _ _ = []
  let exceptional_preds _ _ = []
  let succs = normal_succs
  let preds = normal_preds
  let start_node = Cfg.Procdesc.get_start_node
  let exit_node = Cfg.Procdesc.get_exit_node
  let proc_desc t = t
  let nodes = Cfg.Procdesc.get_nodes
  let from_pdesc pdesc = pdesc
end

(** Forward CFG with exceptional control-flow *)
module Exceptional = struct
  type node = DefaultNode.t
  type id_node_map = node list Cfg.IdMap.t
  type t = Cfg.Procdesc.t * id_node_map
  include (DefaultNode : module type of DefaultNode with type t := node)

  let from_pdesc pdesc =
    (* map from a node to its exceptional predecessors *)
    let add_exn_preds exn_preds_acc n =
      let add_exn_pred exn_preds_acc exn_succ_node =
        let exn_succ_node_id = Cfg.Node.get_id exn_succ_node in
        let existing_exn_preds =
          try Cfg.IdMap.find exn_succ_node_id exn_preds_acc
          with Not_found -> [] in
        if not (IList.mem Cfg.Node.equal n existing_exn_preds)
        then (* don't add duplicates *)
          Cfg.IdMap.add exn_succ_node_id (n :: existing_exn_preds) exn_preds_acc
        else
          exn_preds_acc in
      IList.fold_left add_exn_pred exn_preds_acc (Cfg.Node.get_exn n) in
    let exceptional_preds =
      IList.fold_left add_exn_preds Cfg.IdMap.empty (Cfg.Procdesc.get_nodes pdesc) in
    pdesc, exceptional_preds

  let instrs = Cfg.Node.get_instrs

  let instr_ids n = IList.map (fun i -> i, None) (instrs n)

  let nodes (t, _) = Cfg.Procdesc.get_nodes t

  let normal_succs _ n = Cfg.Node.get_succs n

  let normal_preds _ n = Cfg.Node.get_preds n

  let exceptional_succs _ n = Cfg.Node.get_exn n

  let exceptional_preds (_, exn_pred_map) n =
    try Cfg.IdMap.find (Cfg.Node.get_id n) exn_pred_map
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
end

(** Wrapper that reverses the direction of the CFG *)
module Backward (Base : S) = struct
  include Base
  let instrs n = IList.rev (Base.instrs n)
  let instr_ids n = IList.rev (Base.instr_ids n)

  let succs = Base.preds
  let preds = Base.succs
  let start_node = Base.exit_node
  let exit_node = Base.start_node
  let normal_succs = Base.normal_preds
  let normal_preds = Base.normal_succs
  let exceptional_succs = Base.exceptional_preds
  let exceptional_preds = Base.exceptional_succs
end

module OneInstrPerNode (Base : S with type node = Cfg.Node.t
                                  and type id = Cfg.Node.id) = struct
  include (Base : module type of Base with type id := Cfg.Node.id and type t = Base.t)
  type id = Base.id * index
  include (InstrNode : module type of InstrNode with type t := node and type id := id)

  (* keep the invariants before/after each instruction *)
  let instr_ids t =
    IList.mapi
      (fun i instr ->
         let id = Cfg.Node.get_id t, Instr_index i in
         instr, Some id)
      (instrs t)
end

module NodeIdMap (CFG : S) = Map.Make(struct
    type t = CFG.id
    let compare = CFG.id_compare
  end)

module NodeIdSet (CFG : S) = Set.Make(struct
    type t = CFG.id
    let compare = CFG.id_compare
  end)
