(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). Defines useful wrappers that allows us to do tricks like turn a forward cfg into a
    backward one, or view a cfg as having a single instruction per node. *)

module type Node = sig
  type t

  type id

  val kind : t -> Procdesc.Node.nodekind

  val id : t -> id

  val hash : t -> int

  val loc : t -> Location.t

  val underlying_node : t -> Procdesc.Node.t

  val of_underlying_node : Procdesc.Node.t -> t

  val compare_id : id -> id -> int

  val pp_id : F.formatter -> id -> unit

  module IdMap : PrettyPrintable.PPMap with type key = id

  module IdSet : PrettyPrintable.PPSet with type elt = id
end

module DefaultNode : Node with type t = Procdesc.Node.t and type id = Procdesc.Node.id = struct
  type t = Procdesc.Node.t

  type id = Procdesc.Node.id

  let kind = Procdesc.Node.get_kind

  let id = Procdesc.Node.get_id

  let hash = Procdesc.Node.hash

  let loc = Procdesc.Node.get_loc

  let underlying_node t = t

  let of_underlying_node t = t

  let compare_id = Procdesc.Node.compare_id

  let pp_id = Procdesc.Node.pp_id

  module OrderedId = struct
    type t = id

    let compare = compare_id

    let pp = pp_id
  end

  module IdMap = PrettyPrintable.MakePPMap (OrderedId)
  module IdSet = PrettyPrintable.MakePPSet (OrderedId)
end

module InstrNode : sig
  type instr_index = int

  include Node
          with type t = Procdesc.Node.t * instr_index
           and type id = Procdesc.Node.id * instr_index
end = struct
  type instr_index = int [@@deriving compare]

  type t = Procdesc.Node.t * instr_index

  type id = Procdesc.Node.id * instr_index [@@deriving compare]

  let kind (t, _) = Procdesc.Node.get_kind t

  let underlying_node (t, _) = t

  let of_underlying_node t = (t, 0)

  let id (t, index) = (Procdesc.Node.get_id t, index)

  let hash node = Hashtbl.hash (id node)

  let loc (t, _) = Procdesc.Node.get_loc t

  let pp_id fmt (id, index) = F.fprintf fmt "(%a: %d)" Procdesc.Node.pp_id id index

  module OrderedId = struct
    type t = id

    let compare = compare_id

    let pp = pp_id
  end

  module IdMap = PrettyPrintable.MakePPMap (OrderedId)
  module IdSet = PrettyPrintable.MakePPSet (OrderedId)
end

module type S = sig
  type t

  module Node : Node

  val instrs : Node.t -> Instrs.t
  (** get the instructions from a node *)

  val fold_succs : t -> (Node.t, Node.t, 'accum) Container.fold

  val fold_preds : t -> (Node.t, Node.t, 'accum) Container.fold
  (** fold over all predecessors (normal and exceptional) *)

  val fold_normal_succs : t -> (Node.t, Node.t, 'accum) Container.fold
  (** fold over non-exceptional successors *)

  val fold_normal_preds : t -> (Node.t, Node.t, 'accum) Container.fold
  (** fold over non-exceptional predecessors *)

  val fold_exceptional_succs : t -> (Node.t, Node.t, 'accum) Container.fold
  (** fold over exceptional successors *)

  val fold_exceptional_preds : t -> (Node.t, Node.t, 'accum) Container.fold
  (** fold over exceptional predecessors *)

  val start_node : t -> Node.t

  val exit_node : t -> Node.t

  val proc_desc : t -> Procdesc.t

  val fold_nodes : (t, Node.t, 'accum) Container.fold

  val from_pdesc : Procdesc.t -> t

  val is_loop_head : Procdesc.t -> Node.t -> bool
end

(** Forward CFG with no exceptional control-flow *)
module Normal = struct
  type t = Procdesc.t

  module Node = DefaultNode

  let instrs = Procdesc.Node.get_instrs

  let fold_normal_succs _ n ~init ~f = n |> Procdesc.Node.get_succs |> List.fold ~init ~f

  let fold_normal_preds _ n ~init ~f = n |> Procdesc.Node.get_preds |> List.fold ~init ~f

  (* prune away exceptional control flow *)
  let fold_exceptional_succs _ _ ~init ~f:_ = init

  let fold_exceptional_preds _ _ ~init ~f:_ = init

  let fold_succs = fold_normal_succs

  let fold_preds = fold_normal_preds

  let start_node = Procdesc.get_start_node

  let exit_node = Procdesc.get_exit_node

  let proc_desc t = t

  let fold_nodes = Procdesc.fold_nodes

  let from_pdesc pdesc = pdesc

  let is_loop_head = Procdesc.is_loop_head
end

(** Forward CFG with exceptional control-flow *)
module Exceptional = struct
  module Node = DefaultNode

  type id_node_map = Node.t list Procdesc.IdMap.t

  type t = Procdesc.t * id_node_map

  let fold_exceptional_succs _ n ~init ~f = n |> Procdesc.Node.get_exn |> List.fold ~init ~f

  let from_pdesc pdesc =
    (* map from a node to its exceptional predecessors *)
    let add_exn_preds exn_preds_acc n =
      let add_exn_pred exn_preds_acc exn_succ_node =
        let exn_succ_node_id = Procdesc.Node.get_id exn_succ_node in
        let existing_exn_preds =
          try Procdesc.IdMap.find exn_succ_node_id exn_preds_acc with Caml.Not_found -> []
        in
        if not (List.mem ~equal:Procdesc.Node.equal existing_exn_preds n) then
          (* don't add duplicates *)
          Procdesc.IdMap.add exn_succ_node_id (n :: existing_exn_preds) exn_preds_acc
        else exn_preds_acc
      in
      fold_exceptional_succs pdesc n ~f:add_exn_pred ~init:exn_preds_acc
    in
    let exceptional_preds =
      Procdesc.fold_nodes pdesc ~f:add_exn_preds ~init:Procdesc.IdMap.empty
    in
    (pdesc, exceptional_preds)


  let instrs = Procdesc.Node.get_instrs

  let fold_nodes (t, _) ~init ~f = Procdesc.fold_nodes t ~init ~f

  let fold_normal_succs _ n ~init ~f = n |> Procdesc.Node.get_succs |> List.fold ~init ~f

  let fold_normal_preds _ n ~init ~f = n |> Procdesc.Node.get_preds |> List.fold ~init ~f

  let fold_exceptional_preds (_, exn_pred_map) n ~init ~f =
    match Procdesc.IdMap.find (Procdesc.Node.get_id n) exn_pred_map with
    | exn_preds ->
        List.fold exn_preds ~init ~f
    | exception Caml.Not_found ->
        init


  let fold_avoid_duplicates fold_normal_alpha fold_normal_idset fold_exceptional t n ~init ~f =
    (* need a copy of [fold_normal] otherwise OCaml wants the types *)
    let acc_normal = fold_normal_alpha t n ~init ~f in
    let normal_set =
      lazy
        (fold_normal_idset t n ~init:Node.IdSet.empty ~f:(fun set node ->
             Node.IdSet.add (Procdesc.Node.get_id node) set ))
    in
    let f acc node =
      if Node.IdSet.mem (Procdesc.Node.get_id node) (Lazy.force_val normal_set) then acc
      else f acc node
    in
    fold_exceptional t n ~init:acc_normal ~f


  (** fold over all normal and exceptional successors of [n]. *)
  let fold_succs t n ~init ~f =
    fold_avoid_duplicates fold_normal_succs fold_normal_succs fold_exceptional_succs t n ~init ~f


  (** fold over all normal and exceptional predecessors of [n]. *)
  let fold_preds t n ~init ~f =
    fold_avoid_duplicates fold_normal_preds fold_normal_preds fold_exceptional_preds t n ~init ~f


  let proc_desc (pdesc, _) = pdesc

  let start_node (pdesc, _) = Procdesc.get_start_node pdesc

  let exit_node (pdesc, _) = Procdesc.get_exit_node pdesc

  let is_loop_head = Procdesc.is_loop_head
end

(** Wrapper that reverses the direction of the CFG *)
module Backward (Base : S) = struct
  include Base

  let instrs n = Instrs.reverse_order (Base.instrs n)

  let fold_succs = Base.fold_preds

  let fold_preds = Base.fold_succs

  let start_node = Base.exit_node

  let exit_node = Base.start_node

  let fold_normal_succs = Base.fold_normal_preds

  let fold_normal_preds = Base.fold_normal_succs

  let fold_exceptional_succs = Base.fold_exceptional_preds

  let fold_exceptional_preds = Base.fold_exceptional_succs
end

module OneInstrPerNode (Base : S with module Node = DefaultNode) :
  S with type t = Base.t and module Node = InstrNode =
struct
  type t = Base.t

  module Node = InstrNode

  let instrs (node, index) =
    let instrs = Base.instrs node in
    if Instrs.is_empty instrs then Instrs.empty else Instrs.nth_exn instrs index |> Instrs.single


  let first_of_node node = (node, 0)

  let last_of_node node = (node, max 0 (Instrs.count (Base.instrs node) - 1))

  let fold_normal_succs _ _ ~init:_ ~f:_ = (* not used *) assert false

  let fold_exceptional_succs _ _ ~init:_ ~f:_ = (* not used *) assert false

  let fold_succs cfg (node, index) ~init ~f =
    let succ_index = index + 1 in
    if Instrs.nth_exists (Base.instrs node) succ_index then f init (node, succ_index)
    else
      let f acc node = f acc (first_of_node node) in
      Base.fold_succs cfg node ~init ~f


  let call_on_last ~f acc node = f acc (last_of_node node)

  let fold_normal_preds cfg (node, index) ~init ~f =
    if index >= 1 then f init (node, index - 1)
    else Base.fold_normal_preds cfg node ~init ~f:(call_on_last ~f)


  let fold_exceptional_preds cfg (node, index) ~init ~f =
    if index >= 1 then init else Base.fold_exceptional_preds cfg node ~init ~f:(call_on_last ~f)


  let fold_preds cfg (node, index) ~init ~f =
    if index >= 1 then f init (node, index - 1)
    else Base.fold_preds cfg node ~init ~f:(call_on_last ~f)


  let start_node cfg = first_of_node (Base.start_node cfg)

  let exit_node cfg = last_of_node (Base.exit_node cfg)

  let proc_desc = Base.proc_desc

  let fold_nodes cfg ~init ~f =
    let f init node =
      match Base.instrs node |> Instrs.count with
      | 0 ->
          f init (node, 0)
      | nb_instrs ->
          IContainer.forto nb_instrs ~init ~f:(fun acc index -> f acc (node, index))
    in
    Base.fold_nodes cfg ~init ~f


  let from_pdesc = Base.from_pdesc

  let is_loop_head pdesc = function node, 0 -> Base.is_loop_head pdesc node | _ -> false
end

module NormalOneInstrPerNode = OneInstrPerNode (Normal)
