(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). Defines useful wrappers that allows us to do tricks like turn a forward cfg into a
    backward one, or view a cfg as having a single instruction per node. *)

module type NodeCommonS = sig
  type t [@@deriving hash]

  type id [@@deriving compare, equal]

  val kind : t -> Procdesc.Node.nodekind

  val id : t -> id

  val loc : t -> Location.t

  val underlying_node : t -> Procdesc.Node.t

  val of_underlying_node : Procdesc.Node.t -> t

  val pp_id : F.formatter -> id -> unit

  module IdMap : PrettyPrintable.PPMap with type key = id

  module IdSet : PrettyPrintable.PPSet with type elt = id
end

module InstrNode : sig
  type instr_index = int

  include
    NodeCommonS
      with type t = Procdesc.Node.t * instr_index
       and type id = Procdesc.Node.id * instr_index

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val to_instr : instr_index -> t -> t
end = struct
  type instr_index = int [@@deriving compare, equal, hash]

  type t = Procdesc.Node.t * instr_index [@@deriving compare, equal]

  type id = Procdesc.Node.id * instr_index [@@deriving compare, equal, hash]

  let kind (t, _) = Procdesc.Node.get_kind t

  let underlying_node (t, _) = t

  let of_underlying_node t = (t, 0)

  let id (t, index) = (Procdesc.Node.get_id t, index)

  let hash node = hash_id (id node)

  let hash_fold_t state node = hash_fold_id state (id node)

  let loc (t, _) = Procdesc.Node.get_loc t

  let pp_id fmt (id, index) = F.fprintf fmt "(%a: %d)" Procdesc.Node.pp_id id index

  module OrderedId = struct
    type t = id [@@deriving compare]

    let pp = pp_id
  end

  module IdMap = PrettyPrintable.MakePPMap (OrderedId)
  module IdSet = PrettyPrintable.MakePPSet (OrderedId)

  let to_instr _ t = t
end

module type Node = sig
  include NodeCommonS

  val to_instr : InstrNode.instr_index -> t -> InstrNode.t
end

module DefaultNode : Node with type t = Procdesc.Node.t and type id = Procdesc.Node.id = struct
  type t = Procdesc.Node.t [@@deriving hash]

  type id = Procdesc.Node.id [@@deriving compare, equal]

  let kind = Procdesc.Node.get_kind

  let id = Procdesc.Node.get_id

  let hash = Procdesc.Node.hash

  let loc = Procdesc.Node.get_loc

  let underlying_node t = t

  let of_underlying_node t = t

  let pp_id = Procdesc.Node.pp_id

  module OrderedId = struct
    type t = id [@@deriving compare]

    let pp = pp_id
  end

  module IdMap = Procdesc.IdMap
  module IdSet = PrettyPrintable.MakePPSet (OrderedId)

  let to_instr index node = (node, index)
end

module type S = sig
  type t

  type instrs_dir

  module Node : Node

  val instrs : Node.t -> instrs_dir Instrs.t
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

  val exn_sink_node : t -> Node.t option

  val proc_desc : t -> Procdesc.t

  val fold_nodes : (t, Node.t, 'accum) Container.fold

  val from_pdesc : Procdesc.t -> t

  val is_loop_head : Procdesc.t -> Node.t -> bool

  val wto : t -> Node.t WeakTopologicalOrder.Partition.t
end

(** Forward CFG with no exceptional control-flow *)
module Normal = struct
  type t = Procdesc.t

  type instrs_dir = Instrs.not_reversed

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

  let exn_sink_node = Procdesc.get_exn_sink

  let proc_desc t = t

  let fold_nodes = Procdesc.fold_nodes

  let from_pdesc pdesc = pdesc

  let is_loop_head = Procdesc.is_loop_head

  let wto = Procdesc.get_wto
end

module type ExceptionalS =
  S
    with type t = Procdesc.t * DefaultNode.t list Procdesc.IdMap.t
     and module Node = DefaultNode
     and type instrs_dir = Instrs.not_reversed

(** Forward CFG with exceptional control-flow *)
module Exceptional = struct
  module Node = DefaultNode

  type instrs_dir = Instrs.not_reversed

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
    let exceptional_preds = Procdesc.fold_nodes pdesc ~f:add_exn_preds ~init:Procdesc.IdMap.empty in
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
             Node.IdSet.add (Procdesc.Node.get_id node) set ) )
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

  let exn_sink_node (pdesc, _) = Procdesc.get_exn_sink pdesc

  let is_loop_head = Procdesc.is_loop_head

  module WTO = WeakTopologicalOrder.Bourdoncle_SCC (struct
    module Node = Node

    type t = Procdesc.t

    let fold_succs _cfg n ~init ~f =
      (* we do not care about duplicate edges *)
      let init = List.fold ~init ~f (Procdesc.Node.get_succs n) in
      List.fold ~init ~f (Procdesc.Node.get_exn n)


    let start_node = Procdesc.get_start_node
  end)

  let wto (pdesc, _) = WTO.make pdesc
end

(** Forward CFG with exceptional control-flow, but no edge from exceptions sink to exit node. *)
module ExceptionalNoSinkToExitEdge : ExceptionalS = struct
  include Exceptional

  (** Returns true iff the node [nd] is the exception sink. *)
  let is_exn_sink (nd : Node.t) : bool =
    Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind nd) Procdesc.Node.exn_sink_kind


  (** Returns true iff the node [nd] is the procedure's exit node. *)
  let is_exit_node (nd : Node.t) : bool =
    Procdesc.Node.equal_nodekind (Procdesc.Node.get_kind nd) Procdesc.Node.Exit_node


  (** Redefines [fold_normal_succs] in the [Exceptional] module, such that if the node [n] is the
      exceptions sink, then we filter out the exit node from the successors. *)
  let fold_normal_succs _ n ~init ~f =
    (let cfg_successors = Procdesc.Node.get_succs n in
     if is_exn_sink n then
       List.filter cfg_successors ~f:(fun (succ_node : Node.t) -> not (is_exit_node succ_node))
     else cfg_successors )
    |> List.fold ~init ~f


  (** Redefines [fold_normal_preds] in the [Exceptional] module, such that if the node [n] is the
      exit node, then we filter out the exceptions sink from the predecessors. *)
  let fold_normal_preds _ n ~init ~f =
    (let cfg_predecessors = Procdesc.Node.get_preds n in
     if is_exit_node n then
       List.filter cfg_predecessors ~f:(fun (pred_node : Node.t) -> not (is_exn_sink pred_node))
     else cfg_predecessors )
    |> List.fold ~init ~f


  (** fold over all normal and exceptional successors of [n], but using the version of
      [fold_normal_succs] defined by [ExceptionalNoSinkToExitEdge], instead of [Exceptional].
      Redefines [fold_succs] in the [Exceptional] module. *)
  let fold_succs t n ~init ~f =
    fold_avoid_duplicates fold_normal_succs fold_normal_succs fold_exceptional_succs t n ~init ~f


  (** fold over all normal and exceptional predecessors of [n], but using the version of
      [fold_normal_preds] defined by [ExceptionalNoSinkToExitEdge], instead of [Exceptional].
      Redefines [fold_preds] in the [Exceptional] module. *)
  let fold_preds t n ~init ~f =
    fold_avoid_duplicates fold_normal_preds fold_normal_preds fold_exceptional_preds t n ~init ~f
end

(** Wrapper that reverses the direction of the CFG *)
module Backward (Base : S with type instrs_dir = Instrs.not_reversed) = struct
  include (
    Base : S with type t = Base.t and type instrs_dir := Base.instrs_dir and module Node = Base.Node )

  type instrs_dir = Instrs.reversed

  let instrs n = Instrs.reverse_order (Base.instrs n)

  let fold_succs = Base.fold_preds

  let fold_preds = Base.fold_succs

  let start_node = Base.exit_node

  let exit_node = Base.start_node

  let exn_sink_node = Base.exn_sink_node

  let fold_normal_succs = Base.fold_normal_preds

  let fold_normal_preds = Base.fold_normal_succs

  let fold_exceptional_succs = Base.fold_exceptional_preds

  let fold_exceptional_preds = Base.fold_exceptional_succs

  module WTO = WeakTopologicalOrder.Bourdoncle_SCC (struct
    module Node = Node

    type nonrec t = t

    let fold_succs = fold_succs

    let start_node = start_node
  end)

  let wto = WTO.make
end

module OneInstrPerNode (Base : S with module Node = DefaultNode) : sig
  include
    S with type t = Base.t and module Node = InstrNode and type instrs_dir = Instrs.not_reversed

  val last_of_underlying_node : Procdesc.Node.t -> Node.t
end = struct
  type t = Base.t

  type instrs_dir = Instrs.not_reversed

  module Node = InstrNode

  let instrs (node, index) =
    let instrs = Base.instrs node in
    if Instrs.is_empty instrs then Instrs.empty else Instrs.nth_exn instrs index |> Instrs.singleton


  let first_of_node node = (node, 0)

  let last_of_node node = (node, max 0 (Instrs.count (Base.instrs node) - 1))

  let last_of_underlying_node = last_of_node

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

  let exn_sink_node cfg = Option.map (Base.exn_sink_node cfg) ~f:last_of_node

  let proc_desc = Base.proc_desc

  let fold_instr_nodes node ~init ~f =
    match Base.instrs node |> Instrs.count with
    | 0 ->
        f init (node, 0)
    | nb_instrs ->
        IContainer.forto nb_instrs ~init ~f:(fun acc index -> f acc (node, index))


  let fold_nodes cfg ~init ~f =
    Base.fold_nodes cfg ~init ~f:(fun acc node -> fold_instr_nodes node ~init:acc ~f)


  let from_pdesc = Base.from_pdesc

  let is_loop_head pdesc = function node, 0 -> Base.is_loop_head pdesc node | _ -> false

  let fold_right_instr_nodes node ~init ~f =
    match Base.instrs node |> Instrs.count with
    | 0 ->
        f init (node, 0)
    | nb_instrs ->
        IContainer.forto_right nb_instrs ~init ~f:(fun acc index -> f acc (node, index))


  let wto cfg =
    Base.wto cfg |> WeakTopologicalOrder.Partition.expand ~fold_right:fold_right_instr_nodes
end

module NormalOneInstrPerNode = OneInstrPerNode (Normal)

(* Make ProcCfg compatible with ocamlgraph *)
module MakeOcamlGraph (Base : S) = struct
  type t = Base.t

  module V = struct
    type t = Base.Node.t

    let compare n1 n2 = Base.Node.compare_id (Base.Node.id n1) (Base.Node.id n2)

    let equal = [%compare.equal: t]

    let hash = Base.Node.hash
  end

  let pred g = IContainer.to_rev_list ~fold:(Base.fold_normal_preds g)

  let succ g = IContainer.to_rev_list ~fold:(Base.fold_normal_succs g)

  let iter_succ f g node = Container.iter ~fold:(Base.fold_normal_succs g) ~f node

  let fold_vertex f g init = Base.fold_nodes ~init ~f:(Fn.flip f) g

  let iter_vertex f g = Container.iter ~fold:Base.fold_nodes g ~f

  let nb_vertex = Container.length ~fold:Base.fold_nodes
end
