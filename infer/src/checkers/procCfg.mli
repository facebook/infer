(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). Defines useful wrappers that allows us to do tricks like turn a forward cfg to into a
    backward one, or view a cfg as having a single instruction per block *)

module type Node = sig
  type t
  type id

  val instrs : t -> Sil.instr list
  val kind : t -> Cfg.Node.nodekind
  val id : t -> id
  val id_compare : id -> id -> int
  val pp_id : Format.formatter -> id -> unit
end

module type S = sig
  type t
  type node
  include (Node with type t := node)

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

module DefaultNode : Node with type t = Cfg.Node.t and type id = Cfg.Node.id

(** Forward CFG with no exceptional control-flow *)
module Normal : S with type t = Cfg.Procdesc.t
                   and type node = DefaultNode.t
                   and type id = DefaultNode.id

(** Forward CFG with exceptional control-flow *)
module Exceptional : S with type t = Cfg.Procdesc.t * DefaultNode.t list Cfg.IdMap.t
                        and type node = DefaultNode.t
                        and type id = DefaultNode.id

(** Wrapper that reverses the direction of the CFG *)
module Backward (Base : S) : S with type t = Base.t
                                and type node = Base.node
                                and type id = Base.id

module NodeIdMap (CFG : S) : Map.S with type key = CFG.id

module NodeIdSet (CFG : S) : Set.S with type elt = CFG.id
