(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). Defines useful wrappers that allows us to do tricks like turn a forward cfg to into a
    backward one, or view a cfg as having a single instruction per block *)

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

  val pp_id : Format.formatter -> id -> unit

  module IdMap : PrettyPrintable.PPMap with type key = id

  module IdSet : PrettyPrintable.PPSet with type elt = id
end

module type S = sig
  type t

  type node

  include Node with type t := node

  val instrs : node -> Sil.instr list
  (** get the instructions from a node *)

  val succs : t -> node -> node list
  (** all successors (normal and exceptional) *)

  val preds : t -> node -> node list
  (** all predecessors (normal and exceptional) *)

  val normal_succs : t -> node -> node list
  (** non-exceptional successors *)

  val normal_preds : t -> node -> node list
  (** non-exceptional predecessors *)

  val exceptional_succs : t -> node -> node list
  (** exceptional successors *)

  val exceptional_preds : t -> node -> node list
  (** exceptional predescessors *)

  val start_node : t -> node

  val exit_node : t -> node

  val proc_desc : t -> Procdesc.t

  val nodes : t -> node list

  val from_pdesc : Procdesc.t -> t

  val is_loop_head : Procdesc.t -> node -> bool
end

module DefaultNode : Node with type t = Procdesc.Node.t and type id = Procdesc.Node.id

module InstrNode : sig
  type instr_index

  include Node
          with type t = Procdesc.Node.t * instr_index
           and type id = Procdesc.Node.id * instr_index
end

(** Forward CFG with no exceptional control-flow *)
module Normal :
  S with type t = Procdesc.t and type node = DefaultNode.t and type id = DefaultNode.id

(** Forward CFG with exceptional control-flow *)
module Exceptional :
  S
  with type t = Procdesc.t * DefaultNode.t list Procdesc.IdMap.t
   and type node = DefaultNode.t
   and type id = DefaultNode.id

(** Wrapper that reverses the direction of the CFG *)
module Backward (Base : S) : S with type t = Base.t and type node = Base.node and type id = Base.id

module OneInstrPerNode (Base : S with type node = DefaultNode.t and type id = DefaultNode.id) :
  S
  with type t = Base.t
   and type node = InstrNode.t
   and type id = InstrNode.id
   and module IdMap = InstrNode.IdMap
   and module IdSet = InstrNode.IdSet

module NormalOneInstrPerNode : module type of OneInstrPerNode (Normal)
