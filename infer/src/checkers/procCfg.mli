(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module type Base = sig
  type t
  type node

  val node_id : node -> Cfg.Node.id
  val node_id_compare : Cfg.Node.id -> Cfg.Node.id -> int
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

  val pp_node : Format.formatter -> node -> unit
end

module Normal : Wrapper with type node = Cfg.Node.t

module Exceptional : Wrapper with type node = Cfg.Node.t

module Backward : functor (W : Wrapper) -> Wrapper with type node = W.node
