(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


module F = Format

(** control-flow graph for a single procedure (as opposed to cfg.ml, which represents a cfg for a
    file). *)

module type Base = sig
  type t
  type node
  type node_id

  val node_id : node -> node_id
  val node_id_compare : node_id -> node_id -> int
  val succs : t -> node -> node list
  val preds : t -> node -> node list
end

(* wrapper that allows us to do tricks like turn a forward cfg to into a backward one *)
module type Wrapper = sig
  include Base

  val exn_succs : t -> node -> node list
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

module Forward : Wrapper with type node = Cfg.node = struct
  type t = Cfg.Procdesc.t
  type node = Cfg.node
  type node_id = int

  let node_id = Cfg.Node.get_id
  let succs _ n = Cfg.Node.get_succs n
  let exn_succs _ n = Cfg.Node.get_exn n
  let preds _ n = Cfg.Node.get_preds n
  let start_node = Cfg.Procdesc.get_start_node
  let exit_node = Cfg.Procdesc.get_exit_node
  let instrs = Cfg.Node.get_instrs
  let kind = Cfg.Node.get_kind
  let proc_desc t = t
  let nodes = Cfg.Procdesc.get_nodes

  let from_pdesc pdesc = pdesc

  let node_id_compare = int_compare

  let pp_node = Cfg.Node.pp
  let pp_node_id fmt n = F.fprintf fmt "%d" n

end

module Backward (W : Wrapper) = struct
  include W

  let succs = W.preds
  let preds = W.succs
  let start_node = W.exit_node
  let exit_node = W.start_node
  let instrs t = IList.rev (W.instrs t)

  (* TODO: we'll need to change the CFG to implement this correctly *)
  let exn_succs _ =
    failwith "Getting exceptional preds in backward analysis"

end

module NodeIdMap (B : Base) = Map.Make(struct
    type t = B.node_id
    let compare = B.node_id_compare
  end)

module NodeIdSet (B : Base) = Set.Make(struct
    type t = B.node_id
    let compare = B.node_id_compare
  end)
