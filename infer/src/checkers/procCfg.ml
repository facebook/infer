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

(* wrapper that allows us to do tricks like turn a forward cfg to into a backward one *)
module type Wrapper = sig
  type t
  type node
  type node_id

  val get_node_id : node -> node_id
  val get_succs : node -> node list
  val get_exn_succs : node -> node list
  val get_preds : node -> node list
  val get_start_node : t -> node
  val get_exit_node : t -> node
  val get_instrs : node -> Sil.instr list
  val get_kind : node -> Cfg.Node.nodekind
  val get_proc_desc : t -> Cfg.Procdesc.t
  val get_nodes : t -> node list

  val from_pdesc : Cfg.Procdesc.t -> t

  val node_id_compare : node_id -> node_id -> int

  val pp_node : F.formatter -> node -> unit
  val pp_node_id : F.formatter -> node_id -> unit

end

module Forward : Wrapper with type node = Cfg.node = struct
  type t = Cfg.Procdesc.t
  type node = Cfg.node
  type node_id = int

  let get_node_id = Cfg.Node.get_id
  let get_succs = Cfg.Node.get_succs
  let get_exn_succs = Cfg.Node.get_exn
  let get_preds = Cfg.Node.get_preds
  let get_start_node = Cfg.Procdesc.get_start_node
  let get_exit_node = Cfg.Procdesc.get_exit_node
  let get_instrs = Cfg.Node.get_instrs
  let get_kind = Cfg.Node.get_kind
  let get_proc_desc t = t
  let get_nodes = Cfg.Procdesc.get_nodes

  let from_pdesc pdesc = pdesc

  let node_id_compare = int_compare

  let pp_node = Cfg.Node.pp
  let pp_node_id fmt n = F.fprintf fmt "%d" n

end

module Backward (W : Wrapper) : Wrapper = struct
  include W

  let get_succs = W.get_preds
  let get_preds = W.get_succs
  let get_start_node = W.get_exit_node
  let get_exit_node = W.get_start_node
  let get_instrs t = IList.rev (W.get_instrs t)

  (* TODO: we'll need to change the CFG to implement this correctly *)
  let get_exn_succs _ =
    failwith "Getting exceptional preds in backward analysis"

end

module NodeIdMap (W : Wrapper) = Map.Make(struct
    type t = W.node_id
    let compare = W.node_id_compare
  end)

module NodeIdSet (W : Wrapper) = Set.Make(struct
    type t = W.node_id
    let compare = W.node_id_compare
  end)
