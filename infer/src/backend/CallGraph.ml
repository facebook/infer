(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module type NodeSig = sig
  type t = private {id: int; pname: Typ.Procname.t; mutable successors: int list; mutable flag: bool}

  val make : int -> Typ.Procname.t -> int list -> t

  val add_successor : t -> int -> unit

  val set_flag : t -> unit

  val unset_flag : t -> unit

  val pp_dot : F.formatter -> t -> unit
end

module Node : NodeSig = struct
  type t = {id: int; pname: Typ.Procname.t; mutable successors: int list; mutable flag: bool}

  let make id pname successors = {id; pname; successors; flag= false}

  let add_successor node successor = node.successors <- successor :: node.successors

  let set_flag n = n.flag <- true

  let unset_flag n = n.flag <- false

  let pp_dot fmt {id; pname; successors; flag} =
    let pp_id fmt id = F.fprintf fmt "N%d" id in
    let pp_edge fmt src dst = F.fprintf fmt "  %a -> %a ;@\n" pp_id src pp_id dst in
    let pp_flag fmt flag = F.fprintf fmt "%B" flag in
    F.fprintf fmt "  %a [ label = %S, flag = %a ];@\n" pp_id id
      (F.asprintf "%a" Typ.Procname.pp pname)
      pp_flag flag ;
    List.iter successors ~f:(pp_edge fmt id) ;
    F.pp_print_newline fmt ()
end

module IdMap = Typ.Procname.Hash
module NodeMap = Caml.Hashtbl.Make (Int)

(** [node_map] is a map from ids (unique ints) to nodes corresponding to defined procedures. 
    [id_map] is a map from all encountered (not necessarily defined) procnames to their ids, 
    and thus its image is a superset of the domain of [node_map], and usually a strict superset.
    [trim_id_map] makes the image equal to the domain of [node_map]. *)
type t = {id_map: int IdMap.t; node_map: Node.t NodeMap.t}

let reset {id_map; node_map} = IdMap.reset id_map ; NodeMap.reset node_map

let create initial_capacity =
  {id_map= IdMap.create initial_capacity; node_map= NodeMap.create initial_capacity}


let id_of_procname {id_map} pname = IdMap.find_opt id_map pname

let node_of_id {node_map} id = NodeMap.find_opt node_map id

let mem {node_map} id = NodeMap.mem node_map id

(** [id_map] may contain undefined procedures, so use [node_map] for actual size *)
let n_procs {node_map} = NodeMap.length node_map

let node_of_procname g pname = id_of_procname g pname |> Option.bind ~f:(node_of_id g)

let remove (g : t) pname =
  id_of_procname g pname |> Option.iter ~f:(NodeMap.remove g.node_map) ;
  IdMap.remove g.id_map pname


let get_or_set_id ({id_map} as graph) procname =
  match id_of_procname graph procname with
  | None ->
      let id = IdMap.length id_map in
      IdMap.replace id_map procname id ; id
  | Some id ->
      id


let create_node ({node_map} as graph) pname successor_pnames =
  let id = get_or_set_id graph pname in
  let successors = List.map successor_pnames ~f:(get_or_set_id graph) in
  let node = Node.make id pname successors in
  NodeMap.replace node_map id node


let get_or_init_node node_map id pname =
  match NodeMap.find_opt node_map id with
  | Some node ->
      node
  | None ->
      let new_node = Node.make id pname [] in
      NodeMap.add node_map id new_node ; new_node


let add_edge ({node_map} as graph) ~pname ~successor_pname =
  let id = get_or_set_id graph pname in
  let successor = get_or_set_id graph successor_pname in
  let node = get_or_init_node node_map id pname in
  (* initialize successor node if it isn't already initalized *)
  get_or_init_node node_map successor successor_pname |> ignore ;
  Node.add_successor node successor


let flag g pname = node_of_procname g pname |> Option.iter ~f:Node.set_flag

let flag_reachable g start_pname =
  let process_node init (n : Node.t) =
    if n.flag then init
    else (
      Node.set_flag n ;
      List.fold n.successors ~init ~f:(fun acc id ->
          match node_of_id g id with Some n' when not n'.flag -> n' :: acc | _ -> acc ) )
  in
  let rec flag_list frontier =
    if not (List.is_empty frontier) then flag_list (List.fold frontier ~init:[] ~f:process_node)
  in
  node_of_procname g start_pname |> Option.iter ~f:(fun start_node -> flag_list [start_node])


let trim_id_map (g : t) =
  IdMap.filter_map_inplace (fun _pname id -> Option.some_if (mem g id) id) g.id_map


let pp_dot fmt {node_map} =
  F.fprintf fmt "@\ndigraph callgraph {@\n" ;
  NodeMap.iter (fun _id n -> Node.pp_dot fmt n) node_map ;
  F.fprintf fmt "}@."


let to_dotty g filename =
  let outc = Filename.concat Config.results_dir filename |> Out_channel.create in
  let fmt = F.formatter_of_out_channel outc in
  pp_dot fmt g ; Out_channel.close outc


let remove_unflagged_and_unflag_all {id_map; node_map} =
  NodeMap.filter_map_inplace
    (fun _id (n : Node.t) ->
      if n.flag then (Node.unset_flag n ; Some n) else (IdMap.remove id_map n.pname ; None) )
    node_map


let get_unflagged_leaves g =
  NodeMap.fold
    (fun _id (n : Node.t) acc ->
      if n.flag || List.exists n.successors ~f:(mem g) then acc else n :: acc )
    g.node_map []


let fold_flagged graph ~f =
  NodeMap.fold (fun _id node acc -> if node.Node.flag then f node acc else acc) graph.node_map


(** choose some reasonable minimum capacity that also is a prime number *)
let default_initial_capacity = 1009
