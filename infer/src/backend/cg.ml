(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for call graphs *)

module L = Logging
module F = Format
open Utils

let pp_nodeset fmt set =
  let f node = F.fprintf fmt "%a@ " Procname.pp node in
  Procname.Set.iter f set

type node = Procname.t

type in_out_calls =
  { in_calls: int; (** total number of in calls transitively *)
    out_calls: int (** total number of out calls transitively *)
  }

type node_info =
  { mutable defined : bool; (** defined procedure as opposed to just declared *)
    mutable disabled : bool; (** originally defined procedures disabled by restrict_defined *)
    mutable parents: Procname.Set.t;
    mutable children: Procname.Set.t;
    mutable ancestors : Procname.Set.t option ; (** ancestors are computed lazily *)
    mutable heirs : Procname.Set.t option ; (** heirs are computed lazily *)
    mutable recursive_dependents : Procname.Set.t option ; (** recursive dependents are computed lazily *)
    mutable in_out_calls : in_out_calls option; (** calls are computed lazily *)
  }

(** Type for call graph *)
type t =
  {
    mutable source : DB.source_file; (** path for the source file *)
    mutable nLOC : int; (** number of LOC *)
    node_map : node_info Procname.Hash.t (** map from node to node_info *)
  }

let create () =
  { source = !DB.current_source;
    nLOC = !Config.nLOC;
    node_map = Procname.Hash.create 3 }

let _add_node g n defined disabled =
  try
    let info = Procname.Hash.find g.node_map n in
    (* defined and disabled only go from false to true
       to avoid accidental overwrite to false by calling add_edge *)
    if defined then info.defined <- true;
    if disabled then info.disabled <- true;
  with Not_found ->
    let info =
      { defined = defined;
        disabled = disabled;
        parents = Procname.Set.empty;
        children = Procname.Set.empty;
        ancestors = None;
        heirs = None;
        recursive_dependents = None;
        in_out_calls = None } in
    Procname.Hash.add g.node_map n info

let add_defined_node g n =
  _add_node g n true false

let add_disabled_node g n =
  _add_node g n false true

(** Compute the ancestors of the node, if not already computed *)
let compute_ancestors g node =
  let todo = ref (Procname.Set.singleton node) in
  let seen = ref Procname.Set.empty in
  let result = ref Procname.Set.empty in
  while not (Procname.Set.is_empty !todo) do
    let current = Procname.Set.choose !todo in
    todo := Procname.Set.remove current !todo;
    if not (Procname.Set.mem current !seen) then
      begin
        seen := Procname.Set.add current !seen;
        let info = Procname.Hash.find g current in
        match info.ancestors with
        | Some ancestors ->
            result := Procname.Set.union !result ancestors
        | None ->
            result := Procname.Set.union !result info.parents;
            todo := Procname.Set.union !todo info.parents
      end
  done;
  !result

(** Compute the heirs of the node, if not already computed *)
let compute_heirs g node =
  let todo = ref (Procname.Set.singleton node) in
  let seen = ref Procname.Set.empty in
  let result = ref Procname.Set.empty in
  while not (Procname.Set.is_empty !todo) do
    let current = Procname.Set.choose !todo in
    todo := Procname.Set.remove current !todo;
    if not (Procname.Set.mem current !seen) then
      begin
        seen := Procname.Set.add current !seen;
        let info = Procname.Hash.find g current in
        match info.heirs with
        | Some heirs ->
            result := Procname.Set.union !result heirs
        | None ->
            result := Procname.Set.union !result info.children;
            todo := Procname.Set.union !todo info.children
      end
  done;
  !result

(** Compute the ancestors of the node, if not pre-computed already *)
let get_ancestors (g: t) node =
  let info = Procname.Hash.find g.node_map node in
  match info.ancestors with
  | None ->
      let ancestors = compute_ancestors g.node_map node in
      info.ancestors <- Some ancestors;
      let size = Procname.Set.cardinal ancestors in
      if size > 1000 then L.err "%a has %d ancestors@." Procname.pp node size;
      ancestors
  | Some ancestors -> ancestors

(** Compute the heirs of the node, if not pre-computed already *)
let get_heirs (g: t) node =
  let info = Procname.Hash.find g.node_map node in
  match info.heirs with
  | None ->
      let heirs = compute_heirs g.node_map node in
      info.heirs <- Some heirs;
      let size = Procname.Set.cardinal heirs in
      if size > 1000 then L.err "%a has %d heirs@." Procname.pp node size;
      heirs
  | Some heirs -> heirs

let node_defined (g: t) n =
  try
    let info = Procname.Hash.find g.node_map n in
    info.defined
  with Not_found -> false

let node_set_defined (g: t) n defined =
  try
    let info = Procname.Hash.find g.node_map n in
    info.defined <- defined
  with Not_found -> ()

let add_edge g nfrom nto =
  _add_node g nfrom false false;
  _add_node g nto false false;
  let info_from = Procname.Hash.find g.node_map nfrom in
  let info_to = Procname.Hash.find g.node_map nto in
  info_from.children <- Procname.Set.add nto info_from.children;
  info_to.parents <- Procname.Set.add nfrom info_to.parents

(** iterate over the elements of a node_map in node order *)
let node_map_iter f g =
  let table = ref [] in
  Procname.Hash.iter (fun node info -> table := (node, info) :: !table) g.node_map;
  let cmp ((n1: Procname.t), _) ((n2: Procname.t), _) = Procname.compare n1 n2 in
  IList.iter (fun (n, info) -> f n info) (IList.sort cmp !table)

(** If not None, restrict defined nodes to the given set,
    and mark them as disabled. *)
let restrict_defined (g: t) (nodeset_opt: Procname.Set.t option) =
  match nodeset_opt with
  | None -> ()
  | Some nodeset ->
      let f node info =
        if info.defined && not (Procname.Set.mem node nodeset) then
          begin
            info.defined <- false;
            info.disabled <- true
          end in
      node_map_iter f g

let get_nodes (g: t) =
  let nodes = ref Procname.Set.empty in
  let f node info =
    nodes := Procname.Set.add node !nodes in
  node_map_iter f g;
  !nodes

let map_option f l =
  let lo = IList.filter (function | Some _ -> true | None -> false) (IList.map f l) in
  IList.map (function Some p -> p | None -> assert false) lo

let compute_calls g node =
  { in_calls = Procname.Set.cardinal (get_ancestors g node);
    out_calls = Procname.Set.cardinal (get_heirs g node) }

(** Compute the calls of the node, if not pre-computed already *)
let get_calls (g: t) node =
  let info = Procname.Hash.find g.node_map node in
  match info.in_out_calls with
  | None ->
      let calls = compute_calls g node in
      info.in_out_calls <- Some calls;
      calls
  | Some calls -> calls

let get_all_nodes (g: t) =
  let nodes = Procname.Set.elements (get_nodes g) in
  IList.map (fun node -> (node, get_calls g node)) nodes

let get_nodes_and_calls (g: t) =
  IList.filter (fun (n, calls) -> node_defined g n) (get_all_nodes g)

let node_get_num_ancestors g n =
  (n, Procname.Set.cardinal (get_ancestors g n))

let get_edges (g: t) : ((node * int) * (node * int)) list =
  let edges = ref [] in
  let f node info =
    Procname.Set.iter (fun nto -> edges := (node_get_num_ancestors g node, node_get_num_ancestors g nto)::!edges) info.children in
  node_map_iter f g;
  !edges

(** Return all the children of [n], whether defined or not *)
let get_all_children (g: t) n =
  (Procname.Hash.find g.node_map n).children

(** Return the children of [n] which are defined *)
let get_defined_children (g: t) n =
  Procname.Set.filter (node_defined g) (get_all_children g n)

(** Return the parents of [n] *)
let get_parents (g: t) n =
  (Procname.Hash.find g.node_map n).parents

(** Return true if [n] is recursive *)
let is_recursive (g: t) n =
  Procname.Set.mem n (get_ancestors g n)

(** [call_recursively source dest] returns [true] if function [source] recursively calls function [dest] *)
let calls_recursively (g: t) source dest =
  Procname.Set.mem source (get_ancestors g dest)

(** Return the children of [n] which are not heirs of [n] *)
let get_nonrecursive_dependents (g: t) n =
  let is_not_recursive pn = not (Procname.Set.mem pn (get_ancestors g n)) in
  let res0 = Procname.Set.filter is_not_recursive (get_all_children g n) in
  let res = Procname.Set.filter (node_defined g) res0 in
  (* L.err "Nonrecursive dependents of %s: %a@\n" n pp_nodeset res; *)
  res

(** Return the ancestors of [n] which are also heirs of [n] *)
let compute_recursive_dependents (g: t) n =
  let reached_from_n pn = Procname.Set.mem n (get_ancestors g pn) in
  let res0 = Procname.Set.filter reached_from_n (get_ancestors g n) in
  let res = Procname.Set.filter (node_defined g) res0 in
  (* L.err "Recursive dependents of %s: %a@\n" n pp_nodeset res; *)
  res

(** Compute the ancestors of [n] which are also heirs of [n], if not pre-computed already *)
let get_recursive_dependents (g: t) n =
  let info = Procname.Hash.find g.node_map n in
  match info.recursive_dependents with
  | None ->
      let recursive_dependents = compute_recursive_dependents g n in
      info.recursive_dependents <- Some recursive_dependents;
      recursive_dependents
  | Some recursive_dependents -> recursive_dependents

(** Return the nodes dependent on [n] *)
let get_dependents (g: t) n =
  Procname.Set.union (get_nonrecursive_dependents g n) (get_recursive_dependents g n)

(** Return all the nodes with their defined children *)
let get_nodes_and_defined_children (g: t) =
  let nodes = ref Procname.Set.empty in
  node_map_iter (fun n info -> if info.defined then nodes := Procname.Set.add n !nodes) g;
  let nodes_list = Procname.Set.elements !nodes in
  IList.map (fun n -> (n, get_defined_children g n)) nodes_list

type nodes_and_edges =
  (node * bool * bool) list * (* nodes with defined and disabled flag *)
  (node * node) list (* edges *)

(** Return the list of nodes, with defined+disabled flags, and the list of edges *)
let get_nodes_and_edges (g: t) : nodes_and_edges =
  let nodes = ref [] in
  let edges = ref [] in
  let do_children node info nto =
    edges := (node, nto) :: !edges in
  let f node info =
    nodes := (node, info.defined, info.disabled) :: !nodes;
    Procname.Set.iter (do_children node info) info.children in
  node_map_iter f g;
  (!nodes, !edges)

(** Return the list of nodes which are defined *)
let get_defined_nodes (g: t) =
  let (nodes, _) = get_nodes_and_edges g in
  let get_node (node, _, _) = node in
  IList.map get_node
    (IList.filter (fun (_, defined, _) -> defined)
       nodes)


(** Return the list of nodes which were originally defined,
    i.e. the nodes that were defined before calling restrict_defined.  *)
let get_originally_defined_nodes (g: t) =
  let (nodes, _) = get_nodes_and_edges g in
  let get_node (node, _, _) = node in
  IList.map get_node
    (IList.filter
       (fun (_, defined, disabled) -> defined || disabled)
       nodes)

(** Return the path of the source file *)
let get_source (g: t) =
  g.source

(** Return the number of LOC of the source file *)
let get_nLOC (g: t) =
  g.nLOC

(** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2]; undefined nodes become defined if at least one side is. *)
let extend cg_old cg_new =
  let nodes, edges = get_nodes_and_edges cg_new in
  IList.iter (fun (node, defined, disabled) -> _add_node cg_old node defined disabled) nodes;
  IList.iter (fun (nfrom, nto) -> add_edge cg_old nfrom nto) edges

(** Begin support for serialization *)

let callgraph_serializer : (DB.source_file * int * nodes_and_edges) Serialization.serializer = Serialization.create_serializer Serialization.cg_key

(** Load a call graph from a file *)
let load_from_file (filename : DB.filename) : t option =
  let g = create () in
  match Serialization.from_file callgraph_serializer filename with
  | None -> None
  | Some (source, nLOC, (nodes, edges)) ->
      IList.iter
        (fun (node, defined, disabled) ->
           if defined then add_defined_node g node;
           if disabled then add_disabled_node g node)
        nodes;
      IList.iter (fun (nfrom, nto) -> add_edge g nfrom nto) edges;
      g.source <- source;
      g.nLOC <- nLOC;
      Some g

(** Save a call graph into a file *)
let store_to_file (filename : DB.filename) (call_graph : t) =
  Serialization.to_file callgraph_serializer filename (call_graph.source, call_graph.nLOC, (get_nodes_and_edges call_graph))

let pp_graph_dotty get_specs (g: t) fmt =
  let nodes_with_calls = get_all_nodes g in
  let num_specs n = try IList.length (get_specs n) with exn when exn_not_failure exn -> - 1 in
  let get_color (n, calls) =
    if num_specs n != 0 then "green" else "red" in
  let get_shape (n, calls) =
    if node_defined g n then "box" else "diamond" in
  let pp_node fmt (n, calls) =
    F.fprintf fmt "\"%s\"" (Procname.to_filename n) in
  let pp_node_label fmt (n, calls) =
    F.fprintf fmt "\"%a | calls=%d %d | specs=%d)\"" Procname.pp n calls.in_calls calls.out_calls (num_specs n) in
  F.fprintf fmt "digraph {@\n";
  IList.iter (fun nc -> F.fprintf fmt "%a [shape=box,label=%a,color=%s,shape=%s]@\n" pp_node nc pp_node_label nc (get_color nc) (get_shape nc)) nodes_with_calls;
  IList.iter (fun (s, d) -> F.fprintf fmt "%a -> %a@\n" pp_node s pp_node d) (get_edges g);
  F.fprintf fmt "}@."

(** Print the current call graph as a dotty file. If the filename is [None], use the current file dir inside the DB dir. *)
let save_call_graph_dotty fname_opt get_specs (g: t) =
  let fname_dot = match fname_opt with
    | None -> DB.Results_dir.path_to_filename DB.Results_dir.Abs_source_dir ["call_graph.dot"]
    | Some fname -> fname in
  let outc = open_out (DB.filename_to_string fname_dot) in
  let fmt = F.formatter_of_out_channel outc in
  pp_graph_dotty get_specs g fmt;
  close_out outc
