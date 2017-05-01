/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

module Hashtbl = Caml.Hashtbl;


/** Module for call graphs */
module L = Logging;

module F = Format;

type node = Typ.Procname.t;

type in_out_calls = {
  in_calls: int, /** total number of in calls transitively */
  out_calls: int /** total number of out calls transitively */
};

type node_info = {
  /** defined procedure as opposed to just declared */
  mutable defined: bool,
  mutable parents: Typ.Procname.Set.t,
  mutable children: Typ.Procname.Set.t,
  /** ancestors are computed lazily */
  mutable ancestors: option Typ.Procname.Set.t,
  /** heirs are computed lazily */
  mutable heirs: option Typ.Procname.Set.t,
  /** recursive dependents are computed lazily */
  mutable recursive_dependents: option Typ.Procname.Set.t,
  /** calls are computed lazily */
  mutable in_out_calls: option in_out_calls
};


/** Type for call graph */
type t = {
  source: SourceFile.t, /** path for the source file */
  node_map: Typ.Procname.Hash.t node_info /** map from node to node_info */
};

let create source => {source, node_map: Typ.Procname.Hash.create 3};

let add_node g n ::defined =>
  try {
    let info = Typ.Procname.Hash.find g.node_map n;
    /* defined and disabled only go from false to true
       to avoid accidental overwrite to false by calling add_edge */
    if defined {
      info.defined = true
    }
  } {
  | Not_found =>
    let info = {
      defined,
      parents: Typ.Procname.Set.empty,
      children: Typ.Procname.Set.empty,
      ancestors: None,
      heirs: None,
      recursive_dependents: None,
      in_out_calls: None
    };
    Typ.Procname.Hash.add g.node_map n info
  };

let remove_node_defined g n =>
  try {
    let info = Typ.Procname.Hash.find g.node_map n;
    info.defined = false
  } {
  | Not_found => ()
  };

let add_defined_node g n => add_node g n defined::true;


/** Compute the ancestors of the node, if not already computed */
let compute_ancestors g node => {
  let todo = ref (Typ.Procname.Set.singleton node);
  let seen = ref Typ.Procname.Set.empty;
  let result = ref Typ.Procname.Set.empty;
  while (not (Typ.Procname.Set.is_empty !todo)) {
    let current = Typ.Procname.Set.choose !todo;
    todo := Typ.Procname.Set.remove current !todo;
    if (not (Typ.Procname.Set.mem current !seen)) {
      seen := Typ.Procname.Set.add current !seen;
      let info = Typ.Procname.Hash.find g current;
      switch info.ancestors {
      | Some ancestors => result := Typ.Procname.Set.union !result ancestors
      | None =>
        result := Typ.Procname.Set.union !result info.parents;
        todo := Typ.Procname.Set.union !todo info.parents
      }
    }
  };
  !result
};


/** Compute the heirs of the node, if not already computed */
let compute_heirs g node => {
  let todo = ref (Typ.Procname.Set.singleton node);
  let seen = ref Typ.Procname.Set.empty;
  let result = ref Typ.Procname.Set.empty;
  while (not (Typ.Procname.Set.is_empty !todo)) {
    let current = Typ.Procname.Set.choose !todo;
    todo := Typ.Procname.Set.remove current !todo;
    if (not (Typ.Procname.Set.mem current !seen)) {
      seen := Typ.Procname.Set.add current !seen;
      let info = Typ.Procname.Hash.find g current;
      switch info.heirs {
      | Some heirs => result := Typ.Procname.Set.union !result heirs
      | None =>
        result := Typ.Procname.Set.union !result info.children;
        todo := Typ.Procname.Set.union !todo info.children
      }
    }
  };
  !result
};


/** Compute the ancestors of the node, if not pre-computed already */
let get_ancestors (g: t) node => {
  let info = Typ.Procname.Hash.find g.node_map node;
  switch info.ancestors {
  | None =>
    let ancestors = compute_ancestors g.node_map node;
    info.ancestors = Some ancestors;
    let size = Typ.Procname.Set.cardinal ancestors;
    if (size > 1000) {
      L.err "%a has %d ancestors@." Typ.Procname.pp node size
    };
    ancestors
  | Some ancestors => ancestors
  }
};


/** Compute the heirs of the node, if not pre-computed already */
let get_heirs (g: t) node => {
  let info = Typ.Procname.Hash.find g.node_map node;
  switch info.heirs {
  | None =>
    let heirs = compute_heirs g.node_map node;
    info.heirs = Some heirs;
    let size = Typ.Procname.Set.cardinal heirs;
    if (size > 1000) {
      L.err "%a has %d heirs@." Typ.Procname.pp node size
    };
    heirs
  | Some heirs => heirs
  }
};

let node_defined (g: t) n =>
  try {
    let info = Typ.Procname.Hash.find g.node_map n;
    info.defined
  } {
  | Not_found => false
  };

let add_edge g nfrom nto => {
  add_node g nfrom defined::false;
  add_node g nto defined::false;
  let info_from = Typ.Procname.Hash.find g.node_map nfrom;
  let info_to = Typ.Procname.Hash.find g.node_map nto;
  info_from.children = Typ.Procname.Set.add nto info_from.children;
  info_to.parents = Typ.Procname.Set.add nfrom info_to.parents
};


/** iterate over the elements of a node_map in node order */
let node_map_iter f g => {
  let table = ref [];
  Typ.Procname.Hash.iter (fun node info => table := [(node, info), ...!table]) g.node_map;
  let cmp (n1: Typ.Procname.t, _) (n2: Typ.Procname.t, _) => Typ.Procname.compare n1 n2;
  List.iter f::(fun (n, info) => f n info) (List.sort ::cmp !table)
};

let get_nodes (g: t) => {
  let nodes = ref Typ.Procname.Set.empty;
  let f node _ => nodes := Typ.Procname.Set.add node !nodes;
  node_map_iter f g;
  !nodes
};

let compute_calls g node => {
  in_calls: Typ.Procname.Set.cardinal (get_ancestors g node),
  out_calls: Typ.Procname.Set.cardinal (get_heirs g node)
};


/** Compute the calls of the node, if not pre-computed already */
let get_calls (g: t) node => {
  let info = Typ.Procname.Hash.find g.node_map node;
  switch info.in_out_calls {
  | None =>
    let calls = compute_calls g node;
    info.in_out_calls = Some calls;
    calls
  | Some calls => calls
  }
};

let get_all_nodes (g: t) => {
  let nodes = Typ.Procname.Set.elements (get_nodes g);
  List.map f::(fun node => (node, get_calls g node)) nodes
};

let get_nodes_and_calls (g: t) =>
  List.filter f::(fun (n, _) => node_defined g n) (get_all_nodes g);

let node_get_num_ancestors g n => (n, Typ.Procname.Set.cardinal (get_ancestors g n));

let get_edges (g: t) :list ((node, int), (node, int)) => {
  let edges = ref [];
  let f node info =>
    Typ.Procname.Set.iter
      (
        fun nto =>
          edges := [(node_get_num_ancestors g node, node_get_num_ancestors g nto), ...!edges]
      )
      info.children;
  node_map_iter f g;
  !edges
};


/** Return all the children of [n], whether defined or not */
let get_all_children (g: t) n => (Typ.Procname.Hash.find g.node_map n).children;


/** Return the children of [n] which are defined */
let get_defined_children (g: t) n =>
  Typ.Procname.Set.filter (node_defined g) (get_all_children g n);


/** Return the parents of [n] */
let get_parents (g: t) n => (Typ.Procname.Hash.find g.node_map n).parents;


/** Check if [source] recursively calls [dest] */
let calls_recursively (g: t) source dest => Typ.Procname.Set.mem source (get_ancestors g dest);


/** Return the children of [n] which are not heirs of [n] */
let get_nonrecursive_dependents (g: t) n => {
  let is_not_recursive pn => not (Typ.Procname.Set.mem pn (get_ancestors g n));
  let res0 = Typ.Procname.Set.filter is_not_recursive (get_all_children g n);
  let res = Typ.Procname.Set.filter (node_defined g) res0;
  res
};


/** Return the ancestors of [n] which are also heirs of [n] */
let compute_recursive_dependents (g: t) n => {
  let reached_from_n pn => Typ.Procname.Set.mem n (get_ancestors g pn);
  let res0 = Typ.Procname.Set.filter reached_from_n (get_ancestors g n);
  let res = Typ.Procname.Set.filter (node_defined g) res0;
  res
};


/** Compute the ancestors of [n] which are also heirs of [n], if not pre-computed already */
let get_recursive_dependents (g: t) n => {
  let info = Typ.Procname.Hash.find g.node_map n;
  switch info.recursive_dependents {
  | None =>
    let recursive_dependents = compute_recursive_dependents g n;
    info.recursive_dependents = Some recursive_dependents;
    recursive_dependents
  | Some recursive_dependents => recursive_dependents
  }
};


/** Return the nodes dependent on [n] */
let get_dependents (g: t) n =>
  Typ.Procname.Set.union (get_nonrecursive_dependents g n) (get_recursive_dependents g n);


/** Return all the nodes with their defined children */
let get_nodes_and_defined_children (g: t) => {
  let nodes = ref Typ.Procname.Set.empty;
  node_map_iter
    (
      fun n info =>
        if info.defined {
          nodes := Typ.Procname.Set.add n !nodes
        }
    )
    g;
  let nodes_list = Typ.Procname.Set.elements !nodes;
  List.map f::(fun n => (n, get_defined_children g n)) nodes_list
};


/** nodes with defined flag, and edges */
type nodes_and_edges = (list (node, bool), list (node, node));


/** Return the list of nodes, with defined+disabled flags, and the list of edges */
let get_nodes_and_edges (g: t) :nodes_and_edges => {
  let nodes = ref [];
  let edges = ref [];
  let do_children node nto => edges := [(node, nto), ...!edges];
  let f node info => {
    nodes := [(node, info.defined), ...!nodes];
    Typ.Procname.Set.iter (do_children node) info.children
  };
  node_map_iter f g;
  (!nodes, !edges)
};


/** Return the list of nodes which are defined */
let get_defined_nodes (g: t) => {
  let (nodes, _) = get_nodes_and_edges g;
  let get_node (node, _) => node;
  List.map f::get_node (List.filter f::(fun (_, defined) => defined) nodes)
};


/** Return the path of the source file */
let get_source (g: t) => g.source;


/** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2];
    undefined nodes become defined if at least one side is. */
let extend cg_old cg_new => {
  let (nodes, edges) = get_nodes_and_edges cg_new;
  List.iter f::(fun (node, defined) => add_node cg_old node ::defined) nodes;
  List.iter f::(fun (nfrom, nto) => add_edge cg_old nfrom nto) edges
};


/** Begin support for serialization */
let callgraph_serializer: Serialization.serializer (SourceFile.t, nodes_and_edges) =
  Serialization.create_serializer Serialization.Key.cg;


/** Load a call graph from a file */
let load_from_file (filename: DB.filename) :option t =>
  switch (Serialization.read_from_file callgraph_serializer filename) {
  | None => None
  | Some (source, (nodes, edges)) =>
    let g = create source;
    List.iter
      f::(
        fun (node, defined) =>
          if defined {
            add_defined_node g node
          }
      )
      nodes;
    List.iter f::(fun (nfrom, nto) => add_edge g nfrom nto) edges;
    Some g
  };


/** Save a call graph into a file */
let store_to_file (filename: DB.filename) (call_graph: t) =>
  Serialization.write_to_file
    callgraph_serializer filename data::(call_graph.source, get_nodes_and_edges call_graph);

let pp_graph_dotty (g: t) fmt => {
  let nodes_with_calls = get_all_nodes g;
  let get_shape (n, _) =>
    if (node_defined g n) {
      "box"
    } else {
      "diamond"
    };
  let pp_node fmt (n, _) => F.fprintf fmt "\"%s\"" (Typ.Procname.to_filename n);
  let pp_node_label fmt (n, calls) =>
    F.fprintf fmt "\"%a | calls=%d %d)\"" Typ.Procname.pp n calls.in_calls calls.out_calls;
  F.fprintf fmt "digraph {@\n";
  List.iter
    f::(
      fun nc =>
        F.fprintf
          fmt
          "%a [shape=box,label=%a,color=%s,shape=%s]@\n"
          pp_node
          nc
          pp_node_label
          nc
          "red"
          (get_shape nc)
    )
    nodes_with_calls;
  List.iter f::(fun (s, d) => F.fprintf fmt "%a -> %a@\n" pp_node s pp_node d) (get_edges g);
  F.fprintf fmt "}@."
};


/** Print the call graph as a dotty file. */
let save_call_graph_dotty source (g: t) => {
  let fname_dot =
    DB.Results_dir.path_to_filename (DB.Results_dir.Abs_source_dir source) ["call_graph.dot"];
  let outc = open_out (DB.filename_to_string fname_dot);
  let fmt = F.formatter_of_out_channel outc;
  pp_graph_dotty g fmt;
  Out_channel.close outc
};
