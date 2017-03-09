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


/** Module for call graphs */
type in_out_calls = {
  in_calls: int, /** total number of in calls transitively */
  out_calls: int /** total number of out calls transitively */
};

type t; /** the type of a call graph */


/** A call graph consists of a set of nodes (Typ.Procname.t), and edges between them.
    A node can be defined or undefined (to represent whether we have code for it).
    In an edge from [n1] to [n2], indicating that [n1] calls [n2],
    [n1] is the parent and [n2] is the child.
    Node [n1] is dependent on [n2] if there is a path from [n1] to [n2]
    using the child relationship. */

/** [add_edge cg f t] adds an edge from [f] to [t] in the call graph [cg].
    The nodes are also added as undefined, unless already present. */
let add_edge: t => Typ.Procname.t => Typ.Procname.t => unit;


/** Add a node to the call graph as defined */
let add_defined_node: t => Typ.Procname.t => unit;


/** Check if [source] recursively calls [dest] */
let calls_recursively: t => Typ.Procname.t => Typ.Procname.t => bool;


/** Create an empty call graph */
let create: option SourceFile.t => t;


/** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2];
    undefined nodes become defined if at least one side is. */
let extend: t => t => unit;


/** Return all the children of [n], whether defined or not */
let get_all_children: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Compute the ancestors of the node, if not pre-computed already */
let get_ancestors: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Compute the heirs of the node, if not pre-computed already */
let get_heirs: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the in/out calls of the node */
let get_calls: t => Typ.Procname.t => in_out_calls;


/** Return the list of nodes which are defined */
let get_defined_nodes: t => list Typ.Procname.t;


/** Return the children of [n] which are defined */
let get_defined_children: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the nodes dependent on [n] */
let get_dependents: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the list of nodes with calls */
let get_nodes_and_calls: t => list (Typ.Procname.t, in_out_calls);


/** Return all the nodes with their defined children */
let get_nodes_and_defined_children: t => list (Typ.Procname.t, Typ.Procname.Set.t);


/** Return the list of nodes, with defined flag, and the list of edges */
let get_nodes_and_edges: t => (list (Typ.Procname.t, bool), list (Typ.Procname.t, Typ.Procname.t));


/** Return the children of [n] which are not heirs of [n] and are defined */
let get_nonrecursive_dependents: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the parents of [n] */
let get_parents: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the ancestors of [n] which are also heirs of [n] */
let get_recursive_dependents: t => Typ.Procname.t => Typ.Procname.Set.t;


/** Return the path of the source file */
let get_source: t => SourceFile.t;


/** Load a call graph from a file */
let load_from_file: DB.filename => option t;


/** Returns true if the node is defined */
let node_defined: t => Typ.Procname.t => bool;


/** Remove the defined flag from a node, if it exists. */
let remove_node_defined: t => Typ.Procname.t => unit;


/** Print the call graph as a dotty file. */
let save_call_graph_dotty: SourceFile.t => (Typ.Procname.t => list 'a) => t => unit;


/** Save a call graph into a file */
let store_to_file: DB.filename => t => unit;
