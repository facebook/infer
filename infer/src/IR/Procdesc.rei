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


/** node of the control flow graph */
let module Node: {

  /** type of nodes */
  type t [@@deriving compare];

  /** node id */
  type id = private int [@@deriving compare];
  let equal_id: id => id => bool;

  /** kind of cfg node */
  type nodekind =
    | Start_node Procname.t
    | Exit_node Procname.t
    | Stmt_node string
    | Join_node
    | Prune_node bool Sil.if_kind string /** (true/false branch, if_kind, comment) */
    | Skip_node string
  [@@deriving compare];
  let equal_nodekind: nodekind => nodekind => bool;

  /** kind of Stmt_node for an exception handler. */
  let exn_handler_kind: nodekind;

  /** kind of Stmt_node for an exceptions sink. */
  let exn_sink_kind: nodekind;

  /** kind of Stmt_node for a throw instruction. */
  let throw_kind: nodekind;

  /** Add declarations for local variables and return variable to the node */
  let add_locals_ret_declaration: t => ProcAttributes.t => list (Mangled.t, Typ.t) => unit;

  /** Append the instructions to the list of instructions to execute */
  let append_instrs: t => list Sil.instr => unit;

  /** Dump extended instructions for the node */
  let d_instrs: sub_instrs::bool => option Sil.instr => t => unit;

  /** Create a dummy node */
  let dummy: option Procname.t => t;

  /** Check if two nodes are equal */
  let equal: t => t => bool;

  /** Get the list of callee procnames from the node */
  let get_callees: t => list Procname.t;

  /** Return a description of the node */
  let get_description: Pp.env => t => string;

  /** Get the distance to the exit node, if it has been computed */
  let get_distance_to_exit: t => option int;

  /** Get the exception nodes from the current node */
  let get_exn: t => list t;

  /** Get a list of unique nodes until the first branch starting
      from a node with subsequent applications of a generator function */
  let get_generated_slope: t => (t => list t) => list t;

  /** Get the unique id of the node */
  let get_id: t => id;

  /** Get the instructions to be executed */
  let get_instrs: t => list Sil.instr;

  /** Get the kind of the current node */
  let get_kind: t => nodekind;

  /** Get the source location of the last instruction in the node */
  let get_last_loc: t => Location.t;

  /** Get the source location of the node */
  let get_loc: t => Location.t;

  /** Get the predecessor nodes of the current node */
  let get_preds: t => list t;

  /** Get the name of the procedure the node belongs to */
  let get_proc_name: t => Procname.t;

  /** Get the predecessor nodes of a node where the given predicate evaluates to true */
  let get_sliced_preds: t => (t => bool) => list t;

  /** Get the successor nodes of a node where the given predicate evaluates to true */
  let get_sliced_succs: t => (t => bool) => list t;

  /** Get the successor nodes of the current node */
  let get_succs: t => list t;

  /** Hash function for nodes */
  let hash: t => int;

  /** Pretty print the node */
  let pp: Format.formatter => t => unit;

  /** Pretty print a node id */
  let pp_id: Format.formatter => id => unit;

  /** Print extended instructions for the node,
      highlighting the given subinstruction if present */
  let pp_instrs: Pp.env => sub_instrs::bool => option Sil.instr => Format.formatter => t => unit;

  /** Replace the instructions to be executed. */
  let replace_instrs: t => list Sil.instr => unit;
};


/** Map with node id keys. */
let module IdMap: Caml.Map.S with type key = Node.id;


/** Hash table with nodes as keys. */
let module NodeHash: Caml.Hashtbl.S with type key = Node.t;


/** Map over nodes. */
let module NodeMap: Caml.Map.S with type key = Node.t;


/** Set of nodes. */
let module NodeSet: Caml.Set.S with type elt = Node.t;


/** procedure descriptions */

/** proc description */
type t [@@deriving compare];


/** append a list of new local variables to the existing list of local variables */
let append_locals: t => list (Mangled.t, Typ.t) => unit;


/** Compute the distance of each node to the exit node, if not computed already */
let compute_distance_to_exit_node: t => unit;


/** Create a new cfg node with the given location, kind, list of instructions,
    and add it to the procdesc. */
let create_node: t => Location.t => Node.nodekind => list Sil.instr => Node.t;


/** true if we ran the preanalysis on the CFG associated with [t] */
let did_preanalysis: t => bool;


/** fold over the calls from the procedure: (callee, location) pairs */
let fold_calls: ('a => (Procname.t, Location.t) => 'a) => 'a => t => 'a;


/** fold over all nodes and their instructions */
let fold_instrs: ('a => Node.t => Sil.instr => 'a) => 'a => t => 'a;


/** fold over all nodes */
let fold_nodes: ('a => Node.t => 'a) => 'a => t => 'a;


/** Only call from Cfg. */
let from_proc_attributes: called_from_cfg::bool => ProcAttributes.t => t;


/** Return the visibility attribute */
let get_access: t => PredSymb.access;


/** Get the attributes of the procedure. */
let get_attributes: t => ProcAttributes.t;


/** Return name and type of block's captured variables */
let get_captured: t => list (Mangled.t, Typ.t);

let get_err_log: t => Errlog.t;

let get_exit_node: t => Node.t;


/** Get flags for the proc desc */
let get_flags: t => ProcAttributes.proc_flags;


/** Return name and type of formal parameters */
let get_formals: t => list (Mangled.t, Typ.t);


/** Return loc information for the procedure */
let get_loc: t => Location.t;


/** Return name and type of local variables */
let get_locals: t => list (Mangled.t, Typ.t);

let get_nodes: t => list Node.t;

let get_proc_name: t => Procname.t;


/** Return the return type of the procedure and type string */
let get_ret_type: t => Typ.t;

let get_ret_var: t => Pvar.t;


/** Get the sliced procedure's nodes up until the first branching */
let get_sliced_slope: t => (Node.t => bool) => list Node.t;


/** Get the procedure's nodes up until the first branching */
let get_slope: t => list Node.t;

let get_start_node: t => Node.t;


/** Return [true] iff the procedure is defined, and not just declared */
let is_defined: t => bool;


/** Return [true] if the body of the procdesc is empty (no instructions) */
let is_body_empty: t => bool;


/** Return [true] if the procedure signature has the Java synchronized keyword */
let is_java_synchronized: t => bool;


/** iterate over the calls from the procedure: (callee, location) pairs */
let iter_calls: ((Procname.t, Location.t) => unit) => t => unit;


/** iterate over all nodes and their instructions */
let iter_instrs: (Node.t => Sil.instr => unit) => t => unit;


/** iterate over all the nodes of a procedure */
let iter_nodes: (Node.t => unit) => t => unit;


/** iterate over all nodes until we reach a branching structure */
let iter_slope: (Node.t => unit) => t => unit;


/** iterate over all calls until we reach a branching structure */
let iter_slope_calls: (Procname.t => unit) => t => unit;


/** iterate between two nodes or until we reach a branching structure */
let iter_slope_range: (Node.t => unit) => Node.t => Node.t => unit;


/** Set the successor nodes and exception nodes, and build predecessor links */
let node_set_succs_exn: t => Node.t => list Node.t => list Node.t => unit;


/** Set the exit node of the procedure */
let set_exit_node: t => Node.t => unit;


/** Set a flag for the proc desc */
let set_flag: t => string => string => unit;

let set_start_node: t => Node.t => unit;


/** indicate that we have performed preanalysis on the CFG assoociated with [t] */
let signal_did_preanalysis: t => unit;

let is_loop_head: t => Node.t => bool;

let pp_signature: Format.formatter => t => unit;
