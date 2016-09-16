/*
 * vim: set ft=rust:
 * vim: set ft=reason:
 *
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

open! Utils;


/**  Control Flow Graph for Interprocedural Analysis */
/** {2 ADT node and proc_desc} */
type node;

type cfg;


/** Load a cfg from a file */
let load_cfg_from_file: DB.filename => option cfg;


/** Save a cfg into a file, and save a copy of the source files if the boolean is true */
let store_cfg_to_file: save_sources::bool? => DB.filename => cfg => unit;


/** proc description */
let module Procdesc: {
  /** proc description */
  type t;

  /** Compute the distance of each node to the exit node, if not computed already */
  let compute_distance_to_exit_node: t => unit;

  /** Create a procdesc */
  let create: cfg => ProcAttributes.t => t;

  /** [remove cfg name remove_nodes] remove the procdesc [name]
      from the control flow graph [cfg]. */
  /** It also removes all the nodes from the procedure from the cfg if remove_nodes is true */
  let remove: cfg => Procname.t => bool => unit;

  /** Find the procdesc given the proc name. Return None if not found. */
  let find_from_name: cfg => Procname.t => option t;

  /** Get the attributes of the procedure. */
  let get_attributes: t => ProcAttributes.t;
  let get_err_log: t => Errlog.t;
  let get_exit_node: t => node;

  /** Get flags for the proc desc */
  let get_flags: t => proc_flags;

  /** Return name and type of formal parameters */
  let get_formals: t => list (Mangled.t, Typ.t);

  /** Return loc information for the procedure */
  let get_loc: t => Location.t;

  /** Return name and type of local variables */
  let get_locals: t => list (Mangled.t, Typ.t);

  /** Return name and type of block's captured variables */
  let get_captured: t => list (Mangled.t, Typ.t);

  /** Return the visibility attribute */
  let get_access: t => PredSymb.access;
  let get_nodes: t => list node;

  /** Get the procedure's nodes up until the first branching */
  let get_slope: t => list node;

  /** Get the sliced procedure's nodes up until the first branching */
  let get_sliced_slope: t => (node => bool) => list node;
  let get_proc_name: t => Procname.t;

  /** Return the return type of the procedure and type string */
  let get_ret_type: t => Typ.t;
  let get_ret_var: t => Pvar.t;
  let get_start_node: t => node;

  /** Return [true] iff the procedure is defined, and not just declared */
  let is_defined: t => bool;

  /** Return [true] if the procedure signature has the Java synchronized keyword */
  let is_java_synchronized: t => bool;

  /** iterate over all the nodes of a procedure */
  let iter_nodes: (node => unit) => t => unit;

  /** fold over the calls from the procedure: (callee, location) pairs */
  let fold_calls: ('a => (Procname.t, Location.t) => 'a) => 'a => t => 'a;

  /** iterate over the calls from the procedure: (callee, location) pairs */
  let iter_calls: ((Procname.t, Location.t) => unit) => t => unit;

  /** iterate over all nodes and their instructions */
  let iter_instrs: (node => Sil.instr => unit) => t => unit;

  /** fold over all nodes and their instructions */
  let fold_instrs: ('a => node => Sil.instr => 'a) => 'a => t => 'a;

  /** iterate over all nodes until we reach a branching structure */
  let iter_slope: (node => unit) => t => unit;

  /** iterate over all calls until we reach a branching structure */
  let iter_slope_calls: (Procname.t => unit) => t => unit;

  /** iterate between two nodes or until we reach a branching structure */
  let iter_slope_range: (node => unit) => node => node => unit;
  let set_exit_node: t => node => unit;

  /** Set a flag for the proc desc */
  let set_flag: t => string => string => unit;
  let set_start_node: t => node => unit;

  /** append a list of new local variables to the existing list of local variables */
  let append_locals: t => list (Mangled.t, Typ.t) => unit;
};


/** node of the control flow graph */
let module Node: {
  type t = node; /** type of nodes */
  type id = private int;

  /** kind of cfg node */
  type nodekind =
    | Start_node of Procdesc.t
    | Exit_node of Procdesc.t
    | Stmt_node of string
    | Join_node
    | Prune_node of bool Sil.if_kind string /** (true/false branch, if_kind, comment) */
    | Skip_node of string;

  /** kind of Stmt_node for an exception handler. */
  let exn_handler_kind: nodekind;

  /** kind of Stmt_node for an exceptions sink. */
  let exn_sink_kind: nodekind;

  /** kind of Stmt_node for a throw instruction. */
  let throw_kind: nodekind;

  /** Append the instructions to the list of instructions to execute */
  let append_instrs: t => list Sil.instr => unit;

  /** Add the instructions at the beginning of the list of instructions to execute */
  let prepend_instrs: t => list Sil.instr => unit;

  /** Add declarations for local variables and return variable to the node */
  let add_locals_ret_declaration: t => list (Mangled.t, Typ.t) => unit;

  /** Compare two nodes */
  let compare: t => t => int;

  /** [create cfg loc kind instrs proc_desc] create a new cfg node
      with the given location, kind, list of instructions,
      procdesc */
  let create: cfg => Location.t => nodekind => list Sil.instr => Procdesc.t => t;

  /** create a new empty cfg */
  let create_cfg: unit => cfg;

  /** Dump extended instructions for the node */
  let d_instrs: sub_instrs::bool => option Sil.instr => t => unit;

  /** Create a dummy node */
  let dummy: unit => t;

  /** Check if two nodes are equal */
  let equal: t => t => bool;

  /** Get all the nodes */
  let get_all_nodes: cfg => list t;

  /** Get the (after/before) dead program variables.
      After/before indicated with the true/false flag. */
  let get_dead_pvars: t => bool => list Pvar.t;

  /** Get the distance to the exit node, if it has been computed */
  let get_distance_to_exit: t => option int;

  /** Return a description of the node */
  let get_description: printenv => t => string;

  /** Get the exception nodes from the current node */
  let get_exn: t => list t;

  /** Get the unique id of the node */
  let get_id: t => id;

  /** compare node ids */
  let id_compare: id => id => int;

  /** Get the source location of the node */
  let get_loc: t => Location.t;

  /** Get the source location of the last instruction in the node */
  let get_last_loc: t => Location.t;

  /** Get the kind of the current node */
  let get_kind: t => nodekind;

  /** Get the predecessor nodes of the current node */
  let get_preds: t => list t;

  /** Get a list of unique nodes until the first branch starting
      from a node with subsequent applications of a generator function */
  let get_generated_slope: t => (t => list t) => list t;

  /** Get the proc desc associated to the node */
  let get_proc_desc: t => Procdesc.t;

  /** Get the instructions to be executed */
  let get_instrs: t => list Sil.instr;

  /** Get the list of callee procnames from the node */
  let get_callees: t => list Procname.t;

  /** Get the successor nodes of the current node */
  let get_succs: t => list t;

  /** Get the successor nodes of a node where the given predicate evaluates to true */
  let get_sliced_succs: t => (t => bool) => list t;

  /** Get the predecessor nodes of a node where the given predicate evaluates to true */
  let get_sliced_preds: t => (t => bool) => list t;

  /** Hash function for nodes */
  let hash: t => int;

  /** Comparison for node kind */
  let kind_compare: nodekind => nodekind => int;

  /** Pretty print the node */
  let pp: Format.formatter => t => unit;
  let pp_id: Format.formatter => id => unit;

  /** Print extended instructions for the node,
      highlighting the given subinstruction if present */
  let pp_instrs: printenv => sub_instrs::bool => option Sil.instr => Format.formatter => t => unit;

  /** Replace the instructions to be executed. */
  let replace_instrs: t => list Sil.instr => unit;

  /** Set the (after/before) dead program variables.
      After/before indicated with the true/false flag. */
  let set_dead_pvars: t => bool => list Pvar.t => unit;

  /** Set the node kind */
  let set_kind: t => nodekind => unit;

  /** Set the source location of the node */
  let set_loc: t => Location.t => unit;

  /** Set the successor nodes and exception nodes, and build predecessor links */
  let set_succs_exn: cfg => t => list t => list t => unit;
};


/** Hash table with nodes as keys. */
let module NodeHash: Hashtbl.S with type key = Node.t;


/** Set of nodes. */
let module NodeSet: Set.S with type elt = Node.t;


/** Map with node id keys. */
let module IdMap: Map.S with type key = Node.id;

let pp_node_list: Format.formatter => list Node.t => unit;


/** {2 Functions for manipulating an interprocedural CFG} */
/** Iterate over all the procdesc's */
let iter_proc_desc: cfg => (Procname.t => Procdesc.t => unit) => unit;


/** Get all the procedures (defined and declared) */
let get_all_procs: cfg => list Procdesc.t;


/** Get the procedures whose body is defined in this cfg */
let get_defined_procs: cfg => list Procdesc.t;


/** get the function names which should be analyzed before the other ones */
let get_priority_procnames: cfg => Procname.Set.t;


/** set the function names whose address has been taken in this file */
let set_procname_priority: cfg => Procname.t => unit;


/** remove the return variable from the prop */
let remove_ret: Tenv.t => Procdesc.t => Prop.t Prop.normal => Prop.t Prop.normal;


/** remove locals and return variable from the prop */
let remove_locals_ret: Tenv.t => Procdesc.t => Prop.t Prop.normal => Prop.t Prop.normal;


/** Deallocate the stack variables in [pvars], and replace them by normal variables.
    Return the list of stack variables whose address was still present after deallocation. */
let remove_locals_formals:
  Tenv.t => Procdesc.t => Prop.t Prop.normal => (list Pvar.t, Prop.t Prop.normal);


/** remove seed vars from a prop */
let remove_seed_vars: Tenv.t => Prop.t 'a => Prop.t Prop.normal;


/** checks whether a cfg is connected or not */
let check_cfg_connectedness: cfg => unit;


/** Removes seeds variables from a prop corresponding to captured variables in an objc block */
let remove_seed_captured_vars_block:
  Tenv.t => list Mangled.t => Prop.t Prop.normal => Prop.t Prop.normal;


/** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting procdesc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types */
let specialize_types: Procdesc.t => Procname.t => list (Exp.t, Typ.t) => Procdesc.t;
