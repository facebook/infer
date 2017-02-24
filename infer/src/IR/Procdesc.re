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

let module Hashtbl = Caml.Hashtbl;

let module L = Logging;

let module F = Format;

/* =============== START of module Node =============== */
let module Node = {
  type id = int [@@deriving compare];
  let equal_id = [%compare.equal : id];
  type nodekind =
    | Start_node Procname.t
    | Exit_node Procname.t
    | Stmt_node string
    | Join_node
    | Prune_node bool Sil.if_kind string /** (true/false branch, if_kind, comment) */
    | Skip_node string
  [@@deriving compare];
  let equal_nodekind = [%compare.equal : nodekind];

  /** a node */
  type t = {
    /** unique id of the node */
    id: id,
    /** distance to the exit node */
    mutable dist_exit: option int,
    /** exception nodes in the cfg */
    mutable exn: list t,
    /** instructions for symbolic execution */
    mutable instrs: list Sil.instr,
    /** kind of node */
    kind: nodekind,
    /** location in the source code */
    loc: Location.t,
    /** predecessor nodes in the cfg */
    mutable preds: list t,
    /** name of the procedure the node belongs to */
    pname_opt: option Procname.t,
    /** successor nodes in the cfg */
    mutable succs: list t
  };
  let exn_handler_kind = Stmt_node "exception handler";
  let exn_sink_kind = Stmt_node "exceptions sink";
  let throw_kind = Stmt_node "throw";
  let dummy pname_opt => {
    id: 0,
    dist_exit: None,
    instrs: [],
    kind: Skip_node "dummy",
    loc: Location.dummy,
    pname_opt,
    succs: [],
    preds: [],
    exn: []
  };
  let compare node1 node2 => Int.compare node1.id node2.id;
  let hash node => Hashtbl.hash node.id;
  let equal = [%compare.equal : t];

  /** Get the unique id of the node */
  let get_id node => node.id;
  let get_succs node => node.succs;
  type node = t;
  let module NodeSet = Caml.Set.Make {
    type t = node;
    let compare = compare;
  };
  let module IdMap = Caml.Map.Make {
    type t = id;
    let compare = compare_id;
  };
  let get_sliced_succs node f => {
    let visited = ref NodeSet.empty;
    let rec slice_nodes nodes :NodeSet.t => {
      let do_node acc n => {
        visited := NodeSet.add n !visited;
        if (f n) {
          NodeSet.singleton n
        } else {
          NodeSet.union
            acc (slice_nodes (List.filter f::(fun s => not (NodeSet.mem s !visited)) n.succs))
        }
      };
      List.fold f::do_node init::NodeSet.empty nodes
    };
    NodeSet.elements (slice_nodes node.succs)
  };
  let get_sliced_preds node f => {
    let visited = ref NodeSet.empty;
    let rec slice_nodes nodes :NodeSet.t => {
      let do_node acc n => {
        visited := NodeSet.add n !visited;
        if (f n) {
          NodeSet.singleton n
        } else {
          NodeSet.union
            acc (slice_nodes (List.filter f::(fun s => not (NodeSet.mem s !visited)) n.preds))
        }
      };
      List.fold f::do_node init::NodeSet.empty nodes
    };
    NodeSet.elements (slice_nodes node.preds)
  };
  let get_exn node => node.exn;

  /** Get the name of the procedure the node belongs to */
  let get_proc_name node =>
    switch node.pname_opt {
    | None =>
      L.out "get_proc_name: at node %d@\n" node.id;
      assert false
    | Some pname => pname
    };

  /** Get the predecessors of the node */
  let get_preds node => node.preds;

  /** Generates a list of nodes starting at a given node
      and recursively adding the results of the generator */
  let get_generated_slope start_node generator => {
    let visited = ref NodeSet.empty;
    let rec nodes n => {
      visited := NodeSet.add n !visited;
      let succs = List.filter f::(fun n => not (NodeSet.mem n !visited)) (generator n);
      switch succs {
      | [hd] => [n, ...nodes hd]
      | _ => [n]
      }
    };
    nodes start_node
  };

  /** Get the node kind */
  let get_kind node => node.kind;

  /** Get the instructions to be executed */
  let get_instrs node => node.instrs;

  /** Get the list of callee procnames from the node */
  let get_callees node => {
    let collect callees instr =>
      switch instr {
      | Sil.Call _ exp _ _ _ =>
        switch exp {
        | Exp.Const (Const.Cfun procname) => [procname, ...callees]
        | _ => callees
        }
      | _ => callees
      };
    List.fold f::collect init::[] (get_instrs node)
  };

  /** Get the location of the node */
  let get_loc n => n.loc;

  /** Get the source location of the last instruction in the node */
  let get_last_loc n =>
    switch (List.rev (get_instrs n)) {
    | [instr, ..._] => Sil.instr_get_loc instr
    | [] => n.loc
    };
  let pp_id f id => F.fprintf f "%d" id;
  let pp f node => pp_id f (get_id node);
  let get_distance_to_exit node => node.dist_exit;

  /** Append the instructions to the list of instructions to execute */
  let append_instrs node instrs => node.instrs = node.instrs @ instrs;

  /** Add the instructions at the beginning of the list of instructions to execute */
  let prepend_instrs node instrs => node.instrs = instrs @ node.instrs;

  /** Replace the instructions to be executed. */
  let replace_instrs node instrs => node.instrs = instrs;

  /** Add declarations for local variables and return variable to the node */
  let add_locals_ret_declaration node (proc_attributes: ProcAttributes.t) locals => {
    let loc = get_loc node;
    let pname = proc_attributes.proc_name;
    let ret_var = {
      let ret_type = proc_attributes.ret_type;
      (Pvar.get_ret_pvar pname, ret_type)
    };
    let construct_decl (x, typ) => (Pvar.mk x pname, typ);
    let ptl = [ret_var, ...List.map f::construct_decl locals];
    let instr = Sil.Declare_locals ptl loc;
    prepend_instrs node [instr]
  };

  /** Print extended instructions for the node,
      highlighting the given subinstruction if present */
  let pp_instrs pe0 sub_instrs::sub_instrs instro fmt node => {
    let pe =
      switch instro {
      | None => pe0
      | Some instr => Pp.extend_colormap pe0 (Obj.repr instr) Red
      };
    let instrs = get_instrs node;
    let pp_loc fmt () => F.fprintf fmt " %a " Location.pp (get_loc node);
    let print_sub_instrs () => F.fprintf fmt "%a" (Sil.pp_instr_list pe) instrs;
    switch (get_kind node) {
    | Stmt_node s =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "statements (%s) %a" s pp_loc ()
      }
    | Prune_node _ _ descr =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "assume %s %a" descr pp_loc ()
      }
    | Exit_node _ =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "exit %a" pp_loc ()
      }
    | Skip_node s =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "skip (%s) %a" s pp_loc ()
      }
    | Start_node _ =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "start %a" pp_loc ()
      }
    | Join_node =>
      if sub_instrs {
        print_sub_instrs ()
      } else {
        F.fprintf fmt "join %a" pp_loc ()
      }
    }
  };

  /** Dump extended instructions for the node */
  let d_instrs sub_instrs::(sub_instrs: bool) (curr_instr: option Sil.instr) (node: t) => L.add_print_action (
    L.PTnode_instrs,
    Obj.repr (sub_instrs, curr_instr, node)
  );

  /** Return a description of the cfg node */
  let get_description pe node => {
    let str =
      switch (get_kind node) {
      | Stmt_node _ => "Instructions"
      | Prune_node _ _ descr => "Conditional" ^ " " ^ descr
      | Exit_node _ => "Exit"
      | Skip_node _ => "Skip"
      | Start_node _ => "Start"
      | Join_node => "Join"
      };
    let pp fmt => F.fprintf fmt "%s\n%a@?" str (pp_instrs pe None sub_instrs::true) node;
    F.asprintf "%t" pp
  };
};

/* =============== END of module Node =============== */

/** Map over nodes */
let module NodeMap = Caml.Map.Make Node;


/** Hash table with nodes as keys. */
let module NodeHash = Hashtbl.Make Node;


/** Set of nodes. */
let module NodeSet = Node.NodeSet;


/** Map with node id keys. */
let module IdMap = Node.IdMap;


/** procedure description */
type t = {
  attributes: ProcAttributes.t, /** attributes of the procedure */
  mutable nodes: list Node.t, /** list of nodes of this procedure */
  mutable nodes_num: int, /** number of nodes */
  mutable start_node: Node.t, /** start node of this procedure */
  mutable exit_node: Node.t, /** exit node of ths procedure */
  mutable loop_heads: option NodeSet.t /** loop head nodes of this procedure */
}
[@@deriving compare];


/** Only call from Cfg */
let from_proc_attributes called_from_cfg::called_from_cfg attributes => {
  if (not called_from_cfg) {
    assert false
  };
  let pname_opt = Some attributes.ProcAttributes.proc_name;
  let start_node = Node.dummy pname_opt;
  let exit_node = Node.dummy pname_opt;
  {attributes, nodes: [], nodes_num: 0, start_node, exit_node, loop_heads: None}
};


/** Compute the distance of each node to the exit node, if not computed already */
let compute_distance_to_exit_node pdesc => {
  let exit_node = pdesc.exit_node;
  let rec mark_distance dist nodes => {
    let next_nodes = ref [];
    let do_node (node: Node.t) =>
      switch node.dist_exit {
      | Some _ => ()
      | None =>
        node.dist_exit = Some dist;
        next_nodes := node.preds @ !next_nodes
      };
    List.iter f::do_node nodes;
    if (!next_nodes != []) {
      mark_distance (dist + 1) !next_nodes
    }
  };
  mark_distance 0 [exit_node]
};


/** check or indicate if we have performed preanalysis on the CFG */
let did_preanalysis pdesc => pdesc.attributes.did_preanalysis;

let signal_did_preanalysis pdesc => pdesc.attributes.did_preanalysis = true;

let get_attributes pdesc => pdesc.attributes;

let get_err_log pdesc => pdesc.attributes.err_log;

let get_exit_node pdesc => pdesc.exit_node;


/** Get flags for the proc desc */
let get_flags pdesc => pdesc.attributes.proc_flags;


/** Return name and type of formal parameters */
let get_formals pdesc => pdesc.attributes.formals;

let get_loc pdesc => pdesc.attributes.loc;


/** Return name and type of local variables */
let get_locals pdesc => pdesc.attributes.locals;


/** Return name and type of captured variables */
let get_captured pdesc => pdesc.attributes.captured;


/** Return the visibility attribute */
let get_access pdesc => pdesc.attributes.access;

let get_nodes pdesc => pdesc.nodes;

let get_proc_name pdesc => pdesc.attributes.proc_name;


/** Return the return type of the procedure */
let get_ret_type pdesc => pdesc.attributes.ret_type;

let get_ret_var pdesc => Pvar.mk Ident.name_return (get_proc_name pdesc);

let get_start_node pdesc => pdesc.start_node;


/** List of nodes in the procedure sliced by a predicate up to the first branching */
let get_sliced_slope pdesc f =>
  Node.get_generated_slope (get_start_node pdesc) (fun n => Node.get_sliced_succs n f);


/** List of nodes in the procedure up to the first branching */
let get_slope pdesc => Node.get_generated_slope (get_start_node pdesc) Node.get_succs;


/** Return [true] iff the procedure is defined, and not just declared */
let is_defined pdesc => pdesc.attributes.is_defined;

let is_body_empty pdesc => List.is_empty (Node.get_succs (get_start_node pdesc));

let is_java_synchronized pdesc => pdesc.attributes.is_java_synchronized_method;

let iter_nodes f pdesc => List.iter f::f (List.rev (get_nodes pdesc));

let fold_calls f acc pdesc => {
  let do_node a node =>
    List.fold
      f::(fun b callee_pname => f b (callee_pname, Node.get_loc node))
      init::a
      (Node.get_callees node);
  List.fold f::do_node init::acc (get_nodes pdesc)
};


/** iterate over the calls from the procedure: (callee,location) pairs */
let iter_calls f pdesc => fold_calls (fun _ call => f call) () pdesc;

let iter_instrs f pdesc => {
  let do_node node => List.iter f::(fun i => f node i) (Node.get_instrs node);
  iter_nodes do_node pdesc
};

let fold_nodes f acc pdesc => List.fold f::f init::acc (List.rev (get_nodes pdesc));

let fold_instrs f acc pdesc => {
  let fold_node acc node =>
    List.fold f::(fun acc instr => f acc node instr) init::acc (Node.get_instrs node);
  fold_nodes fold_node acc pdesc
};

let iter_slope f pdesc => {
  let visited = ref NodeSet.empty;
  let rec do_node node => {
    visited := NodeSet.add node !visited;
    f node;
    switch (Node.get_succs node) {
    | [n] =>
      if (not (NodeSet.mem n !visited)) {
        do_node n
      }
    | _ => ()
    }
  };
  do_node (get_start_node pdesc)
};

let iter_slope_calls f pdesc => {
  let do_node node => List.iter f::(fun callee_pname => f callee_pname) (Node.get_callees node);
  iter_slope do_node pdesc
};


/** iterate between two nodes or until we reach a branching structure */
let iter_slope_range f src_node dst_node => {
  let visited = ref NodeSet.empty;
  let rec do_node node => {
    visited := NodeSet.add node !visited;
    f node;
    switch (Node.get_succs node) {
    | [n] =>
      if (not (NodeSet.mem n !visited) && not (Node.equal node dst_node)) {
        do_node n
      }
    | _ => ()
    }
  };
  do_node src_node
};


/** Set the exit node of the proc desc */
let set_exit_node pdesc node => pdesc.exit_node = node;


/** Set a flag for the proc desc */
let set_flag pdesc key value =>
  ProcAttributes.proc_flags_add pdesc.attributes.proc_flags key value;


/** Set the start node of the proc desc */
let set_start_node pdesc node => pdesc.start_node = node;


/** Append the locals to the list of local variables */
let append_locals pdesc new_locals =>
  pdesc.attributes.locals = pdesc.attributes.locals @ new_locals;


/** Set the successor nodes and exception nodes, and build predecessor links */
let set_succs_exn_base (node: Node.t) succs exn => {
  node.succs = succs;
  node.exn = exn;
  List.iter f::(fun (n: Node.t) => n.preds = [node, ...n.preds]) succs
};


/** Create a new cfg node */
let create_node pdesc loc kind instrs => {
  pdesc.nodes_num = pdesc.nodes_num + 1;
  let node_id = pdesc.nodes_num;
  let node = {
    Node.id: node_id,
    dist_exit: None,
    instrs,
    kind,
    loc,
    preds: [],
    pname_opt: Some pdesc.attributes.proc_name,
    succs: [],
    exn: []
  };
  pdesc.nodes = [node, ...pdesc.nodes];
  node
};


/** Set the successor and exception nodes.
    If this is a join node right before the exit node, add an extra node in the middle,
    otherwise nullify and abstract instructions cannot be added after a conditional. */
let node_set_succs_exn pdesc (node: Node.t) succs exn =>
  switch (node.kind, succs) {
  | (Join_node, [{Node.kind: Exit_node _} as exit_node]) =>
    let kind = Node.Stmt_node "between_join_and_exit";
    let node' = create_node pdesc node.loc kind node.instrs;
    set_succs_exn_base node [node'] exn;
    set_succs_exn_base node' [exit_node] exn
  | _ => set_succs_exn_base node succs exn
  };


/** Get loop heads for widening.
    It collects all target nodes of back-edges in a depth-first
    traversal.
    */
let get_loop_heads pdesc => {
  let rec set_loop_head_rec visited heads wl =>
    switch wl {
    | [] => heads
    | [(n, ancester), ...wl'] =>
      if (NodeSet.mem n visited) {
        if (NodeSet.mem n ancester) {
          set_loop_head_rec visited (NodeSet.add n heads) wl'
        } else {
          set_loop_head_rec visited heads wl'
        }
      } else {
        let ancester = NodeSet.add n ancester;
        let succs = List.append (Node.get_succs n) (Node.get_exn n);
        let works = List.map f::(fun m => (m, ancester)) succs;
        set_loop_head_rec (NodeSet.add n visited) heads (List.append works wl')
      }
    };
  let start_wl = [(get_start_node pdesc, NodeSet.empty)];
  let lh = set_loop_head_rec NodeSet.empty NodeSet.empty start_wl;
  pdesc.loop_heads = Some lh;
  lh
};

let is_loop_head pdesc (node: Node.t) => {
  let lh =
    switch pdesc.loop_heads {
    | Some lh => lh
    | None => get_loop_heads pdesc
    };
  NodeSet.mem node lh
};

let pp_variable_list fmt etl =>
  if (List.is_empty etl) {
    Format.fprintf fmt "None"
  } else {
    List.iter
      f::(fun (id, ty) => Format.fprintf fmt " %a:%a" Mangled.pp id (Typ.pp_full Pp.text) ty) etl
  };

let pp_signature fmt pdesc => {
  let pname = get_proc_name pdesc;
  let pname_string = Procname.to_string pname;
  let defined_string = is_defined pdesc ? "defined" : "undefined";
  Format.fprintf
    fmt
    "%s [%s, Return type: %s, Formals: %a, Locals: %a"
    pname_string
    defined_string
    (Typ.to_string (get_ret_type pdesc))
    pp_variable_list
    (get_formals pdesc)
    pp_variable_list
    (get_locals pdesc);
  if (not (List.is_empty (get_captured pdesc))) {
    Format.fprintf fmt ", Captured: %a" pp_variable_list (get_captured pdesc)
  };
  let attributes = get_attributes pdesc;
  let method_annotation = attributes.ProcAttributes.method_annotation;
  if (not (Annot.Method.is_empty method_annotation)) {
    Format.fprintf fmt ", Annotation: %a" (Annot.Method.pp pname_string) method_annotation
  };
  Format.fprintf fmt "]@\n"
};
