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

let module L = Logging;

let module F = Format;

/* =============== START of module Node =============== */
let module Node = {
  type id = int;
  type nodekind =
    | Start_node Procname.t
    | Exit_node Procname.t
    | Stmt_node string
    | Join_node
    | Prune_node bool Sil.if_kind string /** (true/false branch, if_kind, comment) */
    | Skip_node string;

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
    pname: option Procname.t,
    /** successor nodes in the cfg */
    mutable succs: list t
  };
  let exn_handler_kind = Stmt_node "exception handler";
  let exn_sink_kind = Stmt_node "exceptions sink";
  let throw_kind = Stmt_node "throw";
  let dummy () => {
    id: 0,
    dist_exit: None,
    instrs: [],
    kind: Skip_node "dummy",
    loc: Location.dummy,
    pname: None,
    succs: [],
    preds: [],
    exn: []
  };
  let compare node1 node2 => int_compare node1.id node2.id;
  let hash node => Hashtbl.hash node.id;
  let equal node1 node2 => compare node1 node2 == 0;

  /** Get the unique id of the node */
  let get_id node => node.id;

  /** compare node ids */
  let id_compare = int_compare;
  let get_succs node => node.succs;
  type node = t;
  let module NodeSet = Set.Make {
    type t = node;
    let compare = compare;
  };
  let module IdMap = Map.Make {
    type t = id;
    let compare = id_compare;
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
            acc (slice_nodes (IList.filter (fun s => not (NodeSet.mem s !visited)) n.succs))
        }
      };
      IList.fold_left do_node NodeSet.empty nodes
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
            acc (slice_nodes (IList.filter (fun s => not (NodeSet.mem s !visited)) n.preds))
        }
      };
      IList.fold_left do_node NodeSet.empty nodes
    };
    NodeSet.elements (slice_nodes node.preds)
  };
  let get_exn node => node.exn;

  /** Get the name of the procedure the node belongs to */
  let get_proc_name node =>
    switch node.pname {
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
      let succs = IList.filter (fun n => not (NodeSet.mem n !visited)) (generator n);
      switch (IList.length succs) {
      | 1 => [n, ...nodes (IList.hd succs)]
      | _ => [n]
      }
    };
    nodes start_node
  };

  /** Get the node kind */
  let get_kind node => node.kind;

  /** Comparison for node kind */
  let kind_compare k1 k2 =>
    switch (k1, k2) {
    | (Start_node pn1, Start_node pn2) => Procname.compare pn1 pn2
    | (Start_node _, _) => (-1)
    | (_, Start_node _) => 1
    | (Exit_node pn1, Exit_node pn2) => Procname.compare pn1 pn2
    | (Exit_node _, _) => (-1)
    | (_, Exit_node _) => 1
    | (Stmt_node s1, Stmt_node s2) => string_compare s1 s2
    | (Stmt_node _, _) => (-1)
    | (_, Stmt_node _) => 1
    | (Join_node, Join_node) => 0
    | (Join_node, _) => (-1)
    | (_, Join_node) => 1
    | (Prune_node is_true_branch1 if_kind1 descr1, Prune_node is_true_branch2 if_kind2 descr2) =>
      let n = bool_compare is_true_branch1 is_true_branch2;
      if (n != 0) {
        n
      } else {
        let n = Pervasives.compare if_kind1 if_kind2;
        if (n != 0) {
          n
        } else {
          string_compare descr1 descr2
        }
      }
    | (Prune_node _, _) => (-1)
    | (_, Prune_node _) => 1
    | (Skip_node s1, Skip_node s2) => string_compare s1 s2
    };

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
    IList.fold_left collect [] (get_instrs node)
  };

  /** Get the location of the node */
  let get_loc n => n.loc;

  /** Get the source location of the last instruction in the node */
  let get_last_loc n =>
    switch (IList.rev (get_instrs n)) {
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
    let ptl = [ret_var, ...IList.map construct_decl locals];
    let instr = Sil.Declare_locals ptl loc;
    prepend_instrs node [instr]
  };

  /** Print extended instructions for the node,
      highlighting the given subinstruction if present */
  let pp_instrs pe0 sub_instrs::sub_instrs instro fmt node => {
    let pe =
      switch instro {
      | None => pe0
      | Some instr => pe_extend_colormap pe0 (Obj.repr instr) Red
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
    let pp fmt () => F.fprintf fmt "%s\n%a@?" str (pp_instrs pe None sub_instrs::true) node;
    pp_to_string pp ()
  };
};

/* =============== END of module Node =============== */

/** Map over nodes */
let module NodeMap = Map.Make Node;


/** Hash table with nodes as keys. */
let module NodeHash = Hashtbl.Make Node;


/** Set of nodes. */
let module NodeSet = Node.NodeSet;


/** Map with node id keys. */
let module IdMap = Node.IdMap;

/* =============== START of module Procdesc =============== */
let module Procdesc = {

  /** procedure description */
  type t = {
    attributes: ProcAttributes.t, /** attributes of the procedure */
    id: int, /** unique proc_desc identifier */
    mutable nodes: list Node.t, /** list of nodes of this procedure */
    mutable nodes_num: int, /** number of nodes */
    mutable start_node: Node.t, /** start node of this procedure */
    mutable exit_node: Node.t /** exit node of ths procedure */
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
      IList.iter do_node nodes;
      if (!next_nodes !== []) {
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
  let is_java_synchronized pdesc => pdesc.attributes.is_java_synchronized_method;
  let iter_nodes f pdesc => IList.iter f (IList.rev (get_nodes pdesc));
  let fold_calls f acc pdesc => {
    let do_node a node =>
      IList.fold_left
        (fun b callee_pname => f b (callee_pname, Node.get_loc node)) a (Node.get_callees node);
    IList.fold_left do_node acc (get_nodes pdesc)
  };

  /** iterate over the calls from the procedure: (callee,location) pairs */
  let iter_calls f pdesc => fold_calls (fun _ call => f call) () pdesc;
  let iter_instrs f pdesc => {
    let do_node node => IList.iter (fun i => f node i) (Node.get_instrs node);
    iter_nodes do_node pdesc
  };
  let fold_nodes f acc pdesc => IList.fold_left f acc (IList.rev (get_nodes pdesc));
  let fold_instrs f acc pdesc => {
    let fold_node acc node =>
      IList.fold_left (fun acc instr => f acc node instr) acc (Node.get_instrs node);
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
    let do_node node => IList.iter (fun callee_pname => f callee_pname) (Node.get_callees node);
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
  let set_flag pdesc key value => proc_flags_add pdesc.attributes.proc_flags key value;

  /** Set the start node of the proc desc */
  let set_start_node pdesc node => pdesc.start_node = node;

  /** Append the locals to the list of local variables */
  let append_locals pdesc new_locals =>
    pdesc.attributes.locals = pdesc.attributes.locals @ new_locals;

  /** Set the successor nodes and exception nodes, and build predecessor links */
  let set_succs_exn_base (node: Node.t) succs exn => {
    node.succs = succs;
    node.exn = exn;
    IList.iter (fun (n: Node.t) => n.preds = [node, ...n.preds]) succs
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
      pname: Some pdesc.attributes.proc_name,
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
};

/* =============== END of module Procdesc =============== */

/** data type for the control flow graph */
type cfg = {
  mutable proc_desc_id_counter: int /** Counter for identifiers of procdescs */,
  proc_desc_table: Procname.Hash.t Procdesc.t /** Map proc name to procdesc */
};


/** create a new empty cfg */
let create_cfg () => {proc_desc_id_counter: 0, proc_desc_table: Procname.Hash.create 16};

let add_proc_desc cfg pname pdesc => Procname.Hash.add cfg.proc_desc_table pname pdesc;

let remove_proc_desc cfg pname => Procname.Hash.remove cfg.proc_desc_table pname;

let iter_proc_desc cfg f => Procname.Hash.iter f cfg.proc_desc_table;

let find_proc_desc_from_name cfg pname =>
  try (Some (Procname.Hash.find cfg.proc_desc_table pname)) {
  | Not_found => None
  };


/** Create a new procdesc */
let create_proc_desc cfg (proc_attributes: ProcAttributes.t) => {
  cfg.proc_desc_id_counter = cfg.proc_desc_id_counter + 1;
  let pdesc = {
    Procdesc.attributes: proc_attributes,
    id: cfg.proc_desc_id_counter,
    nodes: [],
    nodes_num: 0,
    start_node: Node.dummy (),
    exit_node: Node.dummy ()
  };
  add_proc_desc cfg proc_attributes.proc_name pdesc;
  pdesc
};


/** Iterate over all the nodes in the cfg */
let iter_all_nodes f cfg => {
  let do_proc_desc _ (pdesc: Procdesc.t) => IList.iter (fun node => f pdesc node) pdesc.nodes;
  iter_proc_desc cfg do_proc_desc
};


/** Get all the procdescs (defined and declared) */
let get_all_procs cfg => {
  let procs = ref [];
  let f _ pdesc => procs := [pdesc, ...!procs];
  iter_proc_desc cfg f;
  !procs
};


/** Get the procedures whose body is defined in this cfg */
let get_defined_procs cfg => IList.filter Procdesc.is_defined (get_all_procs cfg);


/** checks whether a cfg is connected or not */
let check_cfg_connectedness cfg => {
  let is_exit_node n =>
    switch (Node.get_kind n) {
    | Node.Exit_node _ => true
    | _ => false
    };
  let broken_node n => {
    let succs = Node.get_succs n;
    let preds = Node.get_preds n;
    switch (Node.get_kind n) {
    | Node.Start_node _ => IList.length succs == 0 || IList.length preds > 0
    | Node.Exit_node _ => IList.length succs > 0 || IList.length preds == 0
    | Node.Stmt_node _
    | Node.Prune_node _
    | Node.Skip_node _ => IList.length succs == 0 || IList.length preds == 0
    | Node.Join_node =>
      /* Join node has the exception that it may be without predecessors
         and pointing to an exit node */
      /* if the if brances end with a return */
      switch succs {
      | [n'] when is_exit_node n' => false
      | _ => IList.length preds == 0
      }
    }
  };
  let do_pdesc pd => {
    let pname = Procname.to_string (Procdesc.get_proc_name pd);
    let nodes = Procdesc.get_nodes pd;
    let broken = IList.exists broken_node nodes;
    if broken {
      L.out "\n ***BROKEN CFG: '%s'\n" pname
    } else {
      L.out "\n ***CONNECTED CFG: '%s'\n" pname
    }
  };
  let pdescs = get_all_procs cfg;
  IList.iter do_pdesc pdescs
};


/** Serializer for control flow graphs */
let cfg_serializer: Serialization.serializer cfg = Serialization.create_serializer Serialization.cfg_key;


/** Load a cfg from a file */
let load_cfg_from_file (filename: DB.filename) :option cfg =>
  Serialization.from_file cfg_serializer filename;


/** save a copy in the results dir of the source files of procedures defined in the cfg,
    unless an updated copy already exists */
let save_source_files cfg => {
  let process_proc _ pdesc => {
    let loc = Procdesc.get_loc pdesc;
    let source_file = loc.Location.file;
    let source_file_str = DB.source_file_to_abs_path source_file;
    let dest_file = DB.source_file_in_resdir source_file;
    let dest_file_str = DB.filename_to_string dest_file;
    let needs_copy =
      Procdesc.is_defined pdesc &&
      Sys.file_exists source_file_str && (
        not (Sys.file_exists dest_file_str) ||
        DB.file_modified_time (DB.filename_from_string source_file_str) >
        DB.file_modified_time dest_file
      );
    if needs_copy {
      switch (copy_file source_file_str dest_file_str) {
      | Some _ => ()
      | None => L.err "Error cannot create copy of source file %s@." source_file_str
      }
    }
  };
  iter_proc_desc cfg process_proc
};


/** Save the .attr files for the procedures in the cfg. */
let save_attributes source_file cfg => {
  let save_proc pdesc => {
    let attributes = Procdesc.get_attributes pdesc;
    let loc = attributes.loc;
    let attributes' = {
      let loc' =
        if (Location.equal loc Location.dummy) {
          {...loc, file: source_file}
        } else {
          loc
        };
      {...attributes, loc: loc', source_file_captured: source_file}
    };
    AttributesTable.store_attributes attributes'
  };
  IList.iter save_proc (get_all_procs cfg)
};


/** Inline a synthetic (access or bridge) method. */
let inline_synthetic_method ret_id etl pdesc loc_call :option Sil.instr => {
  let modified = ref None;
  let debug = false;
  let found instr instr' => {
    modified := Some instr';
    if debug {
      L.stderr "XX inline_synthetic_method found instr: %a@." (Sil.pp_instr pe_text) instr;
      L.stderr "XX inline_synthetic_method instr': %a@." (Sil.pp_instr pe_text) instr'
    }
  };
  let do_instr _ instr =>
    switch (instr, ret_id, etl) {
    | (
        Sil.Load _ (Exp.Lfield (Exp.Var _) fn ft) bt _,
        Some (ret_id, _),
        [(e1, _)] /* getter for fields */
      ) =>
      let instr' = Sil.Load ret_id (Exp.Lfield e1 fn ft) bt loc_call;
      found instr instr'
    | (Sil.Load _ (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _, Some (ret_id, _), [])
        when Pvar.is_global pvar =>
      /* getter for static fields */
      let instr' = Sil.Load ret_id (Exp.Lfield (Exp.Lvar pvar) fn ft) bt loc_call;
      found instr instr'
    | (Sil.Store (Exp.Lfield _ fn ft) bt _ _, _, [(e1, _), (e2, _)] /* setter for fields */) =>
      let instr' = Sil.Store (Exp.Lfield e1 fn ft) bt e2 loc_call;
      found instr instr'
    | (Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _ _, _, [(e1, _)]) when Pvar.is_global pvar =>
      /* setter for static fields */
      let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt e1 loc_call;
      found instr instr'
    | (Sil.Call ret_id' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when ret_id == None == (ret_id' == None) && IList.length etl' == IList.length etl =>
      let instr' = Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl loc_call cf;
      found instr instr'
    | (Sil.Call ret_id' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when ret_id == None == (ret_id' == None) && IList.length etl' + 1 == IList.length etl =>
      let etl1 =
        switch (IList.rev etl) {
        /* remove last element */
        | [_, ...l] => IList.rev l
        | [] => assert false
        };
      let instr' = Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl1 loc_call cf;
      found instr instr'
    | _ => ()
    };
  Procdesc.iter_instrs do_instr pdesc;
  !modified
};


/** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. */
let proc_inline_synthetic_methods cfg pdesc :unit => {
  let instr_inline_synthetic_method =
    fun
    | Sil.Call ret_id (Exp.Const (Const.Cfun pn)) etl loc _ =>
      switch (find_proc_desc_from_name cfg pn) {
      | Some pd =>
        let is_access = Procname.java_is_access_method pn;
        let attributes = Procdesc.get_attributes pd;
        let is_synthetic = attributes.is_synthetic_method;
        let is_bridge = attributes.is_bridge_method;
        if (is_access || is_bridge || is_synthetic) {
          inline_synthetic_method ret_id etl pd loc
        } else {
          None
        }
      | None => None
      }
    | _ => None;
  let node_inline_synthetic_methods node => {
    let modified = ref false;
    let do_instr instr =>
      switch (instr_inline_synthetic_method instr) {
      | None => instr
      | Some instr' =>
        modified := true;
        instr'
      };
    let instrs = Node.get_instrs node;
    let instrs' = IList.map do_instr instrs;
    if !modified {
      Node.replace_instrs node instrs'
    }
  };
  Procdesc.iter_nodes node_inline_synthetic_methods pdesc
};


/** Inline the java synthetic methods in the cfg */
let inline_java_synthetic_methods cfg => {
  let f pname pdesc =>
    if (Procname.is_java pname) {
      proc_inline_synthetic_methods cfg pdesc
    };
  iter_proc_desc cfg f
};


/** compute the list of procedures added or changed in [cfg_new] over [cfg_old] */
let mark_unchanged_pdescs cfg_new cfg_old => {
  let pdescs_eq (pd1: Procdesc.t) (pd2: Procdesc.t) => {
    /* map of exp names in pd1 -> exp names in pd2 */
    let exp_map = ref Exp.Map.empty;
    /* map of node id's in pd1 -> node id's in pd2 */
    let id_map = ref IntMap.empty;
    /* formals are the same if their types are the same */
    let formals_eq formals1 formals2 =>
      IList.equal (fun (_, typ1) (_, typ2) => Typ.compare typ1 typ2) formals1 formals2;
    let nodes_eq n1s n2s => {
      /* nodes are the same if they have the same id, instructions, and succs/preds up to renaming
         with [exp_map] and [id_map] */
      let node_eq (n1: Node.t) (n2: Node.t) => {
        let id_compare (n1: Node.t) (n2: Node.t) => {
          let (id1, id2) = (n1.id, n2.id);
          try {
            let id1_mapping = IntMap.find id1 !id_map;
            Pervasives.compare id1_mapping id2
          } {
          | Not_found =>
            /* assume id's are equal and enforce by adding to [id_map] */
            id_map := IntMap.add id1 id2 !id_map;
            0
          }
        };
        let instrs_eq instrs1 instrs2 =>
          IList.equal
            (
              fun i1 i2 => {
                let (n, exp_map') = Sil.instr_compare_structural i1 i2 !exp_map;
                exp_map := exp_map';
                n
              }
            )
            instrs1
            instrs2;
        id_compare n1 n2 == 0 &&
        IList.equal id_compare n1.succs n2.succs &&
        IList.equal id_compare n1.preds n2.preds && instrs_eq n1.instrs n2.instrs
      };
      try (IList.for_all2 node_eq n1s n2s) {
      | Invalid_argument _ => false
      }
    };
    let att1 = pd1.attributes
    and att2 = pd2.attributes;
    att1.is_defined == att2.is_defined &&
    Typ.equal att1.ret_type att2.ret_type &&
    formals_eq att1.formals att2.formals && nodes_eq pd1.nodes pd2.nodes
  };
  let old_procs = cfg_old.proc_desc_table;
  let new_procs = cfg_new.proc_desc_table;
  let mark_pdesc_if_unchanged pname (new_pdesc: Procdesc.t) =>
    try {
      let old_pdesc = Procname.Hash.find old_procs pname;
      let changed =
        /* in continue_capture mode keep the old changed bit */
        Config.continue_capture && old_pdesc.attributes.changed ||
        not (pdescs_eq old_pdesc new_pdesc);
      new_pdesc.attributes.changed = changed
    } {
    | Not_found => ()
    };
  Procname.Hash.iter mark_pdesc_if_unchanged new_procs
};


/** Save a cfg into a file */
let store_cfg_to_file
    save_sources::save_sources=true
    source_file::source_file
    (filename: DB.filename)
    (cfg: cfg) => {
  inline_java_synthetic_methods cfg;
  if save_sources {
    save_source_files cfg
  };
  if Config.incremental_procs {
    switch (load_cfg_from_file filename) {
    | Some old_cfg => mark_unchanged_pdescs cfg old_cfg
    | None => ()
    }
  };
  save_attributes source_file cfg;
  Serialization.to_file cfg_serializer filename cfg
};


/** clone a procedure description and apply the type substitutions where
    the parameters are used */
let specialize_types_proc callee_pdesc resolved_pdesc substitutions => {
  let resolved_pname = Procdesc.get_proc_name resolved_pdesc
  and callee_start_node = Procdesc.get_start_node callee_pdesc
  and callee_exit_node = Procdesc.get_exit_node callee_pdesc;
  let convert_pvar pvar => Pvar.mk (Pvar.get_name pvar) resolved_pname;
  let convert_exp =
    fun
    | Exp.Lvar origin_pvar => Exp.Lvar (convert_pvar origin_pvar)
    | exp => exp;
  let extract_class_name =
    fun
    | Typ.Tptr (Tstruct name) _ => Typename.name name
    | _ => failwith "Expecting classname for Java types";
  let subst_map = ref Ident.IdentMap.empty;
  let redirected_class_name origin_id =>
    try (Some (Ident.IdentMap.find origin_id !subst_map)) {
    | Not_found => None
    };
  let convert_instr instrs =>
    fun
    | Sil.Load id (Exp.Lvar origin_pvar as origin_exp) origin_typ loc => {
        let (_, specialized_typ) = {
          let pvar_name = Pvar.get_name origin_pvar;
          try (IList.find (fun (n, _) => Mangled.equal n pvar_name) substitutions) {
          | Not_found => (pvar_name, origin_typ)
          }
        };
        subst_map := Ident.IdentMap.add id specialized_typ !subst_map;
        [Sil.Load id (convert_exp origin_exp) specialized_typ loc, ...instrs]
      }
    | Sil.Load id (Exp.Var origin_id as origin_exp) origin_typ loc => {
        let updated_typ =
          switch (Ident.IdentMap.find origin_id !subst_map) {
          | Typ.Tptr typ _ => typ
          | _ => failwith "Expecting a pointer type"
          | exception Not_found => origin_typ
          };
        [Sil.Load id (convert_exp origin_exp) updated_typ loc, ...instrs]
      }
    | Sil.Load id origin_exp origin_typ loc => [
        Sil.Load id (convert_exp origin_exp) origin_typ loc,
        ...instrs
      ]
    | Sil.Store assignee_exp origin_typ origin_exp loc => {
        let set_instr =
          Sil.Store (convert_exp assignee_exp) origin_typ (convert_exp origin_exp) loc;
        [set_instr, ...instrs]
      }
    | Sil.Call
        return_ids
        (Exp.Const (Const.Cfun (Procname.Java callee_pname_java)))
        [(Exp.Var id, _), ...origin_args]
        loc
        call_flags
        when call_flags.CallFlags.cf_virtual && redirected_class_name id != None => {
        let redirected_typ = Option.get (redirected_class_name id);
        let redirected_pname =
          Procname.replace_class
            (Procname.Java callee_pname_java) (extract_class_name redirected_typ)
        and args = {
          let other_args = IList.map (fun (exp, typ) => (convert_exp exp, typ)) origin_args;
          [(Exp.Var id, redirected_typ), ...other_args]
        };
        let call_instr =
          Sil.Call return_ids (Exp.Const (Const.Cfun redirected_pname)) args loc call_flags;
        [call_instr, ...instrs]
      }
    | Sil.Call return_ids origin_call_exp origin_args loc call_flags => {
        let converted_args = IList.map (fun (exp, typ) => (convert_exp exp, typ)) origin_args;
        let call_instr =
          Sil.Call return_ids (convert_exp origin_call_exp) converted_args loc call_flags;
        [call_instr, ...instrs]
      }
    | Sil.Prune origin_exp loc is_true_branch if_kind => [
        Sil.Prune (convert_exp origin_exp) loc is_true_branch if_kind,
        ...instrs
      ]
    | Sil.Declare_locals typed_vars loc => {
        let new_typed_vars = IList.map (fun (pvar, typ) => (convert_pvar pvar, typ)) typed_vars;
        [Sil.Declare_locals new_typed_vars loc, ...instrs]
      }
    | Sil.Nullify _
    | Abstract _
    | Sil.Remove_temps _ =>
      /* these are generated instructions that will be replaced by the preanalysis */
      instrs;
  let convert_node_kind =
    fun
    | Node.Start_node _ => Node.Start_node resolved_pname
    | Node.Exit_node _ => Node.Exit_node resolved_pname
    | node_kind => node_kind;
  let node_map = ref NodeMap.empty;
  let rec convert_node node => {
    let loc = Node.get_loc node
    and kind = convert_node_kind (Node.get_kind node)
    and instrs = IList.fold_left convert_instr [] (Node.get_instrs node) |> IList.rev;
    Procdesc.create_node resolved_pdesc loc kind instrs
  }
  and loop callee_nodes =>
    switch callee_nodes {
    | [] => []
    | [node, ...other_node] =>
      let converted_node =
        try (NodeMap.find node !node_map) {
        | Not_found =>
          let new_node = convert_node node
          and successors = Node.get_succs node
          and exn_nodes = Node.get_exn node;
          node_map := NodeMap.add node new_node !node_map;
          if (Node.equal node callee_start_node) {
            Procdesc.set_start_node resolved_pdesc new_node
          };
          if (Node.equal node callee_exit_node) {
            Procdesc.set_exit_node resolved_pdesc new_node
          };
          Procdesc.node_set_succs_exn callee_pdesc new_node (loop successors) (loop exn_nodes);
          new_node
        };
      [converted_node, ...loop other_node]
    };
  ignore (loop [callee_start_node]);
  resolved_pdesc
};


/** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types */
let specialize_types callee_pdesc resolved_pname args => {
  /* TODO (#9333890): This currently only works when the callee is defined in the same file.
     Add support to search for the callee procedure description in the execution environment */
  let callee_attributes = Procdesc.get_attributes callee_pdesc;
  let resolved_formals =
    IList.fold_left2
      (fun accu (name, _) (_, arg_typ) => [(name, arg_typ), ...accu])
      []
      callee_attributes.formals
      args |> IList.rev;
  let resolved_attributes = {
    ...callee_attributes,
    formals: resolved_formals,
    proc_name: resolved_pname
  };
  AttributesTable.store_attributes resolved_attributes;
  let resolved_pdesc = {
    let tmp_cfg = create_cfg ();
    create_proc_desc tmp_cfg resolved_attributes
  };
  specialize_types_proc callee_pdesc resolved_pdesc resolved_formals
};
