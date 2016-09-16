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

/* ============== START of ADT node and proc_desc ============== */
/* =============== START of module Node =============== */
let module Node = {
  type id = int;
  type nodekind =
    | Start_node of proc_desc
    | Exit_node of proc_desc
    | Stmt_node of string
    | Join_node
    | Prune_node of bool Sil.if_kind string /** (true/false branch, if_kind, comment) */
    | Skip_node of string
  /** a node */
  and t = {
    /** unique id of the node */
    nd_id: id,
    /** distance to the exit node */
    mutable nd_dist_exit: option int,
    /** dead program variables after executing the instructions */
    mutable nd_dead_pvars_after: list Pvar.t,
    /** dead program variables before executing the instructions */
    mutable nd_deads_before: list Pvar.t,
    /** exception nodes in the cfg */
    mutable nd_exn: list t,
    /** instructions for symbolic execution */
    mutable nd_instrs: list Sil.instr,
    /** kind of node */
    mutable nd_kind: nodekind,
    /** location in the source code */
    mutable nd_loc: Location.t,
    /** predecessor nodes in the cfg */
    mutable nd_preds: list t,
    /** proc desc from cil */
    mutable nd_proc: option proc_desc,
    /** successor nodes in the cfg */
    mutable nd_succs: list t
  }
  /** procedure description */
  and proc_desc = {
    pd_attributes: ProcAttributes.t, /** attributes of the procedure */
    pd_id: int, /** unique proc_desc identifier */
    mutable pd_nodes: list t, /** list of nodes of this procedure */
    mutable pd_start_node: t, /** start node of this procedure */
    mutable pd_exit_node: t /** exit node of ths procedure */
  };
  let exn_handler_kind = Stmt_node "exception handler";
  let exn_sink_kind = Stmt_node "exceptions sink";
  let throw_kind = Stmt_node "throw";

  /** data type for the control flow graph */
  type cfg = {
    node_id: ref int,
    node_list: ref (list t),
    name_pdesc_tbl: Procname.Hash.t proc_desc, /** Map proc name to procdesc */
    mutable priority_set: Procname.Set.t
    /** set of function names to be analyzed first */
  };
  let create_cfg () =>
    /** create a new empty cfg */
    {
      node_id: ref 0,
      node_list: ref [],
      name_pdesc_tbl: Procname.Hash.create 1000,
      priority_set: Procname.Set.empty
    };

  /** compute the list of procedures added or changed in [cfg_new] over [cfg_old] */
  let mark_unchanged_pdescs cfg_new cfg_old => {
    let pdescs_eq pd1 pd2 =>
      /* map of exp names in pd1 -> exp names in pd2 */
      {
        let exp_map = ref Exp.Map.empty;
        /* map of node id's in pd1 -> node id's in pd2 */
        let id_map = ref IntMap.empty;
        /* formals are the same if their types are the same */
        let formals_eq formals1 formals2 =>
          IList.equal (fun (_, typ1) (_, typ2) => Typ.compare typ1 typ2) formals1 formals2;
        let nodes_eq n1s n2s =>
          /* nodes are the same if they have the same id, instructions, and succs/preds up to renaming
             with [exp_map] and [id_map] */
          {
            let node_eq n1 n2 => {
              let id_compare n1 n2 => {
                let (id1, id2) = (n1.nd_id, n2.nd_id);
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
                IList.equal id_compare n1.nd_succs n2.nd_succs &&
                IList.equal id_compare n1.nd_preds n2.nd_preds &&
                instrs_eq n1.nd_instrs n2.nd_instrs
            };
            try (IList.for_all2 node_eq n1s n2s) {
            | Invalid_argument _ => false
            }
          };
        let att1 = pd1.pd_attributes
        and att2 = pd2.pd_attributes;
        att1.ProcAttributes.is_defined == att2.ProcAttributes.is_defined &&
          Typ.equal att1.ProcAttributes.ret_type att2.ProcAttributes.ret_type &&
          formals_eq att1.ProcAttributes.formals att2.ProcAttributes.formals &&
          nodes_eq pd1.pd_nodes pd2.pd_nodes
      };
    let old_procs = cfg_old.name_pdesc_tbl;
    let new_procs = cfg_new.name_pdesc_tbl;
    let mark_pdesc_if_unchanged pname new_pdesc =>
      try {
        let old_pdesc = Procname.Hash.find old_procs pname;
        let changed =
          /* in continue_capture mode keep the old changed bit */
          Config.continue_capture && old_pdesc.pd_attributes.ProcAttributes.changed ||
            not (pdescs_eq old_pdesc new_pdesc);
        new_pdesc.pd_attributes.changed = changed
      } {
      | Not_found => ()
      };
    Procname.Hash.iter mark_pdesc_if_unchanged new_procs
  };
  let node_id_gen cfg => {
    incr cfg.node_id;
    !cfg.node_id
  };
  let pdesc_tbl_add cfg proc_name proc_desc =>
    Procname.Hash.add cfg.name_pdesc_tbl proc_name proc_desc;
  let pdesc_tbl_remove cfg proc_name => Procname.Hash.remove cfg.name_pdesc_tbl proc_name;
  let pdesc_tbl_find cfg proc_name => Procname.Hash.find cfg.name_pdesc_tbl proc_name;
  let iter_proc_desc cfg f => Procname.Hash.iter f cfg.name_pdesc_tbl;
  let dummy () => {
    nd_id: 0,
    nd_dist_exit: None,
    nd_dead_pvars_after: [],
    nd_deads_before: [],
    nd_instrs: [],
    nd_kind: Skip_node "dummy",
    nd_loc: Location.dummy,
    nd_proc: None,
    nd_succs: [],
    nd_preds: [],
    nd_exn: []
  };
  let compare node1 node2 => int_compare node1.nd_id node2.nd_id;
  let hash node => Hashtbl.hash node.nd_id;
  let equal node1 node2 => compare node1 node2 == 0;
  let get_all_nodes cfg => !cfg.node_list;
  let create cfg loc kind instrs pdesc => {
    let node_id = node_id_gen cfg;
    let node = {
      nd_id: node_id,
      nd_dist_exit: None,
      nd_dead_pvars_after: [],
      nd_deads_before: [],
      nd_instrs: instrs,
      nd_kind: kind,
      nd_loc: loc,
      nd_preds: [],
      nd_proc: Some pdesc,
      nd_succs: [],
      nd_exn: []
    };
    cfg.node_list := [node, ...!cfg.node_list];
    pdesc.pd_nodes = [node, ...pdesc.pd_nodes];
    node
  };

  /** Get the unique id of the node */
  let get_id node => node.nd_id;

  /** compare node ids */
  let id_compare = int_compare;
  let get_succs node => node.nd_succs;
  type node = t;
  let module NodeSet = Set.Make {
    type t = node;
    let compare = compare;
  };
  let module NodeMap = Map.Make {
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
            acc (slice_nodes (IList.filter (fun s => not (NodeSet.mem s !visited)) n.nd_succs))
        }
      };
      IList.fold_left do_node NodeSet.empty nodes
    };
    NodeSet.elements (slice_nodes node.nd_succs)
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
            acc (slice_nodes (IList.filter (fun s => not (NodeSet.mem s !visited)) n.nd_preds))
        }
      };
      IList.fold_left do_node NodeSet.empty nodes
    };
    NodeSet.elements (slice_nodes node.nd_preds)
  };
  let get_exn node => node.nd_exn;

  /** Get the proc desc of the node */
  let get_proc_desc node =>
    switch node.nd_proc {
    | None =>
      L.out "node_get_proc_desc: at node %d@\n" node.nd_id;
      assert false
    | Some proc_desc => proc_desc
    };

  /** Set the successor nodes and exception nodes, and build predecessor links */
  let set_succs_exn_base node succs exn => {
    node.nd_succs = succs;
    node.nd_exn = exn;
    IList.iter (fun n => n.nd_preds = [node, ...n.nd_preds]) succs
  };

  /** Set the successor and exception nodes.
      If this is a join node right before the exit node, add an extra node in the middle,
      otherwise nullify and abstract instructions cannot be added after a conditional. */
  let set_succs_exn cfg node succs exn =>
    switch (node.nd_kind, succs) {
    | (Join_node, [{nd_kind: Exit_node _} as exit_node]) =>
      let kind = Stmt_node "between_join_and_exit";
      let pdesc = get_proc_desc node;
      let node' = create cfg node.nd_loc kind node.nd_instrs pdesc;
      set_succs_exn_base node [node'] exn;
      set_succs_exn_base node' [exit_node] exn
    | _ => set_succs_exn_base node succs exn
    };

  /** Get the predecessors of the node */
  let get_preds node => node.nd_preds;

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
  let get_kind node => node.nd_kind;

  /** Set the node kind */
  let set_kind node kind => node.nd_kind = kind;

  /** Comparison for node kind */
  let kind_compare k1 k2 =>
    switch (k1, k2) {
    | (Start_node pd1, Start_node pd2) => int_compare pd1.pd_id pd2.pd_id
    | (Start_node _, _) => (-1)
    | (_, Start_node _) => 1
    | (Exit_node pd1, Exit_node pd2) => int_compare pd1.pd_id pd2.pd_id
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
  let get_instrs node => node.nd_instrs;

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
  let get_loc n => n.nd_loc;

  /** Get the source location of the last instruction in the node */
  let get_last_loc n =>
    switch (IList.rev (get_instrs n)) {
    | [instr, ..._] => Sil.instr_get_loc instr
    | [] => n.nd_loc
    };

  /** Set the location of the node */
  let set_loc n loc => n.nd_loc = loc;
  let pp_id f id => F.fprintf f "%d" id;
  let pp f node => pp_id f (get_id node);
  let proc_desc_from_name cfg proc_name =>
    try (Some (pdesc_tbl_find cfg proc_name)) {
    | Not_found => None
    };
  let set_dead_pvars node after dead =>
    if after {
      node.nd_dead_pvars_after = dead
    } else {
      node.nd_deads_before = dead
    };
  let get_dead_pvars node after =>
    if after {
      node.nd_dead_pvars_after
    } else {
      node.nd_deads_before
    };
  let get_distance_to_exit node => node.nd_dist_exit;

  /** Append the instructions to the list of instructions to execute */
  let append_instrs node instrs => node.nd_instrs = node.nd_instrs @ instrs;

  /** Add the instructions at the beginning of the list of instructions to execute */
  let prepend_instrs node instrs => node.nd_instrs = instrs @ node.nd_instrs;

  /** Replace the instructions to be executed. */
  let replace_instrs node instrs => node.nd_instrs = instrs;
  let proc_desc_get_ret_var pdesc => Pvar.get_ret_pvar pdesc.pd_attributes.ProcAttributes.proc_name;

  /** Add declarations for local variables and return variable to the node */
  let add_locals_ret_declaration node locals => {
    let loc = get_loc node;
    let pdesc = get_proc_desc node;
    let proc_name = pdesc.pd_attributes.ProcAttributes.proc_name;
    let ret_var = {
      let ret_type = pdesc.pd_attributes.ProcAttributes.ret_type;
      (proc_desc_get_ret_var pdesc, ret_type)
    };
    let construct_decl (x, typ) => (Pvar.mk x proc_name, typ);
    let ptl = [ret_var, ...IList.map construct_decl locals];
    let instr = Sil.Declare_locals ptl loc;
    prepend_instrs node [instr]
  };

  /** Counter for identifiers of procdescs */
  let proc_desc_id_counter = ref 0;
  let proc_desc_create cfg proc_attributes => {
    incr proc_desc_id_counter;
    let pdesc = {
      pd_attributes: proc_attributes,
      pd_id: !proc_desc_id_counter,
      pd_nodes: [],
      pd_start_node: dummy (),
      pd_exit_node: dummy ()
    };
    pdesc_tbl_add cfg proc_attributes.ProcAttributes.proc_name pdesc;
    pdesc
  };
  let remove_node' filter_out_fun cfg => {
    let remove_node_in_cfg nodes => IList.filter filter_out_fun nodes;
    cfg.node_list := remove_node_in_cfg !cfg.node_list
  };
  let remove_node_set cfg nodes => remove_node' (fun node' => not (NodeSet.mem node' nodes)) cfg;
  let proc_desc_remove cfg name remove_nodes => {
    if remove_nodes {
      let pdesc = pdesc_tbl_find cfg name;
      let proc_nodes =
        IList.fold_right (fun node set => NodeSet.add node set) pdesc.pd_nodes NodeSet.empty;
      remove_node_set cfg proc_nodes
    };
    pdesc_tbl_remove cfg name
  };
  let proc_desc_get_start_node proc_desc => proc_desc.pd_start_node;
  let proc_desc_get_err_log proc_desc => proc_desc.pd_attributes.ProcAttributes.err_log;
  let proc_desc_get_attributes proc_desc => proc_desc.pd_attributes;
  let proc_desc_get_exit_node proc_desc => proc_desc.pd_exit_node;

  /** Compute the distance of each node to the exit node, if not computed already */
  let proc_desc_compute_distance_to_exit_node proc_desc => {
    let exit_node = proc_desc.pd_exit_node;
    let rec mark_distance dist nodes => {
      let next_nodes = ref [];
      let do_node node =>
        switch node.nd_dist_exit {
        | Some _ => ()
        | None =>
          node.nd_dist_exit = Some dist;
          next_nodes := node.nd_preds @ !next_nodes
        };
      IList.iter do_node nodes;
      if (!next_nodes !== []) {
        mark_distance (dist + 1) !next_nodes
      }
    };
    mark_distance 0 [exit_node]
  };

  /** Set the start node of the proc desc */
  let proc_desc_set_start_node pdesc node => pdesc.pd_start_node = node;

  /** Set the exit node of the proc desc */
  let proc_desc_set_exit_node pdesc node => pdesc.pd_exit_node = node;

  /** Set a flag for the proc desc */
  let proc_desc_set_flag pdesc key value =>
    proc_flags_add pdesc.pd_attributes.ProcAttributes.proc_flags key value;

  /** Return the return type of the procedure */
  let proc_desc_get_ret_type proc_desc => proc_desc.pd_attributes.ProcAttributes.ret_type;
  let proc_desc_get_proc_name proc_desc => proc_desc.pd_attributes.ProcAttributes.proc_name;

  /** Return [true] iff the procedure is defined, and not just declared */
  let proc_desc_is_defined proc_desc => proc_desc.pd_attributes.ProcAttributes.is_defined;
  let proc_desc_is_java_synchroinized proc_desc => proc_desc.pd_attributes.ProcAttributes.is_java_synchronized_method;
  let proc_desc_get_loc proc_desc => proc_desc.pd_attributes.ProcAttributes.loc;

  /** Return name and type of formal parameters */
  let proc_desc_get_formals proc_desc => proc_desc.pd_attributes.ProcAttributes.formals;

  /** Return name and type of local variables */
  let proc_desc_get_locals proc_desc => proc_desc.pd_attributes.ProcAttributes.locals;

  /** Return name and type of captured variables */
  let proc_desc_get_captured proc_desc => proc_desc.pd_attributes.ProcAttributes.captured;

  /** Return the visibility attribute */
  let proc_desc_get_access proc_desc => proc_desc.pd_attributes.ProcAttributes.access;
  let proc_desc_get_nodes proc_desc => proc_desc.pd_nodes;

  /** List of nodes in the procedure up to the first branching */
  let proc_desc_get_slope proc_desc =>
    get_generated_slope (proc_desc_get_start_node proc_desc) get_succs;

  /** List of nodes in the procedure sliced by a predicate up to the first branching */
  let proc_desc_get_sliced_slope proc_desc f =>
    get_generated_slope (proc_desc_get_start_node proc_desc) (fun n => get_sliced_succs n f);

  /** Get flags for the proc desc */
  let proc_desc_get_flags proc_desc => proc_desc.pd_attributes.ProcAttributes.proc_flags;

  /** Append the locals to the list of local variables */
  let proc_desc_append_locals proc_desc new_locals =>
    proc_desc.pd_attributes.ProcAttributes.locals =
      proc_desc.pd_attributes.ProcAttributes.locals @ new_locals;

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
  let proc_desc_iter_nodes f proc_desc => IList.iter f (IList.rev (proc_desc_get_nodes proc_desc));
  let proc_desc_fold_nodes f acc proc_desc =>
    IList.fold_left f acc (IList.rev (proc_desc_get_nodes proc_desc));
  let proc_desc_fold_calls f acc pdesc => {
    let do_node a node =>
      IList.fold_left (fun b callee_pname => f b (callee_pname, get_loc node)) a (get_callees node);
    IList.fold_left do_node acc (proc_desc_get_nodes pdesc)
  };

  /** iterate over the calls from the procedure: (callee,location) pairs */
  let proc_desc_iter_calls f pdesc => proc_desc_fold_calls (fun _ call => f call) () pdesc;
  let proc_desc_iter_slope f proc_desc => {
    let visited = ref NodeSet.empty;
    let rec do_node node => {
      visited := NodeSet.add node !visited;
      f node;
      switch (get_succs node) {
      | [n] =>
        if (not (NodeSet.mem n !visited)) {
          do_node n
        }
      | _ => ()
      }
    };
    do_node (proc_desc_get_start_node proc_desc)
  };

  /** iterate between two nodes or until we reach a branching structure */
  let proc_desc_iter_slope_range f src_node dst_node => {
    let visited = ref NodeSet.empty;
    let rec do_node node => {
      visited := NodeSet.add node !visited;
      f node;
      switch (get_succs node) {
      | [n] =>
        if (not (NodeSet.mem n !visited) && not (equal node dst_node)) {
          do_node n
        }
      | _ => ()
      }
    };
    do_node src_node
  };
  let proc_desc_iter_slope_calls f proc_desc => {
    let do_node node => IList.iter (fun callee_pname => f callee_pname) (get_callees node);
    proc_desc_iter_slope do_node proc_desc
  };
  let proc_desc_iter_instrs f proc_desc => {
    let do_node node => IList.iter (fun i => f node i) (get_instrs node);
    proc_desc_iter_nodes do_node proc_desc
  };
  let proc_desc_fold_instrs f acc proc_desc => {
    let fold_node acc node =>
      IList.fold_left (fun acc instr => f acc node instr) acc (get_instrs node);
    proc_desc_fold_nodes fold_node acc proc_desc
  };
  /*
     let remove_node cfg node =
       remove_node' (fun node' -> not (equal node node'))
         cfg node
   */
  /* clone a procedure description and apply the type substitutions where
     the parameters are used */
  let proc_desc_specialize_types callee_proc_desc resolved_attributes substitutions => {
    let cfg = create_cfg ();
    let resolved_proc_desc = proc_desc_create cfg resolved_attributes;
    let resolved_proc_name = proc_desc_get_proc_name resolved_proc_desc
    and callee_start_node = proc_desc_get_start_node callee_proc_desc
    and callee_exit_node = proc_desc_get_exit_node callee_proc_desc;
    let convert_pvar pvar => Pvar.mk (Pvar.get_name pvar) resolved_proc_name;
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
      | Start_node _ => Start_node resolved_proc_desc
      | Exit_node _ => Exit_node resolved_proc_desc
      | node_kind => node_kind;
    let node_map = ref NodeMap.empty;
    let rec convert_node node => {
      let loc = get_loc node
      and kind = convert_node_kind (get_kind node)
      and instrs = IList.fold_left convert_instr [] (get_instrs node) |> IList.rev;
      create cfg loc kind instrs resolved_proc_desc
    }
    and loop callee_nodes =>
      switch callee_nodes {
      | [] => []
      | [node, ...other_node] =>
        let converted_node =
          try (NodeMap.find node !node_map) {
          | Not_found =>
            let new_node = convert_node node
            and successors = get_succs node
            and exn_nodes = get_exn node;
            node_map := NodeMap.add node new_node !node_map;
            if (equal node callee_start_node) {
              proc_desc_set_start_node resolved_proc_desc new_node
            };
            if (equal node callee_exit_node) {
              proc_desc_set_exit_node resolved_proc_desc new_node
            };
            set_succs_exn cfg new_node (loop successors) (loop exn_nodes);
            new_node
          };
        [converted_node, ...loop other_node]
      };
    ignore (loop [callee_start_node]);
    resolved_proc_desc
  };
};

/* =============== END of module Node =============== */
type node = Node.t;

type cfg = Node.cfg;

/* =============== START of module Procdesc =============== */
let module Procdesc = {
  type t = Node.proc_desc;
  let compute_distance_to_exit_node = Node.proc_desc_compute_distance_to_exit_node;
  let create = Node.proc_desc_create;
  let remove = Node.proc_desc_remove;
  let find_from_name = Node.proc_desc_from_name;
  let get_attributes = Node.proc_desc_get_attributes;
  let get_err_log = Node.proc_desc_get_err_log;
  let get_exit_node = Node.proc_desc_get_exit_node;
  let get_flags = Node.proc_desc_get_flags;
  let get_formals = Node.proc_desc_get_formals;
  let get_loc = Node.proc_desc_get_loc;
  let get_locals = Node.proc_desc_get_locals;
  let get_captured = Node.proc_desc_get_captured;
  let get_access = Node.proc_desc_get_access;
  let get_nodes = Node.proc_desc_get_nodes;
  let get_slope = Node.proc_desc_get_slope;
  let get_sliced_slope = Node.proc_desc_get_sliced_slope;
  let get_proc_name = Node.proc_desc_get_proc_name;
  let get_ret_type = Node.proc_desc_get_ret_type;
  let get_ret_var pdesc => Pvar.mk Ident.name_return (get_proc_name pdesc);
  let get_start_node = Node.proc_desc_get_start_node;
  let is_defined = Node.proc_desc_is_defined;
  let is_java_synchronized = Node.proc_desc_is_java_synchroinized;
  let iter_nodes = Node.proc_desc_iter_nodes;
  let fold_calls = Node.proc_desc_fold_calls;
  let iter_calls = Node.proc_desc_iter_calls;
  let iter_instrs = Node.proc_desc_iter_instrs;
  let fold_instrs = Node.proc_desc_fold_instrs;
  let iter_slope = Node.proc_desc_iter_slope;
  let iter_slope_calls = Node.proc_desc_iter_slope_calls;
  let iter_slope_range = Node.proc_desc_iter_slope_range;
  let set_exit_node = Node.proc_desc_set_exit_node;
  let set_flag = Node.proc_desc_set_flag;
  let set_start_node = Node.proc_desc_set_start_node;
  let append_locals = Node.proc_desc_append_locals;
  let specialize_types = Node.proc_desc_specialize_types;
};

/* =============== END of module Procdesc =============== */
/** Hash table with nodes as keys. */
let module NodeHash = Hashtbl.Make Node;


/** Set of nodes. */
let module NodeSet = Node.NodeSet;


/** Map with node id keys. */
let module IdMap = Node.IdMap;

let iter_proc_desc = Node.iter_proc_desc;

let rec pp_node_list f =>
  fun
  | [] => ()
  | [node] => Node.pp f node
  | [node, ...nodes] => F.fprintf f "%a, %a" Node.pp node pp_node_list nodes;


/** Get all the procdescs (defined and declared) */
let get_all_procs cfg => {
  let procs = ref [];
  let f _ pdesc => procs := [pdesc, ...!procs];
  iter_proc_desc cfg f;
  !procs
};


/** Get the procedures whose body is defined in this cfg */
let get_defined_procs cfg => IList.filter Procdesc.is_defined (get_all_procs cfg);


/** get the function names which should be analyzed before the other ones */
let get_priority_procnames cfg => cfg.Node.priority_set;


/** set the function names whose address has been taken in this file */
let set_procname_priority cfg pname =>
  cfg.Node.priority_set = Procname.Set.add pname cfg.Node.priority_set;

let get_name_of_local (curr_f: Procdesc.t) (x, _) => Pvar.mk x (Procdesc.get_proc_name curr_f);

/* returns a list of local static variables (ie local variables defined static) in a proposition */
let get_name_of_objc_static_locals (curr_f: Procdesc.t) p => {
  let pname = Procname.to_string (Procdesc.get_proc_name curr_f);
  let local_static e =>
    switch e {
    /* is a local static if it's a global and it has a static local name */
    | Exp.Lvar pvar when Pvar.is_global pvar && Sil.is_static_local_name pname pvar => [pvar]
    | _ => []
    };
  let hpred_local_static hpred =>
    switch hpred {
    | Sil.Hpointsto e _ _ => [local_static e]
    | _ => []
    };
  let vars_sigma = IList.map hpred_local_static p.Prop.sigma;
  IList.flatten (IList.flatten vars_sigma)
};

/* returns a list of local variables that points to an objc block in a proposition */
let get_name_of_objc_block_locals p => {
  let local_blocks e =>
    switch e {
    | Exp.Lvar pvar when Sil.is_block_pvar pvar => [pvar]
    | _ => []
    };
  let hpred_local_blocks hpred =>
    switch hpred {
    | Sil.Hpointsto e _ _ => [local_blocks e]
    | _ => []
    };
  let vars_sigma = IList.map hpred_local_blocks p.Prop.sigma;
  IList.flatten (IList.flatten vars_sigma)
};

let remove_abduced_retvars tenv p =>
  /* compute the hpreds and pure atoms reachable from the set of seed expressions in [exps] */
  {
    let compute_reachable p seed_exps => {
      let (sigma, pi) = (p.Prop.sigma, p.Prop.pi);
      let rec collect_exps exps =>
        fun
        | Sil.Eexp (Exp.Exn e) _ => Exp.Set.add e exps
        | Sil.Eexp e _ => Exp.Set.add e exps
        | Sil.Estruct flds _ =>
          IList.fold_left (fun exps (_, strexp) => collect_exps exps strexp) exps flds
        | Sil.Earray _ elems _ =>
          IList.fold_left (fun exps (_, strexp) => collect_exps exps strexp) exps elems;
      let rec compute_reachable_hpreds_rec sigma (reach, exps) => {
        let add_hpred_if_reachable (reach, exps) =>
          fun
          | Sil.Hpointsto lhs rhs _ as hpred when Exp.Set.mem lhs exps => {
              let reach' = Sil.HpredSet.add hpred reach;
              let exps' = collect_exps exps rhs;
              (reach', exps')
            }
          | Sil.Hlseg _ _ exp1 exp2 exp_l as hpred => {
              let reach' = Sil.HpredSet.add hpred reach;
              let exps' =
                IList.fold_left
                  (fun exps_acc exp => Exp.Set.add exp exps_acc) exps [exp1, exp2, ...exp_l];
              (reach', exps')
            }
          | Sil.Hdllseg _ _ exp1 exp2 exp3 exp4 exp_l as hpred => {
              let reach' = Sil.HpredSet.add hpred reach;
              let exps' =
                IList.fold_left
                  (fun exps_acc exp => Exp.Set.add exp exps_acc)
                  exps
                  [exp1, exp2, exp3, exp4, ...exp_l];
              (reach', exps')
            }
          | _ => (reach, exps);
        let (reach', exps') = IList.fold_left add_hpred_if_reachable (reach, exps) sigma;
        if (Sil.HpredSet.cardinal reach == Sil.HpredSet.cardinal reach') {
          (reach, exps)
        } else {
          compute_reachable_hpreds_rec sigma (reach', exps')
        }
      };
      let (reach_hpreds, reach_exps) =
        compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, seed_exps);
      /* filter away the pure atoms without reachable exps */
      let reach_pi = {
        let rec exp_contains =
          fun
          | exp when Exp.Set.mem exp reach_exps => true
          | Exp.UnOp _ e _
          | Exp.Cast _ e
          | Exp.Lfield e _ _ => exp_contains e
          | Exp.BinOp _ e0 e1
          | Exp.Lindex e0 e1 => exp_contains e0 || exp_contains e1
          | _ => false;
        IList.filter
          (
            fun
            | Sil.Aeq lhs rhs
            | Sil.Aneq lhs rhs => exp_contains lhs || exp_contains rhs
            | Sil.Apred _ es
            | Sil.Anpred _ es => IList.exists exp_contains es
          )
          pi
      };
      (Sil.HpredSet.elements reach_hpreds, reach_pi)
    };
    /* separate the abduced pvars from the normal ones, deallocate the abduced ones*/
    let (abduceds, normal_pvars) =
      IList.fold_left
        (
          fun pvars hpred =>
            switch hpred {
            | Sil.Hpointsto (Exp.Lvar pvar) _ _ =>
              let (abduceds, normal_pvars) = pvars;
              if (Pvar.is_abduced pvar) {
                ([pvar, ...abduceds], normal_pvars)
              } else {
                (abduceds, [pvar, ...normal_pvars])
              }
            | _ => pvars
            }
        )
        ([], [])
        p.Prop.sigma;
    let (_, p') = Attribute.deallocate_stack_vars tenv p abduceds;
    let normal_pvar_set =
      IList.fold_left
        (fun normal_pvar_set pvar => Exp.Set.add (Exp.Lvar pvar) normal_pvar_set)
        Exp.Set.empty
        normal_pvars;
    /* walk forward from non-abduced pvars, keep everything reachable. remove everything else */
    let (sigma_reach, pi_reach) = compute_reachable p' normal_pvar_set;
    Prop.normalize tenv (Prop.set p' pi::pi_reach sigma::sigma_reach)
  };

let remove_locals tenv (curr_f: Procdesc.t) p => {
  let names_of_locals = IList.map (get_name_of_local curr_f) (Procdesc.get_locals curr_f);
  let names_of_locals' =
    switch !Config.curr_language {
    | Config.Clang =>
      /* in ObjC to deal with block we need to remove static locals */
      let names_of_static_locals = get_name_of_objc_static_locals curr_f p;
      let names_of_block_locals = get_name_of_objc_block_locals p;
      names_of_block_locals @ names_of_locals @ names_of_static_locals
    | _ => names_of_locals
    };
  let (removed, p') = Attribute.deallocate_stack_vars tenv p names_of_locals';
  (
    removed,
    if Config.angelic_execution {
      remove_abduced_retvars tenv p'
    } else {
      p'
    }
  )
};

let remove_formals tenv (curr_f: Procdesc.t) p => {
  let pname = Procdesc.get_proc_name curr_f;
  let formal_vars = IList.map (fun (n, _) => Pvar.mk n pname) (Procdesc.get_formals curr_f);
  Attribute.deallocate_stack_vars tenv p formal_vars
};


/** remove the return variable from the prop */
let remove_ret tenv (curr_f: Procdesc.t) (p: Prop.t Prop.normal) => {
  let pname = Procdesc.get_proc_name curr_f;
  let name_of_ret = Procdesc.get_ret_var curr_f;
  let (_, p') = Attribute.deallocate_stack_vars tenv p [Pvar.to_callee pname name_of_ret];
  p'
};


/** remove locals and return variable from the prop */
let remove_locals_ret tenv (curr_f: Procdesc.t) p => snd (
  remove_locals tenv curr_f (remove_ret tenv curr_f p)
);


/** Remove locals and formal parameters from the prop.
    Return the list of stack variables whose address was still present after deallocation. */
let remove_locals_formals tenv (curr_f: Procdesc.t) p => {
  let (pvars1, p1) = remove_formals tenv curr_f p;
  let (pvars2, p2) = remove_locals tenv curr_f p1;
  (pvars1 @ pvars2, p2)
};


/** remove seed vars from a prop */
let remove_seed_vars tenv (prop: Prop.t 'a) :Prop.t Prop.normal => {
  let hpred_not_seed =
    fun
    | Sil.Hpointsto (Exp.Lvar pv) _ _ => not (Pvar.is_seed pv)
    | _ => true;
  let sigma = prop.sigma;
  let sigma' = IList.filter hpred_not_seed sigma;
  Prop.normalize tenv (Prop.set prop sigma::sigma')
};


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


/** Removes seeds variables from a prop corresponding to captured variables in an objc block */
let remove_seed_captured_vars_block tenv captured_vars prop => {
  let is_captured pname vn => Mangled.equal pname vn;
  let hpred_seed_captured =
    fun
    | Sil.Hpointsto (Exp.Lvar pv) _ _ => {
        let pname = Pvar.get_name pv;
        Pvar.is_seed pv && IList.mem is_captured pname captured_vars
      }
    | _ => false;
  let sigma = prop.Prop.sigma;
  let sigma' = IList.filter (fun hpred => not (hpred_seed_captured hpred)) sigma;
  Prop.normalize tenv (Prop.set prop sigma::sigma')
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
    let loc = Node.proc_desc_get_loc pdesc;
    let source_file = loc.Location.file;
    let source_file_str = DB.source_file_to_abs_path source_file;
    let dest_file = DB.source_file_in_resdir source_file;
    let dest_file_str = DB.filename_to_string dest_file;
    let needs_copy =
      Node.proc_desc_is_defined pdesc &&
        Sys.file_exists source_file_str &&
        (
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
  Node.iter_proc_desc cfg process_proc
};


/** Save the .attr files for the procedures in the cfg. */
let save_attributes cfg => {
  let save_proc proc_desc => {
    let attributes = Procdesc.get_attributes proc_desc;
    let loc = attributes.ProcAttributes.loc;
    let attributes' =
      if (Location.equal loc Location.dummy) {
        let loc' = {...loc, Location.file: !DB.current_source};
        {...attributes, ProcAttributes.loc: loc'}
      } else {
        attributes
      };
    /*
     L.stderr "save_proc@.  proc_name:%a@.  filename:%s@.  current_source:%s@.  loc:%s@."
       Procname.pp (Procdesc.get_proc_name proc_desc)
       (DB.filename_to_string filename)
       (DB.source_file_to_string !DB.current_source)
       (Location.to_string loc);
     */
    AttributesTable.store_attributes attributes'
  };
  IList.iter save_proc (get_all_procs cfg)
};


/** Inline a synthetic (access or bridge) method. */
let inline_synthetic_method ret_ids etl proc_desc loc_call :option Sil.instr => {
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
    switch (instr, ret_ids, etl) {
    | (
        Sil.Load _ (Exp.Lfield (Exp.Var _) fn ft) bt _,
        [ret_id],
        [(e1, _)] /* getter for fields */
      ) =>
      let instr' = Sil.Load ret_id (Exp.Lfield e1 fn ft) bt loc_call;
      found instr instr'
    | (Sil.Load _ (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _, [ret_id], []) when Pvar.is_global pvar =>
      /* getter for static fields */
      let instr' = Sil.Load ret_id (Exp.Lfield (Exp.Lvar pvar) fn ft) bt loc_call;
      found instr instr'
    | (
        Sil.Store (Exp.Lfield _ fn ft) bt _ _,
        _,
        [(e1, _), (e2, _)] /* setter for fields */
      ) =>
      let instr' = Sil.Store (Exp.Lfield e1 fn ft) bt e2 loc_call;
      found instr instr'
    | (Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt _ _, _, [(e1, _)]) when Pvar.is_global pvar =>
      /* setter for static fields */
      let instr' = Sil.Store (Exp.Lfield (Exp.Lvar pvar) fn ft) bt e1 loc_call;
      found instr instr'
    | (Sil.Call ret_ids' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when IList.length ret_ids == IList.length ret_ids' && IList.length etl' == IList.length etl =>
      let instr' = Sil.Call ret_ids (Exp.Const (Const.Cfun pn)) etl loc_call cf;
      found instr instr'
    | (Sil.Call ret_ids' (Exp.Const (Const.Cfun pn)) etl' _ cf, _, _)
        when
          IList.length ret_ids == IList.length ret_ids' &&
            IList.length etl' + 1 == IList.length etl =>
      let etl1 =
        switch (IList.rev etl) {
        /* remove last element */
        | [_, ...l] => IList.rev l
        | [] => assert false
        };
      let instr' = Sil.Call ret_ids (Exp.Const (Const.Cfun pn)) etl1 loc_call cf;
      found instr instr'
    | _ => ()
    };
  Procdesc.iter_instrs do_instr proc_desc;
  !modified
};


/** Find synthetic (access or bridge) Java methods in the procedure and inline them in the cfg. */
let proc_inline_synthetic_methods cfg proc_desc :unit => {
  let instr_inline_synthetic_method =
    fun
    | Sil.Call ret_ids (Exp.Const (Const.Cfun pn)) etl loc _ =>
      switch (Procdesc.find_from_name cfg pn) {
      | Some pd =>
        let is_access = Procname.java_is_access_method pn;
        let attributes = Procdesc.get_attributes pd;
        let is_synthetic = attributes.ProcAttributes.is_synthetic_method;
        let is_bridge = attributes.ProcAttributes.is_bridge_method;
        if (is_access || is_bridge || is_synthetic) {
          inline_synthetic_method ret_ids etl pd loc
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
  Procdesc.iter_nodes node_inline_synthetic_methods proc_desc
};


/** Inline the java synthetic methods in the cfg */
let inline_java_synthetic_methods cfg => {
  let f proc_name proc_desc =>
    if (Procname.is_java proc_name) {
      proc_inline_synthetic_methods cfg proc_desc
    };
  iter_proc_desc cfg f
};


/** Save a cfg into a file */
let store_cfg_to_file save_sources::save_sources=true (filename: DB.filename) (cfg: cfg) => {
  inline_java_synthetic_methods cfg;
  if save_sources {
    save_source_files cfg
  };
  if Config.incremental_procs {
    switch (load_cfg_from_file filename) {
    | Some old_cfg => Node.mark_unchanged_pdescs cfg old_cfg
    | None => ()
    }
  };
  save_attributes cfg;
  Serialization.to_file cfg_serializer filename cfg
};


/** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types */
let specialize_types callee_proc_desc resolved_proc_name args =>
  /** TODO (#9333890): This currently only works when the callee is defined in the same file.
      Add support to search for the callee procedure description in the execution environment */
  {
    let callee_attributes = Procdesc.get_attributes callee_proc_desc;
    let resolved_formals =
      IList.fold_left2
        (fun accu (name, _) (_, arg_typ) => [(name, arg_typ), ...accu])
        []
        callee_attributes.ProcAttributes.formals
        args |> IList.rev;
    let resolved_attributes = {
      ...callee_attributes,
      ProcAttributes.formals: resolved_formals,
      proc_name: resolved_proc_name
    };
    AttributesTable.store_attributes resolved_attributes;
    Procdesc.specialize_types callee_proc_desc resolved_attributes resolved_formals
  };
