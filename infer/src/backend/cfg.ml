(*
* Copyright (c) 2009 - 2013 Monoidics ltd.
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

module L = Logging
module F = Format

open Utils (* No abbreviation for Utils, as every module can depend on it *)

(* ============== START of ADT node and proc_desc ============== *)

(* =============== START of module Node =============== *)
module Node = struct
  type nodekind =
    | Start_node of proc_desc
    | Exit_node of proc_desc
    | Stmt_node of string
    | Join_node
    | Prune_node of bool * Sil.if_kind * string (** (true/false branch, if_kind, comment) *)
    | Skip_node of string

  and t = { (** a node *)
    nd_id : int; (** unique id of the node *)
    mutable nd_dist_exit : int option; (** distance to the exit node *)
    mutable nd_temps : Ident.t list; (** temporary variables *)
    mutable nd_dead_pvars_after : Sil.pvar list; (** dead program variables after executing the instructions *)
    mutable nd_dead_pvars_before : Sil.pvar list; (** dead program variables before executing the instructions *)
    mutable nd_exn : t list; (** exception nodes in the cfg *)
    mutable nd_instrs : Sil.instr list; (** instructions for symbolic execution *)
    mutable nd_kind : nodekind; (** kind of node *)
    mutable nd_loc : Sil.location; (** location in the source code *)
    mutable nd_preds : t list; (** predecessor nodes in the cfg *)
    mutable nd_proc : proc_desc option; (** proc desc from cil *)
    mutable nd_succs : t list; (** successor nodes in the cfg *)
  }
  and proc_desc = { (** procedure description *)
    pd_attributes : Sil.proc_attributes; (** attributes of the procedure *)
    pd_id : int; (** unique proc_desc identifier *)
    pd_name : Procname.t; (** name of the procedure *)
    pd_is_defined : bool; (** true iff the procedure is defined, and not just declared *)
    pd_ret_type : Sil.typ; (** return type *)
    pd_formals : (string * Sil.typ) list; (** name and type of formal parameters *)
    mutable pd_locals : (Mangled.t * Sil.typ) list; (** name and type of local variables *)
    pd_captured : (Mangled.t * Sil.typ) list; (** name and type of blocks' captured variables *)
    mutable pd_nodes : t list; (** list of nodes of this procedure *)
    mutable pd_start_node : t; (** start node of this procedure *)
    mutable pd_exit_node : t; (** exit node of ths procedure *)
    mutable pd_loc : Sil.location; (** location of this procedure in the source code *)
    mutable pd_flags : proc_flags; (** flags for the procedure *)
    pd_err_log: Errlog.t; (** error log at translation time *)
  }

  let exn_handler_kind = Stmt_node "exception handler"
  let exn_sink_kind = Stmt_node "exceptions sink"
  let throw_kind = Stmt_node "throw"

  type cfg = (** data type for the control flow graph *)
    { node_id : int ref;
      node_list : t list ref;
      name_pdesc_tbl : proc_desc Procname.Hash.t;  (** Map proc name to procdesc *)
      mutable priority_set : Procname.Set.t (** set of function names to be analyzed first *) }

  let create_cfg () = (** create a new empty cfg *)
    { node_id = ref 0;
      node_list = ref [];
      name_pdesc_tbl = Procname.Hash.create 1000;
      priority_set = Procname.Set.empty }

  let compute_enabled_verbose = false

  (** restrict the cfg to the given enabled (active and not shadowed) procedures *)
  let cfg_restrict_enabled cfg source enabled =
    match enabled with
    | None -> ()
    | Some enabled_procs ->
        if compute_enabled_verbose then L.err "cfg_restrict_enabled: checking enabled in %s@." (DB.source_file_to_string source);
        let is_enabled pname = Procname.Set.mem pname enabled_procs in
        let in_address_set pname = Procname.Set.mem pname cfg.priority_set in
        let node_list' =
          let filter_node node = match node.nd_proc with
            | None -> true
            | Some pdesc -> is_enabled pdesc.pd_name in
          list_filter filter_node !(cfg.node_list) in
        let procs_to_remove =
          let psetr = ref Procname.Set.empty in
          let do_proc pname pdesc =
            if pdesc.pd_is_defined && not (is_enabled pname) && not (in_address_set pname) then psetr := Procname.Set.add pname !psetr in
          Procname.Hash.iter do_proc cfg.name_pdesc_tbl;
          !psetr in
        let remove_proc pname =
          if compute_enabled_verbose then L.err "cfg_restrict_enabled: Removing proc not enabled from the cfg: %s@." (Procname.to_filename pname);
          Procname.Hash.remove cfg.name_pdesc_tbl pname in
        cfg.node_list := node_list';
        Procname.Set.iter remove_proc procs_to_remove

  let node_id_gen cfg = incr cfg.node_id; !(cfg.node_id)

  let pdesc_tbl_add cfg proc_name proc_desc =
    Procname.Hash.add cfg.name_pdesc_tbl proc_name proc_desc

  let pdesc_tbl_remove cfg proc_name =
    Procname.Hash.remove cfg.name_pdesc_tbl proc_name

  let pdesc_tbl_find cfg proc_name =
    Procname.Hash.find cfg.name_pdesc_tbl proc_name

  let proc_name_to_proc_desc cfg proc_name =
    pdesc_tbl_find cfg proc_name

  let iter_proc_desc cfg f =
    Procname.Hash.iter f cfg.name_pdesc_tbl

  let iter_types cfg f =
    let node_iter_types node =
      list_iter (Sil.instr_iter_types f) node.nd_instrs in
    let pdesc_iter_types pname pdesc =
      Sil.typ_iter_types f pdesc.pd_ret_type;
      list_iter (fun (_, t) -> Sil.typ_iter_types f t) pdesc.pd_formals;
      list_iter (fun (_, t) -> Sil.typ_iter_types f t) pdesc.pd_locals;
      list_iter node_iter_types pdesc.pd_nodes in
    iter_proc_desc cfg pdesc_iter_types

  let dummy () = {
    nd_id = 0;
    nd_dist_exit = None;
    nd_temps = [];
    nd_dead_pvars_after = [];
    nd_dead_pvars_before = [];
    nd_instrs = [];
    nd_kind = Skip_node "dummy";
    nd_loc = Sil.loc_none;
    nd_proc = None;
    nd_succs = []; nd_preds = []; nd_exn = [];
  }

  let compare node1 node2 =
    int_compare node1.nd_id node2.nd_id

  let hash node =
    Hashtbl.hash node.nd_id

  let equal node1 node2 =
    (compare node1 node2 = 0)

  let get_all_nodes cfg = !(cfg.node_list)

  let create cfg loc kind instrs pdesc temps =
    let node_id = node_id_gen cfg in
    let node =
      { nd_id = node_id;
        nd_dist_exit = None;
        nd_temps = temps;
        nd_dead_pvars_after = [];
        nd_dead_pvars_before = [];
        nd_instrs = instrs;
        nd_kind = kind;
        nd_loc = loc;
        nd_preds = [];
        nd_proc = Some pdesc;
        nd_succs = [];
        nd_exn = []
      } in
    cfg.node_list := node :: !(cfg.node_list);
    pdesc.pd_nodes <- node :: pdesc.pd_nodes;
    node

  (** Get the unique id of the node *)
  let get_id node = node.nd_id

  let get_succs node = node.nd_succs

  type node = t
  module NodeSet = Set.Make(struct
      type t = node
      let compare = compare
    end)

  let get_sliced_succs node f =
    let visited = ref NodeSet.empty in
    let rec slice_nodes nodes : NodeSet.t =
      let do_node acc n =
        visited := NodeSet.add n !visited;
        if f n then NodeSet.singleton n
        else NodeSet.union acc (slice_nodes (list_filter (fun s -> not (NodeSet.mem s !visited)) n.nd_succs)) in
      list_fold_left do_node NodeSet.empty nodes in
    NodeSet.elements (slice_nodes node.nd_succs)

  let get_sliced_preds node f =
    let visited = ref NodeSet.empty in
    let rec slice_nodes nodes : NodeSet.t =
      let do_node acc n =
        visited := NodeSet.add n !visited;
        if f n then NodeSet.singleton n
        else NodeSet.union acc (slice_nodes (list_filter (fun s -> not (NodeSet.mem s !visited)) n.nd_preds)) in
      list_fold_left do_node NodeSet.empty nodes in
    NodeSet.elements (slice_nodes node.nd_preds)

  let get_exn node = node.nd_exn

  let set_proc_desc node proc = node.nd_proc <- Some proc

  (** Set the successor nodes and exception nodes, and build predecessor links *)
  let set_succs_exn node succs exn =
    node.nd_succs <- succs;
    node.nd_exn <- exn;
    list_iter (fun n -> n.nd_preds <- (node :: n.nd_preds)) succs

  (** Get the predecessors of the node *)
  let get_preds node = node.nd_preds

  (** Generates a list of nodes starting at a given node and recursively adding the results of the generator *)
  let get_generated_slope start_node generator =
    let visited = ref NodeSet.empty in
    let rec nodes n =
      visited := NodeSet.add n !visited;
      let succs = list_filter (fun n -> not (NodeSet.mem n !visited)) (generator n) in
      match list_length succs with
      | 1 -> n:: (nodes (list_hd succs))
      | _ -> [n] in
    nodes start_node

  (** Get the node kind *)
  let get_kind node = node.nd_kind

  (** Set the node kind *)
  let set_kind node kind = node.nd_kind <- kind

  (** Comparison for node kind *)
  let kind_compare k1 k2 = match k1, k2 with
    | Start_node pd1, Start_node pd2 ->
        int_compare pd1.pd_id pd2.pd_id
    | Start_node _, _ -> -1
    | _, Start_node _ -> 1
    | Exit_node pd1, Exit_node pd2 ->
        int_compare pd1.pd_id pd2.pd_id
    | Exit_node _, _ -> -1
    | _, Exit_node _ -> 1
    | Stmt_node s1, Stmt_node s2 ->
        string_compare s1 s2
    | Stmt_node _, _ -> -1
    | _, Stmt_node _ -> 1
    | Join_node, Join_node -> 0
    | Join_node, _ -> -1
    | _, Join_node -> 1
    | Prune_node (is_true_branch1, if_kind1, descr1),
    Prune_node (is_true_branch2, if_kind2, descr2) ->
        let n = bool_compare is_true_branch1 is_true_branch2 in
        if n <> 0 then n else let n = Pervasives.compare if_kind1 if_kind2 in
          if n <> 0 then n else string_compare descr1 descr2
    | Prune_node _, _ -> -1
    | _, Prune_node _ -> 1
    | Skip_node s1, Skip_node s2 ->
        string_compare s1 s2

  (** Get the instructions to be executed *)
  let get_instrs node =
    node.nd_instrs

  (** Get the list of callee procnames from the node *)
  let get_callees node =
    let collect callees instr =
      match instr with
      | Sil.Call (_, exp, _, _, _) ->
          begin
            match exp with
            | Sil.Const (Sil.Cfun procname) -> procname:: callees
            | _ -> callees
          end
      | _ -> callees in
    list_fold_left collect [] (get_instrs node)

  (** Get the location of the node *)
  let get_loc n = n.nd_loc

  (** Get the source location of the last instruction in the node *)
  let get_last_loc n =
    match list_rev (get_instrs n) with
    | instr :: _ -> Sil.instr_get_loc instr
    | [] -> n.nd_loc

  (** Set the location of the node *)
  let set_loc n loc = n.nd_loc <- loc

  let pp f node =
    F.fprintf f "%n" (get_id node)

  (** Get the proc desc of the node *)
  let get_proc_desc node =
    let proc_desc = match node.nd_proc with
      | None ->
          L.out "node_get_proc_desc: at node %d@\n" node.nd_id;
          assert false
      | Some proc_desc -> proc_desc in
    proc_desc

  let proc_desc_from_name cfg proc_name =
    try Some (pdesc_tbl_find cfg proc_name)
    with Not_found -> None

  (** Set the proc desc of the node *)
  let node_set_proc_desc pdesc node =
    node.nd_proc <- Some pdesc

  let set_temps node temps =
    node.nd_temps <- temps

  let get_temps node =
    node.nd_temps

  let set_dead_pvars node after dead =
    if after then node.nd_dead_pvars_after <- dead
    else node.nd_dead_pvars_before <- dead

  let get_dead_pvars node after =
    if after then node.nd_dead_pvars_after
    else node.nd_dead_pvars_before

  let get_distance_to_exit node =
    node.nd_dist_exit

  (** Append the instructions and temporaries to the list of instructions to execute *)
  let append_instrs_temps node instrs temps =
    node.nd_instrs <- node.nd_instrs @ instrs;
    node.nd_temps <- node.nd_temps @ temps

  (** Add the instructions and temporaties at the beginning of the list of instructions to execute *)
  let prepend_instrs_temps node instrs temps =
    node.nd_instrs <- instrs @ node.nd_instrs;
    node.nd_temps <- temps @ node.nd_temps

  (** Replace the instructions to be executed. *)
  let replace_instrs node instrs =
    node.nd_instrs <- instrs

  let proc_desc_get_ret_var pdesc =
    Sil.get_ret_pvar pdesc.pd_name

  (** Add declarations for local variables and return variable to the node *)
  let add_locals_ret_declaration node locals =
    let loc = get_loc node in
    let pdesc = get_proc_desc node in
    let proc_name = pdesc.pd_name in
    let ret_var =
      let ret_type = pdesc.pd_ret_type in
      (proc_desc_get_ret_var pdesc, ret_type) in
    let construct_decl (x, typ) =
      (Sil.mk_pvar x proc_name, typ) in
    let ptl = ret_var :: list_map construct_decl locals in
    let instr = Sil.Declare_locals (ptl, loc) in
    prepend_instrs_temps node [instr] []

  (** Counter for identifiers of procdescs *)
  let proc_desc_id_counter = ref 0

  let remove_node' filter_out_fun cfg node =
    let remove_node_in_cfg nodes =
      list_filter filter_out_fun nodes in
    cfg.node_list := remove_node_in_cfg !(cfg.node_list)

  let remove_node cfg node =
    remove_node' (fun node' -> not (equal node node'))
      cfg node

  let remove_node_set cfg nodes =
    remove_node' (fun node' -> not (NodeSet.mem node' nodes))
      cfg nodes

  let proc_desc_remove cfg name remove_nodes =
    (if remove_nodes then
        let pdesc = pdesc_tbl_find cfg name in
        let proc_nodes =
          list_fold_right (fun node set -> NodeSet.add node set)
            pdesc.pd_nodes NodeSet.empty in
        remove_node_set cfg proc_nodes);
    pdesc_tbl_remove cfg name

  let proc_desc_get_start_node proc_desc =
    proc_desc.pd_start_node

  let proc_desc_get_err_log proc_desc =
    proc_desc.pd_err_log

  let proc_desc_get_attributes proc_desc =
    proc_desc.pd_attributes

  let proc_desc_get_exit_node proc_desc =
    proc_desc.pd_exit_node

  (** Compute the distance of each node to the exit node, if not computed already *)
  let proc_desc_compute_distance_to_exit_node proc_desc =
    let exit_node = proc_desc.pd_exit_node in
    let rec mark_distance dist nodes =
      let next_nodes = ref [] in
      let do_node node =
        match node.nd_dist_exit with
        | Some _ -> ()
        | None ->
            node.nd_dist_exit <- Some dist;
            next_nodes := node.nd_preds @ !next_nodes in
      list_iter do_node nodes;
      if !next_nodes != [] then mark_distance (dist + 1) !next_nodes in
    mark_distance 0 [exit_node]

  (** Set the start node of the proc desc *)
  let proc_desc_set_start_node pdesc node =
    pdesc.pd_start_node <- node

  (** Set the exit node of the proc desc *)
  let proc_desc_set_exit_node pdesc node =
    pdesc.pd_exit_node <- node

  (** Set a flag for the proc desc *)
  let proc_desc_set_flag pdesc key value =
    proc_flags_add pdesc.pd_flags key value

  (** Return the return type of the procedure *)
  let proc_desc_get_ret_type proc_desc =
    proc_desc.pd_ret_type

  let proc_desc_get_proc_name proc_desc =
    proc_desc.pd_name

  (** Return [true] iff the procedure is defined, and not just declared *)
  let proc_desc_is_defined proc_desc =
    proc_desc.pd_is_defined

  let proc_desc_get_loc proc_desc =
    proc_desc.pd_loc

  (** Return name and type of formal parameters *)
  let proc_desc_get_formals proc_desc =
    proc_desc.pd_formals

  (** Return name and type of local variables *)
  let proc_desc_get_locals proc_desc =
    proc_desc.pd_locals

  (** Return name and type of captured variables *)
  let proc_desc_get_captured proc_desc =
    proc_desc.pd_captured

  let proc_desc_get_nodes proc_desc =
    proc_desc.pd_nodes

  (** List of nodes in the procedure up to the first branching *)
  let proc_desc_get_slope proc_desc =
    get_generated_slope (proc_desc_get_start_node proc_desc) get_succs

  (** List of nodes in the procedure sliced by a predicate up to the first branching *)
  let proc_desc_get_sliced_slope proc_desc f =
    get_generated_slope (proc_desc_get_start_node proc_desc) (fun n -> get_sliced_succs n f)

  (** Get flags for the proc desc *)
  let proc_desc_get_flags proc_desc =
    proc_desc.pd_flags

  (** Append the locals to the list of local variables *)
  let proc_desc_append_locals proc_desc new_locals =
    proc_desc.pd_locals <- proc_desc.pd_locals @ new_locals

  (** Get the cyclomatic complexity for the procedure *)
  let proc_desc_get_cyclomatic proc_desc =
    let num_edges = ref 0 in
    let num_nodes = ref 0 in
    let num_connected = 1 in (* always one connected component in a procedure's cfg *)
    let nodes = proc_desc_get_nodes proc_desc in
    let do_node node =
      incr num_nodes;
      num_edges := !num_edges + list_length (get_succs node) in
    list_iter do_node nodes;
    let cyclo = !num_edges - !num_nodes + 2 * num_connected in (* formula for cyclomatic complexity *)
    cyclo

  (** Print extended instructions for the node, highlighting the given subinstruction if present *)
  let pp_instr pe0 ~sub_instrs instro fmt node =
    let pe = match instro with
      | None -> pe0
      | Some instr -> pe_extend_colormap pe0 (Obj.repr instr) Red in
    let instrs = get_instrs node in
    let pp_loc fmt () = F.fprintf fmt " %a " Sil.pp_loc (get_loc node) in
    let print_sub_instrs () = F.fprintf fmt "%a" (Sil.pp_instr_list pe) instrs in
    match get_kind node with
    | Stmt_node s ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "statements (%s) %a" s pp_loc ()
    | Prune_node (is_true_branch, if_kind, descr) ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "assume %s %a" descr pp_loc ()
    | Exit_node _ ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "exit %a" pp_loc ()
    | Skip_node s ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "skip (%s) %a" s pp_loc ()
    | Start_node _ ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "start %a" pp_loc ()
    | Join_node ->
        if sub_instrs then print_sub_instrs ()
        else F.fprintf fmt "join %a" pp_loc ()

  (** Dump extended instructions for the node *)
  let d_instrs ~(sub_instrs: bool) (curr_instr: Sil.instr option) (node: t) =
    L.add_print_action (L.PTnode_instrs, Obj.repr (sub_instrs, curr_instr, node))

  (** Return a description of the cfg node *)
  let get_description pe node =
    let str =
      match get_kind node with
      | Stmt_node _ ->
          "Instructions"
      | Prune_node (is_true_branch, if_kind, descr) ->
          "Conditional" ^ " " ^ descr
      | Exit_node _ ->
          "Exit"
      | Skip_node s ->
          "Skip"
      | Start_node _ ->
          "Start"
      | Join_node ->
          "Join" in
    let pp fmt () = F.fprintf fmt "%s\n%a@?" str (pp_instr pe None ~sub_instrs: true) node in
    pp_to_string pp ()

  let proc_desc_iter_nodes f proc_desc =
    list_iter f (list_rev (proc_desc_get_nodes proc_desc))

  let proc_desc_fold_nodes f acc proc_desc =
    (*list_fold_left (fun acc node -> f acc node) acc (list_rev (proc_desc_get_nodes proc_desc))*)
    list_fold_left f acc (list_rev (proc_desc_get_nodes proc_desc))

  (** iterate over the calls from the procedure: (callee,location) pairs *)
  let proc_desc_iter_calls f pdesc =
    let do_node node =
      list_iter
        (fun callee_pname -> f (callee_pname, get_loc node))
        (get_callees node) in
    list_iter do_node (proc_desc_get_nodes pdesc)

  let proc_desc_iter_slope f proc_desc =
    let visited = ref NodeSet.empty in
    let rec do_node node = begin
        visited := NodeSet.add node !visited;
        f node;
        match get_succs node with
        | [n] -> if not (NodeSet.mem n !visited) then do_node n
        | _ -> ()
      end in
    do_node (proc_desc_get_start_node proc_desc)

  (** iterate between two nodes or until we reach a branching structure *)
  let proc_desc_iter_slope_range f proc_desc src_node dst_node =
    let visited = ref NodeSet.empty in
    let rec do_node node = begin
        visited := NodeSet.add node !visited;
        f node;
        match get_succs node with
        | [n] ->
            if not (NodeSet.mem n !visited)
            && not (equal node dst_node)
            then do_node n
        | _ -> ()
      end in
    do_node src_node

  let proc_desc_iter_slope_calls f proc_desc =
    let do_node node =
      list_iter
        (fun callee_pname -> f callee_pname)
        (get_callees node) in
    proc_desc_iter_slope do_node proc_desc

  let proc_desc_iter_instrs f proc_desc =
    let do_node node =
      list_iter (fun i -> f node i) (get_instrs node) in
    proc_desc_iter_nodes do_node proc_desc

  let proc_desc_fold_instrs f acc proc_desc =
    let fold_node acc node =
      list_fold_left (fun acc instr -> f acc node instr) acc (get_instrs node) in
    proc_desc_fold_nodes fold_node acc proc_desc

end
(* =============== END of module Node =============== *)

type node = Node.t
type cfg = Node.cfg

(** Serializer for control flow graphs *)
let cfg_serializer : cfg Serialization.serializer = Serialization.create_serializer Serialization.cfg_key

(** Load a cfg from a file *)
let load_cfg_from_file (filename : DB.filename) : cfg option =
  Serialization.from_file cfg_serializer filename

(** save a copy in the results dir of the source files of procedures defined in the cfg, unless an updated copy already exists *)
let save_source_files cfg =
  let process_proc pname pdesc =
    let loc = Node.proc_desc_get_loc pdesc in
    let source_file = loc.Sil.file in
    let source_file_str = DB.source_file_to_abs_path source_file in
    let dest_file = DB.source_file_in_resdir source_file in
    let dest_file_str = DB.filename_to_string dest_file in
    let needs_copy =
      Node.proc_desc_is_defined pdesc &&
      Sys.file_exists source_file_str &&
      (not (Sys.file_exists dest_file_str) ||
        DB.file_modified_time (DB.filename_from_string source_file_str) > DB.file_modified_time dest_file) in
    if needs_copy then
      match Utils.copy_file source_file_str dest_file_str with
      | Some _ -> ()
      | None -> L.err "Error cannot create copy of source file %s@." source_file_str in
  Node.iter_proc_desc cfg process_proc

(** Save a cfg into a file *)
let store_cfg_to_file (filename : DB.filename) (save_sources : bool) (cfg: cfg) =
  if save_sources then save_source_files cfg;
  Serialization.to_file cfg_serializer filename cfg

(* =============== START of module Procdesc =============== *)
module Procdesc = struct
  type t = Node.proc_desc
  let compute_distance_to_exit_node = Node.proc_desc_compute_distance_to_exit_node

  type proc_desc_builder =
    { cfg : cfg;
      name: Procname.t;
      is_defined : bool; (** is defined and not just declared *)
      proc_attributes : Sil.proc_attributes;
      ret_type : Sil.typ; (** return type *)
      formals : (string * Sil.typ) list;
      locals : (Mangled.t * Sil.typ) list;
      captured : (Mangled.t * Sil.typ) list; (** variables captured in an ObjC block *)
      loc : Sil.location;
    }

  let create (b : proc_desc_builder) =
    let open Node in
    incr proc_desc_id_counter;
    let pdesc =
      {
        pd_attributes = b.proc_attributes;
        pd_id = !proc_desc_id_counter;
        pd_name = b.name;
        pd_is_defined = b.is_defined;
        pd_ret_type = b.ret_type;
        pd_formals = b.formals;
        pd_locals = b.locals;
        pd_captured = b.captured;
        pd_nodes =[];
        pd_start_node = dummy ();
        pd_exit_node = dummy ();
        pd_loc = b.loc;
        pd_flags = proc_flags_empty ();
        pd_err_log = Errlog.empty ();
      } in
    pdesc_tbl_add b.cfg b.name pdesc;
    pdesc

  let remove = Node.proc_desc_remove
  let find_from_name = Node.proc_desc_from_name
  let get_attributes = Node.proc_desc_get_attributes
  let get_cyclomatic = Node.proc_desc_get_cyclomatic
  let get_err_log = Node.proc_desc_get_err_log
  let get_exit_node = Node.proc_desc_get_exit_node
  let get_flags = Node.proc_desc_get_flags
  let get_formals = Node.proc_desc_get_formals
  let get_loc = Node.proc_desc_get_loc
  let get_locals = Node.proc_desc_get_locals
  let get_captured = Node.proc_desc_get_captured
  let get_nodes = Node.proc_desc_get_nodes
  let get_slope = Node.proc_desc_get_slope
  let get_sliced_slope = Node.proc_desc_get_sliced_slope
  let get_proc_name = Node.proc_desc_get_proc_name
  let get_ret_type = Node.proc_desc_get_ret_type
  let get_ret_var pdesc = Sil.mk_pvar Ident.name_return (get_proc_name pdesc)
  let get_start_node = Node.proc_desc_get_start_node
  let is_defined = Node.proc_desc_is_defined
  let iter_nodes = Node.proc_desc_iter_nodes
  let iter_calls = Node.proc_desc_iter_calls
  let iter_instrs = Node.proc_desc_iter_instrs
  let fold_instrs = Node.proc_desc_fold_instrs
  let iter_slope = Node.proc_desc_iter_slope
  let iter_slope_calls = Node.proc_desc_iter_slope_calls
  let iter_slope_range = Node.proc_desc_iter_slope_range
  let set_exit_node = Node.proc_desc_set_exit_node
  let set_flag = Node.proc_desc_set_flag
  let set_start_node = Node.proc_desc_set_start_node
  let append_locals = Node.proc_desc_append_locals
end

(* =============== END of module Procdesc =============== *)

(** Hash table with nodes as keys. *)
module NodeHash = Hashtbl.Make(Node)

(** Set of nodes. *)
module NodeSet = Node.NodeSet

let iter_proc_desc = Node.iter_proc_desc

(** Iterate over all the types (and subtypes) in the CFG *)
let iter_types = Node.iter_types

let rec pp_node_list f = function
  | [] -> ()
  | [node] -> Node.pp f node
  | node:: nodes ->
      F.fprintf f "%a, %a" Node.pp node pp_node_list nodes

(** Get all the procdescs (defined and declared) *)
let get_all_procs cfg =
  let procs = ref [] in
  let f pname pdesc = procs := pdesc :: !procs in
  iter_proc_desc cfg f; !procs

(** Get the procedures whose body is defined in this cfg *)
let get_defined_procs cfg =
  list_filter Procdesc.is_defined (get_all_procs cfg)

(** Get the objc procedures whose body is generated *)
let get_objc_generated_procs cfg =
  list_filter (
      fun procdesc ->
          (Procdesc.get_attributes procdesc).Sil.is_generated) (get_all_procs cfg)

(** get the function names which should be analyzed before the other ones *)
let get_priority_procnames cfg =
  cfg.Node.priority_set

(** set the function names whose address has been taken in this file *)
let set_procname_priority cfg pname =
  cfg.Node.priority_set <- Procname.Set.add pname cfg.Node.priority_set

(** add instructions to remove temporaries *)
let add_removetemps_instructions cfg =
  let all_nodes = Node.get_all_nodes cfg in
  let do_node node =
    let loc = Node.get_last_loc node in
    let temps = Node.get_temps node in
    if temps != [] then Node.append_instrs_temps node [Sil.Remove_temps (temps, loc)] [] in
  list_iter do_node all_nodes

(** add instructions to perform abstraction *)
let add_abstraction_instructions cfg =
  let converging_node node = (* true if there is a succ node s.t.: it is an exit node, or the succ of >1 nodes *)
    let is_exit node = match Node.get_kind node with
      | Node.Exit_node _ -> true
      | _ -> false in
    let succ_nodes = Node.get_succs node in
    if list_exists is_exit succ_nodes then true
    else match succ_nodes with
      | [] -> false
      | [h] -> list_length (Node.get_preds h) > 1
      | _ -> false in
  let node_requires_abstraction node =
    match Node.get_kind node with
    | Node.Start_node _
    | Node.Join_node ->
        false
    | Node.Exit_node _
    | Node.Stmt_node _
    | Node.Prune_node _
    | Node.Skip_node _ ->
        converging_node node in
  let all_nodes = Node.get_all_nodes cfg in
  let do_node node =
    let loc = Node.get_last_loc node in
    if node_requires_abstraction node then Node.append_instrs_temps node [Sil.Abstract loc] [] in
  list_iter do_node all_nodes

let get_name_of_parameter (curr_f : Procdesc.t) (x, typ) =
  Sil.mk_pvar (Mangled.from_string x) (Procdesc.get_proc_name curr_f)

let get_name_of_local (curr_f : Procdesc.t) (x, typ) =
  Sil.mk_pvar x (Procdesc.get_proc_name curr_f)

(* returns a list of local static variables (ie local variables defined static) in a proposition *)
let get_name_of_objc_static_locals (curr_f : Procdesc.t) p =
  let pname = Procname.to_string (Procdesc.get_proc_name curr_f) in
  let local_static e =
    match e with (* is a local static if it's a global and it has a static local name *)
    | Sil.Lvar pvar when (Sil.pvar_is_global pvar) && (Sil.is_static_local_name pname pvar) -> [pvar]
    | _ -> [] in
  let hpred_local_static hpred =
    match hpred with
    | Sil.Hpointsto(e, _, _) -> [local_static e]
    | _ -> [] in
  let vars_sigma = list_map hpred_local_static (Prop.get_sigma p) in
  list_flatten (list_flatten vars_sigma)

(* returns a list of local variables that points to an objc block in a proposition *)
let get_name_of_objc_block_locals p =
  let local_blocks e =
    match e with
    | Sil.Lvar pvar when (Sil.is_block_pvar pvar) ->
        [pvar]
    | _ -> [] in
  let hpred_local_blocks hpred =
    match hpred with
    | Sil.Hpointsto(e, _, _) -> [local_blocks e]
    | _ -> [] in
  let vars_sigma = list_map hpred_local_blocks (Prop.get_sigma p) in
  list_flatten (list_flatten vars_sigma)

let remove_abducted_retvars p =
  (* compute the hpreds and pure atoms reachable from the set of seed expressions in [exps] *)
  let compute_reachable p seed_exps =
    let sigma, pi = Prop.get_sigma p, Prop.get_pi p in
    let rec collect_exps exps = function
      | Sil.Eexp (Sil.Const (Sil.Cexn e), _) -> Sil.ExpSet.add e exps
      | Sil.Eexp (e, _) -> Sil.ExpSet.add e exps
      | Sil.Estruct (flds, _) ->
          list_fold_left (fun exps (_, strexp) -> collect_exps exps strexp) exps flds

      | Sil.Earray (_, elems, _) ->
          list_fold_left (fun exps (index, strexp) -> collect_exps exps strexp) exps elems in
    let rec compute_reachable_hpreds_rec sigma (reach, exps) =
      let add_hpred_if_reachable (reach, exps) = function
        | Sil.Hpointsto (lhs, rhs, _) as hpred when Sil.ExpSet.mem lhs exps ->
            let reach' = Sil.HpredSet.add hpred reach in
            let exps' = collect_exps exps rhs in
            (reach', exps')
        | _ -> reach, exps in
      let reach', exps' = list_fold_left add_hpred_if_reachable (reach, exps) sigma in
      if (Sil.HpredSet.cardinal reach) = (Sil.HpredSet.cardinal reach') then (reach, exps)
      else compute_reachable_hpreds_rec sigma (reach', exps') in
    let reach_hpreds, reach_exps =
      compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, seed_exps) in
    (* filter away the pure atoms without reachable exps *)
    let reach_pi =
      let rec exp_contains = function
        | exp when Sil.ExpSet.mem exp reach_exps -> true
        | Sil.UnOp (_, e, _) | Sil.Cast (_, e) | Sil.Lfield (e, _, _) -> exp_contains e
        | Sil.BinOp (_, e0, e1) | Sil.Lindex (e0, e1) -> exp_contains e0 || exp_contains e1
        | _ -> false in
      list_filter
        (function
          | Sil.Aeq (lhs, rhs) | Sil.Aneq (lhs, rhs) -> exp_contains lhs || exp_contains rhs)
        pi in
    Sil.HpredSet.elements reach_hpreds, reach_pi in
  (* separate the abducted pvars from the normal ones, deallocate the abducted ones*)
  let abducted_pvars, normal_pvars =
    list_fold_left
      (fun pvars hpred ->
            match hpred with
            | Sil.Hpointsto (Sil.Lvar pvar, _, _) ->
                let abducted_pvars, normal_pvars = pvars in
                if Sil.pvar_is_abducted pvar then pvar :: abducted_pvars, normal_pvars
                else abducted_pvars, pvar :: normal_pvars
            | _ -> pvars)
      ([], [])
      (Prop.get_sigma p) in
  let _, p' = Prop.deallocate_stack_vars p abducted_pvars in
  let normal_pvar_set =
    list_fold_left
      (fun normal_pvar_set pvar -> Sil.ExpSet.add (Sil.Lvar pvar) normal_pvar_set)
      Sil.ExpSet.empty
      normal_pvars in
  (* walk forward from non-abducted pvars, keep everything reachable. remove everything else *)
  let sigma_reach, pi_reach = compute_reachable p' normal_pvar_set in
  Prop.normalize (Prop.replace_pi pi_reach (Prop.replace_sigma sigma_reach p'))

let remove_locals (curr_f : Procdesc.t) p =
  let names_of_locals = list_map (get_name_of_local curr_f) (Procdesc.get_locals curr_f) in
  let names_of_locals' = match !Sil.curr_language with
    | Sil.C_CPP -> (* in ObjC to deal with block we need to remove static locals *)
        let names_of_static_locals = get_name_of_objc_static_locals curr_f p in
        let names_of_block_locals = get_name_of_objc_block_locals p in
        names_of_block_locals @ names_of_locals @ names_of_static_locals
    | _ -> names_of_locals in
  let removed, p' = Prop.deallocate_stack_vars p names_of_locals' in
  (removed, if !Config.angelic_execution then remove_abducted_retvars p' else p')

let remove_formals (curr_f : Procdesc.t) p =
  let names_of_formals = list_map (get_name_of_parameter curr_f) (Procdesc.get_formals curr_f) in
  Prop.deallocate_stack_vars p names_of_formals

(** remove the return variable from the prop *)
let remove_ret (curr_f : Procdesc.t) (p: Prop.normal Prop.t) =
  let pname = Procdesc.get_proc_name curr_f in
  let name_of_ret = Procdesc.get_ret_var curr_f in
  let _, p' = Prop.deallocate_stack_vars p [(Sil.pvar_to_callee pname name_of_ret)] in
  p'

(** remove locals and return variable from the prop *)
let remove_locals_ret (curr_f : Procdesc.t) p =
  snd (remove_locals curr_f (remove_ret curr_f p))

(** Remove locals and formal parameters from the prop.
Return the list of stack variables whose address was still present after deallocation. *)
let remove_locals_formals (curr_f : Procdesc.t) p =
  let pvars1, p1 = remove_formals curr_f p in
  let pvars2, p2 = remove_locals curr_f p1 in
  pvars1 @ pvars2, p2

(** remove seed vars from a prop *)
let remove_seed_vars (prop: 'a Prop.t) : Prop.normal Prop.t =
  let hpred_not_seed = function
    | Sil.Hpointsto(Sil.Lvar pv, _, _) -> not (Sil.pvar_is_seed pv)
    | _ -> true in
  let sigma = Prop.get_sigma prop in
  let sigma' = list_filter hpred_not_seed sigma in
  Prop.normalize (Prop.replace_sigma sigma' prop)

(** checks whether a cfg is connected or not *)
let check_cfg_connectedness cfg =
  let is_exit_node n =
    match Node.get_kind n with
    | Node.Exit_node _ -> true
    | _ -> false in
  let broken_node n =
    let succs = Node.get_succs n in
    let preds = Node.get_preds n in
    match Node.get_kind n with
    | Node.Start_node _ -> (list_length succs = 0) || (list_length preds > 0)
    | Node.Exit_node _ -> (list_length succs > 0) || (list_length preds = 0)
    | Node.Stmt_node _ | Node.Prune_node _
    | Node.Skip_node _ -> (list_length succs = 0) || (list_length preds = 0)
    | Node.Join_node ->
    (* Join node has the exception that it may be without predecessors and pointing to an exit node *)
    (* if the if brances end with a return *)
        (match succs with
          | [n'] when is_exit_node n' -> false
          | _ -> (list_length preds = 0)) in
  let do_pdesc pd =
    let pname = Procname.to_string (Procdesc.get_proc_name pd) in
    let nodes = Procdesc.get_nodes pd in
    let broken = list_exists broken_node nodes in
    if broken then
      L.out "\n ***BROKEN CFG: '%s'\n" pname
    else
      L.out "\n ***CONNECTED CFG: '%s'\n" pname in
  let pdescs = get_all_procs cfg in
  list_iter do_pdesc pdescs

(** Given a mangled name of a block return its procdesc if exists*)
let get_block_pdesc cfg block =
  let pdescs = get_defined_procs cfg in
  let is_block_pdesc pd =
    let name = Procdesc.get_proc_name pd in
    (Procname.to_string name) = (Mangled.to_string block) in
  try
    let block_pdesc = list_find is_block_pdesc pdescs in
    Some block_pdesc
  with Not_found -> None

(** Removes seeds variables from a prop corresponding to captured variables in an objc block *)
let remove_seed_captured_vars_block captured_vars prop =
  let is_captured pname vn = Mangled.equal pname vn in
  let hpred_seed_captured = function
    | Sil.Hpointsto(Sil.Lvar pv, _, _) ->
        let pname = Sil.pvar_get_name pv in
        (Sil.pvar_is_seed pv) && (list_mem is_captured pname captured_vars)
    | _ -> false in
  let sigma = Prop.get_sigma prop in
  let sigma' = list_filter (fun hpred -> not (hpred_seed_captured hpred)) sigma in
  Prop.normalize (Prop.replace_sigma sigma' prop)
