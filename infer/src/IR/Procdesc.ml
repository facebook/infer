(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

(* =============== START of module Node =============== *)
module Node = struct
  type id = int [@@deriving compare]

  let equal_id = [%compare.equal : id]

  type nodekind =
    | Start_node of Typ.Procname.t
    | Exit_node of Typ.Procname.t
    | Stmt_node of string
    | Join_node
    | Prune_node of bool * Sil.if_kind * string  (** (true/false branch, if_kind, comment) *)
    | Skip_node of string
    [@@deriving compare]

  let equal_nodekind = [%compare.equal : nodekind]

  (** a node *)
  type t =
    { id: id  (** unique id of the node *)
    ; mutable dist_exit: int option  (** distance to the exit node *)
    ; mutable exn: t list  (** exception nodes in the cfg *)
    ; mutable instrs: Sil.instr list  (** instructions for symbolic execution *)
    ; kind: nodekind  (** kind of node *)
    ; loc: Location.t  (** location in the source code *)
    ; mutable preds: t list  (** predecessor nodes in the cfg *)
    ; pname_opt: Typ.Procname.t option  (** name of the procedure the node belongs to *)
    ; mutable succs: t list  (** successor nodes in the cfg *) }

  let exn_handler_kind = Stmt_node "exception handler"

  let exn_sink_kind = Stmt_node "exceptions sink"

  let throw_kind = Stmt_node "throw"

  let dummy pname_opt =
    { id= 0
    ; dist_exit= None
    ; instrs= []
    ; kind= Skip_node "dummy"
    ; loc= Location.dummy
    ; pname_opt
    ; succs= []
    ; preds= []
    ; exn= [] }

  let compare node1 node2 = Int.compare node1.id node2.id

  let hash node = Hashtbl.hash node.id

  let equal = [%compare.equal : t]

  (** Get the unique id of the node *)
  let get_id node = node.id

  let get_succs node = node.succs

  type node = t

  module NodeSet = Caml.Set.Make (struct
    type t = node

    let compare = compare
  end)

  module IdMap = Caml.Map.Make (struct
    type t = id

    let compare = compare_id
  end)

  let get_sliced_succs node f =
    let visited = ref NodeSet.empty in
    let rec slice_nodes nodes : NodeSet.t =
      let do_node acc n =
        visited := NodeSet.add n !visited ;
        if f n then NodeSet.singleton n
        else
          NodeSet.union acc
            (slice_nodes (List.filter ~f:(fun s -> not (NodeSet.mem s !visited)) n.succs))
      in
      List.fold ~f:do_node ~init:NodeSet.empty nodes
    in
    NodeSet.elements (slice_nodes node.succs)

  let get_sliced_preds node f =
    let visited = ref NodeSet.empty in
    let rec slice_nodes nodes : NodeSet.t =
      let do_node acc n =
        visited := NodeSet.add n !visited ;
        if f n then NodeSet.singleton n
        else
          NodeSet.union acc
            (slice_nodes (List.filter ~f:(fun s -> not (NodeSet.mem s !visited)) n.preds))
      in
      List.fold ~f:do_node ~init:NodeSet.empty nodes
    in
    NodeSet.elements (slice_nodes node.preds)

  let get_exn node = node.exn

  (** Get the name of the procedure the node belongs to *)
  let get_proc_name node =
    match node.pname_opt with
    | None
     -> L.internal_error "get_proc_name: at node %d@\n" node.id ;
        assert false
    | Some pname
     -> pname

  (** Get the predecessors of the node *)
  let get_preds node = node.preds

  (** Generates a list of nodes starting at a given node
      and recursively adding the results of the generator *)
  let get_generated_slope start_node generator =
    let visited = ref NodeSet.empty in
    let rec nodes n =
      visited := NodeSet.add n !visited ;
      let succs = List.filter ~f:(fun n -> not (NodeSet.mem n !visited)) (generator n) in
      match succs with [hd] -> n :: nodes hd | _ -> [n]
    in
    nodes start_node

  (** Get the node kind *)
  let get_kind node = node.kind

  (** Get the instructions to be executed *)
  let get_instrs node = node.instrs

  (** Get the list of callee procnames from the node *)
  let get_callees node =
    let collect callees instr =
      match instr with
      | Sil.Call (_, exp, _, _, _) -> (
        match exp with Exp.Const Const.Cfun procname -> procname :: callees | _ -> callees )
      | _
       -> callees
    in
    List.fold ~f:collect ~init:[] (get_instrs node)

  (** Get the location of the node *)
  let get_loc n = n.loc

  (** Get the source location of the last instruction in the node *)
  let get_last_loc n =
    match List.rev (get_instrs n) with instr :: _ -> Sil.instr_get_loc instr | [] -> n.loc

  let pp_id f id = F.fprintf f "%d" id

  let pp f node = pp_id f (get_id node)

  let get_distance_to_exit node = node.dist_exit

  (** Append the instructions to the list of instructions to execute *)
  let append_instrs node instrs = node.instrs <- node.instrs @ instrs

  (** Add the instructions at the beginning of the list of instructions to execute *)
  let prepend_instrs node instrs = node.instrs <- instrs @ node.instrs

  (** Replace the instructions to be executed. *)
  let replace_instrs node instrs = node.instrs <- instrs

  (** Add declarations for local variables and return variable to the node *)
  let add_locals_ret_declaration node (proc_attributes: ProcAttributes.t) locals =
    let loc = get_loc node in
    let pname = proc_attributes.proc_name in
    let ret_var =
      let ret_type = proc_attributes.ret_type in
      (Pvar.get_ret_pvar pname, ret_type)
    in
    let construct_decl (x, typ) = (Pvar.mk x pname, typ) in
    let ptl = ret_var :: List.map ~f:construct_decl locals in
    let instr = Sil.Declare_locals (ptl, loc) in
    prepend_instrs node [instr]

  (** Print extended instructions for the node,
      highlighting the given subinstruction if present *)
  let pp_instrs pe0 ~sub_instrs instro fmt node =
    let pe =
      match instro with None -> pe0 | Some instr -> Pp.extend_colormap pe0 (Obj.repr instr) Red
    in
    let instrs = get_instrs node in
    let pp_loc fmt () = F.fprintf fmt " %a " Location.pp (get_loc node) in
    let print_sub_instrs () = F.fprintf fmt "%a" (Sil.pp_instr_list pe) instrs in
    match get_kind node with
    | Stmt_node s
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "statements (%s) %a" s pp_loc ()
    | Prune_node (_, _, descr)
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "assume %s %a" descr pp_loc ()
    | Exit_node _
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "exit %a" pp_loc ()
    | Skip_node s
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "skip (%s) %a" s pp_loc ()
    | Start_node _
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "start %a" pp_loc ()
    | Join_node
     -> if sub_instrs then print_sub_instrs () else F.fprintf fmt "join %a" pp_loc ()

  (** Dump extended instructions for the node *)
  let d_instrs ~(sub_instrs: bool) (curr_instr: Sil.instr option) (node: t) =
    L.add_print_action (L.PTnode_instrs, Obj.repr (sub_instrs, curr_instr, node))

  (** Return a description of the cfg node *)
  let get_description pe node =
    let str =
      match get_kind node with
      | Stmt_node _
       -> "Instructions"
      | Prune_node (_, _, descr)
       -> "Conditional" ^ " " ^ descr
      | Exit_node _
       -> "Exit"
      | Skip_node _
       -> "Skip"
      | Start_node _
       -> "Start"
      | Join_node
       -> "Join"
    in
    let pp fmt = F.fprintf fmt "%s@\n%a@?" str (pp_instrs pe None ~sub_instrs:true) node in
    F.asprintf "%t" pp
end

(* =============== END of module Node =============== *)

(** Map over nodes *)
module NodeMap = Caml.Map.Make (Node)

(** Hash table with nodes as keys. *)
module NodeHash = Hashtbl.Make (Node)

(** Set of nodes. *)
module NodeSet = Node.NodeSet

(** Map with node id keys. *)
module IdMap = Node.IdMap

(** procedure description *)
type t =
  { attributes: ProcAttributes.t  (** attributes of the procedure *)
  ; mutable nodes: Node.t list  (** list of nodes of this procedure *)
  ; mutable nodes_num: int  (** number of nodes *)
  ; mutable start_node: Node.t  (** start node of this procedure *)
  ; mutable exit_node: Node.t  (** exit node of ths procedure *)
  ; mutable loop_heads: NodeSet.t option  (** loop head nodes of this procedure *) }
  [@@deriving compare]

(** Only call from Cfg *)
let from_proc_attributes ~called_from_cfg attributes =
  if not called_from_cfg then assert false ;
  let pname_opt = Some attributes.ProcAttributes.proc_name in
  let start_node = Node.dummy pname_opt in
  let exit_node = Node.dummy pname_opt in
  {attributes; nodes= []; nodes_num= 0; start_node; exit_node; loop_heads= None}

(** Compute the distance of each node to the exit node, if not computed already *)
let compute_distance_to_exit_node pdesc =
  let exit_node = pdesc.exit_node in
  let rec mark_distance dist nodes =
    let next_nodes = ref [] in
    let do_node (node: Node.t) =
      match node.dist_exit with
      | Some _
       -> ()
      | None
       -> node.dist_exit <- Some dist ;
          next_nodes := node.preds @ !next_nodes
    in
    List.iter ~f:do_node nodes ;
    if !next_nodes <> [] then mark_distance (dist + 1) !next_nodes
  in
  mark_distance 0 [exit_node]

(** check or indicate if we have performed preanalysis on the CFG *)
let did_preanalysis pdesc = pdesc.attributes.did_preanalysis

let signal_did_preanalysis pdesc = (pdesc.attributes).did_preanalysis <- true

let get_attributes pdesc = pdesc.attributes

let get_err_log pdesc = pdesc.attributes.err_log

let get_exit_node pdesc = pdesc.exit_node

(** Get flags for the proc desc *)
let get_flags pdesc = pdesc.attributes.proc_flags

(** Return name and type of formal parameters *)
let get_formals pdesc = pdesc.attributes.formals

let get_loc pdesc = pdesc.attributes.loc

(** Return name and type of local variables *)
let get_locals pdesc = pdesc.attributes.locals

(** Return name and type of captured variables *)
let get_captured pdesc = pdesc.attributes.captured

(** Return the visibility attribute *)
let get_access pdesc = pdesc.attributes.access

let get_nodes pdesc = pdesc.nodes

let get_proc_name pdesc = pdesc.attributes.proc_name

(** Return the return type of the procedure *)
let get_ret_type pdesc = pdesc.attributes.ret_type

let get_ret_var pdesc = Pvar.mk Ident.name_return (get_proc_name pdesc)

let get_start_node pdesc = pdesc.start_node

(** List of nodes in the procedure sliced by a predicate up to the first branching *)
let get_sliced_slope pdesc f =
  Node.get_generated_slope (get_start_node pdesc) (fun n -> Node.get_sliced_succs n f)

(** List of nodes in the procedure up to the first branching *)
let get_slope pdesc = Node.get_generated_slope (get_start_node pdesc) Node.get_succs

(** Return [true] iff the procedure is defined, and not just declared *)
let is_defined pdesc = pdesc.attributes.is_defined

let is_body_empty pdesc = List.is_empty (Node.get_succs (get_start_node pdesc))

let is_java_synchronized pdesc = pdesc.attributes.is_java_synchronized_method

let iter_nodes f pdesc = List.iter ~f (List.rev (get_nodes pdesc))

let fold_calls f acc pdesc =
  let do_node a node =
    List.fold
      ~f:(fun b callee_pname -> f b (callee_pname, Node.get_loc node))
      ~init:a (Node.get_callees node)
  in
  List.fold ~f:do_node ~init:acc (get_nodes pdesc)

(** iterate over the calls from the procedure: (callee,location) pairs *)
let iter_calls f pdesc = fold_calls (fun _ call -> f call) () pdesc

let iter_instrs f pdesc =
  let do_node node = List.iter ~f:(fun i -> f node i) (Node.get_instrs node) in
  iter_nodes do_node pdesc

let fold_nodes f acc pdesc = List.fold ~f ~init:acc (List.rev (get_nodes pdesc))

let fold_instrs f acc pdesc =
  let fold_node acc node =
    List.fold ~f:(fun acc instr -> f acc node instr) ~init:acc (Node.get_instrs node)
  in
  fold_nodes fold_node acc pdesc

let iter_slope f pdesc =
  let visited = ref NodeSet.empty in
  let rec do_node node =
    visited := NodeSet.add node !visited ;
    f node ;
    match Node.get_succs node with
    | [n]
     -> if not (NodeSet.mem n !visited) then do_node n
    | _
     -> ()
  in
  do_node (get_start_node pdesc)

let iter_slope_calls f pdesc =
  let do_node node = List.iter ~f:(fun callee_pname -> f callee_pname) (Node.get_callees node) in
  iter_slope do_node pdesc

(** iterate between two nodes or until we reach a branching structure *)
let iter_slope_range f src_node dst_node =
  let visited = ref NodeSet.empty in
  let rec do_node node =
    visited := NodeSet.add node !visited ;
    f node ;
    match Node.get_succs node with
    | [n]
     -> if not (NodeSet.mem n !visited) && not (Node.equal node dst_node) then do_node n
    | _
     -> ()
  in
  do_node src_node

(** Set the exit node of the proc desc *)
let set_exit_node pdesc node = pdesc.exit_node <- node

(** Set a flag for the proc desc *)
let set_flag pdesc key value = ProcAttributes.proc_flags_add pdesc.attributes.proc_flags key value

(** Set the start node of the proc desc *)
let set_start_node pdesc node = pdesc.start_node <- node

(** Append the locals to the list of local variables *)
let append_locals pdesc new_locals =
  (pdesc.attributes).locals <- pdesc.attributes.locals @ new_locals

(** Set the successor nodes and exception nodes, and build predecessor links *)
let set_succs_exn_base (node: Node.t) succs exn =
  node.succs <- succs ;
  node.exn <- exn ;
  List.iter ~f:(fun (n: Node.t) -> n.preds <- node :: n.preds) succs

(** Create a new cfg node *)
let create_node pdesc loc kind instrs =
  pdesc.nodes_num <- pdesc.nodes_num + 1 ;
  let node_id = pdesc.nodes_num in
  let node =
    { Node.id= node_id
    ; dist_exit= None
    ; instrs
    ; kind
    ; loc
    ; preds= []
    ; pname_opt= Some pdesc.attributes.proc_name
    ; succs= []
    ; exn= [] }
  in
  pdesc.nodes <- node :: pdesc.nodes ;
  node

(** Set the successor and exception nodes.
    If this is a join node right before the exit node, add an extra node in the middle,
    otherwise nullify and abstract instructions cannot be added after a conditional. *)
let node_set_succs_exn pdesc (node: Node.t) succs exn =
  match (node.kind, succs) with
  | Join_node, [({Node.kind= Exit_node _} as exit_node)]
   -> let kind = Node.Stmt_node "between_join_and_exit" in
      let node' = create_node pdesc node.loc kind node.instrs in
      set_succs_exn_base node [node'] exn ;
      set_succs_exn_base node' [exit_node] exn
  | _
   -> set_succs_exn_base node succs exn

(** Get loop heads for widening.
    It collects all target nodes of back-edges in a depth-first
    traversal.
    *)
let get_loop_heads pdesc =
  let rec set_loop_head_rec visited heads wl =
    match wl with
    | []
     -> heads
    | (n, ancester) :: wl'
     -> if NodeSet.mem n visited then
          if NodeSet.mem n ancester then set_loop_head_rec visited (NodeSet.add n heads) wl'
          else set_loop_head_rec visited heads wl'
        else
          let ancester = NodeSet.add n ancester in
          let succs = List.append (Node.get_succs n) (Node.get_exn n) in
          let works = List.map ~f:(fun m -> (m, ancester)) succs in
          set_loop_head_rec (NodeSet.add n visited) heads (List.append works wl')
  in
  let start_wl = [(get_start_node pdesc, NodeSet.empty)] in
  let lh = set_loop_head_rec NodeSet.empty NodeSet.empty start_wl in
  pdesc.loop_heads <- Some lh ;
  lh

let is_loop_head pdesc (node: Node.t) =
  let lh = match pdesc.loop_heads with Some lh -> lh | None -> get_loop_heads pdesc in
  NodeSet.mem node lh

let pp_variable_list fmt etl =
  if List.is_empty etl then Format.fprintf fmt "None"
  else
    List.iter
      ~f:(fun (id, ty) -> Format.fprintf fmt " %a:%a" Mangled.pp id (Typ.pp_full Pp.text) ty)
      etl

let pp_objc_accessor fmt accessor =
  match accessor with
  | Some ProcAttributes.Objc_getter name
   -> Format.fprintf fmt "Getter of %a, " Typ.Fieldname.pp name
  | Some ProcAttributes.Objc_setter name
   -> Format.fprintf fmt "Setter of %a, " Typ.Fieldname.pp name
  | None
   -> ()

let pp_signature fmt pdesc =
  let attributes = get_attributes pdesc in
  let pname = get_proc_name pdesc in
  let pname_string = Typ.Procname.to_string pname in
  let defined_string = match is_defined pdesc with true -> "defined" | false -> "undefined" in
  Format.fprintf fmt "%s [%s, Return type: %s, %aFormals: %a, Locals: %a" pname_string
    defined_string
    (Typ.to_string (get_ret_type pdesc))
    pp_objc_accessor attributes.ProcAttributes.objc_accessor pp_variable_list (get_formals pdesc)
    pp_variable_list (get_locals pdesc) ;
  if not (List.is_empty (get_captured pdesc)) then
    Format.fprintf fmt ", Captured: %a" pp_variable_list (get_captured pdesc) ;
  let method_annotation = attributes.ProcAttributes.method_annotation in
  if not (Annot.Method.is_empty method_annotation) then
    Format.fprintf fmt ", Annotation: %a" (Annot.Method.pp pname_string) method_annotation ;
  Format.fprintf fmt "]@\n"
