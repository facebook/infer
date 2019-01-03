(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module Hashtbl = Caml.Hashtbl
module L = Logging
module F = Format

module NodeKey = struct
  type t = Caml.Digest.t

  let to_string = Caml.Digest.to_hex

  let compute node ~simple_key ~succs ~preds =
    let v =
      (simple_key node, List.rev_map ~f:simple_key succs, List.rev_map ~f:simple_key preds)
    in
    Utils.better_hash v


  let of_frontend_node_key = Utils.better_hash
end

(* =============== START of module Node =============== *)
module Node = struct
  type id = int [@@deriving compare]

  let equal_id = [%compare.equal: id]

  type stmt_nodekind =
    | AssertionFailure
    | BetweenJoinAndExit
    | BinaryConditionalStmtInit
    | BinaryOperatorStmt of string
    | Call of string
    | CallObjCNew
    | ClassCastException
    | ConditionalStmtBranch
    | ConstructorInit
    | CXXDynamicCast
    | CXXNewExpr
    | CXXStdInitializerListExpr
    | CXXTypeidExpr
    | DeclStmt
    | DefineBody
    | Destruction
    | ExceptionHandler
    | ExceptionsSink
    | FallbackNode
    | FinallyBranch
    | GCCAsmStmt
    | GenericSelectionExpr
    | IfStmtBranch
    | InitializeDynamicArrayLength
    | InitListExp
    | MessageCall of string
    | MethodBody
    | MonitorEnter
    | MonitorExit
    | ObjCCPPThrow
    | OutOfBound
    | ReturnStmt
    | Skip of string
    | SwitchStmt
    | ThisNotNull
    | Throw
    | ThrowNPE
    | UnaryOperator
  [@@deriving compare]

  type prune_node_kind =
    | PruneNodeKind_ExceptionHandler
    | PruneNodeKind_FalseBranch
    | PruneNodeKind_InBound
    | PruneNodeKind_IsInstance
    | PruneNodeKind_MethodBody
    | PruneNodeKind_NotNull
    | PruneNodeKind_TrueBranch
  [@@deriving compare]

  type nodekind =
    | Start_node
    | Exit_node
    | Stmt_node of stmt_nodekind
    | Join_node
    | Prune_node of bool * Sil.if_kind * prune_node_kind
        (** (true/false branch, if_kind, comment) *)
    | Skip_node of string
  [@@deriving compare]

  let equal_nodekind = [%compare.equal: nodekind]

  (** a node *)
  type t =
    { id: id  (** unique id of the node *)
    ; mutable dist_exit: int option  (** distance to the exit node *)
    ; mutable wto_index: int
    ; mutable exn: t list  (** exception nodes in the cfg *)
    ; mutable instrs: Instrs.not_reversed_t  (** instructions for symbolic execution *)
    ; kind: nodekind  (** kind of node *)
    ; loc: Location.t  (** location in the source code *)
    ; mutable preds: t list  (** predecessor nodes in the cfg *)
    ; pname: Typ.Procname.t  (** name of the procedure the node belongs to *)
    ; mutable succs: t list  (** successor nodes in the cfg *) }

  let exn_handler_kind = Stmt_node ExceptionHandler

  let exn_sink_kind = Stmt_node ExceptionsSink

  let throw_kind = Stmt_node Throw

  let dummy pname =
    { id= 0
    ; dist_exit= None
    ; wto_index= Int.max_value
    ; instrs= Instrs.empty
    ; kind= Skip_node "dummy"
    ; loc= Location.dummy
    ; pname
    ; succs= []
    ; preds= []
    ; exn= [] }


  let compare node1 node2 = Int.compare node1.id node2.id

  let hash node = Hashtbl.hash node.id

  let equal = [%compare.equal: t]

  (** Get the unique id of the node *)
  let get_id node = node.id

  let get_succs node = node.succs

  type node = t

  let pp_id f id = F.pp_print_int f id

  let pp f node = pp_id f (get_id node)

  module NodeSet = Caml.Set.Make (struct
    type t = node

    let compare = compare
  end)

  module IdMap = PrettyPrintable.MakePPMap (struct
    type t = id

    let compare = compare_id

    let pp = pp_id
  end)

  let get_exn node = node.exn

  (** Get the name of the procedure the node belongs to *)
  let get_proc_name node = node.pname

  (** Get the predecessors of the node *)
  let get_preds node = node.preds

  (** Get siblings *)
  let get_siblings node =
    get_preds node
    |> ISequence.gen_sequence_list ~f:(fun parent ->
           get_succs parent |> Sequence.of_list
           |> Sequence.filter ~f:(fun n -> not (equal node n))
           |> Sequence.Generator.of_sequence )
    |> Sequence.Generator.run


  (** Get the node kind *)
  let get_kind node = node.kind

  (** Get the instructions to be executed *)
  let get_instrs node = node.instrs

  (** Get the location of the node *)
  let get_loc n = n.loc

  (** Get the source location of the last instruction in the node *)
  let get_last_loc n =
    n |> get_instrs |> Instrs.last |> Option.value_map ~f:Sil.instr_get_loc ~default:n.loc


  let find_in_node_or_preds =
    let rec find ~f visited nodes =
      match nodes with
      | node :: nodes when not (NodeSet.mem node visited) -> (
          let instrs = get_instrs node in
          match Instrs.find_map ~f:(f node) instrs with
          | Some res ->
              Some res
          | None ->
              let nodes = get_preds node |> List.rev_append nodes in
              let visited = NodeSet.add node visited in
              find ~f visited nodes )
      | _ :: nodes ->
          find ~f visited nodes
      | _ ->
          None
    in
    fun start_node ~f -> find ~f NodeSet.empty [start_node]


  let get_distance_to_exit node = node.dist_exit

  let get_wto_index node = node.wto_index

  (** Append the instructions to the list of instructions to execute *)
  let append_instrs node instrs =
    if instrs <> [] then node.instrs <- Instrs.append_list node.instrs instrs


  (** Map and replace the instructions to be executed *)
  let replace_instrs node ~f =
    let instrs' = Instrs.map_changed ~equal:phys_equal node.instrs ~f:(f node) in
    if phys_equal instrs' node.instrs then false
    else (
      node.instrs <- instrs' ;
      true )


  let pp_stmt fmt = function
    | AssertionFailure ->
        F.pp_print_string fmt "Assertion failure"
    | BetweenJoinAndExit ->
        F.pp_print_string fmt "between_join_and_exit"
    | BinaryConditionalStmtInit ->
        F.pp_print_string fmt "BinaryConditionalStmt Init"
    | BinaryOperatorStmt bop ->
        F.fprintf fmt "BinaryOperatorStmt: %s" bop
    | Call call ->
        F.fprintf fmt "Call %s" call
    | CallObjCNew ->
        F.pp_print_string fmt "Call objC new"
    | ClassCastException ->
        F.pp_print_string fmt "Class cast exception"
    | ConditionalStmtBranch ->
        F.pp_print_string fmt "ConditionalStmt Branch"
    | ConstructorInit ->
        F.pp_print_string fmt "Constructor Init"
    | CXXDynamicCast ->
        F.pp_print_string fmt "CxxDynamicCast"
    | CXXNewExpr ->
        F.pp_print_string fmt "CXXNewExpr"
    | CXXStdInitializerListExpr ->
        F.pp_print_string fmt "CXXStdInitializerListExpr"
    | CXXTypeidExpr ->
        F.pp_print_string fmt "CXXTypeidExpr"
    | DeclStmt ->
        F.pp_print_string fmt "DeclStmt"
    | DefineBody ->
        F.pp_print_string fmt "define_body"
    | Destruction ->
        F.pp_print_string fmt "Destruction"
    | ExceptionHandler ->
        F.pp_print_string fmt "exception handler"
    | ExceptionsSink ->
        F.pp_print_string fmt "exceptions sink"
    | FallbackNode ->
        F.pp_print_string fmt "Fallback node"
    | FinallyBranch ->
        F.pp_print_string fmt "Finally branch"
    | GCCAsmStmt ->
        F.pp_print_string fmt "GCCAsmStmt"
    | GenericSelectionExpr ->
        F.pp_print_string fmt "GenericSelectionExpr"
    | IfStmtBranch ->
        F.pp_print_string fmt "IfStmt Branch"
    | InitializeDynamicArrayLength ->
        F.pp_print_string fmt "Initialize dynamic array length"
    | InitListExp ->
        F.pp_print_string fmt "InitListExp"
    | MessageCall selector ->
        F.fprintf fmt "Message Call: %s" selector
    | MethodBody ->
        F.pp_print_string fmt "method_body"
    | MonitorEnter ->
        F.pp_print_string fmt "MonitorEnter"
    | MonitorExit ->
        F.pp_print_string fmt "MonitorExit"
    | ObjCCPPThrow ->
        F.pp_print_string fmt "ObjCCPPThrow"
    | OutOfBound ->
        F.pp_print_string fmt "Out of bound"
    | ReturnStmt ->
        F.pp_print_string fmt "Return Stmt"
    | Skip reason ->
        F.pp_print_string fmt reason
    | SwitchStmt ->
        F.pp_print_string fmt "SwitchStmt"
    | ThisNotNull ->
        F.pp_print_string fmt "this not null"
    | Throw ->
        F.pp_print_string fmt "throw"
    | ThrowNPE ->
        F.pp_print_string fmt "Throw NPE"
    | UnaryOperator ->
        F.pp_print_string fmt "UnaryOperator"


  let pp_instrs ~highlight pe0 f node =
    let pe =
      match highlight with
      | None ->
          pe0
      | Some instr ->
          Pp.extend_colormap pe0 (Obj.repr instr) Red
    in
    Instrs.pp pe f (get_instrs node)


  let d_instrs ~highlight (node : t) = L.d_pp_with_pe ~color:Green (pp_instrs ~highlight) node

  let string_of_prune_node_kind = function
    | PruneNodeKind_ExceptionHandler ->
        "exception handler"
    | PruneNodeKind_FalseBranch ->
        "false Branch"
    | PruneNodeKind_InBound ->
        "In bound"
    | PruneNodeKind_IsInstance ->
        "Is instance"
    | PruneNodeKind_MethodBody ->
        "method_body"
    | PruneNodeKind_NotNull ->
        "Not null"
    | PruneNodeKind_TrueBranch ->
        "true Branch"


  (** Return a description of the cfg node *)
  let get_description pe node =
    let str_kind =
      match get_kind node with
      | Stmt_node _ ->
          "Instructions"
      | Prune_node (_, _, prune_node_kind) ->
          "Conditional " ^ string_of_prune_node_kind prune_node_kind
      | Exit_node ->
          "Exit"
      | Skip_node _ ->
          "Skip"
      | Start_node ->
          "Start"
      | Join_node ->
          "Join"
    in
    F.asprintf "%s@\n%a" str_kind (Instrs.pp pe) (get_instrs node)


  (** simple key for a node: just look at the instructions *)
  let simple_key node =
    let add_instr instr =
      if Sil.instr_is_auxiliary instr then None
      else
        let instr_key =
          match instr with
          | Sil.Load _ ->
              1
          | Sil.Store _ ->
              2
          | Sil.Prune _ ->
              3
          | Sil.Call _ ->
              4
          | Sil.Nullify _ ->
              5
          | Sil.Abstract _ ->
              6
          | Sil.ExitScope _ ->
              7
        in
        Some instr_key
    in
    get_instrs node
    |> IContainer.rev_filter_map_to_list ~fold:Instrs.fold ~f:add_instr
    |> Utils.better_hash


  (** key for a node: look at the current node, successors and predecessors *)
  let compute_key node =
    let succs = get_succs node in
    let preds = get_preds node in
    NodeKey.compute node ~simple_key ~succs ~preds
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
  { mutable attributes: ProcAttributes.t  (** attributes of the procedure *)
  ; mutable nodes: Node.t list  (** list of nodes of this procedure *)
  ; mutable nodes_num: int  (** number of nodes *)
  ; mutable start_node: Node.t  (** start node of this procedure *)
  ; mutable exit_node: Node.t  (** exit node of this procedure *)
  ; mutable loop_heads: NodeSet.t option  (** loop head nodes of this procedure *)
  ; mutable wto: Node.t WeakTopologicalOrder.Partition.t option
        (** weak topological order of this procedure *) }

let from_proc_attributes attributes =
  let pname = attributes.ProcAttributes.proc_name in
  let start_node = Node.dummy pname in
  let exit_node = Node.dummy pname in
  {attributes; nodes= []; nodes_num= 0; start_node; exit_node; loop_heads= None; wto= None}


(** Compute the distance of each node to the exit node, if not computed already *)
let compute_distance_to_exit_node pdesc =
  let exit_node = pdesc.exit_node in
  let rec mark_distance dist nodes =
    let next_nodes = ref [] in
    let do_node (node : Node.t) =
      match node.dist_exit with
      | Some _ ->
          ()
      | None ->
          node.dist_exit <- Some dist ;
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

let set_attributes pdesc attributes = pdesc.attributes <- attributes

let get_exit_node pdesc = pdesc.exit_node

let get_proc_name pdesc = pdesc.attributes.proc_name

(** Return name and type of formal parameters *)
let get_formals pdesc = pdesc.attributes.formals

let get_pvar_formals pdesc =
  let proc_name = get_proc_name pdesc in
  get_formals pdesc |> List.map ~f:(fun (name, typ) -> (Pvar.mk name proc_name, typ))


let get_loc pdesc = pdesc.attributes.loc

(** Return name and type of local variables *)
let get_locals pdesc = pdesc.attributes.locals

let has_added_return_param pdesc = pdesc.attributes.has_added_return_param

(** Return name and type of captured variables *)
let get_captured pdesc = pdesc.attributes.captured

(** Return the visibility attribute *)
let get_access pdesc = pdesc.attributes.access

let get_nodes pdesc = pdesc.nodes

let get_nodes_num pdesc = pdesc.nodes_num

(** Return the return type of the procedure *)
let get_ret_type pdesc = pdesc.attributes.ret_type

let get_ret_var pdesc = Pvar.get_ret_pvar (get_proc_name pdesc)

let get_start_node pdesc = pdesc.start_node

(** Return [true] iff the procedure is defined, and not just declared *)
let is_defined pdesc = pdesc.attributes.is_defined

let is_java_synchronized pdesc = pdesc.attributes.is_java_synchronized_method

let iter_nodes f pdesc = List.iter ~f (get_nodes pdesc)

let iter_instrs f pdesc =
  let do_node node = Instrs.iter ~f:(fun i -> f node i) (Node.get_instrs node) in
  iter_nodes do_node pdesc


let fold_nodes pdesc ~init ~f = List.fold ~f ~init (get_nodes pdesc)

let fold_instrs pdesc ~init ~f =
  let fold_node acc node =
    Instrs.fold ~f:(fun acc instr -> f acc node instr) ~init:acc (Node.get_instrs node)
  in
  fold_nodes ~f:fold_node ~init pdesc


let find_map_nodes pdesc ~f = List.find_map ~f (get_nodes pdesc)

let find_map_instrs pdesc ~f =
  let find_map_node node = Instrs.find_map ~f (Node.get_instrs node) in
  find_map_nodes ~f:find_map_node pdesc


let replace_instrs pdesc ~f =
  let f updated node =
    Node.replace_instrs ~f node || (* do not short-circuit [Node.replace_instrs] *) updated
  in
  fold_nodes pdesc ~init:false ~f


(** fold between two nodes or until we reach a branching structure *)
let fold_slope_range =
  let rec aux node visited acc ~f =
    let visited = NodeSet.add node visited in
    let acc = f acc node in
    match Node.get_succs node with
    | [n] when not (NodeSet.mem n visited) ->
        aux n visited acc ~f
    | _ ->
        acc
  in
  fun src_node dst_node ~init ~f -> aux src_node (NodeSet.singleton dst_node) init ~f


(** Set the exit node of the proc desc *)
let set_exit_node pdesc node = pdesc.exit_node <- node

(** Set the start node of the proc desc *)
let set_start_node pdesc node = pdesc.start_node <- node

(** Append the locals to the list of local variables *)
let append_locals pdesc new_locals =
  (pdesc.attributes).locals <- pdesc.attributes.locals @ new_locals


let set_succs_exn_only (node : Node.t) exn = node.exn <- exn

(** Set the successor nodes and exception nodes, and build predecessor links *)
let set_succs_exn_base (node : Node.t) succs exn =
  node.succs <- succs ;
  node.exn <- exn ;
  List.iter ~f:(fun (n : Node.t) -> n.preds <- node :: n.preds) succs


(** Create a new cfg node *)
let create_node_from_not_reversed pdesc loc kind instrs =
  pdesc.nodes_num <- pdesc.nodes_num + 1 ;
  let node_id = pdesc.nodes_num in
  let node =
    { Node.id= node_id
    ; dist_exit= None
    ; wto_index= Int.max_value
    ; instrs
    ; kind
    ; loc
    ; preds= []
    ; pname= pdesc.attributes.proc_name
    ; succs= []
    ; exn= [] }
  in
  pdesc.nodes <- node :: pdesc.nodes ;
  node


let create_node pdesc loc kind instrs =
  create_node_from_not_reversed pdesc loc kind (Instrs.of_list instrs)


(** Set the successor and exception nodes.
    If this is a join node right before the exit node, add an extra node in the middle,
    otherwise nullify and abstract instructions cannot be added after a conditional. *)
let node_set_succs_exn pdesc (node : Node.t) succs exn =
  match (node.kind, succs) with
  | Join_node, [({Node.kind= Exit_node} as exit_node)] ->
      let kind = Node.Stmt_node BetweenJoinAndExit in
      let node' = create_node_from_not_reversed pdesc node.loc kind node.instrs in
      set_succs_exn_base node [node'] exn ;
      set_succs_exn_base node' [exit_node] exn
  | _ ->
      set_succs_exn_base node succs exn


module PreProcCfg = struct
  type nonrec t = t

  let fold_succs _cfg n ~init ~f = n |> Node.get_succs |> List.fold ~init ~f

  let start_node = get_start_node

  module Node = struct
    type t = Node.t

    type id = Node.id

    let id = Node.get_id

    module IdMap = IdMap
  end
end

module WTO = WeakTopologicalOrder.Bourdoncle_SCC (PreProcCfg)

let get_wto pdesc =
  match pdesc.wto with
  | Some wto ->
      wto
  | None ->
      let wto = WTO.make pdesc in
      let _ : int =
        WeakTopologicalOrder.Partition.fold_nodes wto ~init:0 ~f:(fun idx node ->
            node.Node.wto_index <- idx ;
            idx + 1 )
      in
      pdesc.wto <- Some wto ;
      wto


(** Get loop heads for widening.
   It collects all target nodes of back-edges in a depth-first traversal.
   We need to use the exceptional CFG otherwise we will miss loop heads in catch clauses.
*)
let get_loop_heads pdesc =
  match pdesc.loop_heads with
  | Some lh ->
      lh
  | None ->
      let rec set_loop_head_rec visited heads wl =
        match wl with
        | [] ->
            heads
        | (n, ancester) :: wl' ->
            if NodeSet.mem n visited then
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


let is_loop_head pdesc (node : Node.t) = NodeSet.mem node (get_loop_heads pdesc)

let pp_modify_in_block fmt modify_in_block =
  if modify_in_block then Format.pp_print_string fmt "(__block)" else ()


let pp_local fmt (var_data : ProcAttributes.var_data) =
  Format.fprintf fmt " %a:%a%a" Mangled.pp var_data.name (Typ.pp_full Pp.text) var_data.typ
    pp_modify_in_block var_data.modify_in_block


let pp_locals_list fmt etl =
  if List.is_empty etl then Format.pp_print_string fmt "None" else List.iter ~f:(pp_local fmt) etl


let pp_variable_list fmt etl =
  if List.is_empty etl then Format.pp_print_string fmt "None"
  else
    List.iter
      ~f:(fun (id, ty) -> Format.fprintf fmt " %a:%a" Mangled.pp id (Typ.pp_full Pp.text) ty)
      etl


let pp_objc_accessor fmt accessor =
  match accessor with
  | Some (ProcAttributes.Objc_getter field) ->
      Format.fprintf fmt "Getter of %a, " (Typ.Struct.pp_field Pp.text) field
  | Some (ProcAttributes.Objc_setter field) ->
      Format.fprintf fmt "Setter of %a, " (Typ.Struct.pp_field Pp.text) field
  | None ->
      ()


let pp_signature fmt pdesc =
  let attributes = get_attributes pdesc in
  let pname = get_proc_name pdesc in
  let pname_string = Typ.Procname.to_string pname in
  let defined_string = match is_defined pdesc with true -> "defined" | false -> "undefined" in
  Format.fprintf fmt "@[%s [%s, Return type: %s, %aFormals: %a, Locals: %a" pname_string
    defined_string
    (Typ.to_string (get_ret_type pdesc))
    pp_objc_accessor attributes.ProcAttributes.objc_accessor pp_variable_list (get_formals pdesc)
    pp_locals_list (get_locals pdesc) ;
  if not (List.is_empty (get_captured pdesc)) then
    Format.fprintf fmt ", Captured: %a" pp_variable_list (get_captured pdesc) ;
  let method_annotation = attributes.ProcAttributes.method_annotation in
  if not (Annot.Method.is_empty method_annotation) then
    Format.fprintf fmt ", Annotation: %a" (Annot.Method.pp pname_string) method_annotation ;
  Format.fprintf fmt "]@]@;"


let is_specialized pdesc =
  let attributes = get_attributes pdesc in
  attributes.ProcAttributes.is_specialized


(* true if pvar is a captured variable of a cpp lambda or objc block *)
let is_captured_pvar procdesc pvar =
  let procname = get_proc_name procdesc in
  let pvar_name = Pvar.get_name pvar in
  let pvar_local_matches (var_data : ProcAttributes.var_data) =
    Mangled.equal var_data.name pvar_name
  in
  let pvar_matches (name, _) = Mangled.equal name pvar_name in
  let is_captured_var_cpp_lambda =
    match procname with
    | Typ.Procname.ObjC_Cpp cpp_pname ->
        (* var is captured if the procedure is a lambda and the var is not in the locals or formals *)
        Typ.Procname.ObjC_Cpp.is_cpp_lambda cpp_pname
        && not
             ( List.exists ~f:pvar_local_matches (get_locals procdesc)
             || List.exists ~f:pvar_matches (get_formals procdesc) )
    | _ ->
        false
  in
  let is_captured_var_objc_block =
    (* var is captured if the procedure is a objc block and the var is in the captured *)
    Typ.Procname.is_objc_block procname && List.exists ~f:pvar_matches (get_captured procdesc)
  in
  is_captured_var_cpp_lambda || is_captured_var_objc_block


let is_captured_var procdesc var =
  Var.get_pvar var |> Option.exists ~f:(fun pvar -> is_captured_pvar procdesc pvar)


let has_modify_in_block_attr procdesc pvar =
  let pvar_name = Pvar.get_name pvar in
  let pvar_local_matches (var_data : ProcAttributes.var_data) =
    var_data.modify_in_block && Mangled.equal var_data.name pvar_name
  in
  List.exists ~f:pvar_local_matches (get_locals procdesc)


let is_connected proc_desc =
  let is_exit_node n = match Node.get_kind n with Node.Exit_node -> true | _ -> false in
  let is_between_join_and_exit_node n =
    match Node.get_kind n with
    | Node.Stmt_node BetweenJoinAndExit | Node.Stmt_node Destruction -> (
      match Node.get_succs n with [n'] when is_exit_node n' -> true | _ -> false )
    | _ ->
        false
  in
  let rec is_consecutive_join_nodes n visited =
    match Node.get_kind n with
    | Node.Join_node -> (
        if NodeSet.mem n visited then false
        else
          let succs = Node.get_succs n in
          match succs with
          | [n'] ->
              is_consecutive_join_nodes n' (NodeSet.add n visited)
          | _ ->
              false )
    | _ ->
        is_between_join_and_exit_node n
  in
  let find_broken_node n =
    let succs = Node.get_succs n in
    let preds = Node.get_preds n in
    match Node.get_kind n with
    | Node.Start_node ->
        if List.is_empty succs || not (List.is_empty preds) then Error `Other else Ok ()
    | Node.Exit_node ->
        if (not (List.is_empty succs)) || List.is_empty preds then Error `Other else Ok ()
    | Node.Stmt_node _ | Node.Prune_node _ | Node.Skip_node _ ->
        if List.is_empty succs || List.is_empty preds then Error `Other else Ok ()
    | Node.Join_node ->
        (* Join node has the exception that it may be without predecessors
         and pointing to between_join_and_exit which points to an exit node.
         This happens when the if branches end with a return.
         Nested if statements, where all branches have return statements,
         introduce a sequence of join nodes *)
        if
          (List.is_empty preds && not (is_consecutive_join_nodes n NodeSet.empty))
          || ((not (List.is_empty preds)) && List.is_empty succs)
        then Error `Join
        else Ok ()
  in
  (* unconnected nodes generated by Join nodes are expected *)
  let skip_join_errors current_status node =
    match find_broken_node node with
    | Ok () ->
        Ok current_status
    | Error `Join ->
        Ok (Some `Join)
    | Error _ as other_error ->
        other_error
  in
  match List.fold_result (get_nodes proc_desc) ~init:None ~f:skip_join_errors with
  | Ok (Some `Join) ->
      Error `Join
  | Ok None ->
      Ok ()
  | Error _ as error ->
      error


module SQLite = SqliteUtils.MarshalledNullableData (struct
  type nonrec t = t
end)

let load_statement =
  ResultsDatabase.register_statement "SELECT cfg FROM procedures WHERE proc_name = :k"


let load pname =
  ResultsDatabase.with_registered_statement load_statement ~f:(fun db stmt ->
      Typ.Procname.SQLite.serialize pname
      |> Sqlite3.bind stmt 1
      |> SqliteUtils.check_result_code db ~log:"load bind proc name" ;
      SqliteUtils.result_single_column_option ~finalize:false ~log:"Procdesc.load" db stmt
      |> Option.bind ~f:SQLite.deserialize )
