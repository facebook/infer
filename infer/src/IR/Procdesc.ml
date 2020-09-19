(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    let v = (simple_key node, List.rev_map ~f:simple_key succs, List.rev_map ~f:simple_key preds) in
    Utils.better_hash v


  let of_frontend_node_key = Utils.better_hash
end

(* =============== START of module Node =============== *)
module Node = struct
  type id = int [@@deriving compare, equal]

  type destruction_kind =
    | DestrBreakStmt
    | DestrContinueStmt
    | DestrFields
    | DestrReturnStmt
    | DestrScope
    | DestrTemporariesCleanup
    | DestrVirtualBase
  [@@deriving compare]

  let string_of_destruction_kind = function
    | DestrBreakStmt ->
        "break"
    | DestrContinueStmt ->
        "continue"
    | DestrFields ->
        "fields"
    | DestrReturnStmt ->
        "return"
    | DestrScope ->
        "Scope"
    | DestrTemporariesCleanup ->
        "temporaries cleanup"
    | DestrVirtualBase ->
        "virtual base"


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
    | Destruction of destruction_kind
    | ExceptionHandler
    | ExceptionsSink
    | ExprWithCleanups
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
    | Scope of string
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
    ; pname: Procname.t  (** name of the procedure the node belongs to *)
    ; mutable succs: t list  (** successor nodes in the cfg *)
    ; mutable code_block_exit: t option
          (** exit node corresponding to start node in a code block *) }

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
    ; exn= []
    ; code_block_exit= None }


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
    type t = id [@@deriving compare]

    let pp = pp_id
  end)

  let get_exn node = node.exn

  (** Get the name of the procedure the node belongs to *)
  let get_proc_name node = node.pname

  (** Get the predecessors of the node *)
  let get_preds node = node.preds

  let is_dangling node = List.is_empty (get_preds node) && List.is_empty (get_succs node)

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
    n |> get_instrs |> Instrs.last |> Option.value_map ~f:Sil.location_of_instr ~default:n.loc


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
    if not (List.is_empty instrs) then node.instrs <- Instrs.append_list node.instrs instrs


  (** Map and replace the instructions to be executed *)
  let replace_instrs node ~f =
    let instrs' = Instrs.map node.instrs ~f:(f node) in
    if phys_equal instrs' node.instrs then false
    else (
      node.instrs <- instrs' ;
      true )


  (** Map and replace the instructions to be executed using a context *)
  let replace_instrs_using_context node ~f ~update_context ~context_at_node =
    let f node context instr = (update_context context instr, f node context instr) in
    let instrs' = Instrs.map_and_fold node.instrs ~f:(f node) ~init:context_at_node in
    if phys_equal instrs' node.instrs then false
    else (
      node.instrs <- instrs' ;
      true )


  (** Like [replace_instrs], but 1 instr gets replaced by 0, 1, or more instructions. *)
  let replace_instrs_by node ~f =
    let instrs' = Instrs.concat_map node.instrs ~f:(f node) in
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
    | Destruction kind ->
        F.fprintf fmt "Destruction(%s)" (string_of_destruction_kind kind)
    | ExceptionHandler ->
        F.pp_print_string fmt "exception handler"
    | ExceptionsSink ->
        F.pp_print_string fmt "exceptions sink"
    | ExprWithCleanups ->
        F.pp_print_string fmt "ExprWithCleanups"
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
    | Scope descr ->
        F.fprintf fmt "Scope(%s)" descr
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
      match highlight with None -> pe0 | Some instr -> Pp.extend_colormap pe0 (Obj.repr instr) Red
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


  let set_code_block_exit node ~code_block_exit = node.code_block_exit <- Some code_block_exit

  let get_code_block_exit node = node.code_block_exit

  (** simple key for a node: just look at the instructions *)
  let simple_key node =
    let add_instr instr =
      match instr with
      | Sil.Load _ ->
          Some 1
      | Sil.Store _ ->
          Some 2
      | Sil.Prune _ ->
          Some 3
      | Sil.Call _ ->
          Some 4
      | Sil.Metadata _ ->
          None
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
    if not (List.is_empty !next_nodes) then mark_distance (dist + 1) !next_nodes
  in
  mark_distance 0 [exit_node]


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

(** Return the return type of the procedure *)
let get_ret_type pdesc = pdesc.attributes.ret_type

let get_ret_var pdesc = Pvar.get_ret_pvar (get_proc_name pdesc)

let get_start_node pdesc = pdesc.start_node

(** Return [true] iff the procedure is defined, and not just declared *)
let is_defined pdesc = pdesc.attributes.is_defined

let is_java_synchronized pdesc = pdesc.attributes.is_java_synchronized_method

let is_objc_arc_on pdesc = pdesc.attributes.is_objc_arc_on

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


let get_static_callees pdesc =
  let callees =
    fold_instrs pdesc ~init:Procname.Set.empty ~f:(fun acc _node instr ->
        match instr with
        | Sil.Call (_, Exp.Const (Const.Cfun callee_pn), _, _, _) ->
            Procname.Set.add callee_pn acc
        | _ ->
            acc )
  in
  Procname.Set.remove (get_proc_name pdesc) callees |> Procname.Set.elements


let find_map_nodes pdesc ~f = List.find_map ~f (get_nodes pdesc)

let find_map_instrs pdesc ~f =
  let find_map_node node = Instrs.find_map ~f (Node.get_instrs node) in
  find_map_nodes ~f:find_map_node pdesc


let update_nodes pdesc ~(update : Node.t -> bool) : bool =
  let f acc node = update node || acc in
  (* do not shortcut call to [update] *)
  fold_nodes pdesc ~init:false ~f


let replace_instrs pdesc ~f =
  let update node = Node.replace_instrs ~f node in
  update_nodes pdesc ~update


let replace_instrs_using_context pdesc ~f ~update_context ~context_at_node =
  let update node =
    Node.replace_instrs_using_context ~f ~update_context ~context_at_node:(context_at_node node)
      node
  in
  update_nodes pdesc ~update


let replace_instrs_by pdesc ~f =
  let update node = Node.replace_instrs_by ~f node in
  update_nodes pdesc ~update


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
let append_locals pdesc new_locals = pdesc.attributes.locals <- pdesc.attributes.locals @ new_locals

(** Set the successor nodes and exception nodes, and build predecessor links *)
let set_succs (node : Node.t) ~normal:succs_opt ~exn:exn_opt =
  let remove_pred pred_node (from_node : Node.t) =
    from_node.preds <- List.filter from_node.preds ~f:(fun pred -> not (Node.equal pred pred_node))
  in
  let add_pred pred_node (to_node : Node.t) = to_node.preds <- pred_node :: to_node.preds in
  Option.iter succs_opt ~f:(fun new_succs ->
      List.iter node.succs ~f:(remove_pred node) ;
      List.iter new_succs ~f:(add_pred node) ;
      node.succs <- new_succs ) ;
  Option.iter exn_opt ~f:(fun exn -> node.exn <- exn)


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
    ; exn= []
    ; code_block_exit= None }
  in
  pdesc.nodes <- node :: pdesc.nodes ;
  node


let create_node pdesc loc kind instrs =
  create_node_from_not_reversed pdesc loc kind (Instrs.of_list instrs)


let shallow_copy_code_from_pdesc ~orig_pdesc ~dest_pdesc =
  dest_pdesc.nodes <- orig_pdesc.nodes ;
  dest_pdesc.nodes_num <- orig_pdesc.nodes_num ;
  dest_pdesc.start_node <- orig_pdesc.start_node ;
  dest_pdesc.exit_node <- orig_pdesc.exit_node ;
  dest_pdesc.loop_heads <- orig_pdesc.loop_heads ;
  dest_pdesc.wto <- orig_pdesc.wto


(** Set the successor and exception nodes. If this is a join node right before the exit node, add an
    extra node in the middle, otherwise nullify and abstract instructions cannot be added after a
    conditional. *)
let node_set_succs pdesc (node : Node.t) ~normal:succs ~exn =
  match (node.kind, succs) with
  | Join_node, [({Node.kind= Exit_node} as exit_node)] ->
      let kind = Node.Stmt_node BetweenJoinAndExit in
      let node' = create_node_from_not_reversed pdesc node.loc kind node.instrs in
      set_succs node ~normal:(Some [node']) ~exn:(Some exn) ;
      set_succs node' ~normal:(Some [exit_node]) ~exn:(Some exn)
  | _ ->
      set_succs node ~normal:(Some succs) ~exn:(Some exn)


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
      let (_ : int) =
        WeakTopologicalOrder.Partition.fold_nodes wto ~init:0 ~f:(fun idx node ->
            node.Node.wto_index <- idx ;
            idx + 1 )
      in
      pdesc.wto <- Some wto ;
      wto


(** Get loop heads for widening. It collects all target nodes of back-edges in a depth-first
    traversal. We need to use the exceptional CFG otherwise we will miss loop heads in catch
    clauses. *)
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


let pp_captured_list fmt etl =
  List.iter
    ~f:(fun (id, ty, mode) ->
      Format.fprintf fmt " [%s] %a:%a"
        (Pvar.string_of_capture_mode mode)
        Mangled.pp id (Typ.pp_full Pp.text) ty )
    etl


let pp_objc_accessor fmt accessor =
  match accessor with
  | Some (ProcAttributes.Objc_getter field) ->
      Format.fprintf fmt "Getter of %a, " (Struct.pp_field Pp.text) field
  | Some (ProcAttributes.Objc_setter field) ->
      Format.fprintf fmt "Setter of %a, " (Struct.pp_field Pp.text) field
  | None ->
      ()


let pp_signature fmt pdesc =
  let attributes = get_attributes pdesc in
  let pname = get_proc_name pdesc in
  let defined_string = match is_defined pdesc with true -> "defined" | false -> "undefined" in
  Format.fprintf fmt "@[%a [%s, Return type: %a, %aFormals: %a, Locals: %a" Procname.pp pname
    defined_string (Typ.pp_full Pp.text) (get_ret_type pdesc) pp_objc_accessor
    attributes.ProcAttributes.objc_accessor pp_variable_list (get_formals pdesc) pp_locals_list
    (get_locals pdesc) ;
  if not (List.is_empty (get_captured pdesc)) then
    Format.fprintf fmt ", Captured: %a" pp_captured_list (get_captured pdesc) ;
  let method_annotation = attributes.ProcAttributes.method_annotation in
  ( if not (Annot.Method.is_empty method_annotation) then
    let pname_string = Procname.to_string pname in
    Format.fprintf fmt ", Annotation: %a" (Annot.Method.pp pname_string) method_annotation ) ;
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
    | Procname.ObjC_Cpp cpp_pname ->
        (* var is captured if the procedure is a lambda and the var is not in the locals or formals *)
        Procname.ObjC_Cpp.is_cpp_lambda cpp_pname
        && not
             ( List.exists ~f:pvar_local_matches (get_locals procdesc)
             || List.exists ~f:pvar_matches (get_formals procdesc) )
    | _ ->
        false
  in
  let pvar_matches_in_captured (name, _, _) = Mangled.equal name pvar_name in
  let is_captured_var_objc_block =
    (* var is captured if the procedure is a objc block and the var is in the captured *)
    Procname.is_objc_block procname
    && List.exists ~f:pvar_matches_in_captured (get_captured procdesc)
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


module SQLite = SqliteUtils.MarshalledNullableDataNOTForComparison (struct
  type nonrec t = t
end)

let load =
  let load_statement =
    ResultsDatabase.register_statement "SELECT cfg FROM procedures WHERE proc_uid = :k"
  in
  fun pname ->
    ResultsDatabase.with_registered_statement load_statement ~f:(fun db stmt ->
        Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (Procname.to_unique_id pname))
        |> SqliteUtils.check_result_code db ~log:"load bind proc_uid" ;
        SqliteUtils.result_single_column_option ~finalize:false ~log:"Procdesc.load" db stmt
        |> Option.bind ~f:SQLite.deserialize )
