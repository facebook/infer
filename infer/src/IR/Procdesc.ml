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

  type nodekind =
    | Start_node of Typ.Procname.t
    | Exit_node of Typ.Procname.t
    | Stmt_node of stmt_nodekind
    | Join_node
    | Prune_node of bool * Sil.if_kind * string  (** (true/false branch, if_kind, comment) *)
    | Skip_node of string
  [@@deriving compare]

  let equal_nodekind = [%compare.equal: nodekind]

  (** a node *)
  type t =
    { id: id  (** unique id of the node *)
    ; mutable dist_exit: int option  (** distance to the exit node *)
    ; mutable exn: t list  (** exception nodes in the cfg *)
    ; mutable instrs: Instrs.not_reversed_t  (** instructions for symbolic execution *)
    ; kind: nodekind  (** kind of node *)
    ; loc: Location.t  (** location in the source code *)
    ; mutable preds: t list  (** predecessor nodes in the cfg *)
    ; pname_opt: Typ.Procname.t option  (** name of the procedure the node belongs to *)
    ; mutable succs: t list  (** successor nodes in the cfg *) }

  let exn_handler_kind = Stmt_node ExceptionHandler

  let exn_sink_kind = Stmt_node ExceptionsSink

  let throw_kind = Stmt_node Throw

  let dummy pname_opt =
    { id= 0
    ; dist_exit= None
    ; instrs= Instrs.empty
    ; kind= Skip_node "dummy"
    ; loc= Location.dummy
    ; pname_opt
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

  module NodeSet = Caml.Set.Make (struct
    type t = node

    let compare = compare
  end)

  module IdMap = Caml.Map.Make (struct
    type t = id

    let compare = compare_id
  end)

  let get_exn node = node.exn

  (** Get the name of the procedure the node belongs to *)
  let get_proc_name node =
    match node.pname_opt with
    | None ->
        L.internal_error "get_proc_name: at node %d@\n" node.id ;
        assert false
    | Some pname ->
        pname


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


  let pp_id f id = F.pp_print_int f id

  let pp f node = pp_id f (get_id node)

  let get_distance_to_exit node = node.dist_exit

  (** Append the instructions to the list of instructions to execute *)
  let append_instrs node instrs =
    if instrs <> [] then node.instrs <- Instrs.append_list node.instrs instrs


  (** Map and replace the instructions to be executed *)
  let replace_instrs node ~f =
    let instrs' = Instrs.map_changed ~equal:phys_equal node.instrs ~f in
    if not (phys_equal instrs' node.instrs) then node.instrs <- instrs'


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


  (** Print extended instructions for the node,
      highlighting the given subinstruction if present *)
  let pp_instrs pe0 ~sub_instrs ~instro fmt node =
    if sub_instrs then
      let pe =
        match instro with None -> pe0 | Some instr -> Pp.extend_colormap pe0 (Obj.repr instr) Red
      in
      let instrs = get_instrs node in
      Instrs.pp pe fmt instrs
    else
      let () =
        match get_kind node with
        | Stmt_node s ->
            F.fprintf fmt "statements (%a)" pp_stmt s
        | Prune_node (_, _, descr) ->
            F.fprintf fmt "assume %s" descr
        | Exit_node _ ->
            F.pp_print_string fmt "exit"
        | Skip_node s ->
            F.fprintf fmt "skip (%s)" s
        | Start_node _ ->
            F.pp_print_string fmt "start"
        | Join_node ->
            F.pp_print_string fmt "join"
      in
      F.fprintf fmt "  %a " Location.pp (get_loc node)


  (** Dump extended instructions for the node *)
  let d_instrs ~(sub_instrs : bool) (curr_instr : Sil.instr option) (node : t) =
    L.add_print_with_pe ~color:Pp.Green (pp_instrs ~sub_instrs ~instro:curr_instr) node


  (** Return a description of the cfg node *)
  let get_description pe node =
    let str =
      match get_kind node with
      | Stmt_node _ ->
          "Instructions"
      | Prune_node (_, _, descr) ->
          "Conditional" ^ " " ^ descr
      | Exit_node _ ->
          "Exit"
      | Skip_node _ ->
          "Skip"
      | Start_node _ ->
          "Start"
      | Join_node ->
          "Join"
    in
    let pp fmt = F.fprintf fmt "%s@.%a" str (pp_instrs pe ~instro:None ~sub_instrs:true) node in
    F.asprintf "%t" pp


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
          | Sil.Remove_temps _ ->
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
  { attributes: ProcAttributes.t  (** attributes of the procedure *)
  ; mutable nodes: Node.t list  (** list of nodes of this procedure *)
  ; mutable nodes_num: int  (** number of nodes *)
  ; mutable start_node: Node.t  (** start node of this procedure *)
  ; mutable exit_node: Node.t  (** exit node of this procedure *)
  ; mutable loop_heads: NodeSet.t option  (** loop head nodes of this procedure *) }

let from_proc_attributes attributes =
  let pname_opt = Some attributes.ProcAttributes.proc_name in
  let start_node = Node.dummy pname_opt in
  let exit_node = Node.dummy pname_opt in
  {attributes; nodes= []; nodes_num= 0; start_node; exit_node; loop_heads= None}


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

let get_exit_node pdesc = pdesc.exit_node

(** Return name and type of formal parameters *)
let get_formals pdesc = pdesc.attributes.formals

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

let get_proc_name pdesc = pdesc.attributes.proc_name

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
  let do_node node = Node.replace_instrs ~f node in
  iter_nodes do_node pdesc


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
let create_node_internal pdesc loc kind instrs =
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


let create_node pdesc loc kind instrs = create_node_internal pdesc loc kind (Instrs.of_list instrs)

(** Set the successor and exception nodes.
    If this is a join node right before the exit node, add an extra node in the middle,
    otherwise nullify and abstract instructions cannot be added after a conditional. *)
let node_set_succs_exn pdesc (node : Node.t) succs exn =
  match (node.kind, succs) with
  | Join_node, [({Node.kind= Exit_node _} as exit_node)] ->
      let kind = Node.Stmt_node BetweenJoinAndExit in
      let node' = create_node_internal pdesc node.loc kind node.instrs in
      set_succs_exn_base node [node'] exn ;
      set_succs_exn_base node' [exit_node] exn
  | _ ->
      set_succs_exn_base node succs exn


(** Get loop heads for widening.
    It collects all target nodes of back-edges in a depth-first
    traversal.
    *)
let get_loop_heads pdesc =
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


let is_loop_head pdesc (node : Node.t) =
  let lh = match pdesc.loop_heads with Some lh -> lh | None -> get_loop_heads pdesc in
  NodeSet.mem node lh


let pp_var_attributes fmt attrs =
  let pp_attribute fmt attr =
    match attr with ProcAttributes.Modify_in_block -> Format.pp_print_string fmt "__block"
  in
  if List.is_empty attrs then () else F.fprintf fmt "(%a)" (Pp.seq ~sep:"," pp_attribute) attrs


let pp_local fmt (var_data : ProcAttributes.var_data) =
  Format.fprintf fmt " %a:%a%a" Mangled.pp var_data.name (Typ.pp_full Pp.text) var_data.typ
    pp_var_attributes var_data.attributes


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
let is_captured_var procdesc pvar =
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


let has_modify_in_block_attr procdesc pvar =
  let pvar_name = Pvar.get_name pvar in
  let pvar_local_matches (var_data : ProcAttributes.var_data) =
    Mangled.equal var_data.name pvar_name
    && List.exists var_data.attributes ~f:(fun attr ->
           ProcAttributes.var_attribute_equal attr ProcAttributes.Modify_in_block )
  in
  List.exists ~f:pvar_local_matches (get_locals procdesc)


(** Applies f_instr_list to all the instructions in all the nodes of the cfg *)
let convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list =
  let resolved_pname = get_proc_name resolved_pdesc
  and callee_start_node = get_start_node callee_pdesc
  and callee_exit_node = get_exit_node callee_pdesc in
  let convert_node_kind = function
    | Node.Start_node _ ->
        Node.Start_node resolved_pname
    | Node.Exit_node _ ->
        Node.Exit_node resolved_pname
    | node_kind ->
        node_kind
  in
  let node_map = ref NodeMap.empty in
  let rec convert_node node =
    let loc = Node.get_loc node
    and kind = convert_node_kind (Node.get_kind node)
    and instrs = f_instr_list (Node.get_instrs node) in
    create_node_internal resolved_pdesc loc kind instrs
  and loop callee_nodes =
    match callee_nodes with
    | [] ->
        []
    | node :: other_node ->
        let converted_node =
          try NodeMap.find node !node_map with Caml.Not_found ->
            let new_node = convert_node node
            and successors = Node.get_succs node
            and exn_nodes = Node.get_exn node in
            node_map := NodeMap.add node new_node !node_map ;
            if Node.equal node callee_start_node then set_start_node resolved_pdesc new_node ;
            if Node.equal node callee_exit_node then set_exit_node resolved_pdesc new_node ;
            node_set_succs_exn callee_pdesc new_node (loop successors) (loop exn_nodes) ;
            new_node
        in
        converted_node :: loop other_node
  in
  ignore (loop [callee_start_node]) ;
  resolved_pdesc


(** clone a procedure description and apply the type substitutions where
      the parameters are used *)
let specialize_types_proc callee_pdesc resolved_pdesc substitutions =
  let resolved_pname = get_proc_name resolved_pdesc in
  let convert_pvar pvar = Pvar.mk (Pvar.get_name pvar) resolved_pname in
  let mk_ptr_typ typename =
    (* Only consider pointers from objects for now *)
    Typ.mk (Tptr (Typ.mk (Tstruct typename), Typ.Pk_pointer))
  in
  let convert_exp = function
    | Exp.Lvar origin_pvar ->
        Exp.Lvar (convert_pvar origin_pvar)
    | exp ->
        exp
  in
  let subst_map = ref Ident.Map.empty in
  let redirect_typename origin_id =
    try Some (Ident.Map.find origin_id !subst_map) with Caml.Not_found -> None
  in
  let convert_instr = function
    | Sil.Load
        ( id
        , (Exp.Lvar origin_pvar as origin_exp)
        , {Typ.desc= Tptr ({desc= Tstruct origin_typename}, Pk_pointer)}
        , loc ) ->
        let specialized_typname =
          try Mangled.Map.find (Pvar.get_name origin_pvar) substitutions with Caml.Not_found ->
            origin_typename
        in
        subst_map := Ident.Map.add id specialized_typname !subst_map ;
        Some (Sil.Load (id, convert_exp origin_exp, mk_ptr_typ specialized_typname, loc))
    | Sil.Load (id, (Exp.Var origin_id as origin_exp), ({Typ.desc= Tstruct _} as origin_typ), loc)
      ->
        let updated_typ : Typ.t =
          try Typ.mk ~default:origin_typ (Tstruct (Ident.Map.find origin_id !subst_map))
          with Caml.Not_found -> origin_typ
        in
        Some (Sil.Load (id, convert_exp origin_exp, updated_typ, loc))
    | Sil.Load (id, origin_exp, origin_typ, loc) ->
        Some (Sil.Load (id, convert_exp origin_exp, origin_typ, loc))
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        Some set_instr
    | Sil.Call
        ( return_ids
        , Exp.Const (Const.Cfun callee_pname)
        , (Exp.Var id, _) :: origin_args
        , loc
        , call_flags )
      when call_flags.CallFlags.cf_virtual && redirect_typename id <> None ->
        let redirected_typename = Option.value_exn (redirect_typename id) in
        let redirected_typ = mk_ptr_typ redirected_typename in
        let redirected_pname = Typ.Procname.replace_class callee_pname redirected_typename in
        let args =
          let other_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
          (Exp.Var id, redirected_typ) :: other_args
        in
        let call_instr =
          Sil.Call (return_ids, Exp.Const (Const.Cfun redirected_pname), args, loc, call_flags)
        in
        Some call_instr
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
        let call_instr =
          Sil.Call (return_ids, convert_exp origin_call_exp, converted_args, loc, call_flags)
        in
        Some call_instr
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        Some (Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind))
    | Sil.Nullify _ | Abstract _ | Sil.Remove_temps _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        None
  in
  let f_instr_list instrs = Instrs.filter_map ~f:convert_instr instrs in
  convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list


exception UnmatchedParameters

(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)
let specialize_types ?(has_clang_model = false) callee_pdesc resolved_pname args =
  let callee_attributes = get_attributes callee_pdesc in
  let resolved_params, substitutions =
    try
      List.fold2_exn
        ~f:(fun (params, subts) (param_name, param_typ) (_, arg_typ) ->
          match arg_typ.Typ.desc with
          | Tptr ({desc= Tstruct typename}, Pk_pointer) ->
              (* Replace the type of the parameter by the type of the argument *)
              ((param_name, arg_typ) :: params, Mangled.Map.add param_name typename subts)
          | _ ->
              ((param_name, param_typ) :: params, subts) )
        ~init:([], Mangled.Map.empty) callee_attributes.formals args
    with Invalid_argument _ ->
      L.(debug Analysis Medium)
        "Call mismatch: method %a has %i paramters but is called with %i arguments@."
        Typ.Procname.pp resolved_pname
        (List.length callee_attributes.formals)
        (List.length args) ;
      raise UnmatchedParameters
  in
  let translation_unit =
    (* If it is a model, and we are using the procdesc stored in the summary, the default translation unit
       won't be useful because we don't store that tenv, so we aim to find the source file of the caller to
       use its tenv. *)
    if has_clang_model then
      let pname = get_proc_name callee_pdesc in
      match Attributes.find_file_capturing_procedure pname with
      | Some (source_file, _) ->
          source_file
      | None ->
          Logging.die InternalError
            "specialize_types should only be called with defined procedures, but we cannot find \
             the captured file of procname %a"
            Typ.Procname.pp pname
    else callee_attributes.translation_unit
  in
  let resolved_attributes =
    { callee_attributes with
      formals= List.rev resolved_params
    ; proc_name= resolved_pname
    ; is_specialized= true
    ; translation_unit }
  in
  Attributes.store resolved_attributes ;
  let resolved_pdesc = from_proc_attributes resolved_attributes in
  specialize_types_proc callee_pdesc resolved_pdesc substitutions


let specialize_with_block_args_instrs resolved_pdesc substitutions =
  let resolved_pname = get_proc_name resolved_pdesc in
  let convert_pvar pvar = Pvar.mk (Pvar.get_name pvar) resolved_pname in
  let convert_exp exp =
    match exp with
    | Exp.Lvar origin_pvar ->
        let new_pvar = convert_pvar origin_pvar in
        Exp.Lvar new_pvar
    | Exp.Lfield (Exp.Lvar origin_pvar, fname, typ) ->
        let new_pvar = convert_pvar origin_pvar in
        Exp.Lfield (Exp.Lvar new_pvar, fname, typ)
    | _ ->
        exp
  in
  let convert_instr (instrs, id_map) instr =
    let get_block_name_and_load_captured_vars_instrs block_var loc =
      let block_name, extra_formals = Mangled.Map.find block_var substitutions in
      let ids, id_exp_typs, load_instrs =
        List.map extra_formals ~f:(fun (var, typ) ->
            let id = Ident.create_fresh_specialized_with_blocks Ident.knormal in
            let pvar = Pvar.mk var resolved_pname in
            (id, (Exp.Var id, pvar, typ), Sil.Load (id, Exp.Lvar pvar, typ, loc)) )
        |> List.unzip3
      in
      let remove_temps_instr = Sil.Remove_temps (ids, loc) in
      (block_name, id_exp_typs, load_instrs, remove_temps_instr)
    in
    let convert_generic_call return_ids exp origin_args loc call_flags =
      let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
      let call_instr = Sil.Call (return_ids, exp, converted_args, loc, call_flags) in
      (call_instr :: instrs, id_map)
    in
    match instr with
    | Sil.Load (id, Exp.Lvar block_param, _, _)
      when Mangled.Map.mem (Pvar.get_name block_param) substitutions ->
        let id_map = Ident.Map.add id (Pvar.get_name block_param) id_map in
        (* we don't need the load the block param instruction anymore *)
        (instrs, id_map)
    | Sil.Load (id, origin_exp, origin_typ, loc) ->
        (Sil.Load (id, convert_exp origin_exp, origin_typ, loc) :: instrs, id_map)
    | Sil.Store (assignee_exp, origin_typ, Exp.Var id, loc) when Ident.Map.mem id id_map ->
        let block_param = Ident.Map.find id id_map in
        let block_name, id_exp_typs, load_instrs, remove_temps_instr =
          get_block_name_and_load_captured_vars_instrs block_param loc
        in
        let closure = Exp.Closure {name= block_name; captured_vars= id_exp_typs} in
        let instr = Sil.Store (assignee_exp, origin_typ, closure, loc) in
        ((remove_temps_instr :: instr :: load_instrs) @ instrs, id_map)
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        (set_instr :: instrs, id_map)
    | Sil.Call (return_ids, Exp.Var id, origin_args, loc, call_flags) -> (
      try
        let block_name, id_exp_typs, load_instrs, remove_temps_instr =
          let block_var = Ident.Map.find id id_map in
          get_block_name_and_load_captured_vars_instrs block_var loc
        in
        let call_instr =
          let id_exps = List.map ~f:(fun (id, _, typ) -> (id, typ)) id_exp_typs in
          let converted_args =
            List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args
          in
          Sil.Call
            ( return_ids
            , Exp.Const (Const.Cfun block_name)
            , id_exps @ converted_args
            , loc
            , call_flags )
        in
        let instrs = (remove_temps_instr :: call_instr :: load_instrs) @ instrs in
        (instrs, id_map)
      with Caml.Not_found ->
        convert_generic_call return_ids (Exp.Var id) origin_args loc call_flags )
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        convert_generic_call return_ids origin_call_exp origin_args loc call_flags
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        (Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind) :: instrs, id_map)
    | Sil.Nullify _ | Abstract _ | Sil.Remove_temps _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        (instrs, id_map)
  in
  let f_instr_list instrs =
    let rev_instrs, _ = Instrs.fold ~f:convert_instr ~init:([], Ident.Map.empty) instrs in
    Instrs.of_rev_list rev_instrs
  in
  f_instr_list


let append_no_duplicates_formals_and_annot =
  Staged.unstage
    (IList.append_no_duplicates ~cmp:(fun ((name1, _), _) ((name2, _), _) ->
         Mangled.compare name1 name2 ))


let specialize_with_block_args callee_pdesc pname_with_block_args block_args =
  let callee_attributes = get_attributes callee_pdesc in
  (* Substitution from a block parameter to the block name and the new formals
  that correspond to the captured variables *)
  let substitutions : (Typ.Procname.t * (Mangled.t * Typ.t) list) Mangled.Map.t =
    List.fold2_exn callee_attributes.formals block_args ~init:Mangled.Map.empty
      ~f:(fun subts (param_name, _) block_arg_opt ->
        match block_arg_opt with
        | Some (cl : Exp.closure) ->
            let formals_from_captured =
              List.map
                ~f:(fun (_, var, typ) ->
                  (* Here we create fresh names for the new formals, based on the names of the captured
                   variables annotated with the name of the caller method *)
                  (Pvar.get_name_of_local_with_procname var, typ) )
                cl.captured_vars
            in
            Mangled.Map.add param_name (cl.name, formals_from_captured) subts
        | None ->
            subts )
  in
  (* Extend formals with fresh variables for the captured variables of the block arguments,
    without duplications. *)
  let new_formals_blocks_captured_vars, extended_formals_annots =
    let new_formals_blocks_captured_vars_with_annots =
      let formals_annots =
        List.zip_exn callee_attributes.formals (snd callee_attributes.method_annotation)
      in
      List.fold formals_annots ~init:[] ~f:(fun acc ((param_name, typ), annot) ->
          try
            let _, captured = Mangled.Map.find param_name substitutions in
            append_no_duplicates_formals_and_annot acc
              (List.map captured ~f:(fun captured_var -> (captured_var, Annot.Item.empty)))
          with Caml.Not_found ->
            append_no_duplicates_formals_and_annot acc [((param_name, typ), annot)] )
    in
    List.unzip new_formals_blocks_captured_vars_with_annots
  in
  let translation_unit =
    let pname = get_proc_name callee_pdesc in
    match Attributes.find_file_capturing_procedure pname with
    | Some (source_file, _) ->
        source_file
    | None ->
        Logging.die InternalError
          "specialize_with_block_args ahould only be called with defined procedures, but we \
           cannot find the captured file of procname %a"
          Typ.Procname.pp pname
  in
  let resolved_attributes =
    { callee_attributes with
      proc_name= pname_with_block_args
    ; is_defined= true
    ; formals= new_formals_blocks_captured_vars
    ; method_annotation= (fst callee_attributes.method_annotation, extended_formals_annots)
    ; translation_unit }
  in
  Attributes.store resolved_attributes ;
  let resolved_pdesc = from_proc_attributes resolved_attributes in
  Logging.(debug Analysis Verbose) "signature of base method %a@." pp_signature callee_pdesc ;
  Logging.(debug Analysis Verbose)
    "signature of specialized method %a@." pp_signature resolved_pdesc ;
  convert_cfg ~callee_pdesc ~resolved_pdesc
    ~f_instr_list:(specialize_with_block_args_instrs resolved_pdesc substitutions)


let is_connected proc_desc =
  let is_exit_node n = match Node.get_kind n with Node.Exit_node _ -> true | _ -> false in
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
    | Node.Start_node _ ->
        if List.is_empty succs || not (List.is_empty preds) then Error `Other else Ok ()
    | Node.Exit_node _ ->
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
