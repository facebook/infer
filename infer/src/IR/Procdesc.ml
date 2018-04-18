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

  (** Get the node kind *)
  let get_kind node = node.kind

  (** Get the instructions to be executed *)
  let get_instrs node = node.instrs

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
    let construct_decl (var_data: ProcAttributes.var_data) =
      (Pvar.mk var_data.name pname, var_data.typ)
    in
    let ptl = ret_var :: List.map ~f:construct_decl locals in
    let instr = Sil.Declare_locals (ptl, loc) in
    prepend_instrs node [instr]


  (** Print extended instructions for the node,
      highlighting the given subinstruction if present *)
  let pp_instrs pe0 ~sub_instrs instro fmt node =
    if sub_instrs then
      let pe =
        match instro with None -> pe0 | Some instr -> Pp.extend_colormap pe0 (Obj.repr instr) Red
      in
      let instrs = get_instrs node in
      Sil.pp_instr_list pe fmt instrs
    else
      let () =
        match get_kind node with
        | Stmt_node s ->
            F.fprintf fmt "statements (%s)" s
        | Prune_node (_, _, descr) ->
            F.fprintf fmt "assume %s" descr
        | Exit_node _ ->
            F.fprintf fmt "exit"
        | Skip_node s ->
            F.fprintf fmt "skip (%s)" s
        | Start_node _ ->
            F.fprintf fmt "start"
        | Join_node ->
            F.fprintf fmt "join"
      in
      F.fprintf fmt "  %a " Location.pp (get_loc node)


  (** Dump extended instructions for the node *)
  let d_instrs ~(sub_instrs: bool) (curr_instr: Sil.instr option) (node: t) =
    L.add_print_action (L.PTnode_instrs, Obj.repr (sub_instrs, curr_instr, node))


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
  ; mutable exit_node: Node.t  (** exit node of this procedure *)
  ; mutable loop_heads: NodeSet.t option  (** loop head nodes of this procedure *) }
[@@deriving compare]

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
    let do_node (node: Node.t) =
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

(** Return name and type of captured variables *)
let get_captured pdesc = pdesc.attributes.captured

(** Return the visibility attribute *)
let get_access pdesc = pdesc.attributes.access

let get_nodes pdesc = pdesc.nodes

let get_nodes_num pdesc = pdesc.nodes_num

let get_proc_name pdesc = pdesc.attributes.proc_name

(** Return the return type of the procedure *)
let get_ret_type pdesc = pdesc.attributes.ret_type

let get_ret_var pdesc = Pvar.mk Ident.name_return (get_proc_name pdesc)

let get_start_node pdesc = pdesc.start_node

(** Return [true] iff the procedure is defined, and not just declared *)
let is_defined pdesc = pdesc.attributes.is_defined

let is_java_synchronized pdesc = pdesc.attributes.is_java_synchronized_method

let iter_nodes f pdesc = List.iter ~f (List.rev (get_nodes pdesc))

let iter_instrs f pdesc =
  let do_node node = List.iter ~f:(fun i -> f node i) (Node.get_instrs node) in
  iter_nodes do_node pdesc


let fold_nodes pdesc ~init ~f = List.fold ~f ~init (List.rev (get_nodes pdesc))

let fold_instrs pdesc ~init ~f =
  let fold_node acc node =
    List.fold ~f:(fun acc instr -> f acc node instr) ~init:acc (Node.get_instrs node)
  in
  fold_nodes ~f:fold_node ~init pdesc


(** iterate between two nodes or until we reach a branching structure *)
let iter_slope_range f src_node dst_node =
  let visited = ref NodeSet.empty in
  let rec do_node node =
    visited := NodeSet.add node !visited ;
    f node ;
    match Node.get_succs node with
    | [n] ->
        if not (NodeSet.mem n !visited) && not (Node.equal node dst_node) then do_node n
    | _ ->
        ()
  in
  do_node src_node


(** Set the exit node of the proc desc *)
let set_exit_node pdesc node = pdesc.exit_node <- node

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
  | Join_node, [({Node.kind= Exit_node _} as exit_node)] ->
      let kind = Node.Stmt_node "between_join_and_exit" in
      let node' = create_node pdesc node.loc kind node.instrs in
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


let is_loop_head pdesc (node: Node.t) =
  let lh = match pdesc.loop_heads with Some lh -> lh | None -> get_loop_heads pdesc in
  NodeSet.mem node lh


let pp_var_attributes fmt attrs =
  let pp_attribute fmt attr =
    match attr with ProcAttributes.Modify_in_block -> Format.fprintf fmt "__block"
  in
  if List.is_empty attrs then () else F.fprintf fmt "(%a)" (Pp.seq ~sep:"," pp_attribute) attrs


let pp_local fmt (var_data: ProcAttributes.var_data) =
  Format.fprintf fmt " %a:%a%a" Mangled.pp var_data.name (Typ.pp_full Pp.text) var_data.typ
    pp_var_attributes var_data.attributes


let pp_locals_list fmt etl =
  if List.is_empty etl then Format.fprintf fmt "None" else List.iter ~f:(pp_local fmt) etl


let pp_variable_list fmt etl =
  if List.is_empty etl then Format.fprintf fmt "None"
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
  Format.fprintf fmt "%s [%s, Return type: %s, %aFormals: %a, Locals: %a" pname_string
    defined_string
    (Typ.to_string (get_ret_type pdesc))
    pp_objc_accessor attributes.ProcAttributes.objc_accessor pp_variable_list (get_formals pdesc)
    pp_locals_list (get_locals pdesc) ;
  if not (List.is_empty (get_captured pdesc)) then
    Format.fprintf fmt ", Captured: %a" pp_variable_list (get_captured pdesc) ;
  let method_annotation = attributes.ProcAttributes.method_annotation in
  if not (Annot.Method.is_empty method_annotation) then
    Format.fprintf fmt ", Annotation: %a" (Annot.Method.pp pname_string) method_annotation ;
  Format.fprintf fmt "]@\n"


let is_specialized pdesc =
  let attributes = get_attributes pdesc in
  attributes.ProcAttributes.is_specialized


(* true if pvar is a captured variable of a cpp lambda or objc block *)
let is_captured_var procdesc pvar =
  let procname = get_proc_name procdesc in
  let pvar_name = Pvar.get_name pvar in
  let pvar_local_matches (var_data: ProcAttributes.var_data) =
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
  let pvar_local_matches (var_data: ProcAttributes.var_data) =
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
    create_node resolved_pdesc loc kind instrs
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
    (* Only consider pointers from Java objects for now *)
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
  let convert_instr instrs = function
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
        Sil.Load (id, convert_exp origin_exp, mk_ptr_typ specialized_typname, loc) :: instrs
    | Sil.Load (id, (Exp.Var origin_id as origin_exp), ({Typ.desc= Tstruct _} as origin_typ), loc) ->
        let updated_typ : Typ.t =
          try Typ.mk ~default:origin_typ (Tstruct (Ident.Map.find origin_id !subst_map))
          with Caml.Not_found -> origin_typ
        in
        Sil.Load (id, convert_exp origin_exp, updated_typ, loc) :: instrs
    | Sil.Load (id, origin_exp, origin_typ, loc) ->
        Sil.Load (id, convert_exp origin_exp, origin_typ, loc) :: instrs
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        set_instr :: instrs
    | Sil.Call
        ( return_ids
        , Exp.Const (Const.Cfun (Typ.Procname.Java callee_pname_java))
        , (Exp.Var id, _) :: origin_args
        , loc
        , call_flags )
      when call_flags.CallFlags.cf_virtual && redirect_typename id <> None ->
        let redirected_typename = Option.value_exn (redirect_typename id) in
        let redirected_typ = mk_ptr_typ redirected_typename in
        let redirected_pname =
          Typ.Procname.replace_class (Typ.Procname.Java callee_pname_java) redirected_typename
        in
        let args =
          let other_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
          (Exp.Var id, redirected_typ) :: other_args
        in
        let call_instr =
          Sil.Call (return_ids, Exp.Const (Const.Cfun redirected_pname), args, loc, call_flags)
        in
        call_instr :: instrs
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        let converted_args = List.map ~f:(fun (exp, typ) -> (convert_exp exp, typ)) origin_args in
        let call_instr =
          Sil.Call (return_ids, convert_exp origin_call_exp, converted_args, loc, call_flags)
        in
        call_instr :: instrs
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind) :: instrs
    | Sil.Declare_locals (typed_vars, loc) ->
        let new_typed_vars =
          List.map ~f:(fun (pvar, typ) -> (convert_pvar pvar, typ)) typed_vars
        in
        Sil.Declare_locals (new_typed_vars, loc) :: instrs
    | Sil.Nullify _ | Abstract _ | Sil.Remove_temps _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        instrs
  in
  let f_instr_list instrs = List.fold ~f:convert_instr ~init:[] instrs |> List.rev in
  convert_cfg ~callee_pdesc ~resolved_pdesc ~f_instr_list


(** Creates a copy of a procedure description and a list of type substitutions of the form
    (name, typ) where name is a parameter. The resulting proc desc is isomorphic but
    all the type of the parameters are replaced in the instructions according to the list.
    The virtual calls are also replaced to match the parameter types *)
let specialize_types callee_pdesc resolved_pname args =
  let callee_attributes = get_attributes callee_pdesc in
  let resolved_params, substitutions =
    List.fold2_exn
      ~f:(fun (params, subts) (param_name, param_typ) (_, arg_typ) ->
        match arg_typ.Typ.desc with
        | Tptr ({desc= Tstruct typename}, Pk_pointer) ->
            (* Replace the type of the parameter by the type of the argument *)
            ((param_name, arg_typ) :: params, Mangled.Map.add param_name typename subts)
        | _ ->
            ((param_name, param_typ) :: params, subts) )
      ~init:([], Mangled.Map.empty) callee_attributes.formals args
  in
  let resolved_attributes =
    { callee_attributes with
      formals= List.rev resolved_params
    ; proc_name= resolved_pname
    ; is_specialized= true
    ; err_log= Errlog.empty () }
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
    | _ ->
        exp
  in
  let convert_instr (instrs, id_map) instr =
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
    | Sil.Store (assignee_exp, origin_typ, origin_exp, loc) ->
        let set_instr =
          Sil.Store (convert_exp assignee_exp, origin_typ, convert_exp origin_exp, loc)
        in
        (set_instr :: instrs, id_map)
    | Sil.Call (return_ids, Exp.Var id, origin_args, loc, call_flags) -> (
      try
        let block_name, extra_formals =
          let block_var = Ident.Map.find id id_map in
          Mangled.Map.find block_var substitutions
        in
        (* once we find the block in the map, it means that we need to subsitute it with the
        call to the concrete block, and pass the fresh formals as arguments *)
        let ids_typs, load_instrs =
          let captured_ids_instrs =
            List.map extra_formals ~f:(fun (var, typ) ->
                let id = Ident.create_fresh Ident.knormal in
                let pvar = Pvar.mk var resolved_pname in
                ((id, typ), Sil.Load (id, Exp.Lvar pvar, typ, loc)) )
          in
          List.unzip captured_ids_instrs
        in
        let call_instr =
          let id_exps = List.map ~f:(fun (id, typ) -> (Exp.Var id, typ)) ids_typs in
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
        let remove_temps_instrs =
          let ids = List.map ~f:(fun (id, _) -> id) ids_typs in
          Sil.Remove_temps (ids, loc)
        in
        let instrs = remove_temps_instrs :: call_instr :: load_instrs @ instrs in
        (instrs, id_map)
      with Caml.Not_found ->
        convert_generic_call return_ids (Exp.Var id) origin_args loc call_flags )
    | Sil.Call (return_ids, origin_call_exp, origin_args, loc, call_flags) ->
        convert_generic_call return_ids origin_call_exp origin_args loc call_flags
    | Sil.Prune (origin_exp, loc, is_true_branch, if_kind) ->
        (Sil.Prune (convert_exp origin_exp, loc, is_true_branch, if_kind) :: instrs, id_map)
    | Sil.Declare_locals (typed_vars, loc) ->
        let new_typed_vars =
          List.map ~f:(fun (pvar, typ) -> (convert_pvar pvar, typ)) typed_vars
        in
        (Sil.Declare_locals (new_typed_vars, loc) :: instrs, id_map)
    | Sil.Nullify _ | Abstract _ | Sil.Remove_temps _ ->
        (* these are generated instructions that will be replaced by the preanalysis *)
        (instrs, id_map)
  in
  let f_instr_list instrs =
    let instrs, _ = List.fold ~f:convert_instr ~init:([], Ident.Map.empty) instrs in
    List.rev instrs
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
    List.fold2_exn callee_attributes.formals block_args ~init:Mangled.Map.empty ~f:
      (fun subts (param_name, _) block_arg_opt ->
        match block_arg_opt with
        | Some (cl: Exp.closure) ->
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
  let source_file_captured =
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
    ; err_log= Errlog.empty ()
    ; formals= new_formals_blocks_captured_vars
    ; method_annotation= (fst callee_attributes.method_annotation, extended_formals_annots)
    ; source_file_captured }
  in
  Attributes.store resolved_attributes ;
  let resolved_pdesc = from_proc_attributes resolved_attributes in
  Logging.(debug Analysis Verbose) "signature of base method %a@." pp_signature callee_pdesc ;
  Logging.(debug Analysis Verbose)
    "signature of specialized method %a@." pp_signature resolved_pdesc ;
  convert_cfg ~callee_pdesc ~resolved_pdesc
    ~f_instr_list:(specialize_with_block_args_instrs resolved_pdesc substitutions)
