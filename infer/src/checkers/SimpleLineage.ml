(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** NOTE: If performance needs to be improved, especially memory, consider using [ProcCfg.Normal].
    One node per instruction leads to bigger invariant maps, and more states (with less than perfect
    sharing of memory). *)
module CFG = ProcCfg.NormalOneInstrPerNode

module PPNode = struct
  include CFG.Node

  let pp fmt node = pp_id fmt (id node)
end

module LineageGraph = struct
  type data =
    | Local of ((Var.t * PPNode.t)[@sexp.opaque])
    | Argument of int
    | Return
    | ArgumentOf of int * (Procname.t[@sexp.opaque])
    | ReturnOf of (Procname.t[@sexp.opaque])
  [@@deriving compare, sexp]

  type flow_kind =
    | Direct  (** Immediate copy; e.g., assigment or passing an argument *)
    | Summary  (** Summarizes the effect of a procedure call *)

  type flow = {source: data; target: data; kind: flow_kind}

  let flow ?(kind = Direct) ~source ~target () = {source; target; kind}

  type t = flow list

  let pp_data fmt data =
    match data with
    | Local (variable, node) ->
        Format.fprintf fmt "%a@@%a" Var.pp variable PPNode.pp node
    | Argument index ->
        Format.fprintf fmt "arg%d" index
    | Return ->
        Format.fprintf fmt "ret"
    | ArgumentOf (index, proc_name) ->
        Format.fprintf fmt "%a.arg%d" Procname.pp proc_name index
    | ReturnOf proc_name ->
        Format.fprintf fmt "%a.ret" Procname.pp proc_name


  let pp_flow fmt {source; target} =
    Format.fprintf fmt "@[<2>[%a@ ->@ %a]@]@;" pp_data source pp_data target


  let pp fmt flows =
    Format.fprintf fmt "@;@[<2>LineageGraph@;%a@]" (Format.pp_print_list pp_flow) flows
end

module Summary = struct
  (** TITO stands for "taint-in taint-out". In this context a tito argument is one that has a path
      to the return node, without going through call edges; more precisely, [i] is a tito argument
      if there is a path from [Argument i] to [Return] not going through [ArgumentOf _] nodes. *)
  type tito_arguments = IntSet.t

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments}

  let pp_tito_arguments fmt arguments =
    let pp_sep fmt () = Format.fprintf fmt ",@;" in
    Format.fprintf fmt "@;@[<2>TitoArguments@;%a@]"
      (Format.pp_print_list ~pp_sep Int.pp)
      (IntSet.elements arguments)


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<2>LineageSummary@;%a%a@]" pp_tito_arguments tito_arguments
      LineageGraph.pp graph


  (** Given a graph, computes tito_arguments, and makes a summary. *)
  let of_graph graph =
    (* Construct an adjacency list representation of the reversed graph. *)
    let module N = struct
      module T = struct
        type t = LineageGraph.data [@@deriving compare, sexp]
      end

      include T
      include Comparable.Make (T)
    end in
    let graph_rev =
      let add_flow g {LineageGraph.source; target} =
        match target with
        | ArgumentOf _ ->
            g (* skip call edges *)
        | _ ->
            Map.add_multi ~key:target ~data:source g
      in
      List.fold ~init:N.Map.empty ~f:add_flow graph
    in
    (* Do a DFS, to see which arguments are reachable from return. *)
    let rec dfs seen node =
      if Set.mem seen node then seen
      else
        let seen = Set.add seen node in
        let parents = Map.find_multi graph_rev node in
        List.fold ~init:seen ~f:dfs parents
    in
    let reachable = dfs N.Set.empty LineageGraph.Return in
    (* Collect reachable arguments. *)
    let tito_arguments =
      let add_node args (node : LineageGraph.data) =
        match node with Argument i -> IntSet.add i args | _ -> args
      in
      Set.fold reachable ~init:IntSet.empty ~f:add_node
    in
    {graph; tito_arguments}
end

(** A summary is computed by taking the union of all partial summaries present in the final
    invariant map. Partial summaries are stored in abstract states only because it is convenient.
    But, they do not influence how abstract states are joined, widened, etc, which explains why
    below all functions having to do with the abstract domain are dummies. *)
module PartialSummary = struct
  type t = LineageGraph.t

  let pp = LineageGraph.pp

  let bottom = []

  let leq ~lhs:_ ~rhs:_ = true

  let join _ _ = bottom

  let widen ~prev:_ ~next ~num_iters:_ = next
end

module Domain = struct
  module Real = AbstractDomain.FiniteMultiMap (Var) (PPNode)
  module Unit = PartialSummary
  include AbstractDomain.Pair (Real) (Unit)
end

module TransferFunctions = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Summary.t InterproceduralAnalysis.t

  let get_written_var (instr : Sil.instr) : Var.t option =
    match instr with
    | Load {id} ->
        if Ident.is_none id then None else Some (Var.of_id id)
    | Store {e1= Lvar lhs} ->
        Some (Var.of_pvar lhs)
    | Store _ ->
        L.debug Analysis Verbose
          "SimpleLineage: The only lhs I can handle (now) for Store is Lvar@\n" ;
        None
    | Prune _ ->
        None
    | Call _ ->
        L.die InternalError "exec_instr should special-case this"
    | Metadata _ ->
        None


  let vars_of_exp (e : Exp.t) : Var.Set.t =
    Var.get_all_vars_in_exp e |> Sequence.to_seq |> Var.Set.of_seq


  let vars_of_exp_list (expressions : Exp.t list) : Var.Set.t =
    List.map ~f:Var.get_all_vars_in_exp expressions
    |> Sequence.of_list |> Sequence.concat |> Sequence.to_seq |> Var.Set.of_seq


  let get_read_set (instr : Sil.instr) : Var.Set.t =
    match instr with
    | Load {e} ->
        vars_of_exp e
    | Store {e2} ->
        vars_of_exp e2
    | Prune (e, _location, _true, _kind) ->
        vars_of_exp e
    | Call _ ->
        L.die InternalError "exec_instr should special-case this"
    | Metadata _ ->
        Var.Set.empty


  let procname_of_exp (e : Exp.t) : Procname.t option =
    match e with Closure {name} | Const (Cfun name) -> Some name | _ -> None


  let add_arg_flows (callee_pname : Procname.t) (argument_list : Exp.t list) (astate : Domain.t) :
      Domain.t =
    let add_flows_all_to_arg index (last_writes, local_edges) arg =
      let read_set = Var.get_all_vars_in_exp arg in
      let add_flows_var_to_arg local_edges variable =
        let source_nodes = Domain.Real.get_all variable last_writes in
        let add_one_flow local_edges node =
          LineageGraph.flow
            ~source:(Local (variable, node))
            ~target:(ArgumentOf (index, callee_pname))
            ()
          :: local_edges
        in
        List.fold source_nodes ~f:add_one_flow ~init:local_edges
      in
      let local_edges = Sequence.fold read_set ~init:local_edges ~f:add_flows_var_to_arg in
      (last_writes, local_edges)
    in
    List.foldi argument_list ~init:astate ~f:add_flows_all_to_arg


  let add_ret_flows (callee_pname : Procname.t) (ret_id : Ident.t) node (astate : Domain.t) :
      Domain.t =
    let last_writes, local_edges = astate in
    let local_edges =
      LineageGraph.flow ~source:(ReturnOf callee_pname) ~target:(Local (Var.of_id ret_id, node)) ()
      :: local_edges
    in
    (last_writes, local_edges)


  let update_write (kind : LineageGraph.flow_kind) ((write_var, write_node) : Var.t * PPNode.t)
      (read_set : Var.Set.t) ((last_writes, local_edges) : Domain.t) : Domain.t =
    let target = LineageGraph.Local (write_var, write_node) in
    let add_read read_var local_edges =
      let source_nodes = Domain.Real.get_all read_var last_writes in
      let add_edge local_edges read_node =
        LineageGraph.flow ~kind ~source:(Local (read_var, read_node)) ~target () :: local_edges
      in
      let local_edges = List.fold ~init:local_edges ~f:add_edge source_nodes in
      local_edges
    in
    let local_edges = Var.Set.fold add_read read_set local_edges in
    let last_writes = Domain.Real.remove_all write_var last_writes in
    let last_writes = Domain.Real.add write_var write_node last_writes in
    (last_writes, local_edges)


  let add_tito tito_arguments (argument_list : Exp.t list) (ret_id : Ident.t) node
      (astate : Domain.t) : Domain.t =
    let tito_vars =
      let tito_exps =
        List.filter_mapi
          ~f:(fun index arg -> if IntSet.mem index tito_arguments then Some arg else None)
          argument_list
      in
      vars_of_exp_list tito_exps
    in
    update_write LineageGraph.Summary (Var.of_id ret_id, node) tito_vars astate


  let add_tito_all (argument_list : Exp.t list) (ret_id : Ident.t) node (astate : Domain.t) :
      Domain.t =
    let all = IntSet.of_list (List.mapi argument_list ~f:(fun index _ -> index)) in
    add_tito all argument_list ret_id node astate


  let add_summary_flows (callee : (Procdesc.t * Summary.t) option) (argument_list : Exp.t list)
      (ret_id : Ident.t) node (astate : Domain.t) : Domain.t =
    match callee with
    | None ->
        add_tito_all argument_list ret_id node astate
    | Some (_callee_pdesc, {Summary.tito_arguments}) ->
        add_tito tito_arguments argument_list ret_id node astate


  let exec_call (astate : Domain.t) node analyze_dependency ret_id fun_name args : Domain.t =
    let callee_pname = procname_of_exp fun_name in
    let args = List.map ~f:fst args in
    match callee_pname with
    | None ->
        add_tito_all args ret_id node astate
    | Some name ->
        (* TODO: Mechanism to match [name] against custom tito models. *)
        astate |> add_arg_flows name args |> add_ret_flows name ret_id node
        |> add_summary_flows (analyze_dependency name) args ret_id node


  let exec_noncall astate node instr =
    let written_var = get_written_var instr in
    let read_set = get_read_set instr in
    match written_var with
    | None ->
        astate
    | Some written_var ->
        update_write LineageGraph.Direct (written_var, node) read_set astate


  let exec_instr astate {InterproceduralAnalysis.analyze_dependency} node instr_index
      (instr : Sil.instr) =
    if not (Int.equal instr_index 0) then
      L.die InternalError "SimpleLineage: INV broken: CFGs should be single instruction@\n" ;
    let astate = (fst astate, [ (* don't repeat edges*) ]) in
    match instr with
    | Call ((ret_id, _ret_typ), name, args, _location, _flags) ->
        exec_call astate node analyze_dependency ret_id name args
    | _ ->
        exec_noncall astate node instr


  let pp_session_name _node fmt = Format.pp_print_string fmt "SimpleLineage"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

module Out = struct
  module Json = struct
    type location_id = int64 [@@deriving yojson_of]

    type node_id = int64 [@@deriving yojson_of]

    type state_id = int64 [@@deriving yojson_of]

    (* These correspond to CFG nodes in Infer. *)
    type _location =
      { id: location_id
      ; function_: string [@key "function"]
      ; file: string
      ; line: int option (* might be unknown for some locations *) }
    [@@deriving yojson_of, yojson_fields]

    type location = {location: _location} [@@deriving yojson_of]

    (* These correspond to abstract states in AbsInt. *)
    type _state = {id: state_id; location: location_id} [@@deriving yojson_of]

    type state = {state: _state} [@@deriving yojson_of]

    type variable_type = UserVariable | Temporary | Argument | Return

    let yojson_of_variable_type typ =
      match typ with
      | UserVariable ->
          `String "UserVariable"
      | Temporary ->
          `String "Temporary"
      | Argument ->
          `String "Argument"
      | Return ->
          `String "Return"


    type variable =
      { name: string (* "$ret" if type is Return; "$argN" for N=0,1,... if type is Argument *)
      ; variable_type: variable_type }
    [@@deriving yojson_of]

    type _node = {id: node_id; state: state_id; variable: variable} [@@deriving yojson_of]

    type node = {node: _node} [@@deriving yojson_of]

    type edge_type = Copy | Derive

    let yojson_of_edge_type typ =
      match typ with Copy -> `String "Copy" | Derive -> `String "Derive"


    type _edge = {source: node_id; target: node_id; edge_type: edge_type} [@@deriving yojson_of]

    type edge = {edge: _edge} [@@deriving yojson_of]

    type lineage =
      { nodes: node list
      ; edges: edge list
      ; states: state list
      ; variables: variable list
      ; locations: location list }
    [@@deriving yojson_of]
  end

  let channel_ref = ref None

  let channel () =
    let output_dir = Filename.concat Config.results_dir "simple-lineage" in
    Unix.mkdir_p output_dir ;
    match !channel_ref with
    | None ->
        let filename = Format.asprintf "lineage-%a.json" Pid.pp (Unix.getpid ()) in
        let channel = Filename.concat output_dir filename |> Out_channel.create in
        let close_channel () =
          Option.iter !channel_ref ~f:Out_channel.close_no_err ;
          channel_ref := None
        in
        Epilogues.register ~f:close_channel ~description:"close output channel for lineage" ;
        channel_ref := Some channel ;
        channel
    | Some channel ->
        channel


  type node = Start of Location.t option | Exit of Location.t option | Normal of PPNode.t

  type var = Argument of int | Return | Normal of Var.t

  type vertex = {procname: Procname.t; var: var; node: node}

  let id x : Md5.t = Md5.digest_bytes (Marshal.to_bytes x [])

  let id_of_node (node : node) =
    match node with
    | Start _ ->
        id (Start None)
    | Exit _ ->
        id (Exit None)
    | Normal n ->
        id (PPNode.id n)


  let id_of_vertex {procname; var; node} = id (procname, var, id_of_node node)

  let id_of_state (procname, (node : node)) = id (procname, id_of_node node)

  let id_of_edge (source, target, (kind : LineageGraph.flow_kind)) =
    id (id_of_vertex source, id_of_vertex target, kind)


  let short_id (md5 : Md5.t) : int64 =
    (* TODO: Consider alternatives - monitor collisions *)
    Int64.of_string ("0x" ^ String.sub (Md5.to_hex md5) ~pos:0 ~len:16)


  let once_per_id f id =
    let seen = ref Md5.Set.empty in
    Staged.stage (fun x ->
        let key = id x in
        if not (Set.mem !seen key) then (
          f key x ;
          seen := Set.add !seen key ) )


  let variable_of_var (var : var) : Json.variable =
    let name =
      match var with
      | Argument index ->
          Printf.sprintf "$arg%d" index
      | Return ->
          "$ret"
      | Normal x ->
          Format.asprintf "%a" Var.pp x
    in
    let variable_type : Json.variable_type =
      match var with
      | Argument _ ->
          Argument
      | Return ->
          Return
      | Normal x ->
          if Var.appears_in_source_code x then UserVariable else Temporary
    in
    {Json.name; variable_type}


  let vertex_of_data proc_desc (data : LineageGraph.data) : vertex =
    let procname = Procdesc.get_proc_name proc_desc in
    match data with
    | Local (var, node) ->
        let node : node =
          match PPNode.kind node with
          | Start_node ->
              Start None
          | Exit_node ->
              Exit None
          | _ ->
              Normal node
        in
        {procname; var= Normal var; node}
    | Argument index ->
        { procname
        ; var= Argument index
        ; node= Start (Some (Procdesc.Node.get_loc (Procdesc.get_start_node proc_desc))) }
    | Return ->
        { procname
        ; var= Return
        ; node= Exit (Some (Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc))) }
    | ArgumentOf (index, callee) ->
        {procname= callee; var= Argument index; node= Start None}
    | ReturnOf callee ->
        {procname= callee; var= Return; node= Exit None}


  let report_summary summary proc_desc =
    let save_state state_id (procname, (node : node)) =
      let location =
        match node with
        | Start (Some loc) | Exit (Some loc) ->
            loc
        | Normal node ->
            PPNode.loc node
        | _ ->
            L.die InternalError "Attempting to save non-local state"
      in
      let file, line =
        if location.Location.line > 0 then
          (SourceFile.to_rel_path location.Location.file, Some location.Location.line)
        else ("?", None)
      in
      let out_loc =
        { Json.location=
            {id= short_id state_id; function_= Procname.to_unique_id procname; file; line} }
      in
      let out_state = {Json.state= {id= short_id state_id; location= short_id state_id}} in
      Yojson.Safe.to_channel (channel ()) (Json.yojson_of_location out_loc) ;
      Out_channel.newline (channel ()) ;
      Yojson.Safe.to_channel (channel ()) (Json.yojson_of_state out_state) ;
      Out_channel.newline (channel ())
    in
    let save_state = Staged.unstage (once_per_id save_state id_of_state) in
    let save_vertex vertex_id {procname; var; node} =
      (* Terminology. In: procname, var, node; Out: node, state/loc, var. *
         Mapping: (procname,node)->state/loc; ((procname,node)=state/loc,var)->node; var->var *)
      let state = (procname, node) in
      save_state state ;
      let variable = variable_of_var var in
      let out_node =
        {Json.node= {id= short_id vertex_id; state= short_id (id_of_state state); variable}}
      in
      Yojson.Safe.to_channel (channel ()) (Json.yojson_of_node out_node) ;
      Out_channel.newline (channel ())
    in
    let save_vertex = Staged.unstage (once_per_id save_vertex id_of_vertex) in
    let save_vertex ({node} as vertex) =
      match node with
      | Start None | Exit None ->
          () (* non-local: do not save *)
      | _ ->
          save_vertex vertex
    in
    let save_edge _edge_id (source, target, (kind : LineageGraph.flow_kind)) =
      save_vertex source ;
      save_vertex target ;
      let edge_type = match kind with Direct -> Json.Copy | Summary -> Json.Derive in
      let out_edge =
        { Json.edge=
            { source= short_id (id_of_vertex source)
            ; target= short_id (id_of_vertex target)
            ; edge_type } }
      in
      Yojson.Safe.to_channel (channel ()) (Json.yojson_of_edge out_edge) ;
      Out_channel.newline (channel ())
    in
    let save_edge = Staged.unstage (once_per_id save_edge id_of_edge) in
    let record_flow {LineageGraph.source; target; kind} =
      let source = vertex_of_data proc_desc source in
      let target = vertex_of_data proc_desc target in
      save_edge (source, target, kind)
    in
    List.iter ~f:record_flow summary ;
    Out_channel.flush (channel ())
end

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let proc_size = Procdesc.size proc_desc in
  let too_big =
    Option.value_map ~default:false
      ~f:(fun limit -> proc_size > limit)
      Config.simple_lineage_max_cfg_size
  in
  if too_big then (
    L.user_warning "Skipped large (%d) procedure (%a)@." proc_size Procname.pp
      (Procdesc.get_proc_name proc_desc) ;
    None )
  else
    let cfg = CFG.from_pdesc proc_desc in
    let initial =
      let pname = Procdesc.get_proc_name proc_desc in
      let formals =
        List.map
          ~f:(fun (var, _typ) -> Var.of_pvar (Pvar.mk var pname))
          (Procdesc.get_formals proc_desc)
      in
      let start_node = CFG.start_node cfg in
      let add_arg last_writes arg = Domain.Real.add arg start_node last_writes in
      let last_writes = List.fold ~init:Domain.Real.bottom ~f:add_arg formals in
      let local_edges =
        List.mapi
          ~f:(fun i var ->
            LineageGraph.flow ~source:(Argument i) ~target:(Local (var, start_node)) () )
          formals
      in
      (last_writes, local_edges)
    in
    let invmap = Analyzer.exec_pdesc analysis_data ~initial proc_desc in
    let graph =
      let collect _nodeid {AbstractInterpreter.State.post} edges =
        let _last_writes, post_edges = post in
        post_edges :: edges
      in
      let last_writes =
        match Analyzer.InvariantMap.find_opt (PPNode.id (CFG.exit_node cfg)) invmap with
        | None ->
            L.die InternalError "no post for exit_node?"
        | Some {AbstractInterpreter.State.post= last_writes, _} ->
            last_writes
      in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      let mk_ret_edge ret_node =
        LineageGraph.flow ~source:(Local (ret_var, ret_node)) ~target:Return ()
      in
      List.map ~f:mk_ret_edge (Domain.Real.get_all ret_var last_writes)
      @ (Analyzer.InvariantMap.fold collect invmap [snd initial] |> List.concat)
    in
    if Config.simple_lineage_json_report then Out.report_summary graph proc_desc ;
    Some (Summary.of_graph graph)
