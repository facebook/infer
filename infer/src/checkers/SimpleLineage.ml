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
  type tito_arguments = Int.Set.t

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments}

  let pp_tito_arguments fmt arguments =
    let pp_sep fmt () = Format.fprintf fmt ",@;" in
    Format.fprintf fmt "@;@[<2>TitoArguments@;%a@]"
      (Format.pp_print_list ~pp_sep Int.pp)
      (Set.elements arguments)


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
        match node with Argument i -> Set.add args i | _ -> args
      in
      N.Set.fold reachable ~init:Int.Set.empty ~f:add_node
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
          ~f:(fun index arg -> if Set.mem tito_arguments index then Some arg else None)
          argument_list
      in
      vars_of_exp_list tito_exps
    in
    update_write LineageGraph.Summary (Var.of_id ret_id, node) tito_vars astate


  let add_tito_all (argument_list : Exp.t list) (ret_id : Ident.t) node (astate : Domain.t) :
      Domain.t =
    let all = Int.Set.of_list (List.mapi argument_list ~f:(fun index _ -> index)) in
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
    Some (Summary.of_graph graph)
