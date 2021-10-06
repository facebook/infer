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
  [@@deriving compare, sexp]

  let max_flow_kind flow_a flow_b = if compare_flow_kind flow_a flow_b > 0 then flow_a else flow_b

  type flow = {source: data; target: data; kind: flow_kind} [@@deriving compare, sexp]

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


  let pp_flow_kind fmt kind =
    match kind with
    | Direct ->
        Format.fprintf fmt "Direct"
    | Summary ->
        Format.fprintf fmt "Summary"


  let pp_flow fmt {source; target; kind} =
    Format.fprintf fmt "@[<2>[%a@ ->@ %a@ (%a)]@]@;" pp_data source pp_data target pp_flow_kind kind


  let pp fmt flows =
    Format.fprintf fmt "@;@[<v 2>LineageGraph@;@[%a@]@]" (Format.pp_print_list pp_flow) flows


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

      type entity_type = Edge | Node | State | Location [@@deriving compare, hash, sexp]
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


    type state_local = Start of Location.t | Exit of Location.t | Normal of PPNode.t

    type var = Argument of int | Return | Normal of Var.t

    module Id = struct
      (** Internal representation of an Id. *)
      type t = Z.t

      (** Largest prime that fits in 63 bits. *)
      let modulo_i64 = Int64.of_string "9223372036854775783"

      let modulo_z = Z.of_int64 modulo_i64

      let zero : t = Z.zero

      let one : t = Z.one

      let two : t = Z.of_int 2

      let three : t = Z.of_int 3

      let coefficients =
        let open Sequence.Generator in
        let rec gen prng =
          yield (Z.of_int64 (Random.State.int64 prng modulo_i64)) >>= fun () -> gen prng
        in
        Sequence.memoize (run (gen (Random.State.make [|Config.simple_lineage_seed|])))


      let of_sequence ids =
        let hash_add old_hash (a, b) =
          let open Z in
          (old_hash + (a * b)) mod modulo_z
        in
        Sequence.fold ~init:zero ~f:hash_add (Sequence.zip ids coefficients)


      let of_list ids = of_sequence (Sequence.of_list ids)

      let of_state_local (state : state_local) : t =
        match state with
        | Start _ ->
            of_list [zero]
        | Exit _ ->
            of_list [one]
        | Normal n ->
            of_list [two; Z.of_int (PPNode.hash n)]


      (** Workaround: [String.hash] leads to many collisions. *)
      let of_string s =
        of_sequence
          (Sequence.map
             ~f:(fun c -> Z.of_int (int_of_char c))
             (Sequence.of_seq (Caml.String.to_seq s)) )


      let of_procname procname : t = of_string (Procname.hashable_name procname)

      let of_variable_type (variable_type : Json.variable_type) =
        match variable_type with
        | UserVariable ->
            zero
        | Temporary ->
            one
        | Argument ->
            two
        | Return ->
            three


      let of_variable {Json.name; variable_type} : t =
        of_list [of_string name; of_variable_type variable_type]


      let of_kind (kind : flow_kind) : t = match kind with Direct -> zero | Summary -> one

      (** Converts the internal representation to an [int64], as used by the [Out] module. *)
      let out id : int64 =
        try Z.to_int64 id with Z.Overflow -> L.die InternalError "Hash does not fit in int64"
    end

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


    module JsonCacheKey = struct
      module T = struct
        type t = Json.entity_type * Int64.t [@@deriving compare, hash, sexp]
      end

      include T
      include Hashable.Make (T)
    end

    let write_json_cache = JsonCacheKey.Hash_set.create ()

    let write_json =
      let really_write_json json =
        Yojson.Safe.to_channel (channel ()) json ;
        Out_channel.newline (channel ())
      in
      if Config.simple_lineage_dedup then ( fun category id json ->
        let key = (category, Id.out id) in
        if not (Hash_set.mem write_json_cache key) then (
          Hash_set.add write_json_cache key ;
          really_write_json json ) )
      else fun _category _id json -> really_write_json json


    let save_location ~write procname (state_local : state_local) : Id.t =
      let procname_id = Id.of_procname procname in
      let state_local_id = Id.of_state_local state_local in
      let location_id = Id.of_list [procname_id; state_local_id] in
      if write then (
        let location =
          match state_local with
          | Start location | Exit location ->
              location
          | Normal node ->
              PPNode.loc node
        in
        if Location.equal location Location.dummy then
          L.die InternalError "Source file name should always be available" ;
        let function_ = Procname.hashable_name procname in
        let file = SourceFile.to_rel_path location.Location.file in
        let line = if location.Location.line < 0 then None else Some location.Location.line in
        write_json Location location_id
          (Json.yojson_of_location {location= {id= Id.out location_id; function_; file; line}}) ) ;
      location_id


    let save_state ~write procname (state : state_local) : Id.t =
      let location_id = save_location ~write procname state in
      if write then
        write_json State location_id
          (Json.yojson_of_state {state= {id= Id.out location_id; location= Id.out location_id}}) ;
      location_id


    let save_node proc_desc (data : data) =
      let save ?(write = true) procname state_local variable =
        let state_id = save_state ~write procname state_local in
        let variable = variable_of_var variable in
        let node_id = Id.of_list [state_id; Id.of_variable variable] in
        if write then
          write_json Node node_id
            (Json.yojson_of_node {node= {id= Id.out node_id; state= Id.out state_id; variable}}) ;
        node_id
      in
      match data with
      | Local (var, node) ->
          let procname = Procdesc.get_proc_name proc_desc in
          save procname (Normal node) (Normal var)
      | Argument index ->
          let procname = Procdesc.get_proc_name proc_desc in
          save procname
            (Start (Procdesc.Node.get_loc (Procdesc.get_start_node proc_desc)))
            (Argument index)
      | Return ->
          let procname = Procdesc.get_proc_name proc_desc in
          save procname (Exit (Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc))) Return
      | ArgumentOf (index, callee_procname) ->
          save ~write:false callee_procname (Start Location.dummy) (Argument index)
      | ReturnOf callee_procname ->
          save ~write:false callee_procname (Exit Location.dummy) Return


    let save_edge proc_desc {source; target; kind} =
      let source_id = save_node proc_desc source in
      let target_id = save_node proc_desc target in
      let kind_id = Id.of_kind kind in
      let edge_id = Id.of_list [source_id; target_id; kind_id] in
      let edge_type = match kind with Direct -> Json.Copy | Summary -> Json.Derive in
      write_json Edge edge_id
        (Json.yojson_of_edge
           {edge= {source= Id.out source_id; target= Id.out target_id; edge_type}} ) ;
      edge_id


    let report_summary summary proc_desc =
      let record_flow flow = ignore (save_edge proc_desc flow) in
      List.iter ~f:record_flow summary ;
      Out_channel.flush (channel ()) ;
      Hash_set.clear write_json_cache
  end

  let report = Out.report_summary
end

(** Helper function. *)
let get_formals proc_desc : Var.t list =
  let pname = Procdesc.get_proc_name proc_desc in
  let f (var, _typ) = Var.of_pvar (Pvar.mk var pname) in
  List.map ~f (Procdesc.get_formals proc_desc)


module Summary = struct
  (** TITO stands for "taint-in taint-out". In this context a tito argument is one that has a path
      to the return node, without going through call edges; more precisely, [i] is a tito argument
      if there is a path from [Argument i] to [Return] not going through [ArgumentOf _] nodes. *)
  type tito_arguments = IntSet.t

  type t = {graph: LineageGraph.t; tito_arguments: tito_arguments}

  let graph {graph} = graph

  let pp_tito_arguments fmt arguments =
    let pp_sep fmt () = Format.fprintf fmt ",@;" in
    Format.fprintf fmt "@;@[<2>TitoArguments@;%a@]"
      (Format.pp_print_list ~pp_sep Int.pp)
      (IntSet.elements arguments)


  let pp fmt {graph; tito_arguments} =
    Format.fprintf fmt "@;@[<2>LineageSummary@;%a%a@]" pp_tito_arguments tito_arguments
      LineageGraph.pp graph


  (** Make [LineageGraph.data] usable in Maps/Sets. *)
  module Vertex = struct
    module T = struct
      type t = LineageGraph.data [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  (** Make [LineageGraph.flow] usable in Maps/Sets. *)
  module Edge = struct
    module T = struct
      type t = LineageGraph.flow [@@deriving compare, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  let tito_arguments_of_graph graph =
    (* Construct an adjacency list representation of the reversed graph. *)
    let graph_rev =
      let add_flow g {LineageGraph.source; target} =
        match target with
        | ArgumentOf _ ->
            g (* skip call edges *)
        | _ ->
            Map.add_multi ~key:target ~data:source g
      in
      List.fold ~init:Vertex.Map.empty ~f:add_flow graph
    in
    (* Do a DFS, to see which arguments are reachable from return. *)
    let rec dfs seen node =
      if Set.mem seen node then seen
      else
        let seen = Set.add seen node in
        let parents = Map.find_multi graph_rev node in
        List.fold ~init:seen ~f:dfs parents
    in
    let reachable = dfs Vertex.Set.empty LineageGraph.Return in
    (* Collect reachable arguments. *)
    let tito_arguments =
      let add_node args (node : LineageGraph.data) =
        match node with Argument i -> IntSet.add i args | _ -> args
      in
      Set.fold reachable ~init:IntSet.empty ~f:add_node
    in
    tito_arguments


  (** A vertex is uninteresting if it is of the form [Local(x,_)], where x is a variable that does
      not appear in source or a formal parameter or the return variable. The returned graph has an
      edge X0->Xn of type T between interesting vertices X0 and Xn iff the input graph has a path
      X0->X1->...->Xn with X1,...,X(n-1) being uninteresting and T is the maximum of the types of
      the edges in the path. *)
  let remove_temporaries proc_desc graph =
    (* Build adjacency list representation, to make DFS easy. *)
    let children =
      let add_flow g {LineageGraph.source; target; kind} =
        Map.add_multi ~key:source ~data:(target, kind) g
      in
      List.fold ~init:Vertex.Map.empty ~f:add_flow graph
    in
    (* A check for vertex interestingness, used during the DFS that comes next. *)
    let special_variables =
      let formals = get_formals proc_desc in
      let ret_var = Var.of_pvar (Procdesc.get_ret_var proc_desc) in
      Var.Set.of_list (ret_var :: formals)
    in
    let is_interesting (data : LineageGraph.data) =
      match data with
      | Local (var, _node) ->
          Var.appears_in_source_code var && not (Var.Set.mem var special_variables)
      | Argument _ | Return | ArgumentOf _ | ReturnOf _ ->
          true
    in
    (* Do a DFS and produce edges. *)
    let rec dfs (seen, result_graph) ({LineageGraph.source; target; kind} as state) =
      if not (is_interesting source) then L.die InternalError "INV broken" ;
      if not (Set.mem seen state) then
        let seen = Set.add seen state in
        if is_interesting target then (seen, state :: result_graph)
        else
          let f (seen, result_graph) (target, edge_kind) =
            let kind = LineageGraph.max_flow_kind kind edge_kind in
            dfs (seen, result_graph) {LineageGraph.source; target; kind}
          in
          List.fold ~init:(seen, result_graph) ~f (Map.find_multi children target)
      else (seen, result_graph)
    in
    let start_dfs (seen, result_graph) ({LineageGraph.source} as state) =
      if is_interesting source then dfs (seen, result_graph) state else (seen, result_graph)
    in
    let _seen, result_graph = List.fold ~init:(Edge.Set.empty, []) ~f:start_dfs graph in
    result_graph


  (** Given a graph, computes tito_arguments, and makes a summary. *)
  let of_graph proc_desc graph =
    let graph =
      if Config.simple_lineage_keep_temporaries then graph else remove_temporaries proc_desc graph
    in
    let tito_arguments = tito_arguments_of_graph graph in
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
      let formals = get_formals proc_desc in
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
    let summary = Summary.of_graph proc_desc graph in
    if Config.simple_lineage_json_report then LineageGraph.report summary.Summary.graph proc_desc ;
    Some summary
