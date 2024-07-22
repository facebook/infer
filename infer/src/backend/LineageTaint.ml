(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open LineageShape.StdModules
module Shapes = LineageShape.Summary
open IOption.Let_syntax
module V = Lineage.Vertex
module E = Lineage.Edge
module G = Lineage.G

module U = struct
  include Lineage.Unified
  module V = UVertex
end

let pp_caller_table =
  IFmt.Labelled.iter_bindings ~sep:Fmt.semi Hashtbl.iteri
    (Fmt.pair ~sep:IFmt.colon_sp Procname.pp_verbose
       (Fmt.brackets @@ Fmt.list ~sep:Fmt.sp Procname.pp_verbose) )


let analysis_req = AnalysisRequest.one Lineage

(** Returns a hash table associating its callers to each known procname. *)
let create_caller_table () =
  let caller_table = Hashtbl.create (module Procname) in
  let record ~callee ~caller =
    Hashtbl.update caller_table callee ~f:(function
      | None ->
          [caller]
      | Some callers ->
          caller :: callers )
  in
  Summary.OnDisk.iter_specs ~f:(fun {Summary.proc_name; dependencies} ->
      let {Dependencies.summary_loads; other_proc_names} =
        match dependencies with
        | Complete c ->
            c
        | Partial ->
            L.die InternalError "deserialized summary with incomplete dependencies"
      in
      List.iter summary_loads ~f:(fun callee -> record ~callee ~caller:proc_name) ;
      List.iter other_proc_names ~f:(fun callee -> record ~callee ~caller:proc_name) ) ;
  L.debug Report Verbose "@;@[Caller table: @[%a@]@]@." pp_caller_table caller_table ;
  caller_table


let find_callers caller_table procname =
  match Hashtbl.find caller_table procname with None -> [] | Some callers -> callers


let fetch_shapes procname =
  let* summary = Summary.OnDisk.get ~lazy_payloads:true analysis_req procname in
  ILazy.force_option summary.Summary.payloads.lineage_shape


module Todo = struct
  (** Utility module to deal with interprocedural todo nodes when doing DFS across procedure-local
      graphs. *)

  type node =
    | Return of FieldPath.t
    | Argument of int * FieldPath.t
    | ReturnOf of Procname.t * FieldPath.t

  let pp_node fmt node =
    match node with
    | Return path ->
        Fmt.pf fmt "ret%a" FieldPath.pp path
    | Argument (index, path) ->
        Fmt.pf fmt "arg%d%a" index FieldPath.pp path
    | ReturnOf (callee, path) ->
        Fmt.pf fmt "%a.ret%a" Procname.pp callee FieldPath.pp path
  [@@warning "-unused-value-declaration"]


  type t = {procname: Procname.t; node: node}

  let to_vertices {procname; node} =
    let shapes = fetch_shapes procname in
    match node with
    | Return path ->
        Shapes.map_return shapes path ~f:(fun ret_path -> V.Return ret_path)
    | ReturnOf (callee, path) ->
        Shapes.map_return_of shapes callee path ~f:(fun ret_path -> V.ReturnOf (callee, ret_path))
    | Argument (index, path) ->
        Shapes.map_argument shapes index path ~f:(fun arg_path -> V.Argument (index, arg_path))


  let return procname = {procname; node= Return []}

  let argument procname arg_index = {procname; node= Argument (arg_index, [])}
end

type todo = Todo.t

module TaintConfig = struct
  module Endpoint = struct
    type node = Argument of int | Return

    let pp_node fmt node =
      match node with Return -> Fmt.pf fmt "ret" | Argument index -> Fmt.pf fmt "arg%d" index


    type t = {procname: Procname.t; node: node}

    let argument procname index = {procname; node= Argument index}

    let return procname = {procname; node= Return}

    let todo {procname; node} =
      match node with
      | Argument index ->
          Todo.argument procname index
      | Return ->
          Todo.return procname


    let unified_vertices {procname; node} : U.V.t list =
      let shapes = fetch_shapes procname in
      match node with
      | Argument index ->
          Shapes.map_argument shapes index [] ~f:(fun path ->
              {procname; U.V.vertex= Argument (index, path)} )
      | Return ->
          Shapes.map_return shapes [] ~f:(fun ret_path -> {U.V.procname; vertex= Return ret_path})


    let matches_v {procname= endpoint_procname; node= endpoint_node} procname (vertex : V.t) =
      let node_matches =
        match (endpoint_node, vertex) with
        | Argument index, Argument (index', _) ->
            [%equal: int] index index'
        | Return, Return _ ->
            true
        | (Argument _ | Return), _ ->
            false
      in
      node_matches && [%equal: Procname.t] endpoint_procname procname


    let matches_u {procname= endpoint_procname; node= endpoint_node}
        ({procname; vertex} : U.UVertex.t) =
      let node_matches =
        match (endpoint_node, vertex) with
        | Argument index, Argument (index', _) ->
            [%equal: int] index index'
        | Return, Return _ ->
            true
        | (Argument _ | Return), _ ->
            false
      in
      node_matches && [%equal: Procname.t] endpoint_procname procname
  end

  type t =
    { sources: Endpoint.t list
    ; sinks: Endpoint.t list
    ; sanitizers: Procname.Comparable.Set.t
    ; limit: int option }

  let parse_builtin string =
    let builtins = [("__erlang_make_map", BuiltinDecl.__erlang_make_map)] in
    List.Assoc.find ~equal:[%equal: string] builtins string


  let parse_mfa string =
    let module_name, string =
      match String.lsplit2 ~on:':' string with
      | None ->
          (ErlangTypeName.erlang_namespace, string)
      | Some (module_name, rest) ->
          (module_name, rest)
    in
    let* function_name, arity = String.lsplit2 ~on:'/' string in
    let+ arity = int_of_string_opt arity in
    Procname.make_erlang ~module_name ~function_name ~arity


  let parse_procname string = Option.first_some (parse_builtin string) (parse_mfa string)

  let parse_procname_exn s =
    match parse_procname s with
    | Some procname ->
        procname
    | None ->
        L.die InternalError "Can't parse procname `%s`" s


  (** Expects ["[module:]function/arity.{ret,argN}"] and returns the corresponding procname and todo
      node. *)
  let parse_endpoint string =
    let* mfa, node = String.lsplit2 ~on:'.' string in
    let* procname = parse_procname mfa in
    if [%equal: string] node "ret" then Some (Endpoint.return procname)
    else
      let* arg_index = String.chop_prefix ~prefix:"arg" node in
      let* arg_index = int_of_string_opt arg_index in
      Some (Endpoint.argument procname arg_index)


  let parse_endpoint_exn name string =
    match parse_endpoint string with
    | Some node ->
        node
    | None ->
        L.die UserError
          "%s: invalid format. Expected `{mod:fun/arity,builtin}.{ret,argN}, got `%s`." name string


  let parse ~lineage_source ~lineage_sink ~lineage_sanitizers ~lineage_limit =
    match (lineage_source, lineage_sink) with
    | [], [] ->
        None
    | [], _ :: _ | _ :: _, [] ->
        L.die UserError "Lineage: source and taint should be both present or both absent."
    | _ :: _, _ :: _ ->
        Some
          { sources= List.map ~f:(parse_endpoint_exn "lineage-source") lineage_source
          ; sinks= List.map ~f:(parse_endpoint_exn "lineage-sink") lineage_sink
          ; sanitizers=
              Set.of_list (module Procname) @@ List.map ~f:parse_procname_exn lineage_sanitizers
          ; limit= lineage_limit }


  let check caller_table {sources; sinks; sanitizers; _} =
    let check_procname name procname =
      (* Check that this procname exists in the graph. A procname exists if either:
         - It is called by something (eg. stdlib functions with no summary)
         - It has a summary (eg. toplevel analysed function)
      *)
      let exists =
        Hashtbl.mem caller_table procname
        || (Option.is_some @@ Summary.OnDisk.get ~lazy_payloads:true analysis_req procname)
      in
      if not exists then
        L.user_warning "@[LineageTaint: %s `%a` not found. Did you make a typo?@]@." name
          Procname.pp_verbose procname
    in
    let check_endpoint name ({procname; _} : Endpoint.t) = check_procname name procname in
    List.iter ~f:(check_endpoint "source") sources ;
    List.iter ~f:(check_endpoint "sink") sinks ;
    Set.iter ~f:(check_procname "sanitizer") sanitizers


  let todo_sources {sources; _} = List.map ~f:Endpoint.todo sources

  let unified_sources {sources; _} = List.concat_map ~f:Endpoint.unified_vertices sources

  let unified_sinks {sinks; _} = List.concat_map ~f:Endpoint.unified_vertices sinks

  let is_source {sources; _} unified_vertex =
    List.exists ~f:(fun source -> Endpoint.matches_u source unified_vertex) sources


  let is_sink {sinks; _} procname vertex =
    List.exists ~f:(fun sink -> Endpoint.matches_v sink procname vertex) sinks


  let is_sanitizer {sanitizers; _} procname = Set.mem sanitizers procname

  let is_sanitizing_e (config : t) edge =
    match E.kind @@ G.E.label edge with
    | Summary {callee; _} ->
        is_sanitizer config callee
    | Direct | Call | Return | Capture | Builtin | DynamicCallFunction | DynamicCallModule ->
        false


  let limit_or_max_int {limit; _} = match limit with None -> Int.max_value | Some limit -> limit
end

module Weight = struct
  type t = int

  type edge = U.G.edge

  let compare = [%compare: int]

  let zero = 0

  let add = ( + )

  let weight _ = 1
end

module D = Graph.Path.Dijkstra (U.G) (Weight)

module Instrumented : sig
  (** Instrumented graph for easier querying of paths from sources / to sinks. *)

  type t

  type edge := U.G.E.t

  val instrument : TaintConfig.t -> U.G.t -> t

  val clear : t -> U.G.t

  val limit : TaintConfig.t -> t -> t

  val edges_into_sink : t -> edge list

  val shortest_path_from_source : t -> U.V.t -> edge list
  (** Returns the shortest path from any source into the node. Instrumentation nodes will not
      appear. *)
end = struct
  include U.G

  let instrument_procname function_name =
    Procname.make_erlang ~module_name:"__infer__lineage_taint__instrument__" ~function_name ~arity:0


  (* Unique source and sink nodes make taint pathfinding easier: for instance, instead of looking
     from the shortest path from "any source" to a node, we can find the shortest path from the
     unique source (which will then contain an edge into annactual source as its first step). *)

  let unique_source : U.V.t = {procname= instrument_procname "source"; vertex= Function}

  let unique_sink : U.V.t = {procname= instrument_procname "sink"; vertex= Function}

  let instrument (config : TaintConfig.t) graph : t =
    let sources = TaintConfig.unified_sources config in
    let sinks = TaintConfig.unified_sinks config in
    (* Add dummy unique source and sink nodes *)
    let graph =
      List.fold sources ~f:(fun acc source -> U.G.add_edge acc unique_source source) ~init:graph
    in
    let graph =
      List.fold sinks ~f:(fun acc sink -> U.G.add_edge acc sink unique_sink) ~init:graph
    in
    graph


  let clear (instrumented_graph : t) : U.G.t =
    instrumented_graph
    |> Fn.flip U.G.remove_vertex unique_source
    |> Fn.flip U.G.remove_vertex unique_sink


  let limit (config : TaintConfig.t) (graph : t) : t =
    (* Repetitively add the shortest flows *)
    let rec loop acc work_graph fuel =
      match D.shortest_path work_graph unique_source unique_sink with
      | path, distance ->
          let fuel = fuel - distance in
          if fuel < 0 then acc
          else
            let acc = List.fold path ~init:acc ~f:(fun acc edge -> U.G.add_edge_e acc edge) in
            let work_graph =
              List.fold path ~init:work_graph ~f:(fun acc edge ->
                  if
                    [%equal: U.V.t] (U.G.E.src edge) unique_source
                    || [%equal: U.V.t] (U.G.E.dst edge) unique_sink
                  then acc
                  else U.G.remove_edge_e acc edge )
            in
            loop acc work_graph fuel
      | exception Stdlib.Not_found ->
          acc
    in
    loop U.G.empty graph (TaintConfig.limit_or_max_int config)


  let shortest_path_from_source (graph : t) vertex =
    let path_from_unique_source, _weight = D.shortest_path graph unique_source vertex in
    List.tl_exn path_from_unique_source


  let edges_into_sink graph =
    let sinks = U.G.pred graph unique_sink in
    List.concat_map sinks ~f:(U.G.pred_e graph)
end

module I = Instrumented

(** Collects the reachable subgraph from a given source node in the lineage graph of one procedures.
    Returns this subgraph and two lists of nodes from other procedures to explore, the first one
    being reached through Return edges and the second one though Calls.

    If [follow_return] is false then the Return list will be empty (ie. [Return] edges will be
    ignored). *)
let collect_reachable_in_procedure (config : TaintConfig.t) ~follow_return caller_table
    ~init:subgraph procname graph vertices =
  let rec dfs todo acc_subgraph (return_todo : todo list) (call_todo : todo list) =
    match todo with
    | [] ->
        (acc_subgraph, return_todo, call_todo)
    | vertex :: next when TaintConfig.is_sink config procname vertex ->
        (* Stop propagation at the first sink *)
        dfs next acc_subgraph return_todo call_todo
    | vertex :: next ->
        let acc_subgraph', todo' =
          G.fold_succ_e
            (fun edge (acc, todo) ->
              if TaintConfig.is_sanitizing_e config edge then (acc, todo)
              else
                let unified_edges = U.transform_e fetch_shapes procname edge in
                let todo_with_dst = G.E.dst edge :: todo in
                let acc', todo' =
                  List.fold unified_edges
                    ~f:(fun (acc, todo) edge' ->
                      if U.G.mem_edge_e acc edge' then (acc, todo)
                      else (U.G.add_edge_e acc edge', todo_with_dst) )
                    ~init:(acc, todo)
                in
                (acc', todo') )
            graph vertex (acc_subgraph, next)
        in
        let return_todo' =
          if follow_return then
            let callers = find_callers caller_table procname in
            match (vertex : V.t) with
            | Return field_path ->
                List.fold callers ~init:return_todo ~f:(fun acc caller ->
                    {Todo.procname= caller; node= ReturnOf (procname, field_path)} :: acc )
            | Local _
            | Argument (_, _)
            | ArgumentOf (_, _, _)
            | Captured _
            | CapturedBy (_, _)
            | ReturnOf _
            | Self
            | Function _ ->
                return_todo
          else return_todo
        in
        let call_todo' =
          match (vertex : V.t) with
          | ArgumentOf (callee, index, field_path) ->
              {Todo.procname= callee; node= Argument (index, field_path)} :: call_todo
          | Local _
          | Argument (_, _)
          | Captured _
          | CapturedBy (_, _)
          | Return _
          | ReturnOf _
          | Self
          | Function _ ->
              call_todo
        in
        dfs todo' acc_subgraph' return_todo' call_todo'
  in
  dfs vertices subgraph [] []


(** Collect the reachable lineage subgraphs from a node over all program procedures. Returns it as a
    map from each procname to its subgraph.

    The result will not traverse paths that include a Call edge followed (even not immediately) by a
    Return edge (but will include the corresponding Summary edges). It works by first collecting all
    the nodes reachable by traversing Return edges, then from this set the ones reachable through
    Call edges but not Return. *)
let collect_reachable (config : TaintConfig.t) caller_table =
  let rec aux ~follow_return (todo : todo list) (todo_later : todo list) acc =
    match (todo, todo_later) with
    | [], [] ->
        acc
    | [], _ :: _ ->
        aux ~follow_return:false todo_later [] acc
    | {procname; node} :: todo_next, _ ->
        if TaintConfig.is_sanitizer config procname then aux ~follow_return todo_next todo_later acc
        else
          let summary = Summary.OnDisk.get ~lazy_payloads:true analysis_req procname in
          let vertices = Todo.to_vertices {procname; node} in
          let lineage =
            let* summary in
            ILazy.force_option summary.Summary.payloads.lineage
          in
          let lineage_graph =
            match lineage with
            | None ->
                G.of_vertices vertices
            | Some lineage ->
                Lineage.Summary.graph lineage
          in
          let acc', return_todo, call_todo =
            collect_reachable_in_procedure config ~follow_return caller_table procname lineage_graph
              vertices ~init:acc
          in
          let todo' = List.rev_append return_todo todo_next in
          let todo_later' = List.rev_append call_todo todo_later in
          aux ~follow_return todo' todo_later' acc'
  in
  aux ~follow_return:true (TaintConfig.todo_sources config) [] U.G.empty


let collect_coreachable (config : TaintConfig.t) reachable_graph =
  let rec codfs (todo : U.UVertex.t list) acc_graph =
    match todo with
    | [] ->
        acc_graph
    | vertex :: todo_next when TaintConfig.is_source config vertex ->
        (* Stop copropagation from the first source *)
        codfs todo_next acc_graph
    | vertex :: todo_next ->
        let fold_pred_e_if_mem_vertex f graph vertex acc =
          (* The interprocedural todo list may have requested to continue the exploration from
             ArgumentOf vertices from which the sink may be reached but not reachable from the
             source. In that case, they won't be present in the reachable subgraph.

             This typically happens when a procedure `h/1` is in a source->sink path when called by
             `f`, but not when called by `g`. Then the co-reachability analysis of `h` will put
             `({f,g}, h$arg0)` on the todo stack, but only `(f, h$arg0)` will exist as a node. *)
          if U.G.mem_vertex graph vertex then U.G.fold_pred_e f graph vertex acc else acc
        in
        let acc_graph', todo' =
          fold_pred_e_if_mem_vertex
            (fun edge (acc, todo) ->
              if U.G.mem_edge_e acc edge then (acc, todo)
              else (U.G.add_edge_e acc edge, U.G.E.src edge :: todo) )
            reachable_graph vertex (acc_graph, todo_next)
        in
        codfs todo' acc_graph'
  in
  codfs (TaintConfig.unified_sinks config) U.G.empty


let with_result_file filename_parts ~f =
  let filename = Filename.of_parts (Config.results_dir :: filename_parts) in
  Unix.mkdir_p (Filename.dirname filename) ;
  Out_channel.with_file filename ~f:(fun outchan ->
      f outchan ;
      Out_channel.flush outchan ) ;
  filename


let export_result ~name ~fileparts pp_result result =
  let filename =
    with_result_file fileparts ~f:(fun outchan ->
        let fmt = Format.formatter_of_out_channel outchan in
        pp_result fmt result )
  in
  L.progress "@;%s exported as '%s'@." name filename


let export_unified name fileparts graph =
  export_result ~name ~fileparts U.Dot.pp graph ;
  if U.G.is_empty graph then L.user_warning "Warning: %s is empty.@." name


let debug_unified name filename_parts graph =
  if Config.debug_mode then export_unified name filename_parts graph


let ltr_elem edge =
  let edge = U.G.E.label edge in
  Errlog.make_trace_element 0 (E.location edge) (Fmt.to_to_string E.pp edge) []


let log_issue graph issue_log sinking_edge =
  let edge_label = U.G.E.label sinking_edge in
  let message = "LINEAGE_TAINT" in
  let procname = E.procname edge_label in
  let loc = E.location edge_label in
  let path = I.shortest_path_from_source graph (U.G.E.src sinking_edge) @ [sinking_edge] in
  let ltr = List.map ~f:ltr_elem path in
  Reporting.log_issue_external procname ~issue_log ~loc ~ltr Lineage IssueType.lineage_flow message


let report taint_config =
  let caller_table = create_caller_table () in
  TaintConfig.check caller_table taint_config ;
  let reachable_graph = collect_reachable taint_config caller_table in
  debug_unified "Reachability graph"
    ["lineage-taint-debug"; "lineage-reachable.dot"]
    reachable_graph ;
  let coreachable_graph = collect_coreachable taint_config reachable_graph in
  debug_unified "Coreachability graph"
    ["lineage-taint-debug"; "lineage-coreachable.dot"]
    coreachable_graph ;
  let instrumented_graph = I.instrument taint_config coreachable_graph in
  let final_taint_graph = I.limit taint_config instrumented_graph in
  export_unified "Lineage taint graph"
    ["lineage-taint"; "lineage-taint.dot"]
    (I.clear final_taint_graph) ;
  let sinking_edges = I.edges_into_sink instrumented_graph in
  List.fold ~f:(log_issue instrumented_graph) ~init:IssueLog.empty sinking_edges


module Private = struct
  module TaintConfig = TaintConfig
end
