(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let pp_caller_table =
  IFmt.Labelled.iter_bindings ~sep:Fmt.semi Hashtbl.iteri
    (Fmt.pair ~sep:IFmt.colon_sp Procname.pp_verbose
       (Fmt.brackets @@ Fmt.list ~sep:Fmt.sp Procname.pp_verbose) )


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
  L.debug Report Verbose "@;@[Caller table: @[%a@]@]@;" pp_caller_table caller_table ;
  caller_table


let find_callers caller_table procname =
  match Hashtbl.find caller_table procname with None -> [] | Some callers -> callers


(** Collects the reachable subgraph from a given source node in the lineage graph of one procedures.
    Returns this subgraph and two lists of nodes from other procedures to explore, the first one
    being reached through Return edges and the second one though Calls.

    If [follow_return] is false then the Return list will be empty (ie. [Return] edges will be
    ignored). *)
let collect_reachable_in_procedure ~follow_return caller_table ~init:subgraph procname graph node =
  let rec dfs todo acc_subgraph return_todo call_todo =
    match todo with
    | [] ->
        (acc_subgraph, return_todo, call_todo)
    | vertex :: next ->
        let acc_subgraph', todo' =
          Lineage.G.fold_succ_e
            (fun edge (acc, todo) ->
              if Lineage.G.mem_edge_e acc edge then (acc, todo)
              else (Lineage.G.add_edge_e acc edge, Lineage.G.E.dst edge :: todo) )
            graph vertex (acc_subgraph, next)
        in
        let return_todo' =
          if follow_return then
            let callers = find_callers caller_table procname in
            match (vertex : Lineage.Vertex.t) with
            | Return [] ->
                List.fold callers ~init:return_todo ~f:(fun acc caller ->
                    (caller, Lineage.Vertex.ReturnOf (procname, [])) :: acc )
            | Return (_ :: _) ->
                L.die UserError
                  "Structures as returned values aren't supported yet. Re-run the lineage analysis \
                   with --lineage-field-depth=0."
            | Local _
            | Argument (_, _)
            | ArgumentOf (_, _, _)
            | Captured _
            | CapturedBy (_, _)
            | ReturnOf (_, _)
            | Self
            | Function _ ->
                return_todo
          else return_todo
        in
        let call_todo' =
          match (vertex : Lineage.Vertex.t) with
          | ArgumentOf (callee, index, []) ->
              (callee, Lineage.Vertex.Argument (index, [])) :: call_todo
          | ArgumentOf (_, _, _ :: _) ->
              L.die UserError
                "Structures in argument aren't supported yet. Re-run the lineage analysis with \
                 --lineage-field-depth=0."
          | Local _
          | Argument (_, _)
          | Captured _
          | CapturedBy (_, _)
          | Return _
          | ReturnOf (_, _)
          | Self
          | Function _ ->
              call_todo
        in
        dfs todo' acc_subgraph' return_todo' call_todo'
  in
  dfs [node] subgraph [] []


(** Collect the reachable lineage subgraphs from a node over all program procedures. Returns it as a
    map from each procname to its subgraph.

    The result will not traverse paths that include a Call edge followed (even not immediately) by a
    Return edge (but will include the corresponding Summary edges). It works by first collecting all
    the nodes reachable by traversing Return edges, then from this set the ones reachable through
    Call edges but not Return. *)
let collect_reachable caller_table procname node =
  let rec aux ~follow_return todo todo_later acc_graphs =
    match (todo, todo_later) with
    | [], [] ->
        acc_graphs
    | [], _ :: _ ->
        aux ~follow_return:false todo_later [] acc_graphs
    | (procname, node) :: todo_next, _ ->
        let lineage_graph =
          match Summary.OnDisk.get ~lazy_payloads:false procname with
          | None ->
              L.debug Report Verbose "@[No summary found for %a@]@;" Procname.pp_verbose procname ;
              Lineage.G.add_vertex Lineage.G.empty node
          | Some summary -> (
            match Lazy.force summary.payloads.lineage with
            | None ->
                L.debug Report Verbose "Summary has no lineage for %a" Procname.pp_verbose procname ;
                Lineage.G.add_vertex Lineage.G.empty node
            | Some lineage ->
                Lineage.Summary.graph lineage )
        in
        let init_reachable_subgraph =
          match Map.find acc_graphs procname with
          | None ->
              Lineage.G.add_vertex Lineage.G.empty node
          | Some g ->
              g
        in
        let reachable_subgraph, return_todo, call_todo =
          collect_reachable_in_procedure ~follow_return caller_table procname lineage_graph node
            ~init:init_reachable_subgraph
        in
        let acc_reachable_graphs' = Map.set acc_graphs ~key:procname ~data:reachable_subgraph in
        let todo' = List.rev_append return_todo todo_next in
        let todo_later' = List.rev_append call_todo todo_later in
        aux ~follow_return todo' todo_later' acc_reachable_graphs'
  in
  aux ~follow_return:true [(procname, node)] [] (Map.empty (module Procname))


(** Similar to collect_reachable for a subgraph from which you can reach a given node. *)
let collect_coreachable_in_procedure ~init:subgraph caller_table procname graph node =
  let rec codfs todo acc_subgraph interproc_todo =
    match todo with
    | [] ->
        (acc_subgraph, interproc_todo)
    | vertex :: next ->
        let fold_pred_e_if_mem_vertex f graph vertex acc =
          (* The interprocedural todo list may have requested to continue the exploration from
             ArgumentOf vertices from which the sink may be reached but not reachable from the
             source. In that case, they won't be present in the reachable subgraph.

             This typically happens when a procedure `h/1` is in a source->sink path when called by
             `f`, but not when called by `g`. Then the co-reachability analysis of `h` will put
             `({f,g}, h$arg0)` on the todo stack, but only `(f, h$arg0)` will exist as a node. *)
          if Lineage.G.mem_vertex graph vertex then Lineage.G.fold_pred_e f graph vertex acc
          else acc
        in
        let acc_subgraph', todo' =
          fold_pred_e_if_mem_vertex
            (fun edge (acc, todo) ->
              if Lineage.G.mem_edge_e acc edge then (acc, todo)
              else (Lineage.G.add_edge_e acc edge, Lineage.G.E.src edge :: todo) )
            graph vertex (acc_subgraph, next)
        in
        let interproc_todo' =
          let callers = find_callers caller_table procname in
          match (vertex : Lineage.Vertex.t) with
          | Argument (index, []) ->
              List.fold callers ~init:interproc_todo ~f:(fun acc caller ->
                  (caller, Lineage.Vertex.ArgumentOf (procname, index, [])) :: acc )
          | Argument (_, _ :: _) ->
              L.die UserError
                "Structures in argument aren't supported yet. Re-run the lineage analysis with \
                 --lineage-field-depth=0."
          | ReturnOf (callee, []) ->
              (callee, Lineage.Vertex.Return []) :: interproc_todo
          | ReturnOf (_, _ :: _) ->
              L.die UserError
                "Structures as returned values aren't supported yet. Re-run the lineage analysis \
                 with --lineage-field-depth=0."
          | Local _
          | ArgumentOf (_, _, _)
          | Captured _
          | CapturedBy (_, _)
          | Return _
          | Self
          | Function _ ->
              interproc_todo
        in
        codfs todo' acc_subgraph' interproc_todo'
  in
  codfs [node] subgraph []


(** See collect_reachable and collect_coreachable_in_procedure. *)
let collect_coreachable caller_table procname node reachable_graphs =
  let rec aux todo acc_graphs =
    match todo with
    | [] ->
        acc_graphs
    | (procname, node) :: todo_next -> (
      match Map.find reachable_graphs procname with
      | None ->
          (* That node has been added as a caller of some co-reachable function, but it wasn't
             reachable. Skip it.

             TODO: if that function is completely unknown (eg. there is a typo) then the final taint
             graph might be empty, leading the user to falsely believe there is no flow. Check that
             we can efficiently retrieve that information (including on functions with no summary,
             eg. `binary_to_atom`). *)
          aux todo_next acc_graphs
      | Some reachable_graph ->
          let init_coreachable_subgraph =
            match Map.find acc_graphs procname with
            | None ->
                Lineage.G.empty
            | Some subgraph ->
                subgraph
          in
          let coreachable_subgraph, interproc_todo =
            collect_coreachable_in_procedure ~init:init_coreachable_subgraph caller_table procname
              reachable_graph node
          in
          let todo' = List.rev_append interproc_todo todo_next in
          aux todo' (Map.set acc_graphs ~key:procname ~data:coreachable_subgraph) )
  in
  aux [(procname, node)] (Map.empty (module Procname))


(** Expects ["[module:]function/arity${ret,argN}"] and returns the corresponding lineage procname
    and vertex. *)
let parse_node string =
  let open IOption.Let_syntax in
  let module_name, string =
    match String.lsplit2 ~on:':' string with
    | None ->
        ("", string)
    | Some (module_name, rest) ->
        (module_name, rest)
  in
  let* function_name, string = String.lsplit2 ~on:'/' string in
  let* arity, string = String.lsplit2 ~on:'$' string in
  let* arity = int_of_string_opt arity in
  let+ node =
    if [%equal: string] string "ret" then Some (Lineage.Vertex.Return [])
    else
      let* arg_index = String.chop_prefix ~prefix:"arg" string in
      let* arg_index = int_of_string_opt arg_index in
      Some (Lineage.Vertex.Argument (arg_index, []))
  in
  (Procname.make_erlang ~module_name ~function_name ~arity, node)


let parse_node_exn name string =
  match parse_node string with
  | Some (procname, node) ->
      (procname, node)
  | None ->
      L.die UserError "%s: invalid format. Expected `mod:fun/arity${ret,argN}, got `%s`." name
        string


let report_graph_map filename_parts graphs =
  let filename = Filename.of_parts (Config.results_dir :: filename_parts) in
  Unix.mkdir_p (Filename.dirname filename) ;
  Out_channel.with_file filename ~f:(fun outchan ->
      Map.iteri
        ~f:(fun ~key:procname ~data:subgraph ->
          match Procdesc.load procname with
          | None ->
              (* We can't report a graph if we don't have a procdesc. This should only happen when
                 the graph contains only an argument or the return of a function without summary,
                 typically used as a source or sink. In that case the corresponding node will anyway
                 still be reported as ArgumentOf/ReturnOf within another callee procedure. *)
              if Lineage.G.nb_edges subgraph <> 0 then
                L.die InternalError
                  "Unexpected non-singleton taint graph for procname %a without description"
                  Procname.pp procname
          | Some proc_desc ->
              Lineage.Out.report_graph outchan proc_desc subgraph )
        graphs )


let report ~lineage_source ~lineage_sink =
  let source_procname, source_node = parse_node_exn "lineage-source" lineage_source in
  let sink_procname, sink_node = parse_node_exn "lineage-sink" lineage_sink in
  let caller_table = create_caller_table () in
  let reachable_graphs = collect_reachable caller_table source_procname source_node in
  if Config.debug_mode then
    report_graph_map ["lineage-taint-debug"; "lineage-reachable.json"] reachable_graphs ;
  let coreachable_graphs =
    collect_coreachable caller_table sink_procname sink_node reachable_graphs
  in
  report_graph_map ["lineage-taint"; "lineage-taint.json"] coreachable_graphs


module Private = struct
  let parse_node = parse_node
end
