(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let time_and_run ~f ~msg =
  let result, duration = Utils.timeit ~f in
  L.debug Capture Medium "%s took %a.@\n" msg Mtime.Span.pp duration ;
  result


module Loader = struct
  (** Build a hash table from procedure name to source file where it's defined, and a hash table
      from source files to list of procedures defined. *)
  let get_indexes () =
    let counter = ref 0 in
    let empties = ref 0 in
    let proc_index = Procname.Hash.create 11 in
    let source_index = SourceFile.Hash.create 11 in
    let db = Database.get_database CaptureDatabase in
    let stmt = Sqlite3.prepare db "SELECT source_file, procedure_names FROM source_files" in
    SqliteUtils.result_fold_rows db ~log:"loading procname to source index" stmt ~init:()
      ~f:(fun () stmt ->
        let source_file = Sqlite3.column stmt 0 |> SourceFile.SQLite.deserialize in
        if SourceFile.is_invalid source_file then ()
        else
          let procedure_names = Sqlite3.column stmt 1 |> Procname.SQLiteList.deserialize in
          SourceFile.Hash.replace source_index source_file procedure_names ;
          List.iter procedure_names ~f:(fun proc_name ->
              Procname.Hash.replace proc_index proc_name source_file ) ;
          incr counter ;
          if List.is_empty procedure_names then incr empties ) ;
    L.debug Capture Medium "Processed %d source files, of which %d had no procedures.@\n" !counter
      !empties ;
    (proc_index, source_index)


  (** build a hashtable from a procedure to all (static) callees *)
  let get_call_map () =
    let call_map = Procname.Hash.create 11 in
    let f procname callees = Procname.Hash.replace call_map procname callees in
    SyntacticCallGraph.iter_captured_procs_and_callees f ;
    call_map
end

module G = struct
  module Vertex = SourceFile

  module Edge = struct
    (** An edge labelled by the number of calls from procedures in the source file to procedures in
        the destination file. *)
    type t = int [@@deriving compare]

    (** ocamlgraph requires a [default] edge label for labelled edges. *)
    let default = 1
  end

  (* a graph where vertices are source files and edges are labelled with integers *)
  include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)

  (** load the source-file call graph, labelled by (over-approximate) number of calls from source
      file to destination file. *)
  let load_graph () =
    let proc_index, source_index = Loader.get_indexes () in
    let callee_map = Loader.get_call_map () in
    let g = create () in
    let max_label = ref 0 in
    let unique_callees_of_file procedures =
      List.fold procedures ~init:Procname.Set.empty ~f:(fun acc procname ->
          let callees = Procname.Hash.find_opt callee_map procname |> Option.value ~default:[] in
          List.fold callees ~init:acc ~f:(fun acc callee -> Procname.Set.add callee acc) )
    in
    (* update table of number of calls into each callee source file for a given callee *)
    let process_callee_of_file source_counts source callee =
      Procname.Hash.find_opt proc_index callee
      |> Option.filter ~f:(fun callee_source ->
             (* avoid self-loops *) not (SourceFile.equal source callee_source) )
      |> Option.iter ~f:(fun callee_source ->
             let previous_count =
               SourceFile.Hash.find_opt source_counts callee_source |> Option.value ~default:0
             in
             SourceFile.Hash.replace source_counts callee_source (1 + previous_count) )
    in
    let create_callee_edges source callee_source count =
      E.create source count callee_source |> add_edge_e g ;
      max_label := max !max_label count
    in
    let process_source_file source procedures =
      add_vertex g source ;
      let source_counts = SourceFile.Hash.create 11 in
      unique_callees_of_file procedures
      |> Procname.Set.iter (process_callee_of_file source_counts source) ;
      SourceFile.Hash.iter (create_callee_edges source) source_counts
    in
    SourceFile.Hash.iter process_source_file source_index ;
    L.debug Capture Medium "Maximum edge weight = %d.@\n" !max_label ;
    g


  module Cache = Caml.Hashtbl.Make (Vertex)

  let vertex_name =
    (* hashtable from source files to strings of the form "N%d". Used for dot conversion only. *)
    let cache = Cache.create 11 in
    fun v ->
      match Cache.find_opt cache v with
      | Some name ->
          name
      | None ->
          let name = "N" ^ string_of_int (Cache.length cache) in
          Cache.add cache v name ;
          name


  (* all functions below needed for dot output. *)
  let get_subgraph _ = None

  let graph_attributes _ = []

  let default_vertex_attributes _ = []

  let vertex_attributes v = [`Label (SourceFile.to_string v)]

  let default_edge_attributes _ = []

  let edge_attributes e = [`Label (string_of_int (E.label e))]
end

module Dot = Graph.Graphviz.Dot (G)

let to_dotty g filename =
  Out_channel.with_file (Config.results_dir ^/ filename) ~f:(fun out -> Dot.output_graph out g)


module Dfs = Graph.Traverse.Dfs (G)

module Dagify = struct
  (** given a non-empty list of edges representing the reverse of a path with a cycle, where the
      join node is the destination of the first edge, return a minimal edge inside the cycle. *)
  let find_min_edge =
    let min_edge e e' = if G.E.compare e e' <= 0 then e else e' in
    (* [v] is the joint point, [e] is the best edge found so far *)
    let rec find_edge v e = function
      | [] ->
          e
      | e' :: _ when G.Vertex.equal v (G.E.src e') ->
          (* we found the origin of the cycle *)
          min_edge e e'
      | e' :: rest ->
          find_edge v (min_edge e e') rest
    in
    function [] -> assert false | e :: rest -> find_edge (G.E.dst e) e rest


  (** [finished] is a hashset of nodes known not to participate in cycles; [path] is a (reversed)
      path of edges; [nodes_in_path] is the set of source nodes in [path] ; [v] is the node to
      explore in DFS fashion *)
  let rec find_cycle_min_edge g finished path nodes_in_path v =
    if SourceFile.Hash.mem finished v then None
    else if SourceFile.Set.mem v nodes_in_path then Some (find_min_edge path)
    else
      let nodes_in_path' = SourceFile.Set.add v nodes_in_path in
      let res =
        G.succ_e g v
        |> List.find_map ~f:(fun e' ->
               let v' = G.E.dst e' in
               let path' = e' :: path in
               find_cycle_min_edge g finished path' nodes_in_path' v' )
      in
      if Option.is_none res then SourceFile.Hash.add finished v () ;
      res


  exception CycleFound of G.E.t

  let make_acyclic g =
    let finished = SourceFile.Hash.create (G.nb_vertex g) in
    (* given a node, find a cycle and return minimum edge in it, or mark all reachable nodes as [finished] *)
    let start_dfs v =
      if SourceFile.Hash.mem finished v then ()
      else
        find_cycle_min_edge g finished [] SourceFile.Set.empty v
        |> Option.iter ~f:(fun edge -> raise (CycleFound edge))
    in
    let rec make_acyclic_inner () =
      try G.iter_vertex start_dfs g
      with CycleFound edge ->
        if Config.debug_mode then assert (Dfs.has_cycle g) ;
        (* we found a cycle, break it and restart dfs modulo [finished] nodes *)
        L.debug Capture Medium "Found back edge, src=%a, dst=%a@, weight=%d." SourceFile.pp
          (G.E.src edge) SourceFile.pp (G.E.dst edge) (G.E.label edge) ;
        G.remove_edge_e g edge ;
        make_acyclic_inner ()
    in
    make_acyclic_inner () ;
    if Config.debug_mode then (
      assert (not (Dfs.has_cycle g)) ;
      to_dotty g "file-call-graph-dag.dot" )
end

module Tree = struct
  (** trees are compared only by size *)
  type t = {vertices: SourceFile.t list [@compare.ignore]; size: int} [@@deriving compare]

  let size {size} = size

  let iter_vertices {vertices} ~f = List.iter vertices ~f

  (** given a DAG [g], split it into a set of trees by deleting all incoming edges but the heaviest
      for every node *)
  let make_forest g =
    let is_forest g = G.fold_vertex (fun v acc -> acc && G.in_degree g v <= 1) g true in
    let forestify_vertex g v =
      let _max_edge_opt, edges_to_delete =
        G.fold_pred_e
          (fun e (max_edge_opt, edges_to_delete) ->
            match max_edge_opt with
            | None ->
                (Some e, edges_to_delete)
            | Some e' when G.E.compare e e' <= 0 ->
                (max_edge_opt, e :: edges_to_delete)
            | Some e' ->
                (Some e, e' :: edges_to_delete) )
          g v (None, [])
      in
      List.iter edges_to_delete ~f:(G.remove_edge_e g)
    in
    if Config.debug_mode then assert (not (Dfs.has_cycle g)) ;
    G.iter_vertex (forestify_vertex g) g ;
    if Config.debug_mode then (
      assert (is_forest g) ;
      to_dotty g "file-call-graph-forest.dot" )


  (** given a graph that consists of a set of trees, make a list of [t]s sorted by increasing size *)
  let get_sorted_tree_list g =
    let roots =
      G.fold_vertex (fun v acc -> if Int.equal 0 (G.in_degree g v) then v :: acc else acc) g []
    in
    let reachable_nodes v =
      Dfs.fold_component
        (fun v' acc -> {vertices= v' :: acc.vertices; size= 1 + acc.size})
        {vertices= []; size= 0} g v
    in
    List.fold roots ~init:[] ~f:(fun acc v -> reachable_nodes v :: acc) |> List.stable_sort ~compare


  (** given a [t] in a forest graph [g], find and delete a minimal edge from [t] *)
  let split_tree g t =
    let min_edge_of_tree =
      List.fold t.vertices ~init:None ~f:(fun acc v ->
          let parent_edge_opt = G.pred_e g v |> List.hd in
          match (acc, parent_edge_opt) with
          | None, None ->
              None
          | None, some_edge | some_edge, None ->
              some_edge
          | Some e, Some e' ->
              if G.E.compare e e' <= 0 then acc else parent_edge_opt )
    in
    min_edge_of_tree |> Option.value_exn |> G.remove_edge_e g
end

module Bin = struct
  (** a set of trees to send to a single worker, compared only by size *)
  type t = {trees: Tree.t list [@compare.ignore]; bin_size: int} [@@deriving compare]

  let empty = {trees= []; bin_size= 0}

  let add_tree (tree : Tree.t) {trees; bin_size} =
    {trees= tree :: trees; bin_size= tree.size + bin_size}


  let iter_vertices bin ~f = List.iter bin.trees ~f:(Tree.iter_vertices ~f)

  let size {bin_size} = bin_size

  let find_max_tree init {trees} =
    List.fold trees ~init ~f:(fun ((max_size, _) as acc) t ->
        let m = max max_size (Tree.size t) in
        if m > max_size then (m, Some t) else acc )


  let max_tree_size t = find_max_tree (0, None) t |> fst

  let pp fmt bin = F.fprintf fmt "%d(%d)" bin.bin_size (max_tree_size bin)
end

(** a priority heap of [Bin.t]s *)
module Heap = struct
  include Binary_heap.Make (Bin)

  let to_list bins = fold (fun bin acc -> bin :: acc) bins []
end

(** a list of [Bin.t]s of a predefined length (equal to number of workers) *)
module Schedule = struct
  let size schedule = List.fold ~init:0 schedule ~f:(fun acc bin -> Bin.size bin + acc)

  let max_tree_size schedule =
    List.fold schedule ~init:0 ~f:(fun acc c -> max acc (Bin.max_tree_size c))


  let find_max_tree schedule = List.fold schedule ~init:(0, None) ~f:Bin.find_max_tree

  let pp fmt t = F.fprintf fmt "Schedule: %a" (PrettyPrintable.pp_collection ~pp_item:Bin.pp) t

  let histogram_bins = 10

  let pp_histogram fmt schedule =
    let max_size = max_tree_size schedule in
    let histogram_bin_size = 1 + (max_size / (histogram_bins - 1)) in
    let histogram = Array.init histogram_bins ~f:(fun _ -> 0) in
    L.debug Capture Quiet "max_size=%d, histogram_bin_size=%d.@." max_size histogram_bin_size ;
    List.iter schedule ~f:(fun (bin : Bin.t) ->
        List.iter bin.trees ~f:(fun t ->
            let size = Tree.size t in
            let hist_bin = size / histogram_bin_size in
            1 + Array.get histogram hist_bin |> Array.set histogram hist_bin ) ) ;
    F.fprintf fmt "tree histogram:@\n" ;
    Array.iteri histogram ~f:(fun i num ->
        F.fprintf fmt "[%d - %d]: %d@\n" (i * histogram_bin_size)
          (((i + 1) * histogram_bin_size) - 1)
          num )


  (** schedule trees using "shortest processing time first" *)
  let schedule_trees n_workers trees_list =
    let bins = Heap.create ~dummy:Bin.empty n_workers in
    for _ = 1 to n_workers do
      Heap.add bins Bin.empty
    done ;
    List.iter trees_list ~f:(fun tree ->
        Heap.pop_minimum bins |> Bin.add_tree tree |> Heap.add bins ) ;
    Heap.to_list bins


  (** "error" is the difference between the average size of a bin and the actual one. The sum of
      absolute errors divided by the total size (as percentage) is compared to [max_error_pc]. *)
  let is_error_leq_than_max ~n_workers ~max_error_pc schedule =
    let total_size = size schedule in
    let avg_size = total_size / n_workers in
    let total_error =
      List.fold ~init:0 schedule ~f:(fun acc bin -> abs (Bin.size bin - avg_size) + acc)
    in
    let avg_error_pc = 100 * total_error / total_size in
    L.debug Capture Medium "Schedule error pc: %d@\n" avg_error_pc ;
    avg_error_pc <= max_error_pc


  (** given a set of trees [g] produce a schedule that has error lower/equal to [max_error_pc] *)
  let rec find ~orig_size ~n_workers ~max_error_pc g =
    let schedule = Tree.get_sorted_tree_list g |> schedule_trees n_workers in
    L.debug Capture Medium "Found schedule with bins: %a@\n" pp schedule ;
    L.debug Capture Medium "%a" pp_histogram schedule ;
    if Config.debug_mode then assert (Int.equal orig_size (size schedule)) ;
    if is_error_leq_than_max ~n_workers ~max_error_pc schedule then schedule
    else (
      find_max_tree schedule |> snd |> Option.value_exn |> Tree.split_tree g ;
      find ~orig_size ~n_workers ~max_error_pc g )


  let find ~orig_size ~n_workers ~max_error_pc g =
    time_and_run
      ~f:(fun () ->
        Dagify.make_acyclic g ;
        Tree.make_forest g ;
        find ~orig_size ~n_workers ~max_error_pc g )
      ~msg:"Find"


  let output ~n_workers schedule =
    let width = string_of_int (n_workers - 1) |> String.length in
    let schedule_filename = Config.results_dir ^/ "schedule.txt" in
    List.mapi schedule ~f:(fun i bin ->
        L.progress "Schedule for worker %i contains %i files.@\n" i (Bin.size bin) ;
        let filename = Printf.sprintf "%s/worker%*d.idx" Config.results_dir width i in
        Out_channel.with_file filename ~f:(fun out ->
            Bin.iter_vertices bin ~f:(fun source_file ->
                Out_channel.output_string out (SourceFile.to_string source_file) ;
                Out_channel.newline out ) ) ;
        filename )
    |> Out_channel.write_lines schedule_filename


  let find_and_output ~n_workers ~max_error_pc =
    let g = G.load_graph () in
    let orig_size = G.nb_vertex g in
    find ~orig_size ~n_workers ~max_error_pc g |> output ~n_workers
end

let max_error_pc = 10

let partition_source_file_call_graph ~n_workers = Schedule.find_and_output ~n_workers ~max_error_pc

let to_dotty filename =
  let g = G.load_graph () in
  to_dotty g filename
