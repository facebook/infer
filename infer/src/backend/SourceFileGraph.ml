(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

module Loader = struct
  (** Build a hash table from procedure name to source file where it's defined, and a hash table
      from source files to list of procedures defined. *)
  let get_indexes () =
    let counter = ref 0 in
    let empties = ref 0 in
    let proc_index = Procname.Hash.create 11 in
    let source_index = SourceFile.Hash.create 11 in
    let db = ResultsDatabase.get_database () in
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
  module Vertex = struct
    include SourceFile

    let hash = Caml.Hashtbl.hash
  end

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


let to_dotty filename =
  let g = G.load_graph () in
  to_dotty g filename
