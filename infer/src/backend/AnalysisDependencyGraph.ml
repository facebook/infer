(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let build ~changed_files =
  let graph = CallGraph.(create default_initial_capacity) in
  let tenv_deps = SourceFile.Hash.create 0 in
  (* set up a hashtable of procedures in changed files at the current version upfront, to avoid
     querying SQLite repeatedly for [SourceFiles.proc_names_of_source] *)
  let procs_in_changed_files : Procname.Set.t SourceFile.Hash.t =
    let hashtbl = SourceFile.Hash.create (SourceFile.Set.cardinal changed_files) in
    SourceFile.Set.iter
      (fun changed_file ->
        let procs = SourceFiles.proc_names_of_source changed_file |> Procname.Set.of_list in
        SourceFile.Hash.add hashtbl changed_file procs )
      changed_files ;
    hashtbl
  in
  let deleted_procs = ref [] in
  (* First, build a reverse analysis callgraph [graph] and tenv dependency map [tenv_deps]. *)
  Summary.OnDisk.iter_specs ~f:(fun {Summary.proc_name; dependencies} ->
      let {Dependencies.summary_loads; other_proc_names; used_tenv_sources} =
        match dependencies with
        | Complete c ->
            c
        | Partial ->
            L.die InternalError "deserialized summary with incomplete dependencies"
      in
      let is_deleted_proc =
        match Attributes.load proc_name with
        | None ->
            true
        | Some {ProcAttributes.translation_unit} ->
            SourceFile.Set.mem translation_unit changed_files
            && not
                 (Procname.Set.mem proc_name
                    (SourceFile.Hash.find procs_in_changed_files translation_unit) )
      in
      if is_deleted_proc then deleted_procs := proc_name :: !deleted_procs ;
      List.iter summary_loads ~f:(fun callee ->
          CallGraph.add_edge graph callee ~successor:proc_name ) ;
      List.iter other_proc_names ~f:(fun callee ->
          CallGraph.add_edge graph callee ~successor:proc_name ) ;
      List.iter used_tenv_sources ~f:(fun src_file ->
          match SourceFile.Hash.find_opt tenv_deps src_file with
          | Some deps ->
              Procname.HashSet.add proc_name deps
          | None ->
              Procname.HashSet.singleton proc_name |> SourceFile.Hash.add tenv_deps src_file ) ;
      (* Ensure proc_name is part of the graph if it has not been referenced yet *)
      if not (CallGraph.mem_procname graph proc_name) then CallGraph.create_node graph proc_name [] ) ;
  (* Then, flag in [graph] any procedure with a summary depending (transitively) on either (1) a
     deleted procedure, (2) the tenv of a changed file or (3) the summary of a changed procedure. *)
  List.iter !deleted_procs ~f:(CallGraph.flag_reachable graph) ;
  SourceFile.Set.iter
    (fun sf ->
      SourceFile.Hash.find_opt tenv_deps sf
      |> Option.iter ~f:(fun sf_tenv_deps ->
             Procname.HashSet.iter sf_tenv_deps (CallGraph.flag_reachable graph) ) ;
      SourceFile.Hash.find procs_in_changed_files sf
      |> Procname.Set.iter (fun pname ->
             match Attributes.load pname with
             | None ->
                 CallGraph.flag_reachable graph pname
             | Some attrs ->
                 if attrs.changed then CallGraph.flag_reachable graph pname ) )
    changed_files ;
  graph


let invalidate ~changed_files =
  let changed_files =
    match changed_files with
    | Some files ->
        files
    | None ->
        L.die ExternalError
          "incremental invalidation requires specifying changed files via --changed-files-index"
  in
  L.progress "Incremental analysis: invalidating potentially-affected analysis results.@." ;
  let traverse_timer = Mtime_clock.counter () in
  let dependency_graph = build ~changed_files in
  let total_nodes = CallGraph.n_procs dependency_graph in
  (* Only bother with incremental invalidation and logging if there are already some analysis
     results stored in the db. *)
  if total_nodes > 0 then (
    if Config.debug_level_analysis > 0 then
      CallGraph.to_dotty dependency_graph AnalysisDependencyInvalidationGraphDot ;
    let invalidated_pnames, invalidated_files =
      CallGraph.fold_flagged dependency_graph
        ~f:(fun node (acc_pnames, acc_files) ->
          let files =
            match Attributes.load node.pname with
            | Some {translation_unit} ->
                SourceFile.Set.add translation_unit acc_files
            | None ->
                acc_files
          in
          (node.pname :: acc_pnames, files) )
        ([], SourceFile.Set.empty)
    in
    let traverse_time = Mtime_clock.count traverse_timer in
    let db_timer = Mtime_clock.counter () in
    Summary.OnDisk.delete_all ~procedures:invalidated_pnames ;
    let db_time = Mtime_clock.count db_timer in
    SourceFile.Set.iter IssueLog.invalidate invalidated_files ;
    let invalidated_files = SourceFile.Set.cardinal invalidated_files in
    let invalidated_nodes_count = List.length invalidated_pnames in
    let span_to_us s =
      s |> Mtime.Span.to_uint64_ns |> Int64.to_int |> Option.value ~default:0 |> fun d -> d / 1000
    in
    L.progress
      "Incremental analysis: Invalidated %d of %d procedure summaries, and file-level analyses for \
       %d distinct file%s [traversal %a] [db %a]@."
      invalidated_nodes_count total_nodes invalidated_files
      (if Int.equal invalidated_files 1 then "" else "s")
      Mtime.Span.pp traverse_time Mtime.Span.pp db_time ;
    ScubaLogging.log_count ~label:"incremental_analysis.total_nodes" ~value:total_nodes ;
    ScubaLogging.log_count ~label:"incremental_analysis.invalidated_nodes"
      ~value:invalidated_nodes_count ;
    ScubaLogging.log_count ~label:"incremental_analysis.invalidated_files" ~value:invalidated_files ;
    ScubaLogging.log_duration ~label:"incremental_analysis.invalidation_time"
      ~duration_us:(span_to_us @@ Mtime.Span.add traverse_time db_time) ;
    ScubaLogging.log_duration ~label:"incremental_analysis.invalidation_traverse_time"
      ~duration_us:(span_to_us traverse_time) ;
    ScubaLogging.log_duration ~label:"incremental_analysis.invalidation_db_time"
      ~duration_us:(span_to_us db_time) ) ;
  (* save some memory *)
  ResultsDir.scrub_for_incremental ()


let iter_on_disk_dependencies ~f =
  Summary.OnDisk.iter_specs ~f:(fun {Summary.proc_name; dependencies} ->
      let {Dependencies.summary_loads; recursion_edges} =
        match dependencies with
        | Complete c ->
            c
        | Partial ->
            L.die InternalError "deserialized summary with incomplete dependencies"
      in
      f proc_name summary_loads recursion_edges )


let from_summaries () =
  L.progress "Loading previous analysis schedule from summaries@\n" ;
  let graph = CallGraph.(create default_initial_capacity) in
  let edges_to_ignore = ref Procname.Map.empty in
  iter_on_disk_dependencies ~f:(fun proc_name summary_loads recursion_edges ->
      edges_to_ignore := Procname.Map.add proc_name recursion_edges !edges_to_ignore ;
      CallGraph.create_node graph proc_name summary_loads ) ;
  if Config.debug_level_analysis > 0 then CallGraph.to_dotty graph AnalysisDependencyGraphDot ;
  Ondemand.edges_to_ignore := Some !edges_to_ignore ;
  graph


(** on-disk format for the analysis call graph *)
module Serialized = struct
  (** {1 Serialization Schema}

      The goal is to be as independent from the OCaml datatypes as possible so as to be able to
      replay analyses with different (if not-too-distant) infer versions when possible. This helps
      us test infer itself.

      The schema is one line [schema_version=<int>] followed by one line for each procedure in the
      analysis call graph, formatted as:

      [<int>|<proc_uid>|<int list>|<int list>]

      where an <int list> is a comma-separated list of ints. Each int is a procedure id. Let us name
      each column:

      [id|proc_uid|dependencies|recursive dependencies]

      - [id], the first int in the line, is the id assigned to the current procedure
      - [proc_uid] is obtained from [Procname.pp_unique_id] and must coincide with the keys in the
        (capture) database so we can translate them back into a proc name
      - [dependencies] are the ids of the procedures that correspond to the outgoing edges from [id]
        in the analysis dependency graph
      - [recursive dependencies] are the ids of the procedures that should be recorded in
        [Ondemand.edges_to_ignore]

      Note that each procedure uid (which can be long) is only printed once and other times refer to
      it by just an int to make the format as compact as possible.

      It's possible that a procedure id mentioned in [dependencies] or [recursive dependencies]
      remains unresolved, i.e. does not have a line stating what its corresponding [proc_uid] is.
      This happens for procedures that have no summaries. We do not need to create nodes in the
      graph for them anyway as they are ignored by the task generated by [CallGraphScheduler]. *)

  (** change if the format changes in an incompatible way, please *)
  let schema_version = 0

  module IntMap = Caml.Hashtbl.Make (Int)

  (** {2 Serialization} *)

  let write_to_file file =
    let procedure_ids = Procname.Hash.create CallGraph.default_initial_capacity in
    let id_of_proc_name proc_name =
      match Procname.Hash.find_opt procedure_ids proc_name with
      | Some id ->
          id
      | None ->
          let id = Procname.Hash.length procedure_ids in
          Procname.Hash.replace procedure_ids proc_name id ;
          id
    in
    let pp_proc_id fmt proc_name =
      Out_channel.output_string fmt @@ Int.to_string @@ id_of_proc_name proc_name
    in
    let pp_proc_id_iter iter fmt coll =
      let is_first = ref true in
      iter
        (fun proc_name ->
          if not !is_first then Out_channel.output_char fmt ',' ;
          pp_proc_id fmt proc_name ;
          is_first := false )
        coll
    in
    let pp_proc_id_list fmt list = pp_proc_id_iter Caml.List.iter fmt list in
    let pp_proc_id_set fmt set = pp_proc_id_iter Procname.Set.iter fmt set in
    Utils.with_file_out file ~f:(fun outf ->
        Printf.fprintf outf "schema_version=%d\n" schema_version ;
        iter_on_disk_dependencies ~f:(fun proc_name summary_loads recursion_edges ->
            let id = id_of_proc_name proc_name in
            Printf.fprintf outf "%d|%s|%a|%a\n" id (Procname.to_unique_id proc_name) pp_proc_id_list
              summary_loads pp_proc_id_set recursion_edges ) ;
        Out_channel.flush outf )


  (** {2 Deserialization} *)

  (** intermediate datatype while we are parsing a call graph as we cannot yet resolve all ids (int)
      into proc names until we have collected all of them *)
  type node = {proc_uid: string; deps: int list; recursive_deps: int list}

  (** turns a graph of [node] into a [CallGraph.t] and sets [Ondemand.edges_to_ignore] *)
  let of_parsed_call_graph pre_call_graph =
    let call_graph = CallGraph.create CallGraph.default_initial_capacity in
    let edges_to_ignore = ref Procname.Map.empty in
    IntMap.iter
      (fun id pre_node ->
        match Procdesc.load_uid pre_node.proc_uid with
        | None ->
            (* we don't need to bother with procedures that don't have a CFG *)
            ()
        | Some proc_desc ->
            let proc_name = Procdesc.get_proc_name proc_desc in
            CallGraph.create_node_with_id call_graph ~id proc_name ~successors:pre_node.deps )
      pre_call_graph ;
    IntMap.iter
      (fun id pre_node ->
        let proc_name =
          match CallGraph.node_of_id call_graph id with
          | Some {pname} ->
              pname
          | None ->
              (* we just stored all of the same ids in the call graph *)
              assert false
        in
        let recursion_edges =
          List.fold pre_node.recursive_deps ~init:Procname.Set.empty
            ~f:(fun recursive_deps dep_id ->
              match CallGraph.node_of_id call_graph dep_id with
              | None ->
                  recursive_deps
              | Some {pname= recursive_dep} ->
                  Procname.Set.add recursive_dep recursive_deps )
        in
        edges_to_ignore := Procname.Map.add proc_name recursion_edges !edges_to_ignore )
      pre_call_graph ;
    Ondemand.edges_to_ignore := Some !edges_to_ignore ;
    call_graph


  let parse_int_list s =
    if String.is_empty s then [] else String.split s ~on:',' |> List.map ~f:Int.of_string


  let parse_one_line line =
    let id_s, rest = String.lsplit2_exn line ~on:'|' in
    let id = Int.of_string id_s in
    let rest, recursion_edges_s = String.rsplit2_exn rest ~on:'|' in
    let proc_uid, dependencies_s = String.rsplit2_exn rest ~on:'|' in
    let recursive_deps = parse_int_list recursion_edges_s in
    let deps = parse_int_list dependencies_s in
    (id, {proc_uid; deps; recursive_deps})


  exception EmptyFile

  exception NotACallGraph of {first_line: string}

  exception ParseError of {line: string; error: string}

  exception VersionMismatch of {version: int}

  let parse_version call_graph_in =
    let schema_line =
      try In_channel.input_line_exn call_graph_in with End_of_file -> raise EmptyFile
    in
    match Scanf.sscanf schema_line "schema_version=%d" Fn.id with
    | exception (End_of_file | Scanf.Scan_failure _ | Failure _) ->
        raise (NotACallGraph {first_line= schema_line})
    | version ->
        if version <> schema_version then raise (VersionMismatch {version})


  let parse_call_graph call_graph_in call_graph =
    In_channel.iter_lines call_graph_in ~f:(fun line ->
        match parse_one_line line with
        | id, node ->
            IntMap.add call_graph id node
        | exception Not_found_s error ->
            raise (ParseError {line; error= Sexp.to_string error})
        | exception Failure error ->
            raise (ParseError {line; error}) )


  let deserialize call_graph_in =
    let call_graph = IntMap.create CallGraph.default_initial_capacity in
    parse_version call_graph_in ;
    parse_call_graph call_graph_in call_graph ;
    call_graph
end

let analysis_schedule_file () =
  match Config.analysis_schedule_file with
  | Some file ->
      file
  | None ->
      ResultsDir.get_path AnalysisDependencyGraph


let load_previous_schedule_from_file file =
  let open Serialized in
  L.progress "Loading analysis schedule from '%s' for replay@\n" file ;
  let skipping_replay fmt =
    F.kasprintf
      (fun s ->
        L.internal_error
          "Error loading the previous analysis schedule from '%s': %s@\n\
           Skipping attempt at replaying the previous analysis.@." file s ;
        None )
      fmt
  in
  try Some (Utils.with_file_in file ~f:deserialize |> of_parsed_call_graph) with
  | EmptyFile ->
      skipping_replay "empty file"
  | NotACallGraph {first_line} ->
      skipping_replay "schema_version not found in first line: '%s'" first_line
  | ParseError {line; error} ->
      skipping_replay "when parsing %s: %s" line error
  | Sys_error error ->
      skipping_replay "%s" error
  | VersionMismatch {version} ->
      skipping_replay "incompatible schema version: expected %d but got %d" schema_version version


let load_previous_schedule () =
  let file = analysis_schedule_file () in
  if ISys.file_exists file then load_previous_schedule_from_file file else Some (from_summaries ())


let store_previous_schedule () =
  let file = analysis_schedule_file () in
  Serialized.write_to_file file ;
  L.progress "Analysis schedule stored in '%s'@\n" file ;
  ()


let store_previous_schedule_if_needed () =
  if
    Config.is_originator && Config.replay_analysis_schedule
    && not (ISys.file_exists (analysis_schedule_file ()))
  then (
    (* if a schedule wasn't stored already then store one now before we start deleting the results
       databases *)
    (* because it's early in the process we don't have a db connection yet; we don't want to set up
       logging because we are possibly about to delete the current logs file *)
    Database.ensure_database_connection Primary AnalysisDatabase ;
    (* logging isn't set up either but at least this will print to the console *)
    L.progress "Loading previous analysis schedule from summaries@\n" ;
    store_previous_schedule () )
