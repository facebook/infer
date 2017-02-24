(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging

(** Module to register and invoke callbacks *)

type proc_callback_args = {
  get_proc_desc : Procname.t -> Procdesc.t option;
  get_procs_in_file : Procname.t -> Procname.t list;
  idenv : Idenv.t;
  tenv : Tenv.t;
  proc_name : Procname.t;
  proc_desc : Procdesc.t;
}

type proc_callback_t = proc_callback_args -> unit

type cluster_callback_t =
  Exe_env.t ->
  Procname.t list ->
  (Procname.t -> Procdesc.t option) ->
  (Idenv.t * Tenv.t * Procname.t * Procdesc.t) list ->
  unit

let procedure_callbacks = ref []
let cluster_callbacks = ref []

let register_procedure_callback language_opt (callback: proc_callback_t) =
  procedure_callbacks := (language_opt, callback):: !procedure_callbacks

let register_cluster_callback language_opt (callback: cluster_callback_t) =
  cluster_callbacks := (language_opt, callback):: !cluster_callbacks

let unregister_all_callbacks () =
  procedure_callbacks := [];
  cluster_callbacks := []


(** Collect what we need to know about a procedure for the analysis. *)
let get_procedure_definition exe_env proc_name =
  let tenv = Exe_env.get_tenv exe_env proc_name in
  Option.map
    ~f:(fun proc_desc ->
        let idenv = Idenv.create proc_desc
        and language = (Procdesc.get_attributes proc_desc).ProcAttributes.language in
        (idenv, tenv, proc_name, proc_desc, language))
    (Exe_env.get_proc_desc exe_env proc_name)

let get_language proc_name = if Procname.is_java proc_name then Config.Java else Config.Clang

(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks exe_env caller_pname =
  let procedure_language = get_language caller_pname in
  Config.curr_language := procedure_language;

  let get_proc_desc proc_name =
    Exe_env.get_proc_desc exe_env proc_name in

  let get_procs_in_file proc_name =
    match Exe_env.get_cfg exe_env proc_name with
    | Some cfg->
        List.map ~f:Procdesc.get_proc_name (Cfg.get_defined_procs cfg)
    | None ->
        [] in

  let update_time proc_name elapsed =
    match Specs.get_summary proc_name with
    | Some prev_summary ->
        let stats_time = prev_summary.Specs.stats.Specs.stats_time +. elapsed in
        let stats = { prev_summary.Specs.stats with Specs.stats_time = stats_time } in
        let summary = { prev_summary with Specs.stats = stats } in
        Specs.add_summary proc_name summary
    | None -> () in

  Option.iter
    ~f:(fun (idenv, tenv, proc_name, proc_desc, _) ->
        List.iter
          ~f:(fun (language_opt, proc_callback) ->
              let language_matches = match language_opt with
                | Some language -> Config.equal_language language procedure_language
                | None -> true in
              if language_matches then
                begin
                  let init_time = Unix.gettimeofday () in
                  proc_callback
                    {
                      get_proc_desc;
                      get_procs_in_file;
                      idenv;
                      tenv;
                      proc_name;
                      proc_desc;
                    };
                  let elapsed = Unix.gettimeofday () -. init_time in
                  update_time proc_name elapsed
                end)
          !procedure_callbacks)
    (get_procedure_definition exe_env caller_pname)

(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env proc_names =
  let get_procdesc = Exe_env.get_proc_desc exe_env in

  let procedure_definitions =
    List.filter_map ~f:(get_procedure_definition exe_env) proc_names in

  let environment =
    List.map
      ~f:(fun (idenv, tenv, proc_name, proc_desc, _) -> (idenv, tenv, proc_name, proc_desc))
      procedure_definitions in

  (* Procedures matching the given language or all if no language is specified. *)
  let relevant_procedures language_opt =
    Option.value_map
      ~f:(fun l -> List.filter ~f:(fun p -> Config.equal_language l (get_language p)) proc_names)
      ~default:proc_names
      language_opt in

  List.iter
    ~f:(fun (language_opt, cluster_callback) ->
        let proc_names = relevant_procedures language_opt in
        if List.length proc_names > 0 then
          cluster_callback exe_env all_procs get_procdesc environment)
    !cluster_callbacks

(** Invoke all procedure and cluster callbacks on a given environment. *)
let iterate_callbacks call_graph exe_env =
  let procs_to_analyze =
    (* analyze all the currently defined procedures *)
    Cg.get_defined_nodes call_graph in
  let originally_defined_procs =
    Cg.get_defined_nodes call_graph in
  let saved_language = !Config.curr_language in

  let cluster_id proc_name =
    match proc_name with
    | Procname.Java pname_java ->
        Procname.java_get_class_name pname_java
    | _ ->
        "unknown" in
  let cluster proc_names =
    let cluster_map =
      List.fold
        ~f:(fun map proc_name ->
            let proc_cluster = cluster_id proc_name in
            let bucket = try String.Map.find_exn map proc_cluster with Not_found -> [] in
            String.Map.add ~key:proc_cluster ~data:(proc_name:: bucket) map)
        ~init:String.Map.empty
        proc_names in
    (* Return all values of the map *)
    String.Map.data cluster_map in
  let reset_summary proc_name =
    let attributes_opt =
      Specs.proc_resolve_attributes proc_name in
    let should_reset =
      is_none (Specs.get_summary proc_name) in
    if should_reset
    then Specs.reset_summary call_graph proc_name attributes_opt None in

  (* Make sure summaries exists. *)
  List.iter ~f:reset_summary procs_to_analyze;

  (* Invoke callbacks. *)
  List.iter
    ~f:(iterate_procedure_callbacks exe_env)
    procs_to_analyze;

  List.iter
    ~f:(iterate_cluster_callbacks originally_defined_procs exe_env)
    (cluster procs_to_analyze);

  (* Store all the summaries to disk *)
  List.iter
    ~f:(fun pname ->
        let updated_summary_opt =
          Option.map (Specs.get_summary pname) ~f:Specs.increment_timestamp in
        Option.iter ~f:(Specs.store_summary pname) updated_summary_opt)
    procs_to_analyze;

  Config.curr_language := saved_language
