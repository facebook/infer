(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils
module L = Logging

(** Module to register and invoke callbacks *)

(** Inline a synthetic (access or bridge) method. *)
let inline_synthetic_method ret_ids etl proc_desc proc_name loc_call : Sil.instr option =
  let modified = ref None in
  let debug = false in
  let found instr instr' =
    modified := Some instr';
    if debug then
      begin
        L.stderr "XX inline_synthetic_method found instr: %a@." (Sil.pp_instr pe_text) instr;
        L.stderr "XX inline_synthetic_method instr': %a@." (Sil.pp_instr pe_text) instr'
      end in
  let do_instr node instr =
    match instr, ret_ids, etl with
    | Sil.Letderef (id1, Sil.Lfield (Sil.Var id2, fn, ft), bt, loc), [ret_id], [(e1, t1)] -> (* getter for fields *)
        let instr' = Sil.Letderef (ret_id, Sil.Lfield (e1, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Letderef (id1, Sil.Lfield (Sil.Lvar pvar, fn, ft), bt, loc), [ret_id], []
      when Sil.pvar_is_global pvar -> (* getter for static fields *)
        let instr' = Sil.Letderef (ret_id, Sil.Lfield (Sil.Lvar pvar, fn, ft), bt, loc_call) in
        found instr instr'
    | Sil.Set (Sil.Lfield (ex1, fn, ft), bt , ex2, loc), _, [(e1, t1); (e2, t2)] -> (* setter for fields *)
        let instr' = Sil.Set (Sil.Lfield (e1, fn, ft), bt , e2, loc_call) in
        found instr instr'
    | Sil.Set (Sil.Lfield (Sil.Lvar pvar, fn, ft), bt , ex2, loc), _, [(e1, t1)]
      when Sil.pvar_is_global pvar -> (* setter for static fields *)
        let instr' = Sil.Set (Sil.Lfield (Sil.Lvar pvar, fn, ft), bt , e1, loc_call) in
        found instr instr'
    | Sil.Call (ret_ids', Sil.Const (Sil.Cfun pn), etl', loc', cf), _, _
      when list_length ret_ids = list_length ret_ids'
           && list_length etl' = list_length etl ->
        let instr' = Sil.Call (ret_ids, Sil.Const (Sil.Cfun pn), etl, loc_call, cf) in
        found instr instr'
    | Sil.Call (ret_ids', Sil.Const (Sil.Cfun pn), etl', loc', cf), _, _
      when list_length ret_ids = list_length ret_ids'
           && list_length etl' + 1 = list_length etl ->
        let etl1 = match list_rev etl with (* remove last element *)
          | _ :: l -> list_rev l
          | [] -> assert false in
        let instr' = Sil.Call (ret_ids, Sil.Const (Sil.Cfun pn), etl1, loc_call, cf) in
        found instr instr'
    | _ -> () in
  Cfg.Procdesc.iter_instrs do_instr proc_desc;
  !modified

(** Find synthetic (access or bridge) methods in the procedure and inline them in the cfg. *)
let proc_inline_synthetic_methods cfg proc_desc : unit =
  let instr_inline_synthetic_method = function
    | Sil.Call (ret_ids, Sil.Const (Sil.Cfun pn), etl, loc, _) ->
        (match Cfg.Procdesc.find_from_name cfg pn with
         | Some pd ->
             let is_access = Procname.java_is_access_method pn in
             let attributes = Cfg.Procdesc.get_attributes pd in
             let is_synthetic = attributes.ProcAttributes.is_synthetic_method in
             let is_bridge = attributes.ProcAttributes.is_bridge_method in
             if is_access || is_bridge || is_synthetic
             then inline_synthetic_method ret_ids etl pd pn loc
             else None
         | None -> None)
    | _ -> None in
  let node_inline_synthetic_methods node =
    let modified = ref false in
    let do_instr instr = match instr_inline_synthetic_method instr with
      | None -> instr
      | Some instr' ->
          modified := true;
          instr' in
    let instrs = Cfg.Node.get_instrs node in
    let instrs' = list_map do_instr instrs in
    if !modified then Cfg.Node.replace_instrs node instrs' in
  Cfg.Procdesc.iter_nodes node_inline_synthetic_methods proc_desc


type proc_callback_t =
  Procname.t list ->
  (Procname.t -> Cfg.Procdesc.t option) ->
  Idenv.t ->
  Sil.tenv ->
  Procname.t ->
  Cfg.Procdesc.t ->
  unit

type cluster_callback_t =
  Procname.t list ->
  (Procname.t -> Cfg.Procdesc.t option) ->
  (Idenv.t * Sil.tenv * Procname.t * Cfg.Procdesc.t) list ->
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
  let cfg = Exe_env.get_cfg exe_env proc_name in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  Option.map
    (fun proc_desc ->
       proc_inline_synthetic_methods cfg proc_desc;
       let idenv = Idenv.create cfg proc_desc
       and language = (Cfg.Procdesc.get_attributes proc_desc).ProcAttributes.language in
       (idenv, tenv, proc_name, proc_desc, language))
    (Cfg.Procdesc.find_from_name cfg proc_name)

let get_language proc_name = if Procname.is_java proc_name then Config.Java else Config.C_CPP

(** Invoke all registered procedure callbacks on a set of procedures. *)
let iterate_procedure_callbacks all_procs exe_env proc_name =
  let procedure_language = get_language proc_name in
  Config.curr_language := procedure_language;

  let cfg = Exe_env.get_cfg exe_env proc_name in
  let get_procdesc proc_name =
    let cfg = try Exe_env.get_cfg exe_env proc_name with Not_found -> cfg in
    Cfg.Procdesc.find_from_name cfg proc_name in

  let update_time proc_name elapsed =
    match Specs.get_summary proc_name with
    | Some prev_summary ->
        let stats_time = prev_summary.Specs.stats.Specs.stats_time +. elapsed in
        let stats = { prev_summary.Specs.stats with Specs.stats_time = stats_time } in
        let summary = { prev_summary with Specs.stats = stats } in
        Specs.add_summary proc_name summary
    | None -> () in

  Option.may
    (fun (idenv, tenv, proc_name, proc_desc, language) ->
       list_iter
         (fun (language_opt, proc_callback) ->
            let language_matches = match language_opt with
              | Some language -> language = procedure_language
              | None -> true in
            if language_matches then
              begin
                let init_time = Unix.gettimeofday () in
                proc_callback all_procs get_procdesc idenv tenv proc_name proc_desc;
                let elapsed = Unix.gettimeofday () -. init_time in
                update_time proc_name elapsed
              end)
         !procedure_callbacks)
    (get_procedure_definition exe_env proc_name)

(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env proc_names =
  let get_procdesc proc_name =
    try
      let cfg = Exe_env.get_cfg exe_env proc_name in
      Cfg.Procdesc.find_from_name cfg proc_name
    with Not_found -> None in

  let procedure_definitions =
    list_map (get_procedure_definition exe_env) proc_names
    |> list_flatten_options in

  let environment =
    list_map
      (fun (idenv, tenv, proc_name, proc_desc, _) -> (idenv, tenv, proc_name, proc_desc))
      procedure_definitions in

  (** Procedures matching the given language or all if no language is specified. *)
  let relevant_procedures language_opt =
    Option.map_default
      (fun l -> list_filter (fun p -> l = get_language p) proc_names)
      proc_names
      language_opt in

  list_iter
    (fun (language_opt, cluster_callback) ->
       let proc_names = relevant_procedures language_opt in
       if list_length proc_names > 0 then
         cluster_callback all_procs get_procdesc environment)
    !cluster_callbacks

(** Invoke all procedure and cluster callbacks on a given environment. *)
let iterate_callbacks store_summary call_graph exe_env =
  let proc_names = Cg.get_defined_nodes call_graph in
  let saved_language = !Config.curr_language in

  let cluster_id proc_name =
    match get_language proc_name with
    | Config.Java -> Procname.java_get_class proc_name
    | _ -> "unknown" in
  let cluster proc_names =
    let cluster_map =
      list_fold_left
        (fun map proc_name ->
           let proc_cluster = cluster_id proc_name in
           let bucket = try StringMap.find proc_cluster map with Not_found -> [] in
           StringMap.add proc_cluster (proc_name:: bucket) map)
        StringMap.empty
        proc_names in
    (* Return all values of the map *)
    list_map snd (StringMap.bindings cluster_map) in
  let reset_summary proc_name =
    let attributes_opt =
      Specs.proc_resolve_attributes proc_name in
    Specs.reset_summary call_graph proc_name attributes_opt in


  (* Make sure summaries exists. *)
  list_iter reset_summary proc_names;


  (* Invoke callbacks. *)
  list_iter
    (iterate_procedure_callbacks proc_names exe_env)
    proc_names;

  list_iter
    (iterate_cluster_callbacks proc_names exe_env)
    (cluster proc_names);

  list_iter store_summary proc_names;

  Config.curr_language := saved_language
