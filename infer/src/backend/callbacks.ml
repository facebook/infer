(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant
module L = Logging

(** Module to register and invoke callbacks *)

type proc_callback_args =
  { get_proc_desc: Typ.Procname.t -> Procdesc.t option
  ; get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; summary: Specs.summary
  ; proc_desc: Procdesc.t }

type proc_callback_t = proc_callback_args -> Specs.summary

type cluster_callback_args =
  {procedures: (Tenv.t * Procdesc.t) list; get_proc_desc: Typ.Procname.t -> Procdesc.t option}

type cluster_callback_t = cluster_callback_args -> unit

let procedure_callbacks = ref []

let cluster_callbacks = ref []

let register_procedure_callback ?(dynamic_dispath= false) language (callback: proc_callback_t) =
  procedure_callbacks := (language, dynamic_dispath, callback) :: !procedure_callbacks

let register_cluster_callback language (callback: cluster_callback_t) =
  cluster_callbacks := (language, callback) :: !cluster_callbacks

(** Collect what we need to know about a procedure for the analysis. *)
let get_procedure_definition exe_env proc_name =
  let tenv = Exe_env.get_tenv exe_env proc_name in
  Option.map ~f:(fun proc_desc -> (tenv, proc_desc)) (Exe_env.get_proc_desc exe_env proc_name)

let get_language proc_name = if Typ.Procname.is_java proc_name then Config.Java else Config.Clang

(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks get_proc_desc exe_env summary proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let procedure_language = get_language proc_name in
  Config.curr_language := procedure_language ;
  let get_procs_in_file proc_name =
    match Exe_env.get_cfg exe_env proc_name with
    | Some cfg
     -> List.map ~f:Procdesc.get_proc_name (Cfg.get_defined_procs cfg)
    | None
     -> []
  in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold ~init:summary
    ~f:(fun summary (language, resolved, proc_callback) ->
      if Config.equal_language language procedure_language && (resolved || not is_specialized) then
        proc_callback {get_proc_desc; get_procs_in_file; tenv; summary; proc_desc}
      else summary)
    !procedure_callbacks

(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env get_proc_desc =
  let procedures = List.filter_map ~f:(get_procedure_definition exe_env) all_procs in
  let environment = {procedures; get_proc_desc} in
  let language_matches language =
    match procedures with
    | (_, pdesc) :: _
     -> Config.equal_language language (get_language (Procdesc.get_proc_name pdesc))
    | _
     -> true
  in
  List.iter
    ~f:(fun (language_opt, cluster_callback) ->
      if language_matches language_opt then cluster_callback environment)
    !cluster_callbacks

(** Invoke all procedure and cluster callbacks on a given environment. *)
let iterate_callbacks call_graph exe_env =
  let saved_language = !Config.curr_language in
  let procs_to_analyze =
    (* analyze all the currently defined procedures *)
    Cg.get_defined_nodes call_graph
  in
  let get_proc_desc proc_name =
    match Exe_env.get_proc_desc exe_env proc_name with
    | Some pdesc
     -> Some pdesc
    | None when Config.(equal_dynamic_dispatch dynamic_dispatch Lazy)
     -> Option.bind (Specs.get_summary proc_name) ~f:(fun summary -> summary.Specs.proc_desc_option)
    | None
     -> None
  in
  let analyze_ondemand summary proc_desc =
    iterate_procedure_callbacks get_proc_desc exe_env summary proc_desc
  in
  let callbacks = {Ondemand.analyze_ondemand= analyze_ondemand; get_proc_desc} in
  (* Create and register on-demand analysis callback *)
  let analyze_proc_name pname =
    match Ondemand.get_proc_desc pname with
    | None
     -> L.(die InternalError) "Could not find proc desc for %a" Typ.Procname.pp pname
    | Some pdesc
     -> ignore (Ondemand.analyze_proc_desc pdesc pdesc)
  in
  Ondemand.set_callbacks callbacks ;
  (* Invoke procedure callbacks using on-demand anlaysis schedulling *)
  List.iter ~f:analyze_proc_name procs_to_analyze ;
  (* Invoke cluster callbacks. *)
  iterate_cluster_callbacks procs_to_analyze exe_env get_proc_desc ;
  (* Unregister callbacks *)
  Ondemand.unset_callbacks () ;
  Config.curr_language := saved_language
