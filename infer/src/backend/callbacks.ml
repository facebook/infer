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
module F = Format
module L = Logging

(** Module to register and invoke callbacks *)

type proc_callback_args =
  { get_proc_desc: Typ.Procname.t -> Procdesc.t option
  ; get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; summary: Specs.summary
  ; proc_desc: Procdesc.t
  ; exe_env: Exe_env.t }

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


let get_language proc_name =
  if Typ.Procname.is_java proc_name then Language.Java else Language.Clang


(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks get_proc_desc exe_env summary proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let procedure_language = get_language proc_name in
  Language.curr_language := procedure_language ;
  let get_procs_in_file proc_name =
    match Exe_env.get_cfg exe_env proc_name with
    | Some cfg ->
        Cfg.get_all_proc_names cfg
    | None ->
        []
  in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold ~init:summary
    ~f:(fun summary (language, resolved, proc_callback) ->
      if Language.equal language procedure_language && (resolved || not is_specialized) then
        proc_callback {get_proc_desc; get_procs_in_file; tenv; summary; proc_desc; exe_env}
      else summary )
    !procedure_callbacks


(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env get_proc_desc =
  let procedures = List.filter_map ~f:(get_procedure_definition exe_env) all_procs in
  let environment = {procedures; get_proc_desc} in
  let language_matches language =
    match procedures with
    | (_, pdesc) :: _ ->
        Language.equal language (get_language (Procdesc.get_proc_name pdesc))
    | _ ->
        true
  in
  List.iter
    ~f:(fun (language_opt, cluster_callback) ->
      if language_matches language_opt then cluster_callback environment )
    !cluster_callbacks


let dump_duplicate_procs (exe_env: Exe_env.t) procs =
  let duplicate_procs =
    List.filter_map procs ~f:(fun pname ->
        match Exe_env.get_proc_desc exe_env pname with
        | Some pdesc when (* defined in the current file *) Procdesc.is_defined pdesc -> (
          match Attributes.load pname with
          | Some {source_file_captured; loc}
            when (* defined in another file *)
                 not (SourceFile.equal exe_env.source_file source_file_captured)
                 && (* really defined in the current file and not in an include *)
                    SourceFile.equal exe_env.source_file loc.file ->
              Some (pname, source_file_captured)
          | _ ->
              None )
        | _ ->
            None )
  in
  let output_to_file duplicate_procs =
    Out_channel.with_file (Config.results_dir ^/ Config.duplicates_filename)
      ~append:true ~perm:0o666 ~f:(fun outc ->
        let fmt = F.formatter_of_out_channel outc in
        List.iter duplicate_procs ~f:(fun (pname, source_captured) ->
            F.fprintf fmt "@.DUPLICATE_SYMBOLS source:%a source_captured:%a pname:%a@."
              SourceFile.pp exe_env.source_file SourceFile.pp source_captured Typ.Procname.pp pname
        ) )
  in
  if not (List.is_empty duplicate_procs) then output_to_file duplicate_procs


let create_perf_stats_report source_file =
  PerfStats.register_report PerfStats.TimeAndMemory (PerfStats.Backend source_file) ;
  PerfStats.get_reporter (PerfStats.Backend source_file) ()


(** Invoke all procedure and cluster callbacks on a given environment. *)
let iterate_callbacks (exe_env: Exe_env.t) =
  let saved_language = !Language.curr_language in
  let get_proc_desc proc_name =
    match Exe_env.get_proc_desc exe_env proc_name with
    | Some _ as pdesc_opt ->
        pdesc_opt
    | None ->
        Option.map ~f:Specs.get_proc_desc (Specs.get_summary proc_name)
  in
  let analyze_ondemand summary proc_desc =
    iterate_procedure_callbacks get_proc_desc exe_env summary proc_desc
  in
  Ondemand.set_callbacks {Ondemand.analyze_ondemand; get_proc_desc} ;
  (* Invoke procedure callbacks using on-demand analysis schedulling *)
  let procs_to_analyze =
    (* analyze all the currently defined procedures *)
    SourceFiles.proc_names_of_source exe_env.source_file
  in
  if Config.dump_duplicate_symbols then dump_duplicate_procs exe_env procs_to_analyze ;
  let analyze_proc_name pname =
    Option.iter
      ~f:(fun pdesc -> ignore (Ondemand.analyze_proc_desc pdesc))
      (Ondemand.get_proc_desc pname)
  in
  List.iter ~f:analyze_proc_name procs_to_analyze ;
  (* Invoke cluster callbacks. *)
  iterate_cluster_callbacks procs_to_analyze exe_env get_proc_desc ;
  (* Perf logging needs to remain at the end - after analysis work is complete *)
  create_perf_stats_report exe_env.source_file ;
  (* Unregister callbacks *)
  Ondemand.unset_callbacks () ;
  Language.curr_language := saved_language
