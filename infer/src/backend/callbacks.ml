(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Module to register and invoke callbacks *)

type proc_callback_args =
  { get_proc_desc: Typ.Procname.t -> Procdesc.t option
  ; get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; summary: Summary.t
  ; proc_desc: Procdesc.t
  ; exe_env: Exe_env.t }

type proc_callback_t = proc_callback_args -> Summary.t

type cluster_callback_args =
  { procedures: (Tenv.t * Procdesc.t) list
  ; get_proc_desc: Typ.Procname.t -> Procdesc.t option
  ; exe_env: Exe_env.t }

type cluster_callback_t = cluster_callback_args -> unit

type procedure_callback = {dynamic_dispatch: bool; language: Language.t; callback: proc_callback_t}

type cluster_callback = {language: Language.t; callback: cluster_callback_t}

let procedure_callbacks = ref []

let cluster_callbacks = ref []

let register_procedure_callback ?(dynamic_dispatch= false) language (callback: proc_callback_t) =
  procedure_callbacks := {dynamic_dispatch; language; callback} :: !procedure_callbacks


let register_cluster_callback language (callback: cluster_callback_t) =
  cluster_callbacks := {language; callback} :: !cluster_callbacks


(** Collect what we need to know about a procedure for the analysis. *)
let get_procedure_definition exe_env proc_name =
  Option.map
    ~f:(fun proc_desc ->
      let tenv = Exe_env.get_tenv exe_env proc_name in
      (tenv, proc_desc) )
    (Exe_env.get_proc_desc exe_env proc_name)


(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks get_proc_desc exe_env summary proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let procedure_language = Typ.Procname.get_language proc_name in
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
    ~f:(fun summary {dynamic_dispatch; language; callback} ->
      if Language.equal language procedure_language && (dynamic_dispatch || not is_specialized)
      then callback {get_proc_desc; get_procs_in_file; tenv; summary; proc_desc; exe_env}
      else summary )
    !procedure_callbacks


(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env get_proc_desc =
  if !cluster_callbacks <> [] then
    let procedures = List.filter_map ~f:(get_procedure_definition exe_env) all_procs in
    let environment = {procedures; get_proc_desc; exe_env} in
    let language_matches language =
      match procedures with
      | (_, pdesc) :: _ ->
          Language.equal language (Typ.Procname.get_language (Procdesc.get_proc_name pdesc))
      | _ ->
          true
    in
    List.iter
      ~f:(fun {language; callback} -> if language_matches language then callback environment)
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
        Option.map ~f:Summary.get_proc_desc (Summary.get proc_name)
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
  let analyze_proc_name pname = ignore (Ondemand.analyze_proc_name pname : Summary.t option) in
  List.iter ~f:analyze_proc_name procs_to_analyze ;
  (* Invoke cluster callbacks. *)
  iterate_cluster_callbacks procs_to_analyze exe_env get_proc_desc ;
  (* Perf logging needs to remain at the end - after analysis work is complete *)
  create_perf_stats_report exe_env.source_file ;
  (* Unregister callbacks *)
  Ondemand.unset_callbacks () ;
  Language.curr_language := saved_language
