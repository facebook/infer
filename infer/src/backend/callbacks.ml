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
  { get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list
  ; tenv: Tenv.t
  ; integer_type_widths: Typ.IntegerWidths.t
  ; summary: Summary.t
  ; proc_desc: Procdesc.t
  ; exe_env: Exe_env.t }

type proc_callback_t = proc_callback_args -> Summary.t

type cluster_callback_args =
  {procedures: (Tenv.t * Procdesc.t) list; source_file: SourceFile.t; exe_env: Exe_env.t}

type cluster_callback_t = cluster_callback_args -> unit

type procedure_callback =
  {name: string; dynamic_dispatch: bool; language: Language.t; callback: proc_callback_t}

type cluster_callback = {name: string; language: Language.t; callback: cluster_callback_t}

let procedure_callbacks = ref []

let cluster_callbacks = ref []

let register_procedure_callback ~name ?(dynamic_dispatch = false) language
    (callback : proc_callback_t) =
  procedure_callbacks := {name; dynamic_dispatch; language; callback} :: !procedure_callbacks


let register_cluster_callback ~name language (callback : cluster_callback_t) =
  cluster_callbacks := {name; language; callback} :: !cluster_callbacks


(** Collect what we need to know about a procedure for the analysis. *)
let get_procedure_definition exe_env proc_name =
  Procdesc.load proc_name
  |> Option.map ~f:(fun proc_desc ->
         let tenv = Exe_env.get_tenv exe_env proc_name in
         (tenv, proc_desc) )


(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks exe_env summary proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let procedure_language = Typ.Procname.get_language proc_name in
  Language.curr_language := procedure_language ;
  let get_procs_in_file proc_name =
    let source_file =
      match Attributes.load proc_name with
      | Some {ProcAttributes.translation_unit} ->
          Some translation_unit
      | None ->
          None
    in
    Option.value_map source_file ~default:[] ~f:SourceFiles.proc_names_of_source
  in
  let tenv = Exe_env.get_tenv exe_env proc_name in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold ~init:summary
    ~f:(fun summary {name; dynamic_dispatch; language; callback} ->
      if Language.equal language procedure_language && (dynamic_dispatch || not is_specialized)
      then (
        PerfEvent.(
          log (fun logger ->
              log_begin_event logger ~name ~categories:["backend"]
                ~arguments:[("proc", `String (Typ.Procname.to_string proc_name))]
                () )) ;
        let summary =
          callback {get_procs_in_file; tenv; integer_type_widths; summary; proc_desc; exe_env}
        in
        PerfEvent.(log (fun logger -> log_end_event logger ())) ;
        summary )
      else summary )
    !procedure_callbacks


(** Invoke all registered cluster callbacks on a cluster of procedures. *)
let iterate_cluster_callbacks all_procs exe_env source_file =
  if !cluster_callbacks <> [] then
    let procedures = List.filter_map ~f:(get_procedure_definition exe_env) all_procs in
    let environment = {procedures; source_file; exe_env} in
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


let dump_duplicate_procs source_file procs =
  let duplicate_procs =
    List.filter_map procs ~f:(fun pname ->
        match Attributes.load pname with
        | Some
            { is_defined=
                true
                (* likely not needed: if [pname] is part of [procs] then it *is* defined, so we
                   expect the attribute to be defined too *)
            ; translation_unit
            ; loc }
          when (* defined in another file *)
               (not (SourceFile.equal source_file translation_unit))
               && (* really defined in that file and not in an include *)
                  SourceFile.equal translation_unit loc.file ->
            Some (pname, translation_unit)
        | _ ->
            None )
  in
  let output_to_file duplicate_procs =
    Out_channel.with_file (Config.results_dir ^/ Config.duplicates_filename)
      ~append:true ~perm:0o666 ~f:(fun outc ->
        let fmt = F.formatter_of_out_channel outc in
        List.iter duplicate_procs ~f:(fun (pname, source_captured) ->
            F.fprintf fmt "DUPLICATE_SYMBOLS source:%a source_captured:%a pname:%a@\n"
              SourceFile.pp source_file SourceFile.pp source_captured Typ.Procname.pp pname ) ;
        F.pp_print_flush fmt () )
  in
  if not (List.is_empty duplicate_procs) then output_to_file duplicate_procs


let create_perf_stats_report source_file =
  PerfStats.register_report PerfStats.TimeAndMemory (PerfStats.Backend source_file) ;
  PerfStats.get_reporter (PerfStats.Backend source_file) ()


(** Invoke all procedure and cluster callbacks on a given environment. *)
let analyze_file (exe_env : Exe_env.t) source_file =
  let saved_language = !Language.curr_language in
  let analyze_ondemand summary proc_desc = iterate_procedure_callbacks exe_env summary proc_desc in
  (* Invoke procedure callbacks using on-demand analysis schedulling *)
  Ondemand.set_callbacks {Ondemand.exe_env; analyze_ondemand} ;
  let procs_to_analyze =
    (* analyze all the currently defined procedures *)
    SourceFiles.proc_names_of_source source_file
  in
  if Config.dump_duplicate_symbols then dump_duplicate_procs source_file procs_to_analyze ;
  let analyze_proc_name pname = ignore (Ondemand.analyze_proc_name pname : Summary.t option) in
  List.iter ~f:analyze_proc_name procs_to_analyze ;
  (* Invoke cluster callbacks. *)
  iterate_cluster_callbacks procs_to_analyze exe_env source_file ;
  (* Perf logging needs to remain at the end - after analysis work is complete *)
  create_perf_stats_report source_file ;
  (* Unregister callbacks *)
  Ondemand.unset_callbacks () ;
  Language.curr_language := saved_language
