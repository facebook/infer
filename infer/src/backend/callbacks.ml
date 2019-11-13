(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module to register and invoke callbacks *)

type proc_callback_args =
  {get_procs_in_file: Typ.Procname.t -> Typ.Procname.t list; summary: Summary.t; exe_env: Exe_env.t}

type proc_callback_t = proc_callback_args -> Summary.t

type cluster_callback_args =
  {procedures: (Tenv.t * Summary.t) list; source_file: SourceFile.t; exe_env: Exe_env.t}

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
         (tenv, Summary.OnDisk.reset proc_desc) )


(** Invoke all registered procedure callbacks on the given procedure. *)
let iterate_procedure_callbacks exe_env summary =
  let proc_desc = Summary.get_proc_desc summary in
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
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold ~init:summary
    ~f:(fun summary {name; dynamic_dispatch; language; callback} ->
      if Language.equal language procedure_language && (dynamic_dispatch || not is_specialized) then (
        PerfEvent.(
          log (fun logger ->
              log_begin_event logger ~name ~categories:["backend"]
                ~arguments:[("proc", `String (Typ.Procname.to_string proc_name))]
                () )) ;
        let summary = callback {get_procs_in_file; summary; exe_env} in
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
      | (_, summary) :: _ ->
          Language.equal language (Typ.Procname.get_language (Summary.get_proc_name summary))
      | _ ->
          true
    in
    List.iter
      ~f:(fun {language; callback} ->
        if language_matches language then (
          Language.curr_language := language ;
          callback environment ) )
      !cluster_callbacks
