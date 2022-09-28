(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type proc_callback_args = {summary: Summary.t; exe_env: Exe_env.t}

type proc_callback_t = proc_callback_args -> Summary.t

type file_callback_args =
  {procedures: Procname.t list; source_file: SourceFile.t; exe_env: Exe_env.t}

type file_callback_t = file_callback_args -> IssueLog.t

type procedure_callback =
  {checker_name: string; dynamic_dispatch: bool; language: Language.t; callback: proc_callback_t}

type file_callback = {checker: Checker.t; language: Language.t; callback: file_callback_t}

let procedure_callbacks_rev = ref []

let file_callbacks_rev = ref []

let register_procedure_callback ~checker_name ?(dynamic_dispatch = false) language
    (callback : proc_callback_t) =
  procedure_callbacks_rev :=
    {checker_name; dynamic_dispatch; language; callback} :: !procedure_callbacks_rev


let register_file_callback ~checker language (callback : file_callback_t) =
  file_callbacks_rev := {checker; language; callback} :: !file_callbacks_rev


let iterate_procedure_callbacks exe_env summary =
  let proc_desc = Summary.get_proc_desc summary in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let procedure_language = Procname.get_language proc_name in
  Language.curr_language := procedure_language ;
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold_right ~init:summary !procedure_callbacks_rev
    ~f:(fun {checker_name; dynamic_dispatch; language; callback} summary ->
      if Language.equal language procedure_language && (dynamic_dispatch || not is_specialized) then (
        PerfEvent.(
          log (fun logger ->
              log_begin_event logger ~name:checker_name ~categories:["backend"]
                ~arguments:[("proc", `String (Procname.to_string proc_name))]
                () )) ;
        let summary = callback {summary; exe_env} in
        PerfEvent.(log (fun logger -> log_end_event logger ())) ;
        summary )
      else summary )


let iterate_file_callbacks_and_store_issues procedures exe_env source_file =
  if not (List.is_empty !file_callbacks_rev) then
    let environment = {procedures; source_file; exe_env} in
    let language_matches language =
      match procedures with
      | procname :: _ ->
          Language.equal language (Procname.get_language procname)
      | _ ->
          true
    in
    List.iter (List.rev !file_callbacks_rev) ~f:(fun {checker; language; callback} ->
        if language_matches language then (
          Language.curr_language := language ;
          let issue_log = callback environment in
          IssueLog.store ~checker ~file:source_file issue_log ) )
