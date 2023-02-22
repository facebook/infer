(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type proc_callback_args = {summary: Summary.t; exe_env: Exe_env.t; proc_desc: Procdesc.t}

type proc_callback_t = proc_callback_args -> Summary.t

type file_callback_args =
  {procedures: Procname.t list; source_file: SourceFile.t; exe_env: Exe_env.t}

type file_callback_t = file_callback_args -> IssueLog.t

type procedure_callback =
  {checker: Checker.t; dynamic_dispatch: bool; language: Language.t; callback: proc_callback_t}

type file_callback = {checker: Checker.t; language: Language.t; callback: file_callback_t}

let procedure_callbacks_rev = ref []

let file_callbacks_rev = ref []

let register_procedure_callback checker ?(dynamic_dispatch = false) language
    (callback : proc_callback_t) =
  procedure_callbacks_rev :=
    {checker; dynamic_dispatch; language; callback} :: !procedure_callbacks_rev


let register_file_callback checker language (callback : file_callback_t) =
  file_callbacks_rev := {checker; language; callback} :: !file_callbacks_rev


let iterate_procedure_callbacks exe_env ({Summary.proc_name} as summary) proc_desc =
  let procedure_language = Procname.get_language proc_name in
  Language.curr_language := procedure_language ;
  let is_specialized = Procdesc.is_specialized proc_desc in
  List.fold_right ~init:summary !procedure_callbacks_rev
    ~f:(fun {checker; dynamic_dispatch; language; callback} summary ->
      if Language.equal language procedure_language && (dynamic_dispatch || not is_specialized) then (
        PerfEvent.(
          log (fun logger ->
              log_begin_event logger ~name:(Checker.get_id checker) ~categories:["backend"]
                ~arguments:[("proc", `String (Procname.to_string proc_name))]
                () )) ;
        let summary =
          Timer.time (Checker checker)
            ~f:(fun () -> callback {summary; exe_env; proc_desc})
            ~on_timeout:(fun span ->
              L.debug Analysis Quiet "TIMEOUT in %s after %fs of CPU time analyzing %a:%a@\n"
                (Checker.get_id checker) span SourceFile.pp
                (Procdesc.get_attributes proc_desc).translation_unit Procname.pp proc_name ;
              summary )
        in
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
          if not (IssueLog.is_stored ~checker ~file:source_file) then
            let issue_log = callback environment in
            IssueLog.store ~checker ~file:source_file issue_log ) )
