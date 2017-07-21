(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging

type log_t =
  ?loc:Location.t -> ?node_id:int * int -> ?session:int -> ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> exn -> unit

type log_issue_from_errlog = Errlog.t -> log_t

let log_issue_from_errlog err_kind err_log ?loc ?node_id ?session ?ltr ?linters_def_file ?doc_url
    exn =
  let loc = match loc with None -> State.get_loc () | Some loc -> loc in
  let node_id =
    match node_id with None -> (State.get_node_id_key () :> int * int) | Some node_id -> node_id
  in
  let session =
    match session with None -> (State.get_session () :> int) | Some session -> session
  in
  let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
  let err_name =
    match exn with
    | Exceptions.Frontend_warning ((err_name, _), _, _)
     -> err_name
    | _
     -> let err_name, _, _, _, _, _, _ = Exceptions.recognize_exception exn in
        Localise.to_issue_id err_name
  in
  if Inferconfig.is_checker_enabled err_name then
    Errlog.log_issue err_kind err_log loc node_id session ltr ?linters_def_file ?doc_url exn

let log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file ?doc_url
    exn =
  let is_generated_method = Typ.Procname.java_is_generated (Specs.get_proc_name summary) in
  let should_suppress_lint =
    Config.curr_language_is Config.Java
    && Annotations.ia_is_suppress_lint
         (fst summary.Specs.attributes.ProcAttributes.method_annotation)
  in
  if should_suppress_lint || is_generated_method then () (* Skip the reporting *)
  else
    let err_log = summary.Specs.attributes.ProcAttributes.err_log in
    log_issue_from_errlog err_kind err_log ?loc ?node_id ?session ?ltr ?linters_def_file ?doc_url
      exn

let log_issue_deprecated ?(store_summary= false) err_kind proc_name ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url exn =
  match Specs.get_summary proc_name with
  | Some summary
   -> log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file
        ?doc_url exn ;
      if store_summary then
        (* TODO (#16348004): This is currently needed as ThreadSafety works as a cluster checker *)
        Specs.store_summary summary
  | None
   -> failwithf
        "Trying to report error on procedure %a, but cannot because no summary exists for this procedure. Did you mean to log the error on the caller of %a instead?"
        Typ.Procname.pp proc_name Typ.Procname.pp proc_name

let log_error_from_errlog = log_issue_from_errlog Exceptions.Kerror

let log_warning_from_errlog = log_issue_from_errlog Exceptions.Kwarning

let log_info_from_errlog = log_issue_from_errlog Exceptions.Kinfo

let log_error = log_issue_from_summary Exceptions.Kerror

let log_warning = log_issue_from_summary Exceptions.Kwarning

let log_info = log_issue_from_summary Exceptions.Kwarning

let log_error_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kerror

let log_warning_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kwarning

let log_info_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kinfo
