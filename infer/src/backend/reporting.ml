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
  ?loc:Location.t -> ?node_id:int * Caml.Digest.t -> ?session:int -> ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> ?access:string -> exn -> unit

type log_issue_from_errlog = Errlog.t -> log_t

let log_issue_from_errlog procname ?clang_method_kind err_kind err_log ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access exn =
  let clang_method_kind =
    Option.map clang_method_kind ~f:ProcAttributes.string_of_clang_method_kind
  in
  let issue_type = (Exceptions.recognize_exception exn).name in
  if not Config.filtering (* no-filtering takes priority *) || issue_type.IssueType.enabled then
    let loc = match loc with None -> State.get_loc () | Some loc -> loc in
    let node_id =
      match node_id with
      | None ->
          (State.get_node_id_key () :> int * Caml.Digest.t)
      | Some node_id ->
          node_id
    in
    let session =
      match session with None -> (State.get_session () :> int) | Some session -> session
    in
    let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
    Errlog.log_issue procname ?clang_method_kind err_kind err_log loc node_id session ltr
      ?linters_def_file ?doc_url ?access exn


let log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file ?doc_url
    ?access exn =
  let attrs = Specs.get_attributes summary in
  let procname = attrs.proc_name in
  let clang_method_kind = attrs.clang_method_kind in
  let is_java_generated_method =
    match procname with
    | Typ.Procname.Java java_pname ->
        Typ.Procname.Java.is_generated java_pname
    | _ ->
        false
  in
  let should_suppress_lint =
    Language.curr_language_is Java
    && Annotations.ia_is_suppress_lint
         (fst (Specs.get_attributes summary).ProcAttributes.method_annotation)
  in
  if should_suppress_lint || is_java_generated_method then () (* Skip the reporting *)
  else
    let err_log = Specs.get_err_log summary in
    log_issue_from_errlog procname ~clang_method_kind err_kind err_log ?loc ?node_id ?session ?ltr
      ?linters_def_file ?doc_url ?access exn


let log_issue_deprecated ?(store_summary= false) err_kind proc_name ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access exn =
  match Specs.get_summary proc_name with
  | Some summary ->
      log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file
        ?doc_url ?access exn ;
      if store_summary then
        (* TODO (#16348004): This is currently needed as ThreadSafety works as a cluster checker *)
        Specs.store_summary summary
  | None ->
      L.(die InternalError)
        "Trying to report error on procedure %a, but cannot because no summary exists for this \
         procedure. Did you mean to log the error on the caller of %a instead?" Typ.Procname.pp
        proc_name Typ.Procname.pp proc_name


let log_error = log_issue_from_summary Exceptions.Kerror

let log_warning = log_issue_from_summary Exceptions.Kwarning

let log_error_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kerror


let log_warning_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kwarning


let log_info_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kinfo
