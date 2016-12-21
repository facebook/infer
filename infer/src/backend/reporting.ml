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
  ?loc: Location.t ->
  ?node_id: (int * int) ->
  ?session: int ->
  ?ltr: Errlog.loc_trace ->
  exn ->
  unit

type log_issue = Procname.t -> log_t

type log_issue_from_errlog = Errlog.t -> log_t

let log_issue_from_errlog err_kind err_log ?loc ?node_id ?session ?ltr exn =
  let loc = match loc with
    | None -> State.get_loc ()
    | Some loc -> loc in
  let node_id = match node_id with
    | None -> (State.get_node_id_key () :> int * int)
    | Some node_id -> node_id in
  let session = match session with
    | None -> (State.get_session () :> int)
    | Some session -> session in
  let ltr = match ltr with
    | None -> State.get_loc_trace ()
    | Some ltr -> ltr in
  let err_name =  match exn with
    | Exceptions.Frontend_warning (err_name, _, _) -> err_name
    | _ -> let err_name, _, _, _, _, _, _ =  Exceptions.recognize_exception exn in
        (Localise.to_string err_name) in
  if (Inferconfig.is_checker_enabled err_name) then
    Errlog.log_issue err_kind err_log loc node_id session ltr exn


let log_issue
    err_kind
    proc_name
    ?loc
    ?node_id
    ?session
    ?ltr
    exn =
  let should_suppress_lint summary =
    !Config.curr_language = Config.Java &&
    let annotated_signature =
      Annotations.get_annotated_signature summary.Specs.attributes in
    let ret_annotation, _ = annotated_signature.Annotations.ret in
    Annotations.ia_is_suppress_lint ret_annotation in
  match Specs.get_summary proc_name with
  | Some summary when should_suppress_lint summary -> ()
  | Some summary ->
      let err_log = summary.Specs.attributes.ProcAttributes.err_log in
      log_issue_from_errlog err_kind err_log ?loc ?node_id ?session ?ltr exn
  | None ->
      failwithf
        "Trying to report error on procedure %a, but cannot because no summary exists for this \
         procedure. Did you mean to log the error on the caller of %a instead?"
        Procname.pp proc_name
        Procname.pp proc_name

let log_error = log_issue Exceptions.Kerror
let log_warning = log_issue Exceptions.Kwarning
let log_info = log_issue Exceptions.Kinfo

let log_error_from_errlog = log_issue_from_errlog Exceptions.Kerror
let log_warning_from_errlog = log_issue_from_errlog Exceptions.Kwarning
let log_info_from_errlog = log_issue_from_errlog Exceptions.Kinfo
