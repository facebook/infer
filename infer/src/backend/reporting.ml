(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils
module L = Logging

type log_issue =
  Procname.t ->
  ?loc: Location.t option ->
  ?node_id: (int * int) option ->
  ?session: int option ->
  ?ltr: Errlog.loc_trace option ->
  ?pre: Prop.normal Prop.t option ->
  exn ->
  unit

let log_issue
    err_kind
    proc_name
    ?(loc = None)
    ?(node_id = None)
    ?(session = None)
    ?(ltr = None)
    ?(pre = None)
    exn =
  match Specs.get_summary proc_name with
  | Some summary ->
      let err_log = summary.Specs.stats.Specs.err_log in
      let loc = match loc with
        | None -> State.get_loc ()
        | Some loc -> loc in
      let node_id = match node_id with
        | None -> State.get_node_id_key ()
        | Some node_id -> node_id in
      let session = match session with
        | None -> State.get_session ()
        | Some session -> session in
      let ltr = match ltr with
        | None -> State.get_loc_trace ()
        | Some ltr -> ltr in
      Errlog.log_issue err_kind err_log loc node_id session ltr pre exn
  | None -> ()

let log_error = log_issue Exceptions.Kerror
let log_warning = log_issue Exceptions.Kwarning
let log_info = log_issue Exceptions.Kinfo
