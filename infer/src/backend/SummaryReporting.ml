(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type log_t = Reporting.log_t

let log_error summary ~loc ?ltr ?extras issue_type error_message =
  let attrs = Summary.get_attributes summary in
  let err_log = Summary.get_err_log summary in
  Reporting.log_error attrs err_log ~loc ?ltr ?extras issue_type error_message


let log_warning summary ~loc ?ltr ?extras issue_type error_message =
  let attrs = Summary.get_attributes summary in
  let err_log = Summary.get_err_log summary in
  Reporting.log_warning attrs err_log ~loc ?ltr ?extras issue_type error_message


let log_error_using_state summary exn =
  BiabductionReporting.log_error_using_state (Summary.get_proc_desc summary)
    (Summary.get_err_log summary) exn


let log_issue_deprecated_using_state severity proc_name ?node ?loc ?ltr exn =
  if !BiabductionConfig.footprint then
    match Summary.OnDisk.get proc_name with
    | Some summary ->
        let proc_attributes = Summary.get_attributes summary in
        let err_log = Summary.get_err_log summary in
        BiabductionReporting.log_issue_deprecated_using_state proc_attributes err_log severity ?node
          ?loc ?ltr exn
    | None ->
        L.(die InternalError)
          "Trying to report error on procedure %a, but cannot because no summary exists for this \
           procedure. Did you mean to log the error on the caller of %a instead?"
          Procname.pp proc_name Procname.pp proc_name
