(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

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
