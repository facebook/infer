(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Type of functions to report issues to the error_log in a spec. *)

type log_t =
  ?loc: Location.t ->
  ?node_id: (int * int) ->
  ?session: int ->
  ?ltr: Errlog.loc_trace ->
  exn ->
  unit

type log_issue = Typ.Procname.t -> log_t

type log_issue_from_errlog = Errlog.t -> log_t

(** Report an error in the given procedure. *)
val log_error : log_issue

(** Report a warning in the given procedure. *)
val log_warning : log_issue

(** Report an info in the given procedure. *)
val log_info : log_issue

(** Report an issue of a given kind  in the given error log. *)
val log_issue_from_errlog : Exceptions.err_kind -> log_issue_from_errlog

(** Report an error in the given error log. *)
val log_error_from_errlog : log_issue_from_errlog

(** Report a warning in the given error log. *)
val log_warning_from_errlog : log_issue_from_errlog

(** Report an info in the given error log. *)
val log_info_from_errlog : log_issue_from_errlog
