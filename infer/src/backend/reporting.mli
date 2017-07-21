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
  ?loc:Location.t -> ?node_id:int * int -> ?session:int -> ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> exn -> unit

type log_issue_from_errlog = Errlog.t -> log_t

val log_error_deprecated : ?store_summary:bool -> Typ.Procname.t -> log_t
(** Report an error in the given procedure.
    DEPRECATED as it can create race conditions between checkers.
    Use log_error instead *)

val log_warning_deprecated : ?store_summary:bool -> Typ.Procname.t -> log_t
(** Report a warning in the given procedure.
    DEPRECATED as it can create race conditions between checkers.
    Use log_warning instead *)

val log_info_deprecated : ?store_summary:bool -> Typ.Procname.t -> log_t
(** Report an info in the given procedure.
    DEPRECATED as it can create race conditions between checkers.
    Use log_info instead *)

val log_issue_from_errlog : Exceptions.err_kind -> log_issue_from_errlog
(** Report an issue of a given kind  in the given error log. *)

val log_error_from_errlog : log_issue_from_errlog
(** Report an error in the given error log. *)

val log_warning_from_errlog : log_issue_from_errlog
(** Report a warning in the given error log. *)

val log_info_from_errlog : log_issue_from_errlog
(** Report an info in the given error log. *)

val log_error : Specs.summary -> log_t
(** Add an error to the given summary. *)

val log_warning : Specs.summary -> log_t
(** Add an warning to the given summary. *)

val log_info : Specs.summary -> log_t
(** Add an info to the given summary. *)
