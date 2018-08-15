(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Type of functions to report issues to the error_log in a spec. *)

type log_t =
  ?loc:Location.t -> ?node_id:int * Caml.Digest.t -> ?session:int -> ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> ?access:string -> ?extras:Jsonbug_t.extra
  -> exn -> unit

type log_issue_from_errlog = Errlog.t -> log_t

val log_issue_deprecated : Exceptions.severity -> Typ.Procname.t -> log_t
(** Report an issue in the given procedure.
    DEPRECATED as it can create race conditions between checkers.
    Use log_error/warning instead *)

val log_issue_from_errlog :
  Typ.Procname.t -> ?clang_method_kind:ProcAttributes.clang_method_kind -> Exceptions.severity
  -> log_issue_from_errlog
(** Report an issue of a given kind  in the given error log. *)

val log_error : Summary.t -> log_t
(** Add an error to the given summary. *)

val log_warning : Summary.t -> log_t
(** Add an warning to the given summary. *)

val log_issue_external :
  Typ.Procname.t -> ?clang_method_kind:ProcAttributes.clang_method_kind -> Exceptions.severity
  -> log_t
(** Log an issue to the error log in [IssueLog] associated with the given procname. *)

val is_suppressed :
  ?field_name:Typ.Fieldname.t option -> Tenv.t -> Procdesc.t -> IssueType.t -> bool
(** should an issue report be suppressed due to a [@SuppressLint("issue")] annotation? *)
