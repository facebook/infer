(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Type of functions to report issues to the error_log in a spec. *)

type log_t = ?ltr:Errlog.loc_trace -> ?extras:Jsonbug_t.extra -> IssueType.t -> string -> unit

val log_issue_deprecated_using_state :
     Exceptions.severity
  -> Typ.Procname.t
  -> ?node:Procdesc.Node.t
  -> ?loc:Location.t
  -> ?ltr:Errlog.loc_trace
  -> exn
  -> unit
(** Report an issue in the given procedure using biabduction state (DO NOT USE ELSEWHERE).
    DEPRECATED as it can create race conditions between checkers.
    Use log_error/warning instead *)

val log_frontend_issue :
     Typ.Procname.t
  -> Exceptions.severity
  -> Errlog.t
  -> loc:Location.t
  -> node_key:Procdesc.NodeKey.t
  -> ltr:Errlog.loc_trace
  -> exn
  -> unit
(** Report a frontend issue of a given kind in the given error log. *)

val log_error : Summary.t -> loc:Location.t -> log_t
(** Add an error to the given summary. *)

val log_warning : Summary.t -> loc:Location.t -> log_t
(** Add an warning to the given summary. *)

val log_error_using_state : Summary.t -> exn -> unit
(** Add an error to the given summary using biabduction state (DO NOT USE ELSEWHERE). *)

val log_issue_external :
     Typ.Procname.t
  -> Exceptions.severity
  -> loc:Location.t
  -> ltr:Errlog.loc_trace
  -> ?access:string
  -> IssueType.t
  -> string
  -> unit
(** Log an issue to the error log in [IssueLog] associated with the given procname. *)

val is_suppressed :
  ?field_name:Typ.Fieldname.t option -> Tenv.t -> Procdesc.t -> IssueType.t -> bool
(** should an issue report be suppressed due to a [@SuppressLint("issue")] annotation? *)
