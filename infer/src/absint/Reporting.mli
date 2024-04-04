(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Type of functions to report issues to the error_log in a spec. *)

type log_t =
     ?loc_instantiated:Location.t
  -> ?ltr:Errlog.loc_trace
  -> ?extras:Jsonbug_t.extra
  -> ?suggestion:string
  -> Checker.t
  -> IssueType.t
  -> string
  -> unit

val log_issue_from_summary :
     ?severity_override:IssueType.severity
  -> Procdesc.t
  -> Errlog.t
  -> node:Errlog.node
  -> session:int
  -> loc:Location.t
  -> ltr:Errlog.loc_trace
  -> ?extras:Jsonbug_t.extra
  -> Checker.t
  -> IssueToReport.t
  -> unit

val log_issue : Procdesc.t -> Errlog.t -> loc:Location.t -> log_t
(** Add an issue to the given error log. *)

val log_issue_external :
     Procname.t
  -> issue_log:IssueLog.t
  -> ?severity_override:IssueType.severity
  -> loc:Location.t
  -> ltr:Errlog.loc_trace
  -> ?access:string
  -> ?extras:Jsonbug_t.extra
  -> ?suggestion:string
  -> Checker.t
  -> IssueType.t
  -> string
  -> IssueLog.t
(** Log an issue to the error log in [IssueLog] associated with the given procname. *)

val is_suppressed :
  ?field_name:Fieldname.t option -> Tenv.t -> ProcAttributes.t -> IssueType.t -> bool
(** should an issue report be suppressed due to a [@SuppressLint("issue")] annotation? *)
