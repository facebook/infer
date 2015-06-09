(*
* Copyright (c) 2015 - Facebook.
* All rights reserved.
*)

(** Type of functions to report issues to the error_log in a spec. *)
type log_issue =
  Procname.t ->
  ?loc: Sil.location option ->
  ?node_id: (int * int) option ->
  ?session: int option ->
  ?ltr: Errlog.loc_trace option ->
  ?pre: Prop.normal Prop.t option ->
  exn ->
  unit

(** Report an error in the given procedure. *)
val log_error : log_issue

(** Report a warning in the given procedure. *)
val log_warning : log_issue

(** Report an info in the given procedure. *)
val log_info : log_issue
