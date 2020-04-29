(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** convencience functions on top of {!Reporting} *)

type log_t = Reporting.log_t

val log_error : Summary.t -> loc:Location.t -> log_t
(** Add an error to the given summary. *)

val log_warning : Summary.t -> loc:Location.t -> log_t
(** Add a warning to the given summary. *)

val log_error_using_state : Summary.t -> exn -> unit
(** Add an error to the given summary using biabduction state (DO NOT USE ELSEWHERE). *)

val log_issue_deprecated_using_state :
     Exceptions.severity
  -> Procname.t
  -> ?node:Procdesc.Node.t
  -> ?loc:Location.t
  -> ?ltr:Errlog.loc_trace
  -> exn
  -> unit
(** Report an issue in the given procedure using biabduction state. DEPRECATED as it can create race
    conditions between checkers. Use log_error_using_state instead *)
