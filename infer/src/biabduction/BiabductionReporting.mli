(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val log_issue_using_state : Procdesc.t -> Errlog.t -> exn -> unit
(** Add an issue to the given summary using biabduction state. *)

val log_issue_deprecated_using_state :
     Procdesc.t
  -> Errlog.t
  -> ?node:Procdesc.Node.t
  -> ?loc:Location.t
  -> ?ltr:Errlog.loc_trace
  -> exn
  -> unit
(** Report an issue in the given procedure using biabduction state. DEPRECATED as it can create race
    conditions between checkers. Use log_error_using_state instead *)
