(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type mode = On | Off

val should_run_check : mode -> bool

type t =
  { issue_type: IssueType.t
  ; description: string  (** Description in the error message *)
  ; mode: mode
  ; loc: Location.t  (** location in the code *)
  ; severity: IssueType.severity
  ; suggestion: string option  (** an optional suggestion or correction *) }

val pp : Format.formatter -> t -> unit
