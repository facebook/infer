(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type mode = On | Off

type 'issue_type issue_desc0 =
  { issue_type: 'issue_type
  ; (* issue type *)
    description: string
  ; (* Description in the error message *)
    mode: mode
  ; loc: Location.t
  ; (* location in the code *)
    severity: Exceptions.severity
  ; suggestion: string option
  (* an optional suggestion or correction *) }

type issue_desc = IssueType.t issue_desc0

val pp_issue : Format.formatter -> issue_desc -> unit

val should_run_check : mode -> bool
