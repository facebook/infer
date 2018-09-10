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

let string_of_mode m = match m with On -> "On" | Off -> "Off"

let pp_issue fmt issue =
  Format.fprintf fmt "{@\n   Id = %s@\n" issue.issue_type.IssueType.unique_id ;
  Format.fprintf fmt "{  Name = %s@\n" issue.issue_type.IssueType.hum ;
  Format.fprintf fmt "   Severity = %s@\n" (Exceptions.severity_string issue.severity) ;
  Format.fprintf fmt "   Mode = %s@\n" (string_of_mode issue.mode) ;
  Format.fprintf fmt "   Description = %s@\n" issue.description ;
  Format.fprintf fmt "   Suggestion = %s@\n" (Option.value ~default:"" issue.suggestion) ;
  Format.fprintf fmt "   Docs URL = %s@\n"
    (Option.value ~default:"" issue.issue_type.IssueType.doc_url) ;
  Format.fprintf fmt "   Loc = %s@\n" (Location.to_string issue.loc) ;
  Format.fprintf fmt "}@\n"


let should_run_check mode =
  match mode with
  | On ->
      true
  | Off ->
      Config.debug_mode || Config.debug_exceptions || not Config.filtering
