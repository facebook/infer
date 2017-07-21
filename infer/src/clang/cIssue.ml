(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type mode = On | Off

type issue_desc =
  { id: string
  ; (* issue id *)
  description: string
  ; (* Description in the error message *)
  doc_url: string option
  ; mode: mode
  ; name: string option
  ; (* issue name, if no name is given name will be a readable version of id,
                           by removing underscores and capitalizing first letters of words *)
  loc: Location.t
  ; (* location in the code *)
  severity: Exceptions.err_kind
  ; suggestion: string option
  (* an optional suggestion or correction *) }

let string_of_mode m = match m with On -> "On" | Off -> "Off"

let pp_issue fmt issue =
  Format.fprintf fmt "{@\n   Id = %s@\n" issue.id ;
  Format.fprintf fmt "{  Name = %s@\n" (Option.value ~default:"" issue.name) ;
  Format.fprintf fmt "   Severity = %s@\n" (Exceptions.err_kind_string issue.severity) ;
  Format.fprintf fmt "   Mode = %s@\n" (string_of_mode issue.mode) ;
  Format.fprintf fmt "   Description = %s@\n" issue.description ;
  Format.fprintf fmt "   Suggestion = %s@\n" (Option.value ~default:"" issue.suggestion) ;
  Format.fprintf fmt "   Docs URL = %s@\n" (Option.value ~default:"" issue.doc_url) ;
  Format.fprintf fmt "   Loc = %s@\n" (Location.to_string issue.loc) ;
  Format.fprintf fmt "}@\n"

let should_run_check mode =
  match mode with
  | On
   -> true
  | Off
   -> Config.debug_mode || Config.debug_exceptions || not Config.filtering
