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

type issue_desc = {
  name : string; (* issue name *)
  severity : Exceptions.err_kind;
  mode : mode;
  description : string; (* Description in the error message *)
  suggestion : string option; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}

let string_of_mode m =
  match m with
  | On -> "On"
  | Off -> "Off"

let pp_issue fmt issue =
  Format.fprintf fmt "{\n   Name = %s\n" (issue.name);
  Format.fprintf fmt "   Severity = %s \n" (Exceptions.err_kind_string issue.severity);
  Format.fprintf fmt "   Mode = %s \n" (string_of_mode issue.mode);
  Format.fprintf fmt "   Descrption = %s \n" issue.description;
  (match issue.suggestion with
   | Some s -> Format.fprintf fmt "   Suggestion = %s\n" s
   | _ -> ());
  Format.fprintf fmt "   Loc = %s \n" (Location.to_string issue.loc);
  Format.fprintf fmt "}\n"

let should_run_check mode =
  match mode with
  | On -> true
  | Off -> Config.debug_mode || Config.debug_exceptions || not Config.filtering
