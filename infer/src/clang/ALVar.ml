(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

type keyword = Doc_url | Message | Mode | Name | Report_when | Severity | Suggestion

type formula_id = Formula_id of string [@@deriving compare]

type alexp =
  | Const of string
  | Regexp of string
  | Var of string
  | FId of formula_id
  [@@deriving compare]

type t = alexp [@@deriving compare]

let equal = [%compare.equal : t]

let formula_id_to_string fid =
  match fid
  with Formula_id s -> s

let alexp_to_string e =
  match e
  with Const s | Regexp s | Var s | FId Formula_id s -> s

let keyword_to_string k =
  match k with
  | Doc_url
   -> "doc_url"
  | Message
   -> "message"
  | Mode
   -> "mode"
  | Name
   -> "name_hum_readable"
  | Report_when
   -> "report_when"
  | Severity
   -> "severity"
  | Suggestion
   -> "suggestion"

let is_report_when_keyword k = match k with Report_when -> true | _ -> false

let is_message_keyword k = match k with Message -> true | _ -> false

let is_suggestion_keyword k = match k with Suggestion -> true | _ -> false

let is_severity_keyword k = match k with Severity -> true | _ -> false

let is_mode_keyword k = match k with Mode -> true | _ -> false

let is_doc_url_keyword k = match k with Doc_url -> true | _ -> false

let is_name_keyword k = match k with Name -> true | _ -> false

(* true if and only if a substring of container matches the regular
   expression defined by contained
*)
let str_match_regex container re =
  let rexp = Str.regexp re in
  try Str.search_forward rexp container 0 >= 0
  with Not_found -> false

let compare_str_with_alexp s ae =
  match ae with
  | Const s' | Var s'
   -> String.equal s s'
  | Regexp re
   -> str_match_regex s re
  | _
   -> L.(debug Linters Medium)
        "[WARNING]: ALVAR expression '%s' is not a constant/var or regexp@\n" (alexp_to_string ae) ;
      false

module FormulaIdMap = Caml.Map.Make (struct
  type t = formula_id [@@deriving compare]
end)

module VarMap = Caml.Map.Make (struct
  type t = string [@@deriving compare]
end)
