(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

type keyword =
  | Report_when
  | Message
  | Suggestion
  | Severity
  | Mode

type formula_id = Formula_id of string[@@deriving compare]

type alexp =
  | Const of string
  | Regexp of string
  | Var of string
  | FId of formula_id
[@@deriving compare]

type t = alexp[@@deriving compare]

let equal = [%compare.equal : t]

let formula_id_to_string fid =
  match fid with
  | Formula_id s -> s

let alexp_to_string e =
  match e with
  | Const s
  | Regexp s
  | Var s
  | FId (Formula_id s) -> s

let keyword_to_string k =
  match k with
  | Report_when -> "report_when"
  | Message -> "message"
  | Suggestion -> "suggestion"
  | Severity -> "severity"
  | Mode -> "mode"

let is_report_when_keyword k =
  match k with
  | Report_when -> true
  | _ -> false

let is_message_keyword k =
  match k with
  | Message -> true
  | _ -> false

let is_suggestion_keyword k =
  match k with
  | Suggestion -> true
  | _ -> false

let is_severity_keyword k =
  match k with
  | Severity -> true
  | _ -> false

let is_mode_keyword k =
  match k with
  | Mode -> true
  | _ -> false


module FormulaIdMap = Caml.Map.Make (
  struct
    type t = formula_id[@@deriving compare]
  end)
