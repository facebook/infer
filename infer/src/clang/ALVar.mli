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

type formula_id = Formula_id of string

type alexp =
  | Const of string
  | Regexp of string
  | Var of string
  | FId of formula_id

type t = alexp

val equal : t -> t -> bool

val formula_id_to_string : formula_id -> string

val alexp_to_string : t -> string

val keyword_to_string : keyword -> string

val is_report_when_keyword : keyword -> bool

val is_message_keyword : keyword -> bool

val is_suggestion_keyword : keyword -> bool

val is_severity_keyword : keyword -> bool

val is_mode_keyword : keyword -> bool

module FormulaIdMap : Caml.Map.S with type key = formula_id
