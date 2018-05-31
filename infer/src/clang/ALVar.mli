(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type keyword = Doc_url | Message | Mode | Name | Report_when | Severity | Suggestion

type formula_id = Formula_id of string [@@deriving compare]

(** a regexp and its cached compiled version *)
type cached_regexp = {string: string; regexp: Str.regexp Lazy.t} [@@deriving compare]

type alexp = Const of string | Regexp of cached_regexp | Var of string | FId of formula_id
[@@deriving compare]

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

val is_doc_url_keyword : keyword -> bool

val is_name_keyword : keyword -> bool

val str_match_forward : string -> Str.regexp -> bool

val compare_str_with_alexp : string -> alexp -> bool

module FormulaIdMap : Caml.Map.S with type key = formula_id

module VarMap : Caml.Map.S with type key = string
