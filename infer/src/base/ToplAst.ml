(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type property_name = string [@@deriving compare, hash, sexp, show]

type register_name = string [@@deriving compare, equal, show]

type variable_name = string [@@deriving show]

type field_name = string [@@deriving show]

type class_name = string [@@deriving show]

type constant = LiteralInt of int | LiteralStr of string [@@deriving show]

type value =
  | Constant of constant
  | Register of register_name
  | Binding of variable_name
  | FieldAccess of {value: value; class_name: class_name; field_name: field_name}
[@@deriving show]

type binop = (* all return booleans *)
  | LeadsTo | OpEq | OpNe | OpGe | OpGt | OpLe | OpLt
[@@deriving show]

type predicate = Binop of binop * value * value | Value of (* bool *) value [@@deriving show]

type condition = predicate list (* conjunction *) [@@deriving show]

type assignment = register_name * variable_name [@@deriving show]

type regex = {re_text: string; re: (Str.regexp[@show.opaque])} [@@deriving show]

let mk_regex re_text = {re_text; re= Str.regexp re_text}

type call_pattern =
  {procedure_name_regex: regex; type_regexes: regex option list option (* [None] means anything *)}
[@@deriving show]

type label_pattern = ArrayWritePattern | CallPattern of call_pattern [@@deriving show]

(* TODO(rgrigore): Check that variable names don't repeat.  *)
(* TODO(rgrigore): Check that registers are written at most once. *)
(* INV: if [pattern] is ArrayWritePattern, then [arguments] has length 2.
    (Now ensured by parser. TODO: refactor to ensure with types.) *)
type label =
  { arguments: variable_name list option
  ; condition: condition
  ; action: assignment list
  ; pattern: label_pattern }
[@@deriving show]

type vertex = string [@@deriving compare, hash, sexp, show]

type transition = {source: vertex; target: vertex; label: label option} [@@deriving show]

(* TODO(rgrigore): Check that registers are read only after being initialized *)
type t =
  {name: property_name; message: string option; prefixes: string list; transitions: transition list}
[@@deriving show]
