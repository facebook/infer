(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type property_name = string [@@deriving compare, hash, sexp]

type register_name = string

type variable_name = string

type constant = Exp.t

type value = Constant of constant | Register of register_name | Binding of variable_name

type binop = (* all return booleans *)
  | OpEq | OpNe | OpGe | OpGt | OpLe | OpLt

type predicate = Binop of binop * value * value | Value of (* bool *) value

type condition = predicate list (* conjunction *)

type assignment = register_name * variable_name

(** a regular expression *)
type procedure_name_pattern = string

(* TODO(rgrigore): Check that variable names don't repeat.  *)
(* TODO(rgrigore): Check that registers are written at most once. *)
type label =
  { arguments: variable_name list option
  ; condition: condition
  ; action: assignment list
  ; procedure_name: procedure_name_pattern }

type vertex = string [@@deriving compare, hash, sexp]

type transition = {source: vertex; target: vertex; label: label option}

(* TODO(rgrigore): Check that registers are read only after being initialized *)
type t =
  { name: property_name
  ; message: string option
  ; prefixes: string list
  ; nondet: string list
  ; transitions: transition list }
