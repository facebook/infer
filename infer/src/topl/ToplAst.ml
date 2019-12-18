(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type property_name = string [@@deriving compare, hash, sexp]

type register_name = string

type constant = Exp.t

type value_pattern = Ignore | SaveInRegister of register_name | EqualToRegister of register_name

type value = Constant of constant | Register of register_name | Binding of register_name

(* refers to the corresponding SaveInRegister, from the same label *)

type binop = (* all return booleans *)
  | OpEq | OpNe | OpGe | OpGt | OpLe | OpLt

type predicate = Binop of binop * value * value | Value of (* bool *) value

type condition = predicate list (* conjunction *)

(** a regular expression *)
type procedure_name_pattern = string

(* Well-formedness condition (not currently checked): For all x, there are no repeated occurrences
of (SaveInRegister x). *)
type label =
  { arguments: value_pattern list option
  ; condition: condition
  ; procedure_name: procedure_name_pattern
  ; return: value_pattern }

type vertex = string [@@deriving compare, hash, sexp]

type transition = {source: vertex; target: vertex; label: label}

type t =
  { name: property_name
  ; message: string option
  ; prefixes: string list
  ; nondet: string list
  ; transitions: transition list }
