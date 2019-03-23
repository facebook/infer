(*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type register_name = string

(** TODO: use Const.t *)
type constant = string

type value_pattern =
  | Ignore
  | SaveInRegister of register_name
  | EqualToRegister of register_name
  | EqualToConstant of constant

(** a regular expression *)
type procedure_name_pattern = string

type label =
  { return: value_pattern
  ; procedure_name: procedure_name_pattern
  ; arguments: value_pattern list option }

type vertex = string

type transition = {source: vertex; target: vertex; label: label}

type t = {name: string; message: string option; prefixes: string list; transitions: transition list}
