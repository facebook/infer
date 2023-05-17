(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module TaintConfig = PulseTaintConfig

type origin =
  | Argument of {index: int}
  | ReturnValue
  | Allocation of {typ: string}
  | SetField
  | FieldOfValue of {name: string; origin: origin}
[@@deriving compare, equal]

type value =
  | TaintBlockPassedTo of Procname.t
  | TaintField of Fieldname.t
  | TaintProcedure of Procname.t
[@@deriving compare, equal]

type t = {kinds: TaintConfig.Kind.t list; value: value; origin: origin} [@@deriving compare, equal]

val pp_value : F.formatter -> value -> unit

val pp_value_plain : F.formatter -> value -> unit

val pp : F.formatter -> t -> unit
