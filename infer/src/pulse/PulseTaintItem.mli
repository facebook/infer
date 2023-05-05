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
  | Field of {name: string; origin: origin}
[@@deriving compare, equal]

type t =
  { kinds: TaintConfig.Kind.t list
  ; proc_name: Procname.t
  ; origin: origin
  ; block_passed_to: Procname.t option }
[@@deriving compare, equal]

val pp : F.formatter -> t -> unit
