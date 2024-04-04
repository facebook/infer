(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module F = Format

type t = {heap: PulseBaseMemory.t; stack: PulseBaseStack.t; attrs: PulseBaseAddressAttributes.t}
[@@deriving compare, equal, yojson_of]

type cell = PulseBaseMemory.Edges.t * Attributes.t

val empty : t

val pp : F.formatter -> t -> unit

val subst_var : for_summary:bool -> AbstractValue.t * AbstractValue.t -> t -> t SatUnsat.t
