(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type iteration_info = {timestamp: Timestamp.t} [@@deriving compare, equal]

type loop_info = iteration_info list [@@deriving compare, equal]

type t = loop_info Procdesc.IdMap.t [@@deriving compare, equal]

val empty : t

val pp : F.formatter -> t -> unit
