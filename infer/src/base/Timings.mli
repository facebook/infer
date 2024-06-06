(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Data structure to collect timing percentile informations on all {!Timeable.t} elements *)

type t

val init : t

val add : Timeable.t -> float -> t -> t
(** register a new timing measurement *)

val merge : t -> t -> t

val pp : F.formatter -> t -> unit

type serialized

val serialize : t -> serialized

val deserialize : serialized -> t

val to_scuba : t -> LogEntry.t list
