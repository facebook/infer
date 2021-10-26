(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = private int [@@deriving compare, equal]

val t0 : t

val pp : F.formatter -> t -> unit

val incr : t -> t
