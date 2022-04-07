(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type origin = Argument of {index: int} | ReturnValue

type t = {proc_name: Procname.t; origin: origin} [@@deriving compare, equal]

val pp : F.formatter -> t -> unit
