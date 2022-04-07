(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type origin = Argument of {index: int} | ReturnValue [@@deriving compare, equal]

let pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "passed as argument #%d to" index
  | ReturnValue ->
      F.fprintf fmt "value returned from"


type t = {proc_name: Procname.t; origin: origin} [@@deriving compare, equal]

let pp fmt {proc_name; origin} = F.fprintf fmt "%a %a" pp_origin origin Procname.pp proc_name
