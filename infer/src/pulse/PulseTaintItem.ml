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

let rec pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "value passed as argument `#%d` to" index
  | ReturnValue ->
      F.fprintf fmt "value returned from"
  | Allocation {typ} ->
      F.fprintf fmt "allocation of type `%s` by" typ
  | Field {name; origin} ->
      F.fprintf fmt "field `%s` of %a" name pp_origin origin


type t =
  { kinds: TaintConfig.Kind.t list
  ; proc_name: Procname.t
  ; origin: origin
  ; block_passed_to: Procname.t option }
[@@deriving compare, equal]

let pp fmt {kinds; proc_name; origin; block_passed_to} =
  let proc_name_s =
    match block_passed_to with
    | Some passed_to_proc_name ->
        F.asprintf "a block passed to `%a`" Procname.pp passed_to_proc_name
    | None ->
        F.asprintf "`%a`" Procname.pp proc_name
  in
  F.fprintf fmt "%a %s with kind%s %a" pp_origin origin proc_name_s
    (match kinds with [_] -> "" | _ -> "s")
    (Pp.seq ~sep:"," TaintConfig.Kind.pp)
    kinds
