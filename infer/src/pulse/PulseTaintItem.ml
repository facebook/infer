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
  | GetField
  | SetField
  | FieldOfValue of {name: string; origin: origin}
[@@deriving compare, equal]

let rec pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "value passed as argument `#%d` to" index
  | ReturnValue ->
      F.fprintf fmt "value returned from"
  | Allocation {typ} ->
      F.fprintf fmt "allocation of type `%s` by" typ
  | FieldOfValue {name; origin} ->
      F.fprintf fmt "field `%s` of %a" name pp_origin origin
  | GetField ->
      F.fprintf fmt "the field"
  | SetField ->
      F.fprintf fmt "is stored in the field"


type value =
  | TaintBlockPassedTo of Procname.t
  | TaintField of Fieldname.t
  | TaintProcedure of Procname.t
[@@deriving compare, equal]

let pp_value f value =
  match value with
  | TaintBlockPassedTo passed_to_proc_name ->
      F.fprintf f "a block passed to `%a`" Procname.pp passed_to_proc_name
  | TaintProcedure proc_name ->
      F.fprintf f "`%a`" Procname.pp proc_name
  | TaintField field_name ->
      F.fprintf f "`%a`" Fieldname.pp field_name


let pp_value_plain f value =
  match value with
  | TaintBlockPassedTo passed_to_proc_name ->
      F.fprintf f "a block passed to %a" Procname.pp passed_to_proc_name
  | TaintProcedure proc_name ->
      F.fprintf f "%a" Procname.pp proc_name
  | TaintField field_name ->
      F.fprintf f "%a" Fieldname.pp field_name


type t = {kinds: TaintConfig.Kind.t list; value: value; origin: origin} [@@deriving compare, equal]

let pp fmt {kinds; value; origin} =
  F.fprintf fmt "%a %a with kind%s %a" pp_origin origin pp_value value
    (match kinds with [_] -> "" | _ -> "s")
    (Pp.comma_seq TaintConfig.Kind.pp)
    kinds
