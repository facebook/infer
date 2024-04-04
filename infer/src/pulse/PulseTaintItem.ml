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
  | InstanceReference
  | ReturnValue
  | Allocation of {typ: string}
  | GetField
  | SetField
  | FieldOfValue of {name: string; origin: origin}
[@@deriving compare, equal]

let rec pp_origin fmt = function
  | Argument {index} ->
      F.fprintf fmt "value passed as argument `#%d` to" index
  | InstanceReference ->
      F.fprintf fmt "this/self reference"
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


type value_tuple =
  | Basic of {value: value; origin: origin}
  | FieldOf of {name: string; value_tuple: value_tuple}
  | PointedToBy of {value_tuple: value_tuple}
[@@deriving compare, equal]

let rec pp_value_tuple fmt value_tuple =
  match value_tuple with
  | Basic {value; origin} ->
      F.fprintf fmt "%a %a" pp_origin origin pp_value value
  | FieldOf {value_tuple} | PointedToBy {value_tuple} ->
      F.fprintf fmt "%a" pp_value_tuple value_tuple


type t = {kinds: TaintConfig.Kind.t list; value_tuple: value_tuple} [@@deriving compare, equal]

let pp fmt {value_tuple; kinds} =
  F.fprintf fmt "%a with kind%s %a" pp_value_tuple value_tuple
    (match kinds with [_] -> "" | _ -> "s")
    (Pp.comma_seq TaintConfig.Kind.pp)
    kinds


let rec pp_value_tuple_debug fmt value_tuple =
  match value_tuple with
  | Basic {value; origin} ->
      F.fprintf fmt "%a %a" pp_origin origin pp_value value
  | FieldOf {name; value_tuple} ->
      F.fprintf fmt "field %s of %a" name pp_value_tuple_debug value_tuple
  | PointedToBy {value_tuple} ->
      F.fprintf fmt "pointed to by %a" pp_value_tuple_debug value_tuple


let is_argument_origin {value_tuple} =
  let rec aux = function Argument _ -> true | FieldOfValue {origin} -> aux origin | _ -> false in
  let rec is_argument value_tuple =
    match value_tuple with
    | Basic {origin} ->
        aux origin
    | FieldOf {value_tuple} | PointedToBy {value_tuple} ->
        is_argument value_tuple
  in
  is_argument value_tuple


let is_set_field_origin {value_tuple} =
  let rec aux = function SetField -> true | FieldOfValue {origin} -> aux origin | _ -> false in
  let rec is_set_field value_tuple =
    match value_tuple with
    | Basic {origin} ->
        aux origin
    | FieldOf {value_tuple} | PointedToBy {value_tuple} ->
        is_set_field value_tuple
  in
  is_set_field value_tuple


let value_of_taint {value_tuple} =
  let rec value_of value_tuple =
    match value_tuple with
    | Basic {value} ->
        value
    | FieldOf {value_tuple} | PointedToBy {value_tuple} ->
        value_of value_tuple
  in
  value_of value_tuple


let field_of_origin {value_tuple; kinds} fieldname =
  let rec field_of_origin value_tuple =
    match value_tuple with
    | Basic {value; origin} ->
        Basic {value; origin= FieldOfValue {name= fieldname; origin}}
    | FieldOf {name; value_tuple} ->
        FieldOf {name; value_tuple= field_of_origin value_tuple}
    | PointedToBy {value_tuple} ->
        PointedToBy {value_tuple= field_of_origin value_tuple}
  in
  {value_tuple= field_of_origin value_tuple; kinds}


let get_rev_field_access_list vt =
  let rec get_access vt =
    match vt with
    | Basic _ ->
        []
    | PointedToBy {value_tuple} ->
        get_access value_tuple
    | FieldOf {name; value_tuple} ->
        `Field name :: get_access value_tuple
  in
  get_access vt
