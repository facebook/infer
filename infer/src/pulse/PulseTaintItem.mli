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

type value =
  | TaintBlockPassedTo of Procname.t
  | TaintField of Fieldname.t
  | TaintProcedure of Procname.t
[@@deriving compare, equal]

type value_tuple =
  | Basic of {value: value; origin: origin}
  | FieldOf of {name: string; value_tuple: value_tuple}
  | PointedToBy of {value_tuple: value_tuple}
[@@deriving compare, equal]

type t = {kinds: TaintConfig.Kind.t list; value_tuple: value_tuple} [@@deriving compare, equal]

val pp_value : F.formatter -> value -> unit

val pp_value_plain : F.formatter -> value -> unit

val pp_value_tuple_debug : F.formatter -> value_tuple -> unit

val pp : F.formatter -> t -> unit

val is_argument_origin : t -> bool

val is_set_field_origin : t -> bool

val value_of_taint : t -> value

val field_of_origin : t -> string -> t

val get_rev_field_access_list : value_tuple -> [> `Field of string | `Deref] list
