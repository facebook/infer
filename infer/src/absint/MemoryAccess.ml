(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type ('fieldname, 'array_index) t_ =
  | FieldAccess of 'fieldname
  | ArrayAccess of (Typ.t[@ignore]) * 'array_index
  | TakeAddress
  | Dereference
[@@deriving compare, equal, yojson_of]

type 'array_index t = (Fieldname.t, 'array_index) t_ [@@deriving compare, equal, yojson_of]

let pp pp_array_index fmt = function
  | FieldAccess field_name ->
      Fieldname.pp fmt field_name
  | ArrayAccess (_, index) ->
      F.fprintf fmt "[%a]" pp_array_index index
  | TakeAddress ->
      F.pp_print_string fmt "&"
  | Dereference ->
      F.pp_print_string fmt "*"


let is_field_or_array_access = function ArrayAccess _ | FieldAccess _ -> true | _ -> false
