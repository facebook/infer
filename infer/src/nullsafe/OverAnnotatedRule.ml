(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {lhs: Nullability.t; rhs_upper_bound: Nullability.t} [@@deriving compare]

let check ~lhs ~rhs_upper_bound =
  if Nullability.is_strict_subtype ~subtype:rhs_upper_bound ~supertype:lhs then
    Error {lhs; rhs_upper_bound}
  else Ok ()


type violation_type =
  | FieldOverAnnoted of Typ.Fieldname.t
  | ReturnOverAnnotated of Typ.Procname.t  (** Return value of a method can be made non-nullable *)
[@@deriving compare]

let violation_description _ violation_type =
  let module MF = MarkupFormatter in
  let nullable_annotation = "@Nullable" in
  match violation_type with
  | FieldOverAnnoted field_name ->
      Format.asprintf "Field %a is always initialized in the constructor but is declared %a"
        MF.pp_monospaced
        (Typ.Fieldname.to_simplified_string field_name)
        MF.pp_monospaced nullable_annotation
  | ReturnOverAnnotated proc_name ->
      Format.asprintf "Method %a is annotated with %a but never returns null." MF.pp_monospaced
        (Typ.Procname.to_simplified_string proc_name)
        MF.pp_monospaced nullable_annotation
