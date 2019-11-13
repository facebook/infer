(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {is_strict_mode: bool; lhs: Nullability.t; rhs: Nullability.t} [@@deriving compare]

type assignment_type =
  | PassingParamToFunction of
      {param_description: string; param_position: int; function_procname: Typ.Procname.t}
  | AssigningToField of Typ.Fieldname.t
  | ReturningFromFunction of Typ.Procname.t
[@@deriving compare]

let is_whitelisted_assignment ~is_strict_mode ~lhs ~rhs =
  match (is_strict_mode, lhs, rhs) with
  | false, Nullability.Nonnull, Nullability.DeclaredNonnull ->
      (* We allow DeclaredNonnull -> Nonnull conversion outside of strict mode for better adoption.
         Otherwise using strictified classes in non-strict context becomes a pain because
         of extra warnings.
      *)
      true
  | _ ->
      false


let check ~is_strict_mode ~lhs ~rhs =
  let is_allowed_assignment =
    Nullability.is_subtype ~subtype:rhs ~supertype:lhs
    || is_whitelisted_assignment ~is_strict_mode ~lhs ~rhs
  in
  Result.ok_if_true is_allowed_assignment ~error:{is_strict_mode; lhs; rhs}


let violation_description _ assignment_type ~rhs_origin_descr =
  let module MF = MarkupFormatter in
  match assignment_type with
  | PassingParamToFunction {param_description; param_position; function_procname} ->
      Format.asprintf "%a needs a non-null value in parameter %d but argument %a can be null. %s"
        MF.pp_monospaced
        (Typ.Procname.to_simplified_string ~withclass:true function_procname)
        param_position MF.pp_monospaced param_description rhs_origin_descr
  | AssigningToField field_name ->
      Format.asprintf "Field %a can be null but is not declared %a. %s" MF.pp_monospaced
        (Typ.Fieldname.to_simplified_string field_name)
        MF.pp_monospaced "@Nullable" rhs_origin_descr
  | ReturningFromFunction function_proc_name ->
      Format.asprintf "Method %a may return null but it is not annotated with %a. %s"
        MF.pp_monospaced
        (Typ.Procname.to_simplified_string function_proc_name)
        MF.pp_monospaced "@Nullable" rhs_origin_descr
