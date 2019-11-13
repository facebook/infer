(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {base: Nullability.t; overridden: Nullability.t} [@@deriving compare]

type type_role = Param | Ret

let is_whitelisted_violation ~subtype ~supertype =
  match (subtype, supertype) with
  | Nullability.DeclaredNonnull, Nullability.Nonnull ->
      (* It is a violation that we are currently willing to ignore because
         it is hard to obey in practice.
         It might lead to unsoundness issues, so this might be reconsidered.
      *)
      true
  | _ ->
      false


let check type_role ~base ~overridden =
  let subtype, supertype =
    match type_role with
    | Ret ->
        (* covariance for ret *)
        (overridden, base)
    | Param ->
        (* contravariance for param *)
        (base, overridden)
  in
  Result.ok_if_true
    (Nullability.is_subtype ~subtype ~supertype || is_whitelisted_violation ~subtype ~supertype)
    ~error:{base; overridden}


type violation_type =
  | InconsistentParam of {param_description: string; param_position: int}
  | InconsistentReturn
[@@deriving compare]

let violation_description _ violation_type ~base_proc_name ~overridden_proc_name =
  let module MF = MarkupFormatter in
  let nullable_annotation = "@Nullable" in
  let base_method_descr = Typ.Procname.to_simplified_string ~withclass:true base_proc_name in
  let overridden_method_descr =
    Typ.Procname.to_simplified_string ~withclass:true overridden_proc_name
  in
  match violation_type with
  | InconsistentReturn ->
      Format.asprintf "Method %a is annotated with %a but overrides unannotated method %a."
        MF.pp_monospaced overridden_method_descr MF.pp_monospaced nullable_annotation
        MF.pp_monospaced base_method_descr
  | InconsistentParam {param_description; param_position} ->
      let translate_position = function
        | 1 ->
            "First"
        | 2 ->
            "Second"
        | 3 ->
            "Third"
        | n ->
            string_of_int n ^ "th"
      in
      Format.asprintf
        "%s parameter %a of method %a is not %a but is declared %ain the parent class method %a."
        (translate_position param_position)
        MF.pp_monospaced param_description MF.pp_monospaced overridden_method_descr MF.pp_monospaced
        nullable_annotation MF.pp_monospaced nullable_annotation MF.pp_monospaced base_method_descr
