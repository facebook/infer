(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {nullsafe_mode: NullsafeMode.t; base: Nullability.t; overridden: Nullability.t}
[@@deriving compare]

type type_role = Param | Ret

let is_whitelisted_violation ~subtype ~supertype =
  (* When both nullabilities are kind-of non-nullable we don't want to raise the
     issue. Without this suppression there will be a lot of non-actionable issues
     raised for classes in one [NullsafeMode] inheriting from classes in the other
     [NullsafeMode]. *)
  (* TODO(T62521386): consider using caller context when determining nullability to get
     rid of white-lists. *)
  Nullability.is_nonnullish subtype && Nullability.is_nonnullish supertype


let check ~nullsafe_mode type_role ~base ~overridden =
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
    ~error:{nullsafe_mode; base; overridden}


type violation_type =
  | InconsistentParam of {param_description: string; param_position: int}
  | InconsistentReturn
[@@deriving compare]

let is_java_lang_object_equals = function
  | Procname.Java java_procname -> (
    match (Procname.Java.get_class_name java_procname, Procname.Java.get_method java_procname) with
    | "java.lang.Object", "equals" ->
        true
    | _ ->
        false )
  | _ ->
      false


let violation_description _ violation_type ~base_proc_name ~overridden_proc_name =
  let module MF = MarkupFormatter in
  let base_method_descr = Procname.to_simplified_string ~withclass:true base_proc_name in
  let overridden_method_descr =
    Procname.to_simplified_string ~withclass:true overridden_proc_name
  in
  match violation_type with
  | InconsistentReturn ->
      Format.asprintf
        "Child method %a is not substitution-compatible with its parent: the return type is \
         declared as nullable, but parent method %a is missing `@Nullable` declaration. Either \
         mark the parent as `@Nullable` or ensure the child does not return `null`."
        MF.pp_monospaced overridden_method_descr MF.pp_monospaced base_method_descr
  | InconsistentParam {param_description; param_position} ->
      if is_java_lang_object_equals base_proc_name then
        (* This is a popular enough case to make error message specific *)
        Format.asprintf
          "Parameter %a is missing `@Nullable` declaration: according to the Java Specification, \
           for any object `x` call `x.equals(null)` should properly return false."
          MF.pp_monospaced param_description
      else
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
          "%s parameter %a of method %a is missing `@Nullable` declaration when overriding %a. The \
           parent method declared it can handle `null` for this param, so the child should also \
           declare that."
          (translate_position param_position)
          MF.pp_monospaced param_description MF.pp_monospaced overridden_method_descr
          MF.pp_monospaced base_method_descr


let violation_severity {nullsafe_mode} = NullsafeMode.severity nullsafe_mode
