(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {base: Nullability.t; overridden: Nullability.t} [@@deriving compare]

type type_role = Param | Ret

module ReportableViolation = struct
  type t = {nullsafe_mode: NullsafeMode.t; violation: violation}

  type violation_type =
    | InconsistentParam of {param_description: string; param_index: int}
    | InconsistentReturn
  [@@deriving compare]

  let from nullsafe_mode ({base; overridden} as violation) =
    if
      Nullability.is_nonnullish base && Nullability.is_nonnullish overridden
      (* When both nullabilities are kind-of non-nullable we don't want to raise the
         issue. Without this suppression there will be a lot of non-actionable issues
         raised for classes in one [NullsafeMode] inheriting from classes in the other
         [NullsafeMode]. *)
      (* TODO(T62521386): consider using caller context when determining nullability to get
         rid of white-lists. *)
    then None
    else Some {nullsafe_mode; violation}


  let is_java_lang_object_equals procname =
    match (Procname.Java.get_class_name procname, Procname.Java.get_method procname) with
    | "java.lang.Object", "equals" ->
        true
    | _ ->
        false


  let make_nullsafe_issue _ violation_type ~nullsafe_mode ~loc ~base_proc_name ~overridden_proc_name
      =
    let module MF = MarkupFormatter in
    let base_method_descr = Procname.Java.to_simplified_string ~withclass:true base_proc_name in
    let overridden_method_descr =
      Procname.Java.to_simplified_string ~withclass:true overridden_proc_name
    in
    let description =
      match violation_type with
      | InconsistentReturn ->
          Format.asprintf
            "Child method %a is not substitution-compatible with its parent: the return type is \
             declared as nullable, but parent method %a is missing `@Nullable` declaration. Either \
             mark the parent as `@Nullable` or ensure the child does not return `null`."
            MF.pp_monospaced overridden_method_descr MF.pp_monospaced base_method_descr
      | InconsistentParam {param_description; param_index} ->
          if is_java_lang_object_equals base_proc_name then
            (* This is a popular enough case to make error message specific *)
            Format.asprintf
              "Parameter %a is missing `@Nullable` declaration: according to the Java \
               Specification, for any object `x` call `x.equals(null)` should properly return \
               false."
              MF.pp_monospaced param_description
          else
            let position_to_human_readable_string = function
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
              "%s parameter %a of method %a is missing `@Nullable` declaration when overriding %a. \
               The parent method declared it can handle `null` for this param, so the child should \
               also declare that."
              (position_to_human_readable_string
                 ((* For the user reporting, index param positions from 1 *) param_index + 1))
              MF.pp_monospaced param_description MF.pp_monospaced overridden_method_descr
              MF.pp_monospaced base_method_descr
    in
    let severity = NullsafeMode.severity nullsafe_mode in
    let issue_type, param_index =
      match violation_type with
      | InconsistentReturn ->
          (IssueType.eradicate_inconsistent_subclass_return_annotation, None)
      | InconsistentParam {param_index} ->
          (IssueType.eradicate_inconsistent_subclass_parameter_annotation, Some param_index)
    in
    NullsafeIssue.make ~description ~loc ~issue_type ~severity ~field_name:None
    |> NullsafeIssue.with_inconsistent_param_index param_index
end

let check type_role ~base ~overridden =
  if Nullability.equal Nullability.ThirdPartyNonnull base then
    (* In context of inheritance check, third party declarations in base are treated optimistically.
       Meaning return values are assumed [@Nullable] and params are assumed [@NonNull].
       This is done for compatibility reasons so existing [@Nullsafe] classes are preserved Nullsafe.
    *)
    Ok ()
  else
    let subtype, supertype =
      match type_role with
      | Ret ->
          (* covariance for ret *)
          (overridden, base)
      | Param ->
          (* contravariance for param *)
          (base, overridden)
    in
    Result.ok_if_true (Nullability.is_subtype ~subtype ~supertype) ~error:{base; overridden}
