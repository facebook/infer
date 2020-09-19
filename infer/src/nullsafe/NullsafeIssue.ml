(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { issue_type: IssueType.t
  ; description: string  (** Human-readable description *)
  ; loc: Location.t  (** Where to report the error *)
  ; severity: IssueType.severity
  ; third_party_dependent_methods: (Procname.Java.t * AnnotatedSignature.t) list }

let make ~issue_type ~description ~loc ~severity =
  {issue_type; description; loc; severity; third_party_dependent_methods= []}


let with_third_party_dependent_methods methods t = {t with third_party_dependent_methods= methods}

let get_issue_type {issue_type} = issue_type

let get_description {description} = description

let get_loc {loc} = loc

let get_severity {severity} = severity

let to_third_party_nullability = function
  | AnnotatedNullability.Nullable _ ->
      ThirdPartyMethod.Nullable
  | _ ->
      ThirdPartyMethod.Nonnull


(* Given a third party method, convert it to `.sig` format according to the current
   source file annotations *)
let to_third_party_method_according_to_source_code_annotations (proc_name, annotated_signature) =
  let ThirdPartyAnnotationInfo.{class_name; method_name; param_types} =
    ThirdPartyAnnotationInfo.unique_repr_of_java_proc_name proc_name
  in
  (* We need to provide annotations for return value and param.
     Do it according to the current annotations from the source code
     (assuming everything not @Nullable is non-null).
  *)
  let ret_nullability =
    annotated_signature.AnnotatedSignature.ret.ret_annotated_type.nullability
    |> to_third_party_nullability
  in
  let params_nullability =
    AnnotatedSignature.get_non_virtual_params annotated_signature
    |> List.map ~f:(fun AnnotatedSignature.{param_annotated_type= {nullability}} ->
           to_third_party_nullability nullability )
  in
  let params =
    match List.zip param_types params_nullability with
    | Ok params ->
        params
    | Unequal_lengths ->
        (* This can happen for some synthetic methods. In this case just fallback to non-nullable annotations. *)
        List.map param_types ~f:(fun typ -> (typ, ThirdPartyMethod.Nonnull))
  in
  ThirdPartyMethod.{class_name; method_name; ret_nullability; params}


let get_nullsafe_extra {third_party_dependent_methods} proc_name =
  let class_name = Procname.Java.get_simple_class_name proc_name in
  let package = Procname.Java.get_package proc_name in
  let unvetted_3rd_party_list =
    List.map third_party_dependent_methods
      ~f:to_third_party_method_according_to_source_code_annotations
    |> List.map ~f:ThirdPartyMethod.to_canonical_string
  in
  let unvetted_3rd_party =
    if List.is_empty unvetted_3rd_party_list then None else Some unvetted_3rd_party_list
  in
  Jsonbug_t.{class_name; package; meta_issue_info= None; unvetted_3rd_party}
