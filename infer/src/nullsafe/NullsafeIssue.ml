(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type parameter_not_nullable_info =
  {param_index: int; proc_name: Procname.Java.t  (** Offending method called *)}

type t =
  { issue_type: IssueType.t
  ; description: string  (** Human-readable description *)
  ; loc: Location.t  (** Where to report the error *)
  ; field_name: Fieldname.t option  (** If the issue is about a field, here's this field *)
  ; inconsistent_param_index: int option
        (** Only for "inconsistent subclass param annotation" issue *)
  ; parameter_not_nullable_info: parameter_not_nullable_info option
        (** Only for "Parameter Not Nullable" issue *)
  ; severity: IssueType.severity
  ; nullable_methods: TypeOrigin.method_call_origin list
        (** If the issue is associated with misusing nullable values coming from method calls,
            here's the list *)
  ; third_party_dependent_methods: (Procname.Java.t * AnnotatedSignature.t) list }

let make ~issue_type ~description ~loc ~severity ~field_name =
  { issue_type
  ; description
  ; loc
  ; inconsistent_param_index= None
  ; parameter_not_nullable_info= None
  ; severity
  ; third_party_dependent_methods= []
  ; nullable_methods= []
  ; field_name }


let with_third_party_dependent_methods methods t = {t with third_party_dependent_methods= methods}

let with_nullable_methods methods t = {t with nullable_methods= methods}

let with_inconsistent_param_index index t = {t with inconsistent_param_index= index}

let with_parameter_not_nullable_info ~param_index ~proc_name t =
  {t with parameter_not_nullable_info= Some {param_index; proc_name}}


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


let to_nullable_method_json nullable_methods =
  List.map nullable_methods ~f:(fun TypeOrigin.{pname; call_loc} ->
      Jsonbug_t.
        { class_name= Procname.Java.get_simple_class_name pname
        ; method_name= Procname.Java.get_method pname
        ; package= Procname.Java.get_package pname |> Option.value ~default:""
        ; call_line= call_loc.Location.line } )


let java_type_to_string java_type = Pp.string_of_pp (Typ.pp_java ~verbose:true) java_type

let get_params_string_list procname =
  Procname.Java.get_parameters procname |> List.map ~f:java_type_to_string


let parameter_not_nullable_info_to_json {param_index; proc_name} :
    Jsonbug_t.parameter_not_nullable_info =
  let package_name = Procname.Java.get_package proc_name in
  let class_name = Procname.Java.get_simple_class_name proc_name in
  let method_info =
    Jsonbug_t.{name= Procname.Java.get_method proc_name; params= get_params_string_list proc_name}
  in
  Jsonbug_t.{class_name; package_name; method_info; param_index}


let get_nullsafe_extra
    { third_party_dependent_methods
    ; nullable_methods
    ; inconsistent_param_index
    ; parameter_not_nullable_info
    ; field_name } proc_name =
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
  let nullable_methods =
    if List.is_empty nullable_methods then None else Some (to_nullable_method_json nullable_methods)
  in
  let field =
    Option.map field_name ~f:(fun field_name ->
        let field = Fieldname.get_field_name field_name in
        let class_typ_name = Fieldname.get_class_name field_name in
        let java_class_name = Typ.Name.Java.get_java_class_name_exn class_typ_name in
        Jsonbug_t.
          { class_name= JavaClassName.classname java_class_name
          ; package_name= JavaClassName.package java_class_name
          ; field } )
  in
  let method_params = get_params_string_list proc_name in
  let method_info = Jsonbug_t.{name= Procname.Java.get_method proc_name; params= method_params} in
  let parameter_not_nullable_info =
    Option.map ~f:parameter_not_nullable_info_to_json parameter_not_nullable_info
  in
  Jsonbug_t.
    { class_name
    ; package
    ; method_info= Some method_info
    ; inconsistent_param_index
    ; parameter_not_nullable_info
    ; meta_issue_info= None
    ; unvetted_3rd_party
    ; nullable_methods
    ; field
    ; annotation_graph= None }
