(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

(* TODO(T54088319) remove Annot.Item.t from t:
  For all helper annotations guiding Nullsafe behavior, introduce corresponding datatypes:
    a. Known ret value annotations (if any)
    b. Known param annotations
    c. Known method-level annotations.
*)

type t =
  {nullsafe_mode: NullsafeMode.t; kind: kind; ret: ret_signature; params: param_signature list}
[@@deriving compare]

and ret_signature = {ret_annotation_deprecated: Annot.Item.t; ret_annotated_type: AnnotatedType.t}
[@@deriving compare]

and param_signature =
  { param_annotation_deprecated: Annot.Item.t
  ; mangled: Mangled.t
  ; param_annotated_type: AnnotatedType.t }
[@@deriving compare]

and kind = FirstParty | ThirdParty of third_party_model_source [@deriving compare]

and third_party_model_source =
  | Unregistered
  | ModelledInternally
  | InThirdPartyRepo of {filename: string; line_number: int}
[@@deriving compare]

let get_non_virtual_params {params} =
  match params with x :: tail when Mangled.is_this x.mangled -> tail | _ -> params


(* get nullability of method's return type given its annotations and information about its params *)
let nullability_for_return ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
    ~is_provisional_annotation_mode ret_type ret_annotations ~has_propagates_nullable_in_param =
  let nullability =
    AnnotatedNullability.of_type_and_annotation ~is_callee_in_trust_list ~nullsafe_mode
      ~is_third_party ret_type ret_annotations
  in
  (* if any param is annotated with propagates nullable, the return nullability is also nullable *)
  let nullability =
    match nullability with
    | AnnotatedNullability.Nullable _ ->
        nullability (* We already know it is nullable - lets not overwrite the origin *)
    | _ when has_propagates_nullable_in_param ->
        (* if any params is propagates nullable, the return type can be only nullable *)
        AnnotatedNullability.Nullable AnnotatedNullability.HasPropagatesNullableInParam
    | _ ->
        nullability
  in
  let final_nullability =
    if
      is_provisional_annotation_mode
      && AnnotatedNullability.can_be_considered_for_provisional_annotation nullability
    then AnnotatedNullability.ProvisionallyNullable (ProvisionalAnnotation.Method proc_name)
    else nullability
  in
  final_nullability


(* Given annotations for method signature, extract nullability information
   for return type and params *)
let extract_for_ret ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
    ~is_provisional_annotation_mode ret_type ret_annotations param_info =
  let has_propagates_nullable_in_param =
    List.exists param_info ~f:(fun {param_annotated_type= {nullability}} ->
        match nullability with
        | AnnotatedNullability.Nullable AnnotatedNullability.AnnotatedPropagatesNullable ->
            true
        | _ ->
            false )
  in
  let return_nullability =
    nullability_for_return ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
      ret_type ~is_provisional_annotation_mode ret_annotations ~has_propagates_nullable_in_param
  in
  { ret_annotation_deprecated= ret_annotations
  ; ret_annotated_type= AnnotatedType.{nullability= return_nullability; typ= ret_type} }


let get_param_nullability ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party param_name
    param_type param_annotations =
  if Mangled.is_this param_name then AnnotatedNullability.StrictNonnull ImplicitThis
  else
    AnnotatedNullability.of_type_and_annotation ~is_callee_in_trust_list ~nullsafe_mode
      ~is_third_party param_type param_annotations


(* When getting param indices, we might need to offset to account for synthetic virtual params *)
let get_param_index_offset param_nullabilities =
  match param_nullabilities with
  | AnnotatedNullability.StrictNonnull ImplicitThis :: _ ->
      1
  | _ ->
      0


let correct_by_provisional_annotations ~proc_name param_nullabilities =
  let index_offset = get_param_index_offset param_nullabilities in
  List.mapi param_nullabilities ~f:(fun param_index nullability ->
      if AnnotatedNullability.can_be_considered_for_provisional_annotation nullability then
        AnnotatedNullability.ProvisionallyNullable
          (ProvisionalAnnotation.Param {method_info= proc_name; num= param_index - index_offset})
      else nullability )


let extract_for_params ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
    ~is_provisional_annotation_mode param_info =
  let param_nullability =
    List.map param_info ~f:(fun (param_name, typ, annotations) ->
        get_param_nullability ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party param_name typ
          annotations )
  in
  let corrected_nullability =
    if is_provisional_annotation_mode then
      correct_by_provisional_annotations ~proc_name param_nullability
    else param_nullability
  in
  List.map2_exn param_info corrected_nullability
    ~f:(fun (mangled, typ, param_annotation_deprecated) nullability ->
      {param_annotation_deprecated; mangled; param_annotated_type= AnnotatedType.{nullability; typ}} )


let get_impl ~is_callee_in_trust_list ~nullsafe_mode ~is_provisional_annotation_mode
    ( {ProcAttributes.proc_name; ret_type; method_annotation= {return= ret_annotation}} as
    proc_attributes ) : t =
  let proc_name =
    Procname.as_java_exn ~explanation:"AnnotatedSignature.get:: should call only for Java methods"
      proc_name
  in
  let is_third_party =
    ThirdPartyAnnotationInfo.is_third_party_proc
      (ThirdPartyAnnotationGlobalRepo.get_repo ())
      proc_name
  in
  let param_info =
    ProcAttributes.get_annotated_formals proc_attributes
    |> List.map ~f:(fun ((a, b), c) -> (a, b, c))
  in
  let params =
    extract_for_params ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
      ~is_provisional_annotation_mode param_info
  in
  let ret =
    extract_for_ret ~proc_name ~is_callee_in_trust_list ~nullsafe_mode ~is_third_party
      ~is_provisional_annotation_mode ret_type ret_annotation params
  in
  let kind = if is_third_party then ThirdParty Unregistered else FirstParty in
  {nullsafe_mode; kind; ret; params}


let get = get_impl ~is_provisional_annotation_mode:false

let get_for_class_under_analysis tenv proc_attributes =
  (* Signature makes special meaning when the method is inside the class we are currently analysing.
     Various non-nullable levels (as dictated by nullsafe mode of the class)
     make sense only for external (for the class under analysis) methods.
     But in context of currently analyzed class we effectively have two levels of nullability for signatures:
     nullable and (strict) non-null.
     We achieve it via passing Strict mode to the signature extractor.
  *)
  let result =
    get_impl ~is_callee_in_trust_list:false ~nullsafe_mode:NullsafeMode.Strict proc_attributes
      ~is_provisional_annotation_mode:Config.nullsafe_annotation_graph
  in
  (* Don't forget about the original mode *)
  let nullsafe_mode = NullsafeMode.of_procname tenv proc_attributes.ProcAttributes.proc_name in
  {result with nullsafe_mode}


let pp_ia fmt ia = if not (List.is_empty ia) then F.fprintf fmt "%a " Annot.Item.pp ia

let pp_annotated_param fmt {mangled; param_annotation_deprecated; param_annotated_type} =
  F.fprintf fmt " %a%a %a" pp_ia param_annotation_deprecated AnnotatedType.pp param_annotated_type
    Mangled.pp mangled


let pp proc_name fmt annotated_signature =
  let {ret_annotation_deprecated; ret_annotated_type} = annotated_signature.ret in
  F.fprintf fmt "[%a] %a%a %a (%a )" NullsafeMode.pp annotated_signature.nullsafe_mode pp_ia
    ret_annotation_deprecated AnnotatedType.pp ret_annotated_type
    (Procname.pp_simplified_string ~withclass:false)
    proc_name (Pp.comma_seq pp_annotated_param) annotated_signature.params


let mk_ann_str s = {Annot.class_name= s; parameters= []}

let mk_ia_nullable ia =
  if Annotations.ia_is_nullable ia then ia else (mk_ann_str Annotations.nullable, true) :: ia


let mark_ia_nullability ia x = if x then mk_ia_nullable ia else ia

(** Override existing information about nullability for a given type and set it to either nullable
    or nonnull *)
let set_modelled_nullability_for_annotated_type annotated_type should_set_nullable =
  let nullability =
    if should_set_nullable then AnnotatedNullability.Nullable ModelledNullable
    else AnnotatedNullability.StrictNonnull ModelledNonnull
  in
  AnnotatedType.{annotated_type with nullability}


let set_modelled_nullability proc_name asig model_source (nullability_for_ret, params_nullability) =
  let set_modelled_nullability_for_param param should_set_nullable =
    { param with
      param_annotation_deprecated=
        mark_ia_nullability param.param_annotation_deprecated should_set_nullable
    ; param_annotated_type=
        set_modelled_nullability_for_annotated_type param.param_annotated_type should_set_nullable
    }
  in
  let set_modelled_nullability_for_ret ret should_set_nullable =
    { ret_annotation_deprecated=
        mark_ia_nullability ret.ret_annotation_deprecated should_set_nullable
    ; ret_annotated_type=
        set_modelled_nullability_for_annotated_type ret.ret_annotated_type should_set_nullable }
  in
  let final_params =
    let fail () =
      L.die InternalError
        "Annotation for procedure %a has wrong number of arguments.@\n  Annotated signature: %a"
        Procname.pp_unique_id proc_name (pp proc_name) asig
    in
    let rec model_param_nullability original_params params_nullability =
      match (original_params, params_nullability) with
      | param :: params_tail, nullability_tail when Mangled.is_this param.mangled ->
          (* Skip "this" param - there is no notion of "nullable this" *)
          param :: model_param_nullability params_tail nullability_tail
      | param :: params_tail, should_set_nullable :: nullability_tail ->
          set_modelled_nullability_for_param param should_set_nullable
          :: model_param_nullability params_tail nullability_tail
      | [], _ :: _ | _ :: _, [] ->
          (* One list extausted before the other one *)
          fail ()
      | [], [] ->
          []
    in
    model_param_nullability asig.params params_nullability
  in
  match model_source with
  | Unregistered ->
      Logging.die InternalError "the method should be either internally or externally modelled"
  | ModelledInternally | InThirdPartyRepo _ ->
      { asig with
        ret= set_modelled_nullability_for_ret asig.ret nullability_for_ret
      ; kind= ThirdParty model_source
      ; params= final_params }
