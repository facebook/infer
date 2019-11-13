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

type t = {is_strict_mode: bool; ret: ret_signature; params: param_signature list}
[@@deriving compare]

and ret_signature = {ret_annotation_deprecated: Annot.Item.t; ret_annotated_type: AnnotatedType.t}
[@@deriving compare]

and param_signature =
  { param_annotation_deprecated: Annot.Item.t
  ; mangled: Mangled.t
  ; param_annotated_type: AnnotatedType.t }
[@@deriving compare]

(* get nullability of method's return type given its annotations and information about its params *)
let nullability_for_return ret_type ret_annotations ~is_strict_mode
    ~has_propagates_nullable_in_param =
  let nullability =
    AnnotatedNullability.of_type_and_annotation ~is_strict_mode ret_type ret_annotations
  in
  (* if any param is annotated with propagates nullable, then the result is nullable *)
  match nullability with
  | AnnotatedNullability.Nullable _ ->
      nullability (* We already know it is nullable - lets not overwrite the origin *)
  | _ when has_propagates_nullable_in_param ->
      (* if any params is propagates nullable, the return type can be only nullable *)
      AnnotatedNullability.Nullable AnnotatedNullability.HasPropagatesNullableInParam
  | _ ->
      nullability


(* Given annotations for method signature, extract nullability information
   for return type and params *)
let extract_nullability ~is_strict_mode ret_type ret_annotations param_annotated_types =
  let params_nullability =
    List.map param_annotated_types ~f:(fun (typ, annotations) ->
        AnnotatedNullability.of_type_and_annotation typ annotations ~is_strict_mode )
  in
  let has_propagates_nullable_in_param =
    List.exists params_nullability ~f:(function
      | AnnotatedNullability.Nullable AnnotatedNullability.AnnotatedPropagatesNullable ->
          true
      | _ ->
          false )
  in
  let return_nullability =
    nullability_for_return ret_type ret_annotations ~is_strict_mode
      ~has_propagates_nullable_in_param
  in
  (return_nullability, params_nullability)


let get ~is_strict_mode proc_attributes : t =
  let Annot.Method.{return= ret_annotation; params= original_params_annotation} =
    proc_attributes.ProcAttributes.method_annotation
  in
  let formals = proc_attributes.ProcAttributes.formals in
  let ret_type = proc_attributes.ProcAttributes.ret_type in
  (* zip formal params with annotation *)
  let params_with_annotations =
    let rec zip_params ial parl =
      match (ial, parl) with
      | ia :: ial', param :: parl' ->
          (param, ia) :: zip_params ial' parl'
      | [], param :: parl' ->
          (* List of annotations exhausted before the list of params -
             treat lack of annotation info as an empty annotation *)
          (param, Annot.Item.empty) :: zip_params [] parl'
      | [], [] ->
          []
      | _ :: _, [] ->
          (* List of params exhausted before the list of annotations -
             this should never happen *)
          assert false
    in
    List.rev (zip_params (List.rev original_params_annotation) (List.rev formals))
  in
  let param_annotated_types =
    List.map params_with_annotations ~f:(fun ((_, typ), annotations) -> (typ, annotations))
  in
  let return_nullability, params_nullability =
    extract_nullability ~is_strict_mode ret_type ret_annotation param_annotated_types
  in
  let ret =
    { ret_annotation_deprecated= ret_annotation
    ; ret_annotated_type= AnnotatedType.{nullability= return_nullability; typ= ret_type} }
  in
  let params =
    List.map2_exn params_with_annotations params_nullability
      ~f:(fun ((mangled, typ), param_annotation_deprecated) nullability ->
        { param_annotation_deprecated
        ; mangled
        ; param_annotated_type= AnnotatedType.{nullability; typ} } )
  in
  {is_strict_mode; ret; params}


let param_has_annot predicate pvar ann_sig =
  List.exists
    ~f:(fun {mangled; param_annotation_deprecated} ->
      Mangled.equal mangled (Pvar.get_name pvar) && predicate param_annotation_deprecated )
    ann_sig.params


let pp proc_name fmt annotated_signature =
  let pp_ia fmt ia = if ia <> [] then F.fprintf fmt "%a " Annot.Item.pp ia in
  let pp_annotated_param fmt {mangled; param_annotation_deprecated; param_annotated_type} =
    F.fprintf fmt " %a%a %a" pp_ia param_annotation_deprecated AnnotatedType.pp param_annotated_type
      Mangled.pp mangled
  in
  let {ret_annotation_deprecated; ret_annotated_type} = annotated_signature.ret in
  let mode_as_string = if annotated_signature.is_strict_mode then "Strict" else "Def" in
  F.fprintf fmt "[%s] %a%a %a (%a )" mode_as_string pp_ia ret_annotation_deprecated AnnotatedType.pp
    ret_annotated_type
    (Typ.Procname.pp_simplified_string ~withclass:false)
    proc_name (Pp.comma_seq pp_annotated_param) annotated_signature.params


let mk_ann_str s = {Annot.class_name= s; parameters= []}

let mk_ia_nullable ia =
  if Annotations.ia_is_nullable ia then ia else (mk_ann_str Annotations.nullable, true) :: ia


let mark_ia_nullability ia x = if x then mk_ia_nullable ia else ia

(* Override existing information about nullability for a given type and
   set it to either nullable or nonnull *)
let set_modelled_nullability_for_annotated_type annotated_type should_set_nullable =
  let nullability =
    if should_set_nullable then AnnotatedNullability.Nullable ModelledNullable
    else AnnotatedNullability.Nonnull ModelledNonnull
  in
  AnnotatedType.{annotated_type with nullability}


let set_modelled_nullability proc_name asig (nullability_for_ret, params_nullability) =
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
        Typ.Procname.pp_unique_id proc_name (pp proc_name) asig
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
  { asig with
    ret= set_modelled_nullability_for_ret asig.ret nullability_for_ret
  ; params= final_params }
