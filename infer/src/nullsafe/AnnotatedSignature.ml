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
   1. For everything dealing with nullability, use info from NullsafeType instead.
   2. For other annotations guiding Nullsafe behavior, introduce corresponding datatypes:
      a. Known ret value annotations (if any)
      b. Known param annotations
      c. Known method-level annotations.
*)
type t =
  {ret: Annot.Item.t * NullsafeType.t; params: (Mangled.t * Annot.Item.t * NullsafeType.t) list}
[@@deriving compare]

(* get nullability of method's return type given its annotations and information about its params *)
let nullability_for_return ia ~has_propagates_nullable_in_param =
  let nullability = NullsafeType.nullability_of_annot_item ia in
  (* if any param is annotated with propagates nullable, then the result is nullable *)
  match nullability with
  | NullsafeType.Nullable _ ->
      nullability (* We already know it is nullable - lets not overwrite the origin *)
  | _ when has_propagates_nullable_in_param ->
      (* if any params is propagates nullable, the return type can be only nullable *)
      NullsafeType.Nullable NullsafeType.HasPropagatesNullableInParam
  | _ ->
      nullability


(* Given annotations for method signature, extract nullability information
   for return type and params *)
let extract_nullability return_annotation param_annotations =
  let params_nullability = List.map param_annotations ~f:NullsafeType.nullability_of_annot_item in
  let has_propagates_nullable_in_param =
    List.exists params_nullability ~f:(function
      | NullsafeType.Nullable NullsafeType.AnnotatedPropagatesNullable ->
          true
      | _ ->
          false )
  in
  let return_nullability =
    nullability_for_return return_annotation ~has_propagates_nullable_in_param
  in
  (return_nullability, params_nullability)


let get proc_attributes : t =
  let method_annotation = proc_attributes.ProcAttributes.method_annotation in
  let formals = proc_attributes.ProcAttributes.formals in
  let ret_type = proc_attributes.ProcAttributes.ret_type in
  let Annot.Method.{return; params} = method_annotation in
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
    List.rev (zip_params (List.rev params) (List.rev formals))
  in
  let _, param_annotations = List.unzip params_with_annotations in
  let return_nullability, params_nullability = extract_nullability return param_annotations in
  let ret = (return, NullsafeType.{nullability= return_nullability; typ= ret_type}) in
  let params =
    List.zip_exn params_with_annotations params_nullability
    |> List.map ~f:(function ((mangled, typ), annot), nullability ->
           (mangled, annot, NullsafeType.{nullability; typ}) )
  in
  {ret; params}


let param_has_annot predicate pvar ann_sig =
  List.exists
    ~f:(fun (param, param_annot, _) ->
      Mangled.equal param (Pvar.get_name pvar) && predicate param_annot )
    ann_sig.params


let pp proc_name fmt annotated_signature =
  let pp_ia fmt ia = if ia <> [] then F.fprintf fmt "%a " Annot.Item.pp ia in
  let pp_annotated_param fmt (mangled, ia, nullsafe_type) =
    F.fprintf fmt " %a%a %a" pp_ia ia NullsafeType.pp nullsafe_type Mangled.pp mangled
  in
  let ia, nullsafe_type = annotated_signature.ret in
  F.fprintf fmt "%a%a %s (%a )" pp_ia ia NullsafeType.pp nullsafe_type
    (Typ.Procname.to_simplified_string proc_name)
    (Pp.comma_seq pp_annotated_param) annotated_signature.params


let mk_ann_str s = {Annot.class_name= s; parameters= []}

let mk_ia_nullable ia =
  if Annotations.ia_is_nullable ia then ia else (mk_ann_str Annotations.nullable, true) :: ia


let mark_ia_nullability ia x = if x then mk_ia_nullable ia else ia

(* Override existing information about nullability for a given type and
   set it to either nullable or nonnull *)
let set_modelled_nullability_for_nullsafe_type nullsafe_type should_set_nullable =
  let nullability =
    if should_set_nullable then NullsafeType.Nullable ModelledNullable
    else NullsafeType.Nonnull ModelledNonnull
  in
  NullsafeType.{nullsafe_type with nullability}


let set_modelled_nullability proc_name asig (nullability_for_ret, params_nullability) =
  let set_modelled_nullability_for_param (mangled, original_annotation, original_nullsafe_type)
      should_set_nullable =
    let final_annotation =
      if should_set_nullable then mk_ia_nullable original_annotation else original_annotation
    in
    ( mangled
    , final_annotation
    , set_modelled_nullability_for_nullsafe_type original_nullsafe_type should_set_nullable )
  in
  let final_params =
    let fail () =
      L.die InternalError
        "Annotation for procedure %s has wrong number of arguments.@\n  Annotated signature: %a"
        (Typ.Procname.to_unique_id proc_name)
        (pp proc_name) asig
    in
    let rec model_param_nullability original_params params_nullability =
      match (original_params, params_nullability) with
      | (mangled, annotation, nullsafe_type) :: params_tail, nullability_tail
        when Mangled.is_this mangled ->
          (* Skip "this" param - there is no notion of "nullable this" *)
          (mangled, annotation, nullsafe_type)
          :: model_param_nullability params_tail nullability_tail
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
  let original_ret_annotations, original_ret_nullsafe_type = asig.ret in
  let final_ret_nullsafe_type =
    set_modelled_nullability_for_nullsafe_type original_ret_nullsafe_type nullability_for_ret
  in
  let final_ret_annotation = mark_ia_nullability original_ret_annotations nullability_for_ret in
  {ret= (final_ret_annotation, final_ret_nullsafe_type); params= final_params}
