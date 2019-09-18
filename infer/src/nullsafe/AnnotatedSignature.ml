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

(* TODO(T54088319) Make this update NullsafeType.t as well*)
let mark_nullability proc_name asig (b, bs) =
  let ia, t = asig.ret in
  let ret' = (mark_ia_nullability ia b, t) in
  let mark_param (s, ia, t) x =
    let ia' = if x then mk_ia_nullable ia else ia in
    (s, ia', t)
  in
  let params' =
    let fail () =
      L.die InternalError
        "Annotation for procedure %s has wrong number of arguments.@\n  Annotated signature: %a"
        (Typ.Procname.to_unique_id proc_name)
        (pp proc_name) asig
    in
    let rec combine l1 l2 =
      match (l1, l2) with
      | (p, ia, t) :: l1', l2' when Mangled.is_this p ->
          (p, ia, t) :: combine l1' l2'
      | (s, ia, t) :: l1', x :: l2' ->
          mark_param (s, ia, t) x :: combine l1' l2'
      | [], _ :: _ ->
          fail ()
      | _ :: _, [] ->
          fail ()
      | [], [] ->
          []
    in
    combine asig.params bs
  in
  {ret= ret'; params= params'}
