(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* TODO(T54088319) get rid of annotation_deprecated:
  Introduce "field flags" and move all other usages to this dedicated datatype
  *)
type t = {annotation_deprecated: Annot.Item.t; annotated_type: AnnotatedType.t}

let is_class_in_strict_mode tenv typ =
  match PatternMatch.type_get_annotation tenv typ with
  | Some ia ->
      Annotations.ia_is_nullsafe_strict ia
  | None ->
      false


let get tenv fn typ =
  let lookup = Tenv.lookup tenv in
  (* We currently don't support field-level strict mode annotation, so fetch it from class *)
  let is_strict_mode = is_class_in_strict_mode tenv typ in
  let type_and_annotation_to_field_type (typ, annotation) =
    { annotation_deprecated= annotation
    ; annotated_type=
        AnnotatedType.
          { nullability= AnnotatedNullability.of_type_and_annotation typ annotation ~is_strict_mode
          ; typ } }
  in
  Option.map
    (Typ.Struct.get_field_type_and_annotation ~lookup fn typ)
    ~f:type_and_annotation_to_field_type
