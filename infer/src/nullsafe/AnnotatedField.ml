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

let get tenv fn typ =
  let lookup = Tenv.lookup tenv in
  let type_and_annotation_to_field_type (typ, annotation) =
    { annotation_deprecated= annotation
    ; annotated_type=
        AnnotatedType.{nullability= AnnotatedNullability.of_annot_item annotation; typ} }
  in
  Option.map
    (Typ.Struct.get_field_type_and_annotation ~lookup fn typ)
    ~f:type_and_annotation_to_field_type
