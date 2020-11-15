(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Representation of a declared class field with nullsafe-specific data *)

type t = {annotation_deprecated: Annot.Item.t; annotated_type: AnnotatedType.t}

val get : Tenv.t -> Fieldname.t -> class_typ:Typ.t -> class_under_analysis:Typ.name -> t option
(** Looks up for a field declaration and, in case of success, converts it to [t] *)
