(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** Annotation graph for a Java class comprises of: * Nodes (annotation points): potential places in
    the class declaration to be annotated as [@Nullable] * For each annotation point the graph
    stores what will happen if this annotation point becomes real [@Nullable] (new violations that
    will arise) * Edges: if annotating a point A will require annotating another point B as
    [@Nullable], A -> B are connected in the graph. *)

val build_graph :
  Tenv.t -> Struct.t -> Typ.name -> ProvisionalViolation.t list -> Jsonbug_t.annotation_point list
(** Given a Java class and the list of all provisional violations found in that class, build the
    annotation graph *)
