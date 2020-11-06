(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** A helper module to deal with Nullsafe annotation graph's JSON representation *)

val pp_annotation_graph : Format.formatter -> Jsonbug_t.annotation_point list -> unit
