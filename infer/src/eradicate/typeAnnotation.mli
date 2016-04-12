(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Nodule to represent annotations on types. *)

type t

val const : Annotations.annotation -> bool -> TypeOrigin.t -> t

(** Human-readable description of the origin of a nullable value. *)
val descr_origin : t -> TypeErr.origin_descr

val equal : t -> t -> bool
val from_item_annotation : Sil.item_annotation -> TypeOrigin.t -> t
val get_origin : t -> TypeOrigin.t
val get_value : Annotations.annotation -> t -> bool
val join : t -> t -> t option
val origin_is_fun_library : t -> bool
val set_value : Annotations.annotation -> bool -> t -> t
val to_string : t -> string
val with_origin : t -> TypeOrigin.t -> t
