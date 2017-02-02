(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Nodule to represent annotations on types. *)

type t [@@deriving compare]

val equal : t -> t -> bool

val const : AnnotatedSignature.annotation -> bool -> TypeOrigin.t -> t

(** Human-readable description of the origin of a nullable value. *)
val descr_origin : Tenv.t -> t -> TypeErr.origin_descr

val from_item_annotation : Annot.Item.t -> TypeOrigin.t -> t
val get_origin : t -> TypeOrigin.t
val get_value : AnnotatedSignature.annotation -> t -> bool
val join : t -> t -> t option
val origin_is_fun_library : t -> bool
val set_value : AnnotatedSignature.annotation -> bool -> t -> t
val to_string : t -> string
val with_origin : t -> TypeOrigin.t -> t
