(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Nodule to represent annotations on types. *)

type t [@@deriving compare]

val const_nullable : bool -> TypeOrigin.t -> t

val descr_origin : t -> TypeErr.origin_descr
(** Human-readable description of the origin of a nullable value. *)

val from_item_annotation : Annot.Item.t -> TypeOrigin.t -> t

val get_origin : t -> TypeOrigin.t

val is_nullable : t -> bool

val join : t -> t -> t option

val origin_is_fun_library : t -> bool

val set_nullable : bool -> t -> t

val to_string : t -> string

val with_origin : t -> TypeOrigin.t -> t
