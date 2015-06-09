(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)

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
val to_string : t -> string
val with_origin : t -> TypeOrigin.t -> t
