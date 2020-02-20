(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t [@@deriving compare]

val from_string : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit

val package : t -> string option

val classname : t -> string

val is_external_via_config : t -> bool
(** Considered external based on config flags. *)
