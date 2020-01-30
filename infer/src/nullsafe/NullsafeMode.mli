(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Represents a type-checking mode of nullsafe. *)

type t = Default | Strict [@@deriving compare, equal]

val of_class : Tenv.t -> Typ.name -> t
(** Extracts mode information from class annotations *)

val severity : t -> Exceptions.severity
(** Provides a default choice of issue severity for a particular mode *)

val pp : Format.formatter -> t -> unit
