(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Formal type in program together with its nullability information. *)

type t = {nullability: AnnotatedNullability.t; typ: Typ.t} [@@deriving compare]

val pp : Format.formatter -> t -> unit
