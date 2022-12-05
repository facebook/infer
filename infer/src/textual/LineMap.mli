(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t

val create : string -> t

val find : t -> int -> int option [@@warning "-unused-value-declaration"]

val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]
