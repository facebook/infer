(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! NS
module F = Format

val seq : ?sep:string -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
[@@warning "-unused-value-declaration"]

val comma_seq : (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
[@@warning "-unused-value-declaration"]

val semicolon_seq : (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
[@@warning "-unused-value-declaration"]

val option : (F.formatter -> 'a -> unit) -> F.formatter -> 'a option -> unit
[@@warning "-unused-value-declaration"]

val array : (F.formatter -> 'a -> unit) -> F.formatter -> 'a array -> unit
[@@warning "-unused-value-declaration"]

val iarray : (F.formatter -> 'a -> unit) -> F.formatter -> 'a iarray -> unit
[@@warning "-unused-value-declaration"]
