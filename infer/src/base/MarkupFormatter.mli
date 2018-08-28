(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val wrap_monospaced : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** used to combine pp together, wrap content into a monospaced block  *)

val pp_monospaced : Format.formatter -> string -> unit
(** pp to wrap into a monospaced block  *)

val monospaced_to_string : string -> string
(** wrap into a monospaced block *)

val wrap_code : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  [@@warning "-32"]
(** used to combine pp together, wrap content into a code block  *)

val pp_code : Format.formatter -> string -> unit
  [@@warning "-32"]
(** pp to wrap into a code block  *)

val wrap_bold : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
  [@@warning "-32"]
(** used to combine pp together, wrap content into a bold block  *)

val pp_bold : Format.formatter -> string -> unit
(** pp to wrap into a bold block  *)

val bold_to_string : string -> string [@@warning "-32"]
(** wrap into a bold block *)
