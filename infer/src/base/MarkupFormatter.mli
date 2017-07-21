(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val wrap_monospaced : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** used to combine pp together, wrap content into a monospaced block  *)

val pp_monospaced : Format.formatter -> string -> unit
(** pp to wrap into a monospaced block  *)

(* wrap into a monospaced block *)

val monospaced_to_string : string -> string

val wrap_code : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** used to combine pp together, wrap content into a code block  *)

val pp_code : Format.formatter -> string -> unit
(** pp to wrap into a code block  *)

(* wrap into a code block *)

val code_to_string : string -> string

val wrap_bold : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
(** used to combine pp together, wrap content into a bold block  *)

val pp_bold : Format.formatter -> string -> unit
(** pp to wrap into a bold block  *)

(* wrap into a bold block *)

val bold_to_string : string -> string
