(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type capture_mode = ByReference | ByValue
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val string_of_capture_mode : capture_mode -> string

val is_captured_by_ref : capture_mode -> bool

type captured_info = {loc: Location.t; is_formal: Procname.t option}
[@@deriving compare, equal, sexp, hash, normalize]

type context_info = {is_checked_for_null: bool; is_internal_pointer_of: Typ.t option}
[@@deriving compare, equal, sexp, hash, normalize]

(** captured_from and context_info only set for captured variables in Objective-C blocks *)
type t =
  { pvar: Pvar.t
  ; typ: Typ.t
  ; capture_mode: capture_mode
  ; captured_from: captured_info option
  ; context_info: context_info option }
[@@deriving compare, equal, sexp, hash, normalize]

val pp : Format.formatter -> t -> unit
