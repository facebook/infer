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

type t = {pvar: Pvar.t; typ: Typ.t; capture_mode: capture_mode}
[@@deriving compare, equal, normalize]

val pp : Format.formatter -> t -> unit
