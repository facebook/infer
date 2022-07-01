(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type capture_mode = ByReference | ByValue [@@deriving compare, equal]

let string_of_capture_mode = function ByReference -> "by ref" | ByValue -> "by value"

type t = {pvar: Pvar.t; typ: Typ.t; capture_mode: capture_mode} [@@deriving compare, equal]

let pp fmt {pvar; typ; capture_mode} =
  F.fprintf fmt "(%a,@,%a,@,%s)" (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ
    (string_of_capture_mode capture_mode)
