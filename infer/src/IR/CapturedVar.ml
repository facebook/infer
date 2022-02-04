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

type t = {name: Mangled.t; typ: Typ.t; capture_mode: capture_mode} [@@deriving compare, equal]

let make ~name ~typ ~capture_mode = {name; typ; capture_mode}

let pp fmt {name; typ; capture_mode} =
  F.fprintf fmt "(%a,@,%a,@,%s)" Mangled.pp name (Typ.pp_full Pp.text) typ
    (string_of_capture_mode capture_mode)
