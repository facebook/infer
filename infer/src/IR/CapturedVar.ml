(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = {name: Mangled.t; typ: Typ.t; capture_mode: Pvar.capture_mode} [@@deriving compare]

let make ~name ~typ ~capture_mode = {name; typ; capture_mode}

let pp fmt {name; typ; capture_mode} =
  F.fprintf fmt "(%a,@,%a,@,%s)" Mangled.pp name (Typ.pp_full Pp.text) typ
    (Pvar.string_of_capture_mode capture_mode)
