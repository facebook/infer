(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type capture_mode = ByReference | ByValue
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let string_of_capture_mode = function ByReference -> "by ref" | ByValue -> "by value"

let is_captured_by_ref captured_mode =
  match captured_mode with ByReference -> true | ByValue -> false


type captured_info = {loc: Location.t; is_formal: Procname.t option}
[@@deriving compare, equal, sexp, hash, normalize]

type t = {pvar: Pvar.t; typ: Typ.t; capture_mode: capture_mode; captured_from: captured_info option}
[@@deriving compare, equal, sexp, hash, normalize]

let captured_info_pp fmt {loc; is_formal} =
  F.fprintf fmt "(%a, %a)" (Pp.option Procname.pp) is_formal Location.pp loc


let pp fmt {pvar; typ; capture_mode; captured_from} =
  F.fprintf fmt "(%a,@,%a,@,%s,%a)" (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ
    (string_of_capture_mode capture_mode)
    (Pp.option (fun fmt info -> F.fprintf fmt "captured from %a" captured_info_pp info))
    captured_from
