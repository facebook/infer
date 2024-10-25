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

type context_info = {is_checked_for_null: bool; is_internal_pointer_of: Typ.t option}
[@@deriving compare, equal, sexp, hash, normalize]

type t =
  { pvar: Pvar.t
  ; typ: Typ.t
  ; capture_mode: capture_mode
  ; captured_from: captured_info option
  ; context_info: context_info option }
[@@deriving compare, equal, sexp, hash, normalize]

let pp_captured_info fmt {loc; is_formal} =
  F.fprintf fmt "(%a, %a)" (Pp.option Procname.pp) is_formal Location.pp loc


let pp_context_info fmt {is_checked_for_null; is_internal_pointer_of} =
  F.fprintf fmt "is_checked_for_null=%b, is_internal_pointer=%a" is_checked_for_null
    (Pp.option (Typ.pp Pp.text))
    is_internal_pointer_of


let pp fmt {pvar; typ; capture_mode; captured_from; context_info} =
  F.fprintf fmt "(%a,@,%a,@,%s%a%a)" (Pvar.pp Pp.text) pvar (Typ.pp_full Pp.text) typ
    (string_of_capture_mode capture_mode)
    (Pp.option (fun fmt info -> F.fprintf fmt ", captured from %a" pp_captured_info info))
    captured_from
    (Pp.option (fun fmt info -> F.fprintf fmt ", context info %a" pp_context_info info))
    context_info
