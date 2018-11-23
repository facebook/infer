(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let utf8 =
  let rec from_vars = function
    | [] ->
        false
    | var :: otherwise -> (
      match Sys.getenv var with
      | Some ("C" | "POSIX") ->
          false
      | None | Some "" ->
          from_vars otherwise
      | Some lc ->
          String.is_suffix lc ~suffix:"UTF-8" )
  in
  from_vars ["LC_ALL"; "LC_CTYPE"; "LANG"]


let dot_operator = if utf8 then "⋅" else "."

let down_tack = if utf8 then "⊤" else "T"

let leftwards_double_arrow = if utf8 then "⇐" else "<="

let multiplication_sign = if utf8 then "×" else "x"

let right_tack = if utf8 then "⊢" else "|-"

let superscript_digits =
  if utf8 then ("", [|"⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"|])
  else ("^", [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|])


let up_tack = if utf8 then "⊥" else "_|_"
