(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function Symbols *)

type t =
  | Mul
  | Div
  | Rem
  | BitAnd
  | BitOr
  | BitXor
  | BitShl
  | BitLshr
  | BitAshr
  | Signed of int
  | Unsigned of int
  | Uninterp of string
[@@deriving compare, equal, sexp]

let pp ppf f =
  let pf fmt = Format.fprintf ppf fmt in
  match f with
  | Mul -> pf "@<1>×"
  | Div -> pf "/"
  | Rem -> pf "%%"
  | BitAnd -> pf "&&"
  | BitOr -> pf "||"
  | BitXor -> pf "xor"
  | BitShl -> pf "shl"
  | BitLshr -> pf "lshr"
  | BitAshr -> pf "ashr"
  | Signed n -> pf "(s%i)" n
  | Unsigned n -> pf "(u%i)" n
  | Uninterp sym -> pf "%s" sym
