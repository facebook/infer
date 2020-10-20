(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Function Symbols *)

type t =
  | Float of string
  | Label of {parent: string; name: string}
  | Mul
  | Div
  | Rem
  | EmptyRecord
  | RecRecord of int
  | BitAnd
  | BitOr
  | BitXor
  | BitShl
  | BitLshr
  | BitAshr
  | Signed of int
  | Unsigned of int
  | Convert of {src: Llair.Typ.t; dst: Llair.Typ.t}
  | Uninterp of string
[@@deriving compare, equal, sexp]

val pp : t pp
