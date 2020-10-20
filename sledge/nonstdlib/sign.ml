(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

type t = Neg | Zero | Pos

let of_int i = if i < 0 then Neg else if i = 0 then Zero else Pos

let of_float f =
  if Float.(f < 0.) then Neg else if Float.(f = 0.) then Zero else Pos
