(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

type t = Neg | Zero | Pos

val of_int : int -> t
val of_float : float -> t
