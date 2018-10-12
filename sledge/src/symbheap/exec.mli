(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

val assume : Exp.t -> Sh.t -> Sh.t option
val inst : Sh.t -> Llair.inst -> (Sh.t, Sh.t * Llair.inst) result
