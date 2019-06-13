(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Symbolic Execution *)

val assume : Exp.t -> Sh.t -> Sh.t option
val inst : Sh.t -> Llair.inst -> (Sh.t, unit) result

val intrinsic :
  Sh.t -> Var.t option -> Var.t -> Exp.t list -> (Sh.t, unit) result option
