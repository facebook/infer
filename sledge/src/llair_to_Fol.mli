(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Fol

val lookup_func : (string -> 'a option) -> Term.t -> 'a option
val global : Llair.Global.t -> Term.t
val reg : Llair.Reg.t -> Var.t
val regs : Llair.Reg.Set.t -> Var.Set.t
val term : Llair.Exp.t -> Term.t
val formula : Llair.Exp.t -> Formula.t
