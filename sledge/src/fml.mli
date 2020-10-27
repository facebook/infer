(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Formulas *)

open Propositional_intf
include FORMULA with type trm := Trm.trm
module Fmls : FORMULA_SET with type elt := fml with type t = fmls

type trm := Trm.trm

val tt : fml
val ff : fml
val bool : bool -> fml
val _Eq0 : trm -> fml
val _Pos : trm -> fml
val _Eq : trm -> trm -> fml
val vars : fml -> Trm.Var.t iter
